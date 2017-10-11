/**
 * Wire
 * Copyright (C) 2017 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.zclient.conversation

import android.content.Context
import com.waz.api.{EphemeralExpiration, Verification}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{ConvId, ConversationData, UserData, UserId}
import com.waz.service.ZMessaging
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.{EventContext, EventStream, Signal}
import com.waz.zclient.controllers.UserAccountsController
import com.waz.zclient.conversation.ConversationController.ConversationChange
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.utils.Callback
import com.waz.zclient.{BaseActivity, Injectable, Injector}
import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api
import com.waz.api.MessageContent.Asset.ErrorHandler
import com.waz.api.impl.{AssetForUpload, ImageAsset}
import com.waz.model.otr.Client
import com.waz.utils.Serialized
import com.waz.utils.wrappers.URI

import scala.concurrent.Future
import scala.collection.JavaConverters._

class ConversationController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui

  private val zms = inject[Signal[ZMessaging]]
  private val userAccounts = inject[UserAccountsController]
  private val storage = zms.map(_.convsStorage)
  private val stats = zms.map(_.convsStats)

  private var currentConvId = Option.empty[ConvId]

  val selectedConvId = zms.flatMap(_.convsStats.selectedConversationId).collect { case Some(convId) =>
    verbose(s"selected conv id changed to $convId")
    convId
  }

  for {
    z <- zms
    id <- selectedConvId
  } yield z.conversations.forceNameUpdate(id)

  def getSelectedConvId(): ConvId = selectedConvId.currentValue.orNull; // TODO: remove when not used anymore

  // this should be the only UI entry point to change conv in SE
  def selectConv(id: Option[ConvId], requester: ConversationChangeRequester): Future[Unit] = id match {
    case None => Future.successful({})
    case Some(convId) if currentConvId.contains(convId) => Future.successful({})
    case Some(convId) =>
      stats.head.flatMap(_.selectConversation(id)).map { _ =>
        verbose(s"selectConv $id")
        convChanged ! ConversationChange(from = currentConvId, to = id, requester = requester)
        currentConvId = id
      }
  }

  def selectConv(id: ConvId, requester: ConversationChangeRequester): Future[Unit] = selectConv(Some(id), requester)

  val convChanged = EventStream[ConversationChange]()

  def onConvChanged(callback: Callback[ConversationChange]): Unit = { // TODO: remove when not used anymore
    convChanged.on(Threading.Ui) { cc => callback.callback(cc) }
  }

  def changeWithinConv(convId: ConvId): Signal[Option[ConversationData]] = for {
    z <- zms
    conv <- z.convsStorage.optSignal(convId)
  } yield conv

  def loadConv(id: ConvId): Future[Option[ConversationData]] = storage.head.flatMap(_.get(id))

  def withConvLoaded(id: ConvId, callback: Callback[ConversationData]): Unit =  // TODO: remove when not used anymore
    loadConv(id).foreach {
      case Some(data) => callback.callback(data)
      case None =>
    }

  def isGroup(conv: ConversationData): Future[Boolean] =
    if (conv.team.isEmpty) Future.successful(conv.convType == ConversationType.Group)
    else zms.map(_.membersStorage).head.flatMap(_.getByConv(conv.id)).map(_.map(_.userId).size > 2)

  def createAndOpenConv(users: Array[UserId], requester: ConversationChangeRequester, activity: BaseActivity): Future[Unit] =
    for {
      z <- zms.head
      user <- z.usersStorage.get(z.selfUserId)
      conv <- if (users.length == 1 && !userAccounts.isTeamAccount) z.convsUi.getOrCreateOneToOneConversation(users.head)
              else z.convsUi.createGroupConversation(ConvId(), users, userAccounts.teamId)
    } yield selectConv(Option(conv.id), ConversationChangeRequester.START_CONVERSATION)

  def getOrCreateConv(userId: UserId): Future[ConversationData] = for {
    z <- zms.head
    conv <- z.convsUi.getOrCreateOneToOneConversation(userId)
  } yield conv

  val selectedConv: Signal[Option[ConversationData]] = selectedConvId.flatMap { id => storage.flatMap(_.optSignal(id)) }

  def withSelectedConv(callback: Callback[ConversationData]): Unit = { // TODO: remove when not used anymore
    selectedConv.collect { case Some(c) => c }.head.foreach( callback.callback )
  }

  def withSelectedConv(f: (ConversationData) => Unit): Future[Unit] =
    selectedConv.collect { case Some(c) => c }.head.map( f )

  val selectedConvIsGroup: Signal[Boolean] = selectedConv.flatMap {
    case Some(conv) => Signal.future(isGroup(conv))
    case None => Signal.const(false)
  }

  val selectedConvIsVerified: Signal[Boolean] = selectedConv.map(_.fold(false)(_.verified == Verification.VERIFIED))

  def unreadCountForConv(conversationData: ConversationData): Int =
    if (conversationData.archived || conversationData.muted || conversationData.hidden) 0
    else conversationData.unreadCount.total

  val selectedConversationIsActive: Signal[Boolean] = selectedConv.map {
    case Some(conv) => conv.isActive
    case None => false
  }

  def setEphemeralExpiration(expiration: EphemeralExpiration): Future[Unit] = for {
    z <- zms.head
    id <- selectedConvId.head
    _ <- z.convsUi.setEphemeral(id, expiration)
  } yield ()

  def loadMembers(convId: ConvId): Future[Seq[UserData]] = for {
    z <- zms.head
    userIds <- z.membersStorage.activeMembers(convId).head
    users <- z.users.getUsers(userIds.toSeq)
  } yield users

  def withMembers(convId: ConvId, callback: Callback[java.util.Collection[UserData]]): Unit = // TODO: remove when not used anymore
    loadMembers(convId).foreach { users => callback.callback(users.asJavaCollection) }
  def withCurrentConvMembers(callback: Callback[java.util.Collection[UserData]]): Unit = selectedConvId.head.foreach { id => withMembers(id, callback) }

  def loadClients(userId: UserId): Future[Seq[Client]] = zms.head.flatMap(_.otrClientsStorage.getClients(userId)) // TODO: move to SE maybe?

  def sendMessage(uri: URI, errorHandler: ErrorHandler): Future[Unit] = selectedConvId.head.map { convId => sendMessage(convId, uri, errorHandler) }
  def sendMessage(convId: ConvId, uri: URI, errorHandler: ErrorHandler): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, uri, errorHandler) }
  def sendMessage(audioAsset: AssetForUpload, errorHandler: ErrorHandler): Future[Unit] = selectedConvId.head.map { convId => sendMessage(convId, audioAsset, errorHandler) }
  def sendMessage(convId: ConvId, audioAsset: AssetForUpload, errorHandler: ErrorHandler): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, audioAsset, errorHandler) }
  def sendMessage(text: String): Future[Unit] = selectedConvId.head.map { convId => sendMessage(convId, text) }
  def sendMessage(convId: ConvId, text: String): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, text) }
  def sendMessage(text: String, mentions: Set[UserId]): Future[Unit] = selectedConvId.head.map { convId => sendMessage(convId, text, mentions) }
  def sendMessage(convId: ConvId, text: String, mentions: Set[UserId]): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, text, mentions) }
  def sendMessage(jpegData: Array[Byte]): Future[Unit] = selectedConvId.head.map { convId => sendMessage(convId, jpegData) }
  def sendMessage(convId: ConvId, jpegData: Array[Byte]): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, jpegData) }
  def sendMessage(imageAsset: com.waz.api.ImageAsset): Future[Unit] = imageAsset match { // TODO: remove when not used anymore
    case a: com.waz.api.impl.ImageAsset => selectedConvId.head.map { convId => sendMessage(convId, a) }
    case _ => Future.successful({})
  }
  def sendMessage(imageAsset: ImageAsset): Future[Unit] = selectedConvId.head.map { convId => sendMessage(convId, imageAsset) }
  def sendMessage(convId: ConvId, imageAsset: ImageAsset): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, imageAsset) }
  def sendMessage(location: api.MessageContent.Location): Future[Unit] = selectedConvId.head.map { convId => sendMessage(convId, location) }
  def sendMessage(convId: ConvId, location: api.MessageContent.Location): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, location) }

  def setCurrentConvName(name: String): Future[Unit] = selectedConvId.head.map { id => setName(id, name) }
  def setName(id: ConvId, name: String): Future[Unit] = for {
    z <- zms.head
    Some(conv) <- loadConv(id)
  } yield if (conv.displayName != name) z.convsUi.setConversationName(id, name)  else Future.successful({})

  def addMembers(id: ConvId, users: java.util.List[UserId]): Unit = addMembers(id, users.asScala.toSet) // TODO: remove when not used anymore
  def addMembers(id: ConvId, users: Set[UserId]): Future[Unit] = zms.head.map { _.convsUi.addConversationMembers(id, users.toSeq) }

  def removeMember(id: ConvId, user: UserId): Future[Unit] = zms.head.map { _.convsUi.removeConversationMember(id, user) }

  def leave(id: ConvId): CancellableFuture[Option[ConversationData]] = Serialized("Conversations", id) { CancellableFuture.lift( zms.head.flatMap { _.convsUi.leaveConversation(id) } ) }

  def setArchived(id: ConvId, archived: Boolean): Future[Unit] = zms.head.map { _.convsUi.setConversationArchived(id, archived) }

  def setMuted(id: ConvId, muted: Boolean): Future[Unit] = zms.head.map { _.convsUi.setConversationMuted(id, muted) }

  def delete(id: ConvId, alsoLeave: Boolean): CancellableFuture[Option[ConversationData]] =
    if (alsoLeave) leave(id).flatMap(_ => clear(id))
    else clear(id)

  def clear(id: ConvId): CancellableFuture[Option[ConversationData]] = Serialized("Conversations", id) { CancellableFuture.lift( zms.head.flatMap { _.convsUi.clearConversation(id) } ) }

  def createGroupConversation(users: Seq[UserId], localId: ConvId = ConvId()): Future[ConversationData] =
    zms.head.flatMap { _.convsUi.createGroupConversation(localId, users) }

  def createGroupConversation(users: java.util.List[UserId], conversationChangerSender: ConversationChangeRequester): Unit = // TODO: remove when not used anymore
    createGroupConversation(users.asScala).map { data =>
      selectConv(Some(data.id),
        if (conversationChangerSender != ConversationChangeRequester.START_CONVERSATION_FOR_CALL &&
          conversationChangerSender != ConversationChangeRequester.START_CONVERSATION_FOR_VIDEO_CALL &&
          conversationChangerSender != ConversationChangeRequester.START_CONVERSATION_FOR_CAMERA) ConversationChangeRequester.START_CONVERSATION
        else conversationChangerSender
      )
    }

  def knock(id: ConvId): Unit = zms(_.convsUi.knock(id))
}

object ConversationController {
  case class ConversationChange(from: Option[ConvId], to: Option[ConvId], requester: ConversationChangeRequester) {
    def fromConversation(): ConvId = from.orNull // TODO: remove when not used anymore
    def toConversation(): ConvId = to.orNull // TODO: remove when not used anymore
    lazy val noChange: Boolean = from == to
  }

  val emptyImageAsset: com.waz.api.ImageAsset = ImageAsset.Empty.asInstanceOf[com.waz.api.ImageAsset]

  def getOtherParticipantForOneToOneConv(id: ConvId): UserId = UserId(id.str) // one-to-one conversation has the same id as the other user, so we can access it directly
}
