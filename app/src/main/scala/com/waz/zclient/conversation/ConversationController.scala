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
import com.waz.api.{EphemeralExpiration, IConversation, Verification}
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
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
import com.waz.utils.{Serialized, returning}
import com.waz.utils.wrappers.URI
import com.waz.zclient.core.stores.IStoreFactory
import org.threeten.bp.Instant

import scala.concurrent.Future
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import com.waz.utils._

class ConversationController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui

  private val zms = inject[Signal[ZMessaging]]
  private val userAccounts = inject[UserAccountsController]
  private lazy val convStore = inject[IStoreFactory].conversationStore
  private val storage = zms.map(_.convsStorage)
  private val stats = zms.map(_.convsStats)

  private var lastConvId = Option.empty[ConvId]

  val currentConvId = zms.flatMap(_.convsStats.selectedConversationId).collect { case Some(convId) =>
    verbose(s"CC selected conv id changed to $convId")
    convId
  }

  val currentConv: Signal[Option[ConversationData]] = currentConvId.flatMap { id => storage.flatMap(_.optSignal(id)) } // updates on every change of the conversation data, not only on switching

  val currentConvType: Signal[Option[ConversationType]] = for { // convType can be changed only by switching to a new conversation
    id <- currentConvId
    conv <- Signal.future(loadConv(id))
  } yield conv.map(_.convType)

  val currentConvName: Signal[String] = currentConv.map { // the name of the current conversation can be edited (without switching)
    case Some(conv) => conv.displayName
    case _ => ""
  }

  for {
    z <- zms
    convId <- currentConvId
  } yield {
    verbose(s"CC forcing name update for $convId")
    z.conversations.forceNameUpdate(convId)
  }

  def getCurrentConvId(): ConvId = currentConvId.currentValue.orNull; // TODO: remove when not used anymore

  // this should be the only UI entry point to change conv in SE
  def selectConv(convId: Option[ConvId], requester: ConversationChangeRequester): Future[Unit] = convId match {
    case None => Future.successful({})
    case Some(_) =>
      stats.head.flatMap(_.selectConversation(convId)).map { _ =>
        verbose(s"CC selectConv $convId")
        convChanged ! ConversationChange(from = lastConvId, to = convId, requester = requester)
        lastConvId = convId
      }
  }

  def selectConv(id: ConvId, requester: ConversationChangeRequester): Future[Unit] = selectConv(Some(id), requester)

  val convChanged = EventStream[ConversationChange]()

  def onConvChanged(callback: Callback[ConversationChange]): Unit = { // TODO: remove when not used anymore
    convChanged.onUi { cc =>
      verbose(s"CC calling a callback onConversationChange from ${cc.from} to ${cc.to}")
      callback.callback(cc)
    }
  }

  val currentConvRequester: Signal[Option[ConversationChangeRequester]] = Signal.wrap(Option.empty[ConversationChangeRequester], convChanged.map(c => Option(c.requester)))

  def onConvRequesterChanged(callback: Callback[ConversationChangeRequester]): Unit =
    currentConvRequester.onUi { r => callback.callback(r.orNull) }

  def changeWithinConv(convId: ConvId): Signal[Option[ConversationData]] = for {
    z <- zms
    conv <- z.convsStorage.optSignal(convId)
  } yield {
    verbose(s"CC change within conv $convId")
    conv
  }

  def loadConv(convId: ConvId): Future[Option[ConversationData]] = {
    verbose(s"CC load conv $convId")
    storage.head.flatMap(_.get(convId))
  }

  def withConvLoaded(convId: ConvId, callback: Callback[ConversationData]): Unit =  // TODO: remove when not used anymore
    loadConv(convId).foreach {
      case Some(data) => callback.callback(data)
      case None =>
    }

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
  } yield {
    conv
  }

  def withCurrentConv(callback: Callback[ConversationData]): Unit =  // TODO: remove when not used anymore
    currentConv.collect { case Some(c) => c }.head.foreach( callback.callback )

  def withCurrentConvName(callback: Callback[String]): Unit = currentConvName.head.foreach(callback.callback) // TODO: remove when not used anymore
  def withCurrentConvType(callback: Callback[IConversation.Type]) = currentConvType.collect { case Some(t) => t }.head.foreach(callback.callback) // TODO: remove when not used anymore

  def withCurrentConv(f: (ConversationData) => Unit): Future[Unit] = currentConv.collect { case Some(c) => c }.head.map(f)

  val currentConvIsGroup: Signal[Boolean] = currentConv.flatMap {
    case Some(conv) =>
      if (conv.team.isEmpty) Signal.const(conv.convType == ConversationType.Group)
      else Signal.future(zms.map(_.membersStorage).head.flatMap(_.getByConv(conv.id)).map(_.map(_.userId).size > 2))
    case None => Signal.const(false)
  }

  def isGroup(conv: ConversationData): Future[Boolean] =
  if (conv.team.isEmpty) Future.successful(conv.convType == ConversationType.Group)
      else zms.map(_.membersStorage).head.flatMap(_.getByConv(conv.id)).map(_.map(_.userId).size > 2)

  val currentConvIsVerified: Signal[Boolean] = currentConv.map(_.fold(false)(_.verified == Verification.VERIFIED))

  def unreadCountForConv(conversationData: ConversationData): Int =
    if (conversationData.archived || conversationData.muted || conversationData.hidden) 0
    else conversationData.unreadCount.total

  def setEphemeralExpiration(expiration: EphemeralExpiration): Future[Unit] = for {
    z <- zms.head
    id <- currentConvId.head
    _ <- z.convsUi.setEphemeral(id, expiration)
  } yield ()

  def loadMembers(convId: ConvId): Future[Seq[UserData]] = for {
    z <- zms.head
    userIds <- z.membersStorage.activeMembers(convId).head
    users <- z.users.getUsers(userIds.toSeq)
  } yield {
    verbose(s"CC loadMembers($convId): ${users.size}")
    users
  }

  def withMembers(convId: ConvId, callback: Callback[java.util.Collection[UserData]]): Unit = // TODO: remove when not used anymore
    loadMembers(convId).foreach { users =>
      verbose(s"CC withMembers($convId): ${users.size}")
      callback.callback(users.asJavaCollection)
    }

  def withCurrentConvMembers(callback: Callback[java.util.Collection[UserData]]): Unit = currentConvId.head.foreach { id => withMembers(id, callback) }

  def loadClients(userId: UserId): Future[Seq[Client]] = zms.head.flatMap(_.otrClientsStorage.getClients(userId)) // TODO: move to SE maybe?

  def sendMessage(uri: URI, errorHandler: ErrorHandler): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, uri, errorHandler) }
  def sendMessage(convId: ConvId, uri: URI, errorHandler: ErrorHandler): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, uri, errorHandler) }
  def sendMessage(audioAsset: AssetForUpload, errorHandler: ErrorHandler): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, audioAsset, errorHandler) }
  def sendMessage(convId: ConvId, audioAsset: AssetForUpload, errorHandler: ErrorHandler): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, audioAsset, errorHandler) }
  def sendMessage(text: String): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, text) }
  def sendMessage(convId: ConvId, text: String): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, text) }
  def sendMessage(text: String, mentions: Set[UserId]): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, text, mentions) }
  def sendMessage(convId: ConvId, text: String, mentions: Set[UserId]): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, text, mentions) }
  def sendMessage(jpegData: Array[Byte]): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, jpegData) }
  def sendMessage(convId: ConvId, jpegData: Array[Byte]): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, jpegData) }
  def sendMessage(imageAsset: com.waz.api.ImageAsset): Future[Unit] = imageAsset match { // TODO: remove when not used anymore
    case a: com.waz.api.impl.ImageAsset => currentConvId.head.map { convId => sendMessage(convId, a) }
    case _ => Future.successful({})
  }
  def sendMessage(imageAsset: ImageAsset): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, imageAsset) }
  def sendMessage(convId: ConvId, imageAsset: ImageAsset): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, imageAsset) }
  def sendMessage(location: api.MessageContent.Location): Future[Unit] = currentConvId.head.map { convId => sendMessage(convId, location) }
  def sendMessage(convId: ConvId, location: api.MessageContent.Location): Future[Unit] = zms.head.map { _.convsUi.sendMessage(convId, location) }

  def setCurrentConvName(name: String): Future[Unit] = currentConvId.head.map { id => setName(id, name) }
  def setName(id: ConvId, name: String): Future[Unit] = for {
    z <- zms.head
    Some(conv) <- loadConv(id)
  } yield if (conv.displayName != name) z.convsUi.setConversationName(id, name)  else Future.successful({})

  def addMembers(id: ConvId, users: java.util.List[UserId]): Unit = addMembers(id, users.asScala.toSet) // TODO: remove when not used anymore
  def addMembers(id: ConvId, users: Set[UserId]): Future[Unit] = zms.head.map { _.convsUi.addConversationMembers(id, users.toSeq) }

  def removeMember(user: UserId): Future[Unit] = for {
    z <- zms.head
    id <- currentConvId.head
  } yield z.convsUi.removeConversationMember(id, user)

  def leave(convId: ConvId): CancellableFuture[Option[ConversationData]] =
    returning (Serialized("Conversations", convId) { CancellableFuture.lift( zms.head.flatMap { _.convsUi.leaveConversation(convId) } ) } ) { _ =>
      verbose(s"CC leave($convId)")
      currentConvId.head.map { id => if (id == convId) setCurrentConversationToNext(ConversationChangeRequester.LEAVE_CONVERSATION) }
    }

  def setCurrentConversationToNext(requester: ConversationChangeRequester): Future[Unit] =
    currentConvId.head.map { id => convStore.nextConversation(id) }.map { convId =>
      verbose(s"CC setCurrentConversationToNext: $convId")
      selectConv(convId, requester)
    }

  def archive(convId: ConvId, archive: Boolean): Unit = {
    zms.head.map { _.convsUi.setConversationArchived(convId, archive) }
    currentConvId.head.map { id => if (id == convId) CancellableFuture.delayed(ConversationController.ARCHIVE_DELAY){
      if (!archive) selectConv(convId, ConversationChangeRequester.CONVERSATION_LIST_UNARCHIVED_CONVERSATION)
      else setCurrentConversationToNext(ConversationChangeRequester.ARCHIVED_RESULT)
    }}
  }

  def setMuted(id: ConvId, muted: Boolean): Future[Unit] = zms.head.map { _.convsUi.setConversationMuted(id, muted) }

  def delete(id: ConvId, alsoLeave: Boolean): CancellableFuture[Option[ConversationData]] =
    if (alsoLeave) leave(id).flatMap(_ => clear(id)) else clear(id)

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

  def iConv(id: ConvId): IConversation = convStore.getConversation(id.str) // TODO: remove when not used anymore
  def iCurrentConv: IConversation = currentConvId.currentValue.map(iConv).orNull

  object messages {

    val ActivityTimeout = 3.seconds

    /**
      * Currently focused message.
      * There is only one focused message, switched by tapping.
      */
    val focused = Signal(Option.empty[MessageId])

    /**
      * Tracks last focused message together with last action time.
      * It's not cleared when message is unfocused, and toggleFocus takes timeout into account.
      * This is used to decide if timestamp view should be shown in footer when message has likes.
      */
    val lastActive = Signal((MessageId.Empty, Instant.EPOCH)) // message showing status info

    currentConv.onChanged { c =>
      verbose(s"selected conv changed: $c")
      clear()
    }

    def clear() = {
      focused ! None
      lastActive ! (MessageId.Empty, Instant.EPOCH)
    }

    def isFocused(id: MessageId): Boolean = focused.currentValue.flatten.contains(id)

    /**
      * Switches current msg focus state to/from given msg.
      */
    def toggleFocused(id: MessageId) = {
      verbose(s"toggleFocused($id)")
      focused mutate {
        case Some(`id`) => None
        case _ => Some(id)
      }
      lastActive.mutate {
        case (`id`, t) if !ActivityTimeout.elapsedSince(t) => (id, Instant.now - ActivityTimeout)
        case _ => (id, Instant.now)
      }
    }
  }
}

object ConversationController {
  val ARCHIVE_DELAY = 500.millis

  case class ConversationChange(from: Option[ConvId], to: Option[ConvId], requester: ConversationChangeRequester) {
    def fromConvId(): ConvId = from.orNull // TODO: remove when not used anymore
    def toConvId(): ConvId = to.orNull // TODO: remove when not used anymore
    lazy val noChange: Boolean = from == to
  }

  val emptyImageAsset: com.waz.api.ImageAsset = ImageAsset.Empty.asInstanceOf[com.waz.api.ImageAsset]

  def getOtherParticipantForOneToOneConv(conv: ConversationData): UserId = {
    if (conv != ConversationData.Empty &&
        conv.convType != IConversation.Type.ONE_TO_ONE &&
        conv.convType != IConversation.Type.WAIT_FOR_CONNECTION &&
        conv.convType != IConversation.Type.INCOMING_CONNECTION)
      error(s"unexpected call, most likely UI error", new UnsupportedOperationException(s"Can't get other participant for: ${conv.convType} conversation"))
    UserId(conv.id.str) // one-to-one conversation has the same id as the other user, so we can access it directly
  }

}
