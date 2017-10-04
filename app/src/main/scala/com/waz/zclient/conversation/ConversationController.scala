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
import com.waz.model.{ConvId, ConversationData, UserId}
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, EventStream, Signal}
import com.waz.zclient.controllers.UserAccountsController
import com.waz.zclient.conversation.ConversationController.ConversationChange
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.utils.Callback
import com.waz.zclient.{BaseActivity, Injectable, Injector}
import com.waz.ZLog.ImplicitTag._
import com.waz.api
import com.waz.api.MessageContent.Asset.ErrorHandler
import com.waz.api.impl.{AssetForUpload, ImageAsset}
import com.waz.utils.wrappers.URI

import scala.concurrent.Future

class ConversationController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui

  private val zms = inject[Signal[ZMessaging]]
  private val userAccounts = inject[UserAccountsController]
  private val storage = zms.map(_.convsStorage)
  private val stats = zms.map(_.convsStats)

  private var currentConvId = Option.empty[ConvId]

  val selectedConvId = zms.flatMap(_.convsStats.selectedConversationId).collect { case Some(convId) => convId }
  def getSelectedConvId(): ConvId = selectedConvId.currentValue.orNull; // TODO: remove when not used anymore

  // this should be the only UI entry point to change conv in SE
  def selectConv(id: Option[ConvId], requester: ConversationChangeRequester = ConversationChangeRequester.UPDATER): Future[Unit] = id match {
    case None => Future.successful({})
    case Some(convId) if currentConvId.contains(convId) => Future.successful({})
    case Some(convId) =>
      stats.head.flatMap(_.selectConversation(id)).map { _ =>
        convChanged ! ConversationChange(from = currentConvId, to = id, requester = requester)
        currentConvId = id
      }
  }

  val convChanged = EventStream[ConversationChange]()

  def onConvChanged(callback: Callback[ConversationChange]): Unit = { // TODO: remove when not used anymore
    convChanged.on(Threading.Ui) { cc => callback.callback(cc) }
  }

  def loadConv(id: ConvId): Future[Option[ConversationData]] = storage.head.flatMap(_.get(id))

  def onConvLoaded(id: ConvId, callback: Callback[ConversationData]) =  // TODO: remove when not used anymore
    loadConv(id).foreach {
      case Some(data) => callback.callback(data)
      case None =>
    }

  def isGroup(id: ConvId): Future[Boolean] = loadConv(id).flatMap {
    case None => Future.successful(false)
    case Some(conv) => isGroup(conv)
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

  val selectedConv: Signal[Option[ConversationData]] = selectedConvId.flatMap { id => storage.flatMap(_.optSignal(id)) }

  def onSelectedConv(callback: Callback[ConversationData]): Unit = { // TODO: remove when not used anymore
    selectedConv.collect { case Some(c) => c } .on(Threading.Ui) { c => callback.callback(c) }
  }

  def withSelectedConv(callback: Callback[ConversationData]): Unit = { // TODO: remove when not used anymore
    selectedConv.collect { case Some(c) => c }.currentValue.foreach( callback.callback )
  }

  val selectedConvIsGroup: Signal[Boolean] = selectedConv.flatMap {
    case Some(conv) => Signal.future(isGroup(conv))
    case None => Signal.const(false)
  }

  def onSelectedConvIsGroup(callback: Callback[java.lang.Boolean]): Unit = { // TODO: remove when not used anymore
    selectedConvIsGroup.on(Threading.Ui) { b => callback.callback(b) }
  }

  val selectedConvIsVerified: Signal[Boolean] = selectedConv.map(_.fold(false)(_.verified == Verification.VERIFIED))
  val selectedConvIsEphemeral: Signal[Boolean] = selectedConv.map(_.fold(false)(_.ephemeral != EphemeralExpiration.NONE))

  def unreadCountForConv(conversationData: ConversationData): Int =
    if (conversationData.archived || conversationData.muted || conversationData.hidden) 0
    else conversationData.unreadCount.total

  val selectedConvName: Signal[Option[String]] = selectedConv.map(_.flatMap(_.name))

  def onSelectedConvName(callback: Callback[String]): Unit = { // TODO: remove when not used anymore
    selectedConvName.on(Threading.Ui) { _.foreach(callback.callback) }
  }

  val selectedConversationIsActive: Signal[Boolean] = selectedConv.map {
    case Some(conv) => conv.isActive
    case None => false
  }

  def onSelectedConversationIsActive(callback: Callback[java.lang.Boolean]): Unit = { // TODO: remove when not used anymore
    selectedConversationIsActive.on(Threading.Ui) { b => callback.callback(b) }
  }

  def sendMessage(uri: URI, errorHandler: ErrorHandler): Unit = selectedConvId.currentValue.foreach { convId => sendMessage(convId, uri, errorHandler) }
  def sendMessage(convId: ConvId, uri: URI, errorHandler: ErrorHandler): Unit = zms(_.convsUi.sendMessage(convId, uri, errorHandler))
  def sendMessage(audioAsset: AssetForUpload, errorHandler: ErrorHandler): Unit = selectedConvId.currentValue.foreach { convId => sendMessage(convId, audioAsset, errorHandler) }
  def sendMessage(convId: ConvId, audioAsset: AssetForUpload, errorHandler: ErrorHandler): Unit = zms(_.convsUi.sendMessage(convId, audioAsset, errorHandler))
  def sendMessage(text: String): Unit = selectedConvId.currentValue.foreach { convId => sendMessage(convId, text) }
  def sendMessage(convId: ConvId, text: String): Unit = zms(_.convsUi.sendMessage(convId, text))
  def sendMessage(text: String, mentions: Set[UserId]): Unit = selectedConvId.currentValue.foreach { convId => sendMessage(convId, text, mentions) }
  def sendMessage(convId: ConvId, text: String, mentions: Set[UserId]): Unit = zms(_.convsUi.sendMessage(convId, text, mentions))
  def sendMessage(jpegData: Array[Byte]): Unit = selectedConvId.currentValue.foreach { convId => sendMessage(convId, jpegData) }
  def sendMessage(convId: ConvId, jpegData: Array[Byte]): Unit = zms(_.convsUi.sendMessage(convId, jpegData))
  def sendMessage(imageAsset: ImageAsset): Unit = selectedConvId.currentValue.foreach { convId => sendMessage(convId, imageAsset) }
  def sendMessage(convId: ConvId, imageAsset: ImageAsset): Unit = zms(_.convsUi.sendMessage(convId, imageAsset))
  def sendMessage(location: api.MessageContent.Location): Unit = selectedConvId.currentValue.foreach { convId => sendMessage(convId, location) }
  def sendMessage(convId: ConvId, location: api.MessageContent.Location): Unit = zms(_.convsUi.sendMessage(convId, location))
}

object ConversationController {
  case class ConversationChange(from: Option[ConvId], to: Option[ConvId], requester: ConversationChangeRequester) {
    def fromConversation(): ConvId = from.orNull // TODO: remove when not used anymore
    def toConversation(): ConvId = to.orNull // TODO: remove when not used anymore
  }
}
