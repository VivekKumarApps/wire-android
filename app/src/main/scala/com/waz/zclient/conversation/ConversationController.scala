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
import com.waz.api.Verification
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

import scala.concurrent.Future

class ConversationController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui

  private val zms = inject[Signal[ZMessaging]]
  private val userAccounts = inject[UserAccountsController]
  private val storage = zms.map(_.convsStorage)
  private val stats = zms.map(_.convsStats)

  private var currentConvId = Option.empty[ConvId]

  val selectedConvId = zms.flatMap(_.convsStats.selectedConversationId).collect { case Some(convId) => convId }
  def getSelectedConvId(): ConvId = selectedConvId.currentValue.getOrElse(null); // TODO: remove when not used anymore

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

  val selectedConvIsGroup: Signal[Boolean] = selectedConv.flatMap {
    case Some(conv) => Signal.future(isGroup(conv))
    case None => Signal.const(false)
  }

  def onSelectedConvIsGroup(callback: Callback[java.lang.Boolean]): Unit = { // TODO: remove when not used anymore
    selectedConvIsGroup.on(Threading.Ui) { b => callback.callback(b) }
  }

  val selectedConvIsVerified: Signal[Boolean] = selectedConv.map(_.fold(false)(_.verified == Verification.VERIFIED))

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

}

object ConversationController {
  case class ConversationChange(from: Option[ConvId], to: Option[ConvId], requester: ConversationChangeRequester) {
    def fromConversation(): ConvId = from.getOrElse(null) // TODO: remove when not used anymore
    def toConversation(): ConvId = to.getOrElse(null) // TODO: remove when not used anymore
  }
}
