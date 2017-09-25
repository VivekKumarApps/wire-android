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
import com.waz.utils.events.{EventContext, Signal}
import com.waz.zclient.controllers.UserAccountsController
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.utils.Callback
import com.waz.zclient.{BaseActivity, Injectable, Injector}

import scala.concurrent.Future
import scala.collection.JavaConversions._

class ConversationController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui

  private val zms = inject[Signal[ZMessaging]]
  private val userAccounts = inject[UserAccountsController]
  private val storage = zms.map(_.convsStorage)
  private val stats = zms.map(_.convsStats)

  private var previousConvId = Option.empty[ConvId]
  private var currentConvId = Option.empty[ConvId]

  def loadConversation(id: ConvId): Future[Option[ConversationData]] = storage.head.flatMap(_.get(id))

  def isGroup(id: ConvId): Future[Boolean] = loadConversation(id).flatMap {
    case None => Future.successful(false)
    case Some(conv) => isGroup(conv)
  }

  def isGroup(conv: ConversationData): Future[Boolean] =
    if (conv.team.isEmpty) Future.successful(conv.convType == ConversationType.Group)
    else zms.map(_.membersStorage).head.flatMap(_.getByConv(conv.id)).map(_.map(_.userId).size > 2)

  def createAndOpenConversation(users: Array[UserId], requester: ConversationChangeRequester,  activity: BaseActivity): Future[Unit] =
    for {
      z <- zms.head
      user <- z.usersStorage.get(z.selfUserId)
      conv <- if (users.length == 1 && !userAccounts.isTeamAccount) z.convsUi.getOrCreateOneToOneConversation(users.head)
              else z.convsUi.createGroupConversation(ConvId(), users, userAccounts.teamId)
    } yield selectConversation(Option(conv.id))

  def selectConversation(convId: Option[ConvId]): Future[Unit] = stats.head.flatMap(_.selectConversation(convId))

  val selectedConversationId: Signal[Option[ConvId]] = stats.flatMap(_.selectedConversationId)
  val prevAndCur = selectedConversationId.map { id =>
    if (currentConvId.isDefined) previousConvId = currentConvId
    currentConvId = id
    (previousConvId, currentConvId)
  }

  val selectedConversation: Signal[Option[ConversationData]] = selectedConversationId.flatMap {
    case Some(convId) => storage.flatMap(_.optSignal(convId))
    case None => Signal.const(None)
  }

  val selectedConversationIsGroup: Signal[Boolean] = selectedConversation.flatMap {
    case Some(conv) => Signal.future(isGroup(conv))
    case None => Signal.const(false)
  }

  def onSelectedConversationIsGroup(callback: Callback[java.lang.Boolean]): Unit = { // TODO: remove when not used anymore
    selectedConversationIsGroup.on(Threading.Ui) { b => callback.callback(b) }
  }

  val selectedConversationVerified: Signal[Boolean] = selectedConversation.map(_.fold(false)(_.verified == Verification.VERIFIED))

  def unreadCountForConv(conversationData: ConversationData): Int =
    if (conversationData.archived || conversationData.muted || conversationData.hidden) 0
    else conversationData.unreadCount.total

  val selectedConversationName: Signal[Option[String]] = selectedConversation.map(_.flatMap(_.name))

  def onSelectedConversationName(callback: Callback[String]): Unit = { // TODO: remove when not used anymore
    selectedConversationName.on(Threading.Ui) { _.foreach(callback.callback) }
  }

  val selectedConversationIsActive: Signal[Boolean] = selectedConversation.map {
    case Some(conv) => conv.isActive
    case None => false
  }

  def onSelectedConversationIsActive(callback: Callback[java.lang.Boolean]): Unit = { // TODO: remove when not used anymore
    selectedConversationIsActive.on(Threading.Ui) { b => callback.callback(b) }
  }
}
