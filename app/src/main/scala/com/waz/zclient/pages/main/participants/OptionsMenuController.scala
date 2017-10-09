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
package com.waz.zclient.pages.main.participants

import android.content.Context
import com.waz.model.{ConvId, ConversationData, UserId}
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, Signal}
import com.waz.zclient.conversation.ConversationController
import com.waz.zclient.{Injectable, Injector}
import com.waz.zclient.utils.Callback

class OptionsMenuController(implicit injector: Injector, context: Context, ec: EventContext) extends Injectable {
  import Threading.Implicits.Ui

  val convId = Signal(Option.empty[ConvId])

  private val convController = inject[ConversationController]

  private var convChangedCallback = Option.empty[Callback[ConversationData]]

  def onMenuConversationHasChanged(callback: Callback[ConversationData]): Unit = convChangedCallback = Some(callback)

  convId.flatMap {
    case Some(id)   => convController.conversationChanged(id)
    case _          => Signal.const(Option.empty[ConversationData])
  } {
    case Some(conv) => convChangedCallback.foreach(_.callback(conv))
    case _          =>
  }

  def setConversationId(convId: ConvId): Unit = this.convId ! Some(convId)

  def withConvId(callback: Callback[ConvId]): Unit = convId.head.foreach {
    case Some(id) => callback.callback(id)
    case _        => callback.callback(null)
  }

  def withConv(callback: Callback[ConversationData]): Unit = convId.head.foreach {
    case Some(id) => convController.withConvLoaded(id, callback)
    case _        =>
  }

  def getOtherParticipant(convId: ConvId): UserId = ConversationController.getOtherParticipantForOneToOneConv(convId)
}
