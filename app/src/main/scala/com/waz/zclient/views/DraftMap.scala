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
package com.waz.zclient.views

import com.waz.ZLog.ImplicitTag._

import scala.collection.mutable
import com.waz.model.ConvId
import com.waz.utils.events.Signal
import com.waz.zclient.{Injectable, Injector}
import com.waz.zclient.conversation.ConversationController

import scala.concurrent.{ExecutionContext, Future}

class DraftMap(implicit injector: Injector) extends Injectable {
  private val map = mutable.HashMap[ConvId, String]()
  private lazy val conversationController = inject[ConversationController]

  def setCurrent(text: String)(implicit ec: ExecutionContext): Future[Unit] = conversationController.currentConvId.head.map { id => set(id, text) }
  def set(id: ConvId, text: String): Unit = map.put(id, text)

  def get(id: ConvId): String = map.getOrElse(id, "")

  val currentDraft: Signal[String] = conversationController.currentConvId.map(id => map.getOrElse(id, "") )
  def withCurrentDraft(f: (String) => Unit)(implicit ec: ExecutionContext): Future[Unit] = currentDraft.head.map( f )

  def tearDown(): Unit = map.clear()
}
