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
/**
  * Wire
  * Copyright (C) 2016 Wire Swiss GmbH
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
  *//**
  * Wire
  * Copyright (C) 2016 Wire Swiss GmbH
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

import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.View
import android.widget.FrameLayout
import android.widget.TextView
import com.waz.zclient.{R, ViewHelper}
import com.waz.zclient.utils.ViewUtils
import com.waz.model.UserData
import com.waz.service.ZMessaging
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.Signal
import com.waz.zclient.conversation.ConversationController

import scala.concurrent.duration._

class TypingIndicatorView(val context: Context, val attrs: AttributeSet, val defStyleAttr: Int) extends FrameLayout(context, attrs, defStyleAttr) with ViewHelper {
  import Threading.Implicits.Ui

  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null)

  inflate(R.layout.typing_indicator)

  private val nameTextView: TextView = ViewUtils.getView(this, R.id.ttv__typing_indicator_names)
  private val dotsView: View = ViewUtils.getView(this, R.id.gtv__is_typing_dots)
  private val penView: View = ViewUtils.getView(this, R.id.gtv__is_typing_pen)
  private val animationRunning = Signal(false)

  private lazy val zms: Signal[ZMessaging] = inject[Signal[ZMessaging]]
  private lazy val convController = inject[ConversationController]

  (for {
    z <- zms
    convId <- convController.selectedConvId
    userIds <- z.typing.typingUsers(convId)
    userList <- Signal.future(z.users.getUsers(userIds))
  } yield userList) { userList =>
    update(userList)
  }

  private def update(userList: Seq[UserData]): Unit = userList.toList match {
    case Nil =>
      nameTextView.setText("")
      setVisibility(View.GONE)
      animationRunning ! false
    case users =>
      nameTextView.setText(users.map(_.displayName).mkString(", "))
      setVisibility(View.VISIBLE)
      animationRunning ! true
  }

  private var animationState = false

  animationRunning { st =>
    animationState = st
    if (st) runAnimation()
  }

  private def runAnimation(): Unit = if (animationState) {
    val stepDuration = getResources.getInteger(R.integer.animation_duration_medium_rare)
    val step = dotsView.getWidth / 3

    penView.animate.translationX(step).setDuration(stepDuration).start()

    CancellableFuture.delayed((stepDuration * 2).millis){
      penView.animate.translationX(step * 2).setDuration(stepDuration).start()
    }

    CancellableFuture.delayed((stepDuration * 4).millis){
      penView.animate.translationX(dotsView.getWidth).setDuration(stepDuration).start()
    }

    CancellableFuture.delayed((stepDuration * 8).millis){
      runAnimation()
    }
  }

  def clear(): Unit = {
    nameTextView.setText("")
    animationRunning ! false
  }
}
