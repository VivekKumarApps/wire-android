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

import android.content.Context
import android.graphics.Color
import android.os.Build
import android.os.Handler
import android.os.Looper
import android.util.AttributeSet
import android.util.TypedValue
import android.view.{Gravity, View, ViewGroup}
import android.view.View.{VISIBLE, GONE}
import android.widget.FrameLayout
import com.waz.utils.returning
import com.waz.zclient.{R, ViewHelper}
import com.waz.zclient.utils.ViewUtils


class LoadingIndicatorView(context: Context, attrs: AttributeSet, defStyle: Int) extends FrameLayout(context, attrs, defStyle) with ViewHelper {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null)

  import LoadingIndicatorView._

  private val animations = Map(
    INFINITE_LOADING_BAR -> new Runnable() {
      override def run(): Unit = if (setToVisible) {
        progressView.setVisibility(GONE)
        infiniteLoadingBarView.setVisibility(VISIBLE)
        progressLoadingBarView.setVisibility(GONE)
        setBackgroundColor(Color.TRANSPARENT)
        ViewUtils.fadeInView(LoadingIndicatorView.this)
      }
    },
    SPINNER -> new Runnable() {
      override def run(): Unit = if (setToVisible) {
        progressView.setVisibility(VISIBLE)
        infiniteLoadingBarView.setVisibility(GONE)
        progressLoadingBarView.setVisibility(GONE)
        setBackgroundColor(Color.TRANSPARENT)
        ViewUtils.fadeInView(LoadingIndicatorView.this)
      }
    },
    SPINNER_WITH_DIMMED_BACKGROUND -> new Runnable() {
      override def run(): Unit = if (setToVisible) {
        progressView.setVisibility(VISIBLE)
        infiniteLoadingBarView.setVisibility(GONE)
        progressLoadingBarView.setVisibility(GONE)
        setBackgroundColor(backgroundColor)
        ViewUtils.fadeInView(LoadingIndicatorView.this)
      }
    },
    PROGRESS_LOADING_BAR -> new Runnable() {
      override def run(): Unit = if (setToVisible) {
        progressView.setVisibility(GONE)
        infiniteLoadingBarView.setVisibility(GONE)
        progressLoadingBarView.setVisibility(VISIBLE)
        setBackgroundColor(Color.TRANSPARENT)
        ViewUtils.fadeInView(LoadingIndicatorView.this)
      }
    }
  )

  private val hideRunnable = new Runnable() {
    override def run(): Unit = {
      ViewUtils.fadeOutView(LoadingIndicatorView.this)
    }
  }

  private lazy val infiniteLoadingBarView = returning(new InfiniteLoadingBarView(context)) { view =>
    view.setVisibility(View.GONE)
    addView(view, new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT))
  }

  private lazy val progressLoadingBarView = returning(new ProgressLoadingBarView(context)) { view =>
    view.setVisibility(View.GONE)
    addView(view, new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT))
  }

  private lazy val progressView = returning(new ProgressView(context)) { view =>
    view.setTextColor(Color.WHITE)
    view.setTextSize(TypedValue.COMPLEX_UNIT_PX, getContext.getResources.getDimensionPixelSize(R.dimen.loading_spinner__size))
    view.setVisibility(View.GONE)

    val params = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT)
    params.gravity = Gravity.CENTER
    addView(view, params)
  }

  private val handler = new Handler(Looper.getMainLooper)
  private var setToVisible = false
  private var backgroundColor = 0

  def show(animationType: AnimationType): Unit = show(animationType, 0)

  def show(animationType: AnimationType, darkTheme: Boolean): Unit = {
    if (darkTheme) applyDarkTheme() else applyLightTheme()
    show(animationType)
  }

  def show(animationType: AnimationType, delayMs: Long): Unit = {
    setToVisible = true
    handler.removeCallbacks(null)
    handler.postDelayed(animations(animationType), delayMs)
  }

  def hide(): Unit = {
    setToVisible = false
    handler.removeCallbacks(null)
    handler.post(hideRunnable)
  }

  def setColor(color: Int): Unit = {
    infiniteLoadingBarView.setColor(color)
    progressLoadingBarView.setColor(color)
  }

  def setProgress(progress: Float): Unit = progressLoadingBarView.setProgress(progress)

  def applyLightTheme(): Unit = {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.M) { //noinspection deprecation
      progressView.setTextColor(getResources.getColor(R.color.text__primary_light))
      backgroundColor = getResources.getColor(R.color.text__primary_disabled_dark)
    }
    else {
      progressView.setTextColor(getResources.getColor(R.color.text__primary_light, getContext.getTheme))
      backgroundColor = getResources.getColor(R.color.text__primary_disabled_dark, getContext.getTheme)
    }
  }

  def applyDarkTheme(): Unit = {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.M) {
      progressView.setTextColor(getResources.getColor(R.color.text__primary_dark))
      backgroundColor = getResources.getColor(R.color.text__primary_disabled_light)
    }
    else {
      progressView.setTextColor(getResources.getColor(R.color.text__primary_dark, getContext.getTheme))
      backgroundColor = getResources.getColor(R.color.text__primary_disabled_light, getContext.getTheme)
    }
  }

}

object LoadingIndicatorView extends Enumeration {
  type AnimationType = Int

  val INFINITE_LOADING_BAR = 0
  val SPINNER = 1
  val SPINNER_WITH_DIMMED_BACKGROUND = 2
  val PROGRESS_LOADING_BAR = 3
}
