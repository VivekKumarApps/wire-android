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

import android.content.{DialogInterface, Intent}
import android.os.{Build, Bundle, Handler}
import android.provider.MediaStore
import android.support.annotation.Nullable
import android.support.v4.app.{ActivityCompat, FragmentActivity}
import android.support.v7.widget.{ActionMenuView, Toolbar}
import android.text.TextUtils
import android.text.format.Formatter
import android.view._
import android.widget.{AbsListView, FrameLayout, TextView, Toast}
import com.waz.zclient.{FragmentHelper, R}
import com.waz.zclient.pages.BaseFragment
import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api._
import com.waz.api.impl.ContentUriAssetForUpload
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{AssetId, ConvId, ConversationData, MessageData}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.Signal
import com.waz.utils.returning
import com.waz.utils.wrappers.URI
import com.waz.zclient.camera.controllers.GlobalCameraController
import com.waz.zclient.controllers.{SharingController, ThemeController}
import com.waz.zclient.controllers.currentfocus.IFocusController
import com.waz.zclient.controllers.drawing.IDrawingController
import com.waz.zclient.controllers.navigation.Page
import com.waz.zclient.controllers.orientation.OrientationControllerObserver
import com.waz.zclient.controllers.permission.RequestPermissionsObserver
import com.waz.zclient.conversation.ConversationController.ConversationChange
import com.waz.zclient.conversation.{CollectionController, ConversationController}
import com.waz.zclient.core.controllers.tracking.attributes.OpenedMediaAction
import com.waz.zclient.core.controllers.tracking.events.media._
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.cursor.{CursorCallback, CursorView}
import com.waz.zclient.media.SoundController
import com.waz.zclient.messages.MessagesListView
import com.waz.zclient.pages.extendedcursor.ExtendedCursorContainer
import com.waz.zclient.pages.extendedcursor.emoji.EmojiKeyboardLayout
import com.waz.zclient.pages.extendedcursor.ephemeral.EphemeralLayout
import com.waz.zclient.pages.extendedcursor.image.{CursorImagesLayout, ImagePreviewLayout}
import com.waz.zclient.pages.extendedcursor.voicefilter.VoiceFilterLayout
import com.waz.zclient.pages.main.conversation.AssetIntentsManager
import com.waz.zclient.pages.main.pickuser.controller.IPickUserController
import com.waz.zclient.pages.main.profile.camera.CameraContext
import com.waz.zclient.tracking.GlobalTrackingController
import com.waz.zclient.ui.animation.interpolators.penner.Expo
import com.waz.zclient.ui.audiomessage.AudioMessageRecordingView
import com.waz.zclient.ui.cursor.CursorMenuItem
import com.waz.zclient.ui.utils.KeyboardUtils
import com.waz.zclient.utils.{AssetUtils, Callback, LayoutSpec, PermissionUtils, SquareOrientation, TrackingUtils, ViewUtils}

import scala.concurrent.duration._
import scala.collection.mutable

class ConvFragment extends BaseFragment[ConvFragment.Container] with FragmentHelper {
  import Threading.Implicits.Ui
  import ConvFragment._

  private val convController = inject[ConversationController]
  private val collectionController = inject[CollectionController]
  private val globalTrackingController = inject[GlobalTrackingController]

  private val previewShown = Signal(false)

  private val draftMap = inject[DraftMap]

  private var assetIntentsManager: Option[AssetIntentsManager] = None

  private lazy val typingIndicatorView = ViewUtils.getView(getView, R.id.tiv_typing_indicator_view).asInstanceOf[TypingIndicatorView]
  private lazy val containerPreview = ViewUtils.getView(getView, R.id.fl__conversation_overlay).asInstanceOf[ViewGroup]
  private lazy val cursorView = ViewUtils.getView(getView, R.id.cv__cursor).asInstanceOf[CursorView]
  private lazy val audioMessageRecordingView = ViewUtils.getView(getView, R.id.amrv_audio_message_recording).asInstanceOf[AudioMessageRecordingView]
  private lazy val extendedCursorContainer = ViewUtils.getView(getView, R.id.ecc__conversation).asInstanceOf[ExtendedCursorContainer] // this one now
  private val sharingUris = new mutable.ListBuffer[URI]()

  private lazy val leftMenu = returning(ViewUtils.getView(getView, R.id.conversation_left_menu).asInstanceOf[ActionMenuView]){
    _.setOnMenuItemClickListener(new ActionMenuView.OnMenuItemClickListener() {
      override def onMenuItemClick(item: MenuItem): Boolean = item.getItemId match {
        case R.id.action_collection =>
          collectionController.openCollection()
          true
        case _ => false
      }
    })
  }

  private lazy val toolbar = returning(ViewUtils.getView(getView, R.id.t_conversation_toolbar).asInstanceOf[Toolbar]) { toolbar =>

    toolbar.setOnClickListener(new View.OnClickListener() {
      override def onClick(v: View): Unit = getControllerFactory.getConversationScreenController.showParticipants(toolbar, false)
    })

    toolbar.setOnMenuItemClickListener(new Toolbar.OnMenuItemClickListener() {
      override def onMenuItemClick(item: MenuItem): Boolean = item.getItemId match {
        case R.id.action_audio_call =>
          getControllerFactory.getCallingController.startCall(false)
          cursorView.closeEditMessage(false)
          true
        case R.id.action_video_call =>
          getControllerFactory.getCallingController.startCall(true)
          cursorView.closeEditMessage(false)
          true
        case _ => false
      }
    })

    toolbar.setNavigationOnClickListener(new View.OnClickListener() {
      override def onClick(v: View): Unit =  if (cursorView != null) {
        cursorView.closeEditMessage(false)
        getActivity.onBackPressed()
        KeyboardUtils.closeKeyboardIfShown(getActivity)
      }
    })

    if (LayoutSpec.isTablet(getContext) && ViewUtils.isInLandscape(getContext)) toolbar.setNavigationIcon(null)
  }

  convController.selectedConversationIsActive.zip(convController.selectedConvIsGroup) {
    case (true, isGroup) =>
      inflateCollectionIcon()
      toolbar.getMenu.clear()
      toolbar.inflateMenu(if (isGroup) R.menu.conversation_header_menu_audio else R.menu.conversation_header_menu_video)
    case _ =>
  }

  private lazy val toolbarTitle = ViewUtils.getView(toolbar, R.id.tv__conversation_toolbar__title).asInstanceOf[TextView]
  private lazy val listView = ViewUtils.getView(getView, R.id.messages_list_view).asInstanceOf[MessagesListView]

  // invisible footer to scroll over inputfield
  private lazy val invisibleFooter = returning(new FrameLayout(getActivity)) {
    _.setLayoutParams(
      new AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, getResources.getDimensionPixelSize(R.dimen.cursor__list_view_footer__height))
    )
  }

  convController.selectedConvName {
    case Some(name) => toolbarTitle.setText(name)
    case None =>
  }

  override def onCreateView(inflater: LayoutInflater, viewGroup: ViewGroup, savedInstanceState: Bundle): View = {
    val view = inflater.inflate(R.layout.fragment_conversation, viewGroup, false)
    ViewUtils.getView(getView, R.id.sv__conversation_toolbar__verified_shield)

    // Recording audio messages
    audioMessageRecordingView.setCallback(audioMessageRecordingCallback)
    if (savedInstanceState != null) previewShown ! savedInstanceState.getBoolean(SAVED_STATE_PREVIEW, false)
    view
  }

  override def onViewCreated(view: View, @Nullable savedInstanceState: Bundle): Unit = {
    super.onViewCreated(view, savedInstanceState)
    audioMessageRecordingView.setVisibility(View.INVISIBLE)
  }

  override def onCreate(@Nullable savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    assetIntentsManager = Option(new AssetIntentsManager(getActivity, assetIntentsManagerCallback, savedInstanceState))
  }

  override def onStart(): Unit = {
    super.onStart()

    draftMap.withCurrentDraft { draftText => if (!TextUtils.isEmpty(draftText)) cursorView.setText(draftText) }

    getControllerFactory.getGlobalLayoutController.addKeyboardHeightObserver(extendedCursorContainer)
    getControllerFactory.getGlobalLayoutController.addKeyboardVisibilityObserver(extendedCursorContainer)
    extendedCursorContainer.setCallback(extendedCursorContainerCallback)

    getControllerFactory.getRequestPermissionsController.addObserver(requestPermissionsObserver)
    getControllerFactory.getOrientationController.addOrientationControllerObserver(orientationControllerObserver)
    cursorView.setCallback(cursorCallback)

    audioMessageRecordingView.setDarkTheme(inject[ThemeController].isDarkTheme)
    /*getControllerFactory.getNavigationController.addNavigationControllerObserver(this)
    getControllerFactory.getNavigationController.addPagerControllerObserver(this)
    getControllerFactory.getGiphyController.addObserver(this)
    getControllerFactory.getSingleImageController.addSingleImageObserver(this)
    getControllerFactory.getAccentColorController.addAccentColorObserver(this)
    getStoreFactory.participantsStore.addParticipantsStoreObserver(this)
    getControllerFactory.getGlobalLayoutController.addKeyboardVisibilityObserver(this)
    getStoreFactory.inAppNotificationStore.addInAppNotificationObserver(this)
    getControllerFactory.getSlidingPaneController.addObserver(this)*/

  }

  override def onResume(): Unit = {
    super.onResume()
  }

  override def onSaveInstanceState(outState: Bundle): Unit = {
    super.onSaveInstanceState(outState)
    assetIntentsManager.foreach { _.onSaveInstanceState(outState) }
    outState.putBoolean(SAVED_STATE_PREVIEW, previewShown.currentValue.getOrElse(false))
  }

  override def onPause(): Unit = {
    super.onPause()
    KeyboardUtils.hideKeyboard(getActivity)
    hideAudioMessageRecording()
  }

  override def onStop(): Unit = {
    extendedCursorContainer.close(true)
    extendedCursorContainer.setCallback(null)
    cursorView.setCallback(null)
    getControllerFactory.getGlobalLayoutController.removeKeyboardHeightObserver(extendedCursorContainer)
    getControllerFactory.getGlobalLayoutController.removeKeyboardVisibilityObserver(extendedCursorContainer)
    if (!cursorView.isEditingMessage) draftMap.setCurrent(cursorView.getText.trim)

    getControllerFactory.getOrientationController.removeOrientationControllerObserver(orientationControllerObserver)
    /*getControllerFactory.getGiphyController.removeObserver(this)
    getControllerFactory.getSingleImageController.removeSingleImageObserver(this)

    getStoreFactory.inAppNotificationStore.removeInAppNotificationObserver(this)
    getStoreFactory.participantsStore.removeParticipantsStoreObserver(this)
    getControllerFactory.getGlobalLayoutController.removeKeyboardVisibilityObserver(this)
    getControllerFactory.getNavigationController.removePagerControllerObserver(this)
    getControllerFactory.getAccentColorController.removeAccentColorObserver(this)
    getControllerFactory.getNavigationController.removeNavigationControllerObserver(this)
    getControllerFactory.getSlidingPaneController.removeObserver(this)
    getControllerFactory.getConversationScreenController.setConversationStreamUiReady(false)*/
    getControllerFactory.getRequestPermissionsController.removeObserver(requestPermissionsObserver)

    super.onStop()
  }

  override def onDestroyView(): Unit = {
    typingIndicatorView.clear()
    super.onDestroyView()
  }

  private val convChange = convController.convChanged.filter { _.to.isDefined }

  Signal.wrap(convChange).zip(previewShown) {
    case (ConversationChange(Some(from), Some(to), _), true) if from != to  => imagePreviewCallback.onCancelPreview()
    case _ =>
  }

  convChange { _ =>
    extendedCursorContainer.close(true)
    convController.withSelectedConv { conv =>
      getControllerFactory.getConversationScreenController.setSingleConversation(conv.convType == IConversation.Type.ONE_TO_ONE)
      // TODO: ConversationScreenController should listen to this signal and do it itself
    }
  }

  Signal.wrap(convChange) {
    case ConversationChange(from, Some(to), requester) =>

      CancellableFuture.delay(getResources.getInteger(R.integer.framework_animation_duration_short).millis).map { _ =>
        convController.loadConv(to).map {
          case Some(toConv) =>
            setDraft(from)
            if (toConv.convType != ConversationType.WaitForConnection) updateConv(from, toConv)
          case None =>
        }
      }

      // Saving factories since this fragment may be re-created before the runnable is done,
      // but we still want runnable to work.
      val storeFactory = Option(getStoreFactory)
      val controllerFactory = Option(getControllerFactory)
      // TODO: Remove when call issue is resolved with https://wearezeta.atlassian.net/browse/CM-645
      // And also why do we use the ConversationFragment to start a call from somewhere else....
      CancellableFuture.delay(1000.millis).map { _ =>
        (storeFactory, controllerFactory, requester) match {
          case (Some(sf), Some(cf), ConversationChangeRequester.START_CONVERSATION_FOR_VIDEO_CALL) if !sf.isTornDown && !cf.isTornDown =>
            cf.getCallingController.startCall(true)
          case (Some(sf), Some(cf), ConversationChangeRequester.START_CONVERSATION_FOR_CALL) if !sf.isTornDown && !cf.isTornDown =>
            cf.getCallingController.startCall(false)
          case (Some(sf), Some(cf), ConversationChangeRequester.START_CONVERSATION_FOR_CALL) if !sf.isTornDown && !cf.isTornDown =>
            cf.getCameraController.openCamera(CameraContext.MESSAGE)
          case _ =>
        }
      }

    case _ =>
  }

  private def setDraft(fromId: Option[ConvId]) = fromId.foreach{ id => getStoreFactory.draftStore.setDraft(id, cursorView.getText.trim) }

  private def updateConv(fromId: Option[ConvId], toConv: ConversationData): Unit = {
    KeyboardUtils.hideKeyboard(getActivity)
    cursorView.enableMessageWriting()

    val sharingController = inject[SharingController]

    fromId.filter(_ != toConv.id).foreach { id =>
      getControllerFactory.getConversationScreenController.setConversationStreamUiReady(false)
      sharingController.clearSharingFor(id)

      cursorView.setVisibility(if (toConv.isActive) View.VISIBLE else View.GONE)
      cursorView.setText(getStoreFactory.draftStore.getDraft(toConv.id))
      cursorView.setConversation()

      hideAudioMessageRecording()
    }

    val sharedText = sharingController.getSharedText(toConv.id)
    if (!TextUtils.isEmpty(sharedText)) {
      cursorView.setText(sharedText)
      cursorView.enableMessageWriting()
      KeyboardUtils.showKeyboard(getActivity)
      sharingController.clearSharingFor(toConv.id)
    }
  }

  private def inflateCollectionIcon(): Unit = {
    leftMenu.getMenu.clear()

    val searchInProgress = collectionController.contentSearchQuery.currentValue("").get.originalString.nonEmpty

    getActivity.getMenuInflater.inflate(
      if (searchInProgress) R.menu.conversation_header_menu_collection_searching
      else R.menu.conversation_header_menu_collection,
      leftMenu.getMenu
    )
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent): Unit =
    assetIntentsManager.foreach { _.onActivityResult(requestCode, resultCode, data) }

  private lazy val imagePreviewCallback = new ImagePreviewLayout.Callback {
    private var currentConv: Option[ConversationData] = None
    convController.selectedConv.on(Threading.Ui) { currentConv = _ }

    override def onCancelPreview(): Unit = {
      previewShown ! false
      getControllerFactory.getNavigationController.setPagerEnabled(true)
      containerPreview
        .animate
        .translationY(getView.getMeasuredHeight)
        .setDuration(getResources.getInteger(R.integer.animation_duration_medium))
        .setInterpolator(new Expo.EaseIn)
        .withEndAction(new Runnable() {
          override def run(): Unit = if (containerPreview != null) containerPreview.removeAllViews()
        })
    }

    override def onSketchOnPreviewPicture(imageAsset: ImageAsset, source: ImagePreviewLayout.Source, method: IDrawingController.DrawingMethod): Unit = {
      getControllerFactory.getDrawingController.showDrawing(imageAsset, IDrawingController.DrawingDestination.CAMERA_PREVIEW_VIEW, method)
      extendedCursorContainer.close(true)
    }

    override def onSendPictureFromPreview(imageAsset: ImageAsset, source: ImagePreviewLayout.Source): Unit = imageAsset match {
      case a: com.waz.api.impl.ImageAsset => currentConv.foreach { conv =>
        convController.sendMessage(conv.id, a)
        extendedCursorContainer.close(true)
        onCancelPreview()

        globalTrackingController.tagEvent(new SentPictureEvent(
          if (source == ImagePreviewLayout.Source.CAMERA) SentPictureEvent.Source.CAMERA else SentPictureEvent.Source.GALLERY,
          conv.convType.name(),
          source match {
            case ImagePreviewLayout.Source.IN_APP_GALLERY => SentPictureEvent.Method.KEYBOARD
            case ImagePreviewLayout.Source.DEVICE_GALLERY => SentPictureEvent.Method.FULL_SCREEN
            case _                                        => SentPictureEvent.Method.DEFAULT
          },
          SentPictureEvent.SketchSource.NONE,
          false,
          conv.ephemeral != EphemeralExpiration.NONE,
          String.valueOf(conv.ephemeral.duration.toSeconds))
        )
      }
      case _ =>
    }

  }

  private def errorHandler = new MessageContent.Asset.ErrorHandler() {
    override def noWifiAndFileIsLarge(sizeInBytes: Long, net: NetworkMode, answer: MessageContent.Asset.Answer): Unit = answer.ok()
  }

  private lazy val audioMessageRecordingCallback = new AudioMessageRecordingView.Callback {

    private var currentConv: Option[ConversationData] = None
    convController.selectedConv.on(Threading.Ui) { currentConv = _ }

    override def onPreviewedAudioMessage(): Unit = currentConv.foreach { conv =>
      globalTrackingController.tagEvent(new PreviewedAudioMessageEvent(conv.convType.name()))
    }

    override def onSendAudioMessage(audioAssetForUpload: AudioAssetForUpload, appliedAudioEffect: AudioEffect, sentWithQuickAction: Boolean): Unit = audioAssetForUpload match {
      case a: com.waz.api.impl.AudioAssetForUpload =>
        currentConv.foreach { conv =>
          convController.sendMessage(conv.id, a, errorHandler)
          hideAudioMessageRecording()
          TrackingUtils.tagSentAudioMessageEvent(globalTrackingController, audioAssetForUpload, appliedAudioEffect, true, sentWithQuickAction, conv)
        }
      case _ =>
    }

    override def onCancelledAudioMessageRecording(): Unit = currentConv.foreach { conv =>
      hideAudioMessageRecording()
      globalTrackingController.tagEvent(new CancelledRecordingAudioMessageEvent(conv.name.getOrElse("")))
    }

    override def onStartedRecordingAudioMessage(): Unit = getControllerFactory.getGlobalLayoutController.keepScreenAwake()
  }

  private def sendVideo(uri: URI): Unit = {
    convController.sendMessage(ContentUriAssetForUpload(AssetId(), uri), assetErrorHandlerVideo)
    getControllerFactory.getNavigationController.setRightPage(Page.MESSAGE_STREAM, TAG)
    extendedCursorContainer.close(true)
  }

  private def sendImage(uri: URI): Unit = ImageAssetFactory.getImageAsset(uri) match {
    case a: com.waz.api.impl.ImageAsset => convController.sendMessage(a)
    case _ =>
  }

  private val assetErrorHandler = new MessageContent.Asset.ErrorHandler() {
    override def noWifiAndFileIsLarge(sizeInBytes: Long, net: NetworkMode, answer: MessageContent.Asset.Answer): Unit = Option(getActivity) match {
      case None => answer.ok()
      case Some(activity) =>
        val dialog = ViewUtils.showAlertDialog(
          activity,
          R.string.asset_upload_warning__large_file__title,
          R.string.asset_upload_warning__large_file__message_default,
          R.string.asset_upload_warning__large_file__button_accept,
          R.string.asset_upload_warning__large_file__button_cancel,
          new DialogInterface.OnClickListener() { override def onClick(dialog: DialogInterface, which: Int): Unit = answer.ok() },
          new DialogInterface.OnClickListener() { override def onClick(dialog: DialogInterface, which: Int): Unit = answer.cancel() }
        )
        dialog.setCancelable(false)
        if (sizeInBytes > 0)
          dialog.setMessage(getString(R.string.asset_upload_warning__large_file__message, Formatter.formatFileSize(getContext, sizeInBytes)))
    }
  }

  private val assetErrorHandlerVideo = new MessageContent.Asset.ErrorHandler() {
    override def noWifiAndFileIsLarge(sizeInBytes: Long, net: NetworkMode, answer: MessageContent.Asset.Answer): Unit = Option(getActivity) match {
      case None => answer.ok()
      case Some(activity) =>
        val dialog = ViewUtils.showAlertDialog(
          activity,
          R.string.asset_upload_warning__large_file__title,
          R.string.asset_upload_warning__large_file__message_default,
          R.string.asset_upload_warning__large_file__button_accept,
          R.string.asset_upload_warning__large_file__button_cancel,
          new DialogInterface.OnClickListener() { override def onClick(dialog: DialogInterface, which: Int): Unit = answer.ok() },
          new DialogInterface.OnClickListener() { override def onClick(dialog: DialogInterface, which: Int): Unit = answer.cancel() }
      )
      dialog.setCancelable(false)
      if (sizeInBytes > 0) dialog.setMessage(getString(R.string.asset_upload_warning__large_file__message__video))
    }
  }


  private val assetIntentsManagerCallback = new AssetIntentsManager.Callback {
    override def onDataReceived(intentType: AssetIntentsManager.IntentType, uri: URI): Unit = intentType match {
      case AssetIntentsManager.IntentType.FILE_SHARING =>
        sharingUris.clear()
        if (PermissionUtils.hasSelfPermissions(getActivity, FILE_SHARING_PERMISSION:_*)) convController.sendMessage(ContentUriAssetForUpload(AssetId(), uri), errorHandler)
        else {
          sharingUris += uri
          ActivityCompat.requestPermissions(getActivity, FILE_SHARING_PERMISSION, FILE_SHARING_PERMISSION_REQUEST_ID)
        }
      case AssetIntentsManager.IntentType.GALLERY =>
        showImagePreview(ImageAssetFactory.getImageAsset(uri), ImagePreviewLayout.Source.DEVICE_GALLERY)
      case AssetIntentsManager.IntentType.VIDEO_CURSOR_BUTTON =>
        sendVideo(uri)
        convController.selectedConv.currentValue.foreach {
          case None =>
          case Some(conv) =>
            globalTrackingController.tagEvent(new SentVideoMessageEvent((AssetUtils.getVideoAssetDurationMilliSec(getContext, uri) / 1000).toInt, conv, SentVideoMessageEvent.Source.CURSOR_BUTTON))
        }
      case AssetIntentsManager.IntentType.VIDEO =>
        sendVideo(uri)
        convController.selectedConv.currentValue.foreach {
          case None =>
          case Some(conv) =>
            globalTrackingController.tagEvent(new SentVideoMessageEvent((AssetUtils.getVideoAssetDurationMilliSec(getContext, uri) / 1000).toInt, conv, SentVideoMessageEvent.Source.KEYBOARD))
        }
      case AssetIntentsManager.IntentType.CAMERA =>
        sendImage(uri)
        convController.selectedConv.currentValue.foreach {
          case None =>
          case Some(conv) =>
            TrackingUtils.onSentPhotoMessage(globalTrackingController, conv, SentPictureEvent.Source.CAMERA, SentPictureEvent.Method.FULL_SCREEN)
        }
        extendedCursorContainer.close(true)
    }

    override def openIntent(intent: Intent, intentType: AssetIntentsManager.IntentType): Unit = {
      if (MediaStore.ACTION_VIDEO_CAPTURE.equals(intent.getAction) &&
        extendedCursorContainer.getType == ExtendedCursorContainer.Type.IMAGES &&
        extendedCursorContainer.isExpanded) {
        // Close keyboard camera before requesting external camera for recording video
        extendedCursorContainer.close(true)
      }
      startActivityForResult(intent, intentType.requestCode)
      getActivity.overridePendingTransition(R.anim.camera_in, R.anim.camera_out)
    }

    override def onPermissionFailed(`type`: AssetIntentsManager.IntentType): Unit = {}

    override def onFailed(`type`: AssetIntentsManager.IntentType): Unit = {}

    override def onCanceled(`type`: AssetIntentsManager.IntentType): Unit = {}
  }

  private val extendedCursorContainerCallback = new ExtendedCursorContainer.Callback {
    override def onExtendedCursorClosed(lastType: ExtendedCursorContainer.Type): Unit = {
      cursorView.onExtendedCursorClosed()

      if (lastType == ExtendedCursorContainer.Type.EPHEMERAL)
        convController.selectedConv.collect { case Some(conv) => conv.ephemeral }.currentValue.foreach {
          case expiration if expiration == EphemeralExpiration.NONE =>
          case expiration => getControllerFactory.getUserPreferencesController.setLastEphemeralValue(expiration.milliseconds)
        }

      getControllerFactory.getGlobalLayoutController.resetScreenAwakeState()
    }
  }

  private val requestPermissionsObserver = new RequestPermissionsObserver() {
    override def onRequestPermissionsResult(requestCode: Int, grantResults: Array[Int]) =
      if (!assetIntentsManager.fold(true){_.onRequestPermissionsResult(requestCode, grantResults)}) requestCode match {
        case OPEN_EXTENDED_CURSOR_IMAGES =>
          if (PermissionUtils.verifyPermissions(grantResults:_*)) openExtendedCursor(ExtendedCursorContainer.Type.IMAGES)
        case CAMERA_PERMISSION_REQUEST_ID =>
          if (PermissionUtils.verifyPermissions(grantResults:_*)) {
            val takeVideoIntent = new Intent(MediaStore.ACTION_VIDEO_CAPTURE)
            if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.JELLY_BEAN_MR1) takeVideoIntent.putExtra(MediaStore.EXTRA_VIDEO_QUALITY, 0)
            startActivityForResult(takeVideoIntent, REQUEST_VIDEO_CAPTURE)
          }
          else cameraPermissionsFail
        case FILE_SHARING_PERMISSION_REQUEST_ID =>
          if (PermissionUtils.verifyPermissions(grantResults:_*)) {
            sharingUris.foreach { uri => convController.sendMessage(ContentUriAssetForUpload(AssetId(), uri), assetErrorHandler) }
            sharingUris.clear()
          }
          else ViewUtils.showAlertDialog(
            getActivity,
            R.string.asset_upload_error__not_found__title,
            R.string.asset_upload_error__not_found__message,
            R.string.asset_upload_error__not_found__button,
            null,
            true
          )
        case AUDIO_PERMISSION_REQUEST_ID =>
          // No actions required if permission is granted
          // TODO: https://wearezeta.atlassian.net/browse/AN-4027 Show information dialog if permission is not granted
        case AUDIO_FILTER_PERMISSION_REQUEST_ID =>
          if (PermissionUtils.verifyPermissions(grantResults:_*)) openExtendedCursor(ExtendedCursorContainer.Type.VOICE_FILTER_RECORDING)
          else Toast.makeText(getActivity, R.string.audio_message_error__missing_audio_permissions, Toast.LENGTH_SHORT).show()
        case _ =>
      }
    }

  private def cameraPermissionsFail =
    Toast.makeText(getActivity, R.string.video_message_error__missing_camera_permissions, Toast.LENGTH_SHORT).show()

  private def openExtendedCursor(cursorType: ExtendedCursorContainer.Type): Unit = cursorType match {
      case ExtendedCursorContainer.Type.NONE =>
      case ExtendedCursorContainer.Type.EMOJIS =>
        extendedCursorContainer.openEmojis(getControllerFactory.getUserPreferencesController.getRecentEmojis, getControllerFactory.getUserPreferencesController.getUnsupportedEmojis, emojiKeyboardLayoutCallback)
      case ExtendedCursorContainer.Type.EPHEMERAL => convController.withSelectedConv { conv =>
        extendedCursorContainer.openEphemeral(ephemeralLayoutCallback, conv.ephemeral)
      }
      case ExtendedCursorContainer.Type.VOICE_FILTER_RECORDING =>
        extendedCursorContainer.openVoiceFilter(voiceFilterLayoutCallback)
        convController.withSelectedConv { conv =>
          globalTrackingController.tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.AUDIO_MESSAGE, conv, false))
        }
      case ExtendedCursorContainer.Type.IMAGES =>
        extendedCursorContainer.openCursorImages(cursorImageLayoutCallback)
        convController.withSelectedConv { conv =>
          globalTrackingController.tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.PHOTO, conv, false))
        }
  }

  private val emojiKeyboardLayoutCallback = new EmojiKeyboardLayout.Callback {
    override def onEmojiSelected(emoji: LogTag) = {
      cursorView.insertText(emoji)
      getControllerFactory.getUserPreferencesController.addRecentEmoji(emoji)
    }
  }

  private val ephemeralLayoutCallback = new EphemeralLayout.Callback {
    override def onEphemeralExpirationSelected(expiration: EphemeralExpiration, close: Boolean) = {
      if (close) extendedCursorContainer.close(false)
      convController.setEphemeralExpiration(expiration)
    }
  }

  private val voiceFilterLayoutCallback = new VoiceFilterLayout.Callback {
    override def onAudioMessageRecordingStarted(): Unit = {
      getControllerFactory.getGlobalLayoutController.keepScreenAwake()
      convController.withSelectedConv { conv =>
        globalTrackingController.tagEvent(new StartedRecordingAudioMessageEvent(conv.convType.name(), false))
      }
    }

    override def onCancel(): Unit = extendedCursorContainer.close(false)

    override def sendRecording(audioAssetForUpload: AudioAssetForUpload, appliedAudioEffect: AudioEffect): Unit = {
      audioAssetForUpload match {
        case a: com.waz.api.impl.AudioAssetForUpload => convController.sendMessage(a, errorHandler)
        case _ =>
      }

      hideAudioMessageRecording()

      convController.withSelectedConv { conv =>
        TrackingUtils.tagSentAudioMessageEvent(globalTrackingController, audioAssetForUpload, appliedAudioEffect, false, false, conv)
      }
      extendedCursorContainer.close(true)
    }
  }

  private val cursorImageLayoutCallback = new CursorImagesLayout.Callback {
    override def openCamera(): Unit = getControllerFactory.getCameraController.openCamera(CameraContext.MESSAGE)

    override def openVideo(): Unit = assetIntentsManager.foreach { _.maybeCaptureVideo(getActivity, AssetIntentsManager.IntentType.VIDEO) }

    override def onGalleryPictureSelected(asset: ImageAsset): Unit = {
      previewShown ! true
      showImagePreview(asset, ImagePreviewLayout.Source.IN_APP_GALLERY)
    }

    override def openGallery(): Unit = assetIntentsManager.foreach { _.openGallery() }

    override def onPictureTaken(imageAsset: ImageAsset): Unit = showImagePreview(imageAsset, ImagePreviewLayout.Source.CAMERA)
  }

  private val inLandscape = Signal(Option.empty[Boolean])

  private val orientationControllerObserver = new  OrientationControllerObserver {
    override def onOrientationHasChanged(squareOrientation: SquareOrientation): Unit = inLandscape.head.foreach { oldInLandscape =>
      val newInLandscape = ViewUtils.isInLandscape(getContext)
      oldInLandscape match {
        case Some(landscape) if landscape != newInLandscape =>
          val conversationListVisible = getControllerFactory.getNavigationController.getCurrentPage eq Page.CONVERSATION_LIST
          if (newInLandscape && !conversationListVisible) {
            val duration = getResources.getInteger(R.integer.framework_animation_duration_short)
            CancellableFuture.delayed(duration.millis){
              Option(getActivity).foreach(_.onBackPressed())
            }
          }
      }
      inLandscape ! Some(newInLandscape)
    }
  }

  private val cursorCallback = new CursorCallback {
    override def onMotionEventFromCursorButton(cursorMenuItem: CursorMenuItem, motionEvent: MotionEvent): Unit =
      if (cursorMenuItem == CursorMenuItem.AUDIO_MESSAGE && audioMessageRecordingView.getVisibility != View.INVISIBLE)
        audioMessageRecordingView.onMotionEventFromAudioMessageButton(motionEvent)

    override def captureVideo(): Unit = inject[GlobalCameraController].releaseCamera().map { _ =>
      assetIntentsManager.foreach { _.maybeCaptureVideo(getActivity, AssetIntentsManager.IntentType.VIDEO_CURSOR_BUTTON) }
    }

    override def hideExtendedCursor(): Unit = if (extendedCursorContainer.isExpanded) extendedCursorContainer.close(false)

    override def onMessageSent(msg: MessageData): Unit = {
      getStoreFactory.networkStore.doIfHasInternetOrNotifyUser(null)
      convController.selectedConvId.head.foreach { inject[SharingController].clearSharingFor }
    }

    override def openExtendedCursor(tpe: ExtendedCursorContainer.Type): Unit = ConvFragment.this.openExtendedCursor(tpe)

    override def onCursorClicked(): Unit = if (!cursorView.isEditingMessage) listView.scrollToBottom()

    override def onFocusChange(hasFocus: Boolean): Unit = {
      if (hasFocus) getControllerFactory.getFocusController.setFocus(IFocusController.CONVERSATION_CURSOR)
      if (!LayoutSpec.isPhone(getActivity) && getControllerFactory.getPickUserController.isShowingPickUser(IPickUserController.Destination.CONVERSATION_LIST)) {
        // On tablet, apply Page.MESSAGE_STREAM soft input mode when conversation cursor has focus (soft input mode of page gets changed when left startui is open)
        val softInputMode = getControllerFactory.getGlobalLayoutController.getSoftInputModeForPage(if (hasFocus) Page.MESSAGE_STREAM else Page.PICK_USER)
        ViewUtils.setSoftInputMode(getActivity.getWindow, softInputMode, TAG)
      }
    }

    override def openFileSharing(): Unit = assetIntentsManager.foreach { _.openFileSharing() }

    override def onCursorButtonLongPressed(cursorMenuItem: CursorMenuItem): Unit = if (cursorMenuItem == CursorMenuItem.AUDIO_MESSAGE) {
      if (PermissionUtils.hasSelfPermissions(getActivity, AUDIO_PERMISSION:_*)) {
        extendedCursorContainer.close(true)
        if (audioMessageRecordingView.getVisibility != View.VISIBLE) {
          inject[SoundController].shortVibrate()
          audioMessageRecordingView.prepareForRecording()
          audioMessageRecordingView.setVisibility(View.VISIBLE)
        }
      } else ActivityCompat.requestPermissions(getActivity, AUDIO_PERMISSION, AUDIO_PERMISSION_REQUEST_ID)
    }
  }

  private def splitPortraitMode = LayoutSpec.isTablet(getActivity) && ViewUtils.isInPortrait(getActivity) && getControllerFactory.getNavigationController.getPagerPosition == 0

  private def hideAudioMessageRecording() = if (audioMessageRecordingView.getVisibility != View.INVISIBLE) {
    audioMessageRecordingView.reset()
    audioMessageRecordingView.setVisibility(View.INVISIBLE)
    getControllerFactory.getGlobalLayoutController.resetScreenAwakeState()
  }

  private def showImagePreview(asset: ImageAsset, source: ImagePreviewLayout.Source): Unit = {
    val imagePreviewLayout = LayoutInflater.from(getContext).inflate(R.layout.fragment_cursor_images_preview, containerPreview, false).asInstanceOf[ImagePreviewLayout]
    imagePreviewLayout.setImageAsset(asset, source, imagePreviewCallback)
    imagePreviewLayout.setAccentColor(getControllerFactory.getAccentColorController.getAccentColor.getColor)
    imagePreviewLayout.setTitle(getStoreFactory.conversationStore.getCurrentConversation.getName)
    containerPreview.addView(imagePreviewLayout)
    openPreview(containerPreview)
  }

  private def openPreview(containerPreview: View): Unit = {
    previewShown ! true
    getControllerFactory.getNavigationController.setPagerEnabled(false)
    containerPreview.setTranslationY(getView.getMeasuredHeight)
    containerPreview.animate.translationY(0).setDuration(getResources.getInteger(R.integer.animation_duration_medium)).setInterpolator(new Expo.EaseOut)
  }
}

object ConvFragment {
  val TAG = ConvFragment.getClass.getName
  val SAVED_STATE_PREVIEW = "SAVED_STATE_PREVIEW"
  val REQUEST_VIDEO_CAPTURE = 911
  val CAMERA_PERMISSION_REQUEST_ID = 21

  val OPEN_EXTENDED_CURSOR_IMAGES = 1254

  val FILE_SHARING_PERMISSION = Array[String](android.Manifest.permission.READ_EXTERNAL_STORAGE)
  val FILE_SHARING_PERMISSION_REQUEST_ID = 179

  val AUDIO_PERMISSION = Array[String](android.Manifest.permission.RECORD_AUDIO)
  val AUDIO_PERMISSION_REQUEST_ID = 864
  val AUDIO_FILTER_PERMISSION_REQUEST_ID = 865

  def apply() = new ConvFragment

  trait Container {

  }
}
