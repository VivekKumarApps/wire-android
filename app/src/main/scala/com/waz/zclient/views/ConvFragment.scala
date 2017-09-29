package com.waz.zclient.views

import android.os.Bundle
import android.support.v7.widget.{ActionMenuView, Toolbar}
import android.text.TextUtils
import android.view._
import android.widget.{AbsListView, FrameLayout, TextView}
import com.waz.zclient.{BaseActivity, FragmentHelper, R}
import com.waz.zclient.pages.BaseFragment
import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.api.{EphemeralExpiration, ImageAsset}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.{ConvId, ConversationData}
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.events.Signal
import com.waz.utils.returning
import com.waz.zclient.controllers.SharingController
import com.waz.zclient.controllers.drawing.IDrawingController
import com.waz.zclient.conversation.ConversationController.ConversationChange
import com.waz.zclient.conversation.{CollectionController, ConversationController}
import com.waz.zclient.core.controllers.tracking.events.media.SentPictureEvent
import com.waz.zclient.core.stores.conversation.ConversationChangeRequester
import com.waz.zclient.cursor.CursorView
import com.waz.zclient.pages.extendedcursor.ExtendedCursorContainer
import com.waz.zclient.pages.extendedcursor.image.ImagePreviewLayout
import com.waz.zclient.pages.main.profile.camera.CameraContext
import com.waz.zclient.tracking.GlobalTrackingController
import com.waz.zclient.ui.animation.interpolators.penner.Expo
import com.waz.zclient.ui.audiomessage.AudioMessageRecordingView
import com.waz.zclient.ui.utils.KeyboardUtils
import com.waz.zclient.utils.{Callback, LayoutSpec, TrackingUtils, ViewUtils}

import scala.concurrent.Future
import scala.concurrent.duration._

class ConvFragment extends BaseFragment[ConvFragment.Container] with FragmentHelper {
  import Threading.Implicits.Ui
  import ConvFragment._

  private val convController = inject[ConversationController]
  private val collectionController = inject[CollectionController]

  private val previewShown = Signal(false)

  private lazy val typingIndicatorView = ViewUtils.getView(getView, R.id.tiv_typing_indicator_view).asInstanceOf[TypingIndicatorView]
  private lazy val extendedCursorContainer = ViewUtils.getView(getView, R.id.ecc__conversation).asInstanceOf[ExtendedCursorContainer]
  private lazy val containerPreview = ViewUtils.getView(getView, R.id.fl__conversation_overlay).asInstanceOf[ViewGroup]
  private lazy val cursorView = ViewUtils.getView(getView, R.id.cv__cursor).asInstanceOf[CursorView]
  private lazy val audioMessageRecordingView = ViewUtils.getView(getView, R.id.amrv_audio_message_recording).asInstanceOf[AudioMessageRecordingView]

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
  private lazy val listView = ViewUtils.getView(getView, R.id.messages_list_view)

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

  override def onDestroyView(): Unit = {
    typingIndicatorView.clear()
  }

  override def onCreateView(inflater: LayoutInflater, viewGroup: ViewGroup, savedInstanceState: Bundle): View = {
    val view = inflater.inflate(R.layout.fragment_conversation, viewGroup, false)
    ViewUtils.getView(getView, R.id.sv__conversation_toolbar__verified_shield)

    // Recording audio messages
    //audioMessageRecordingView.setCallback(this)
    if (savedInstanceState != null) previewShown ! savedInstanceState.getBoolean(SAVED_STATE_PREVIEW, false)
    view
  }

  private val convChange = convController.convChanged.filter { _.to.isDefined }

  Signal.wrap(convChange).zip(previewShown) {
    case (ConversationChange(Some(from), Some(to), _), true) if from != to  => imagePreviewCallback.onCancelPreview()
    case _ =>
  }

  convChange { _ =>
    extendedCursorContainer.close(true)
    // getControllerFactory().getConversationScreenController().setSingleConversation(toConversation.getType() == IConversation.Type.ONE_TO_ONE); // TOD: ConversationScreenController should listen to this signal and do it itself
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
    val changeToDifferentConversation = fromId.fold(true){ _ != toConv.id }

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

  private lazy val imagePreviewCallback = new ImagePreviewLayout.Callback() {

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

    override def onSendPictureFromPreview(imageAsset: ImageAsset, source: ImagePreviewLayout.Source): Unit = {
      getStoreFactory.conversationStore.sendMessage(imageAsset)
      extendedCursorContainer.close(true)
      onCancelPreview()

      convController.selectedConv.currentValue.foreach(_.foreach { data =>
        inject[GlobalTrackingController].tagEvent(new SentPictureEvent(
          if (source == ImagePreviewLayout.Source.CAMERA) SentPictureEvent.Source.CAMERA else SentPictureEvent.Source.GALLERY,
          data.convType.name(),
          source match {
            case ImagePreviewLayout.Source.IN_APP_GALLERY => SentPictureEvent.Method.KEYBOARD
            case ImagePreviewLayout.Source.DEVICE_GALLERY => SentPictureEvent.Method.FULL_SCREEN
            case _                                        => SentPictureEvent.Method.DEFAULT
          },
          SentPictureEvent.SketchSource.NONE,
          false,
          data.ephemeral != EphemeralExpiration.NONE,
          String.valueOf(data.ephemeral.duration.toSeconds))
        )
      })
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
  val SAVED_STATE_PREVIEW = "SAVED_STATE_PREVIEW"
  val REQUEST_VIDEO_CAPTURE = 911
  val CAMERA_PERMISSION_REQUEST_ID = 21

  val OPEN_EXTENDED_CURSOR_IMAGES = 1254

  val FILE_SHARING_PERMISSION = Array[String](android.Manifest.permission.READ_EXTERNAL_STORAGE)
  val FILE_SHARING_PERMISSION_REQUEST_ID = 179

  val AUDIO_PERMISSION = Array[String](android.Manifest.permission.RECORD_AUDIO)
  val AUDIO_PERMISSION_REQUEST_ID = 864
  val AUDIO_FILTER_PERMISSION_REQUEST_ID = 865


  trait Container {

  }
}
