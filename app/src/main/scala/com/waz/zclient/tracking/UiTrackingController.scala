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
package com.waz.zclient.tracking

import android.content.Context
import com.waz.ZLog.ImplicitTag._
import com.waz.api.{EphemeralExpiration, Message}
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.service.ZMessaging
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, EventStream, FutureEventStream, Signal}
import com.waz.zclient.controllers.tracking.events.conversation.EditedMessageEvent
import com.waz.zclient.controllers.tracking.events.navigation.OpenedMoreActionsEvent
import com.waz.zclient.controllers.{AssetsController, BrowserController, SharingController}
import com.waz.zclient.conversation.{CollectionController, ConversationController}
import com.waz.zclient.core.controllers.tracking.attributes.OpenedMediaAction
import com.waz.zclient.core.controllers.tracking.events.media.{OpenedActionHintEvent, OpenedEmojiKeyboardEvent, OpenedMediaActionEvent, SentPictureEvent}
import com.waz.zclient.cursor.{CursorController, CursorMenuItem}
import com.waz.zclient.messages.LikesController
import com.waz.zclient.messages.MessageBottomSheetDialog.MessageAction
import com.waz.zclient.messages.MessageBottomSheetDialog.MessageAction.{Copy, Forward, Like, Unlike}
import com.waz.zclient.messages.controllers.MessageActionsController
import com.waz.zclient.utils.{LayoutSpec, TrackingUtils}
import com.waz.zclient.{Injectable, Injector}
import org.threeten.bp.Duration

import scala.concurrent.Future

class UiTrackingController(implicit injector: Injector, ctx: Context, ec: EventContext) extends Injectable {
  import GlobalTrackingController._
  import Threading.Implicits.Background
  import UiTrackingController._

  val global = inject[GlobalTrackingController]

  import global._

  val msgActionController   = inject[MessageActionsController]
  val assetsController      = inject[AssetsController]
  val browserController     = inject[BrowserController]
  val likesController       = inject[LikesController]
  val collectionsController = inject[CollectionController]
  val sharingController     = inject[SharingController]
  val cursorController      = inject[CursorController]

  val zms                   = inject[Signal[ZMessaging]]
  val convInfo = for {
    z <- zms
    Some(c) <- inject[ConversationController].selectedConv
    user <- z.usersStorage.signal(c.creator)
  } yield {
    (c, c.convType == ConversationType.OneToOne && user.isWireBot)
  }

  def withConv[T](stream: EventStream[T]): EventStream[(T, ConversationData, Boolean)] =
    new FutureEventStream[T, (T, ConversationData, Boolean)](stream, { ev =>
      convInfo.head map { case (tpe, otto) => (ev, tpe, otto) }
    })

  msgActionController.onMessageAction {
    case (MessageAction.Reveal, message) if MessageContentIndex.TextMessageTypes.contains(message.msgType) =>
      collectionsController.currentConv.currentValue.foreach { conv =>
        convTrackingData(conv).map{ data =>
          tagEvent(SelectedSearchResultCollectionsEvent(data.convType, data.withOtto))
        }
      }
    case (action, message) if collectionsController.openedCollection.currentValue.exists(_.nonEmpty) =>
      collectionsController.openedCollection.currentValue.foreach(_.foreach{ info=>
        convTrackingData(info.conversation.id).map { data =>
          tagEvent(DidItemActionCollectionsEvent(action, message.msgType, data.convType, data.withOtto))
        }
      })
    case (action, message) => tagEvent(OpenedMessageActionEvent(action, message.msgType.name))
      action match {
        case Copy            => tagEvent(CopiedMessageEvent(message.msgType.name))
        case Forward         => tagEvent(ForwardedMessageEvent(message.msgType.name))
        case a@(Like|Unlike) => reactedToMessageEvent(message.id, a == Like, "menu")
        case _ => //other types handled after dialog and confirmation
      }
  }

  likesController.onLikeButtonClicked { mAndL => reactedToMessageEvent(mAndL.message.id, !mAndL.likedBySelf, "button")}

  likesController.onViewDoubleClicked { mAndL => reactedToMessageEvent(mAndL.message.id, !mAndL.likedBySelf, "douple_tap")} //sic

  private def reactedToMessageEvent(msgId: MessageId, liked: Boolean, method: String) = messageTrackingData(msgId).map {
    case MessageTrackingData(convType, msgType, withOtto, fromSelf, isLastMsg) =>
      tagEvent(ReactedToMessageEvent(liked, withOtto, fromSelf, msgType, convType, isLastMsg, method))
  }

  msgActionController.onDeleteConfirmed { case (m, forEveryone) => tagEvent(DeletedMessageEvent(m.msgType.name, forEveryone)) }
  msgActionController.onAssetSaved(asset => tagEvent(SavedFileEvent(asset.mime.str, asset.sizeInBytes.toInt)))

  assetsController.onFileOpened {
    case a@AssetData.IsVideo() => tagEvent(OpenedFileEvent(a.mime.str, a.sizeInBytes.toInt))
    case a => tagEvent(OpenedFileEvent(a.mime.str, a.sizeInBytes.toInt))
  }
  assetsController.onFileSaved(a => tagEvent(SavedFileEvent(a.mime.str, a.sizeInBytes.toInt)))

  assetsController.onVideoPlayed(a => messageTrackingData(MessageId(a.id.str)).map {
    case MessageTrackingData(convType, _, isOtto, fromSelf, _) => tagEvent(PlayedVideoMessageEvent(durationInSeconds(a), !fromSelf, isOtto, convType))
  })

  assetsController.onAudioPlayed(a => messageTrackingData(MessageId(a.id.str)).map {
    case MessageTrackingData(convType, _, isOtto, fromSelf, _) => tagEvent(PlayedAudioMessageEvent(a.mime.orDefault.str, durationInSeconds(a), !fromSelf, isOtto, convType))
  })

  browserController.onYoutubeLinkOpened(mId => messageTrackingData(mId).map {
    case MessageTrackingData(convType, _, isOtto, fromSelf, _) => tagEvent(PlayedYouTubeMessageEvent(!fromSelf, isOtto, convType))
  })

  collectionsController.openedCollection.on(Threading.Ui){
    _.foreach { info => convTrackingData(info.conversation.id).map { data =>
      tagEvent(OpenedCollectionsEvent(info.empty, collectionsController.contentSearchQuery.currentValue.exists(_.originalString.nonEmpty), data.convType, data.withOtto))}
    }
  }

  collectionsController.openContextMenuForMessage{
    m => messageTrackingData(m.id).map { data =>
      tagEvent(OpenedItemMenuCollectionsEvent(m.msgType, data.convType, data.withOtto))
    }
  }

  collectionsController.clickedMessage{
    m => messageTrackingData(m.id).map { data =>
      tagEvent(OpenedItemCollectionsEvent(m.msgType, data.convType, data.withOtto))
    }
  }

  collectionsController.contentSearchQuery.on(Threading.Ui){
    query =>
      if (query.originalString.nonEmpty) {
        collectionsController.currentConv.currentValue.foreach { conv =>
          convTrackingData(conv).map{ data =>
            tagEvent(EnteredSearchCollectionsEvent(data.convType, data.withOtto))
          }
        }
      }
  }

  sharingController.sendEvent.on(Threading.Ui){
    sharedData =>
      sharedData._2.map(convId => convTrackingData(convId)).foreach{ c => c.foreach{ convInfo =>
        if (sharedData._1.isInstanceOf[SharingController.FileContent]){
          val event = new SentPictureEvent(
            SentPictureEvent.Source.SHARING,
            convInfo.convType.name(),
            SentPictureEvent.Method.DEFAULT,
            SentPictureEvent.SketchSource.NONE,
            convInfo.withOtto,
            sharedData._3 != EphemeralExpiration.NONE,
            sharedData._3.duration().toSeconds.toString)
          tagEvent(event)
        }
        }
      }
  }

  import CursorMenuItem._
  withConv(cursorController.onCursorItemClick).on(Threading.Ui) {
    case (Camera, conv, otto) =>
      if (LayoutSpec.isTablet(ctx)) {
        tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.PHOTO, conv, otto))
      }
    case (Ping, conv, otto) =>
      TrackingUtils.onSentPingMessage(global, conv, otto)
    case (Sketch, conv, otto) =>
      tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.SKETCH, conv, otto))
    case (File, conv, otto) =>
      tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.FILE, conv, otto))
    case (VideoMessage, conv, otto) =>
      tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.VIDEO_MESSAGE, conv, otto))
    case (Location, conv, otto) =>
      tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.LOCATION, conv, otto))
    case (More, conv, otto) =>
      tagEvent(new OpenedMoreActionsEvent(conv))
    case (Gif, conv, otto) =>
      tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.GIPHY, conv, otto))
    case _ => // ignore
  }

  import com.waz.zclient.pages.extendedcursor.ExtendedCursorContainer.Type._
  withConv(cursorController.extendedCursor.onChanged).on(Threading.Ui) {
    case (NONE, _, _) =>
    case (VOICE_FILTER_RECORDING, conv, otto) =>
      tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.AUDIO_MESSAGE, conv, otto))
    case (IMAGES, conv, otto) =>
      tagEvent(OpenedMediaActionEvent.cursorAction(OpenedMediaAction.PHOTO, conv, otto))
    case _ => // ignore
  }

  withConv(cursorController.onShowTooltip).on(Threading.Ui) { case ((item, _), conv, _) =>
      tagEvent(new OpenedActionHintEvent(item.name, conv))
  }

  withConv(cursorController.extendedCursor.onChanged).on(Threading.Ui) {
    case (EMOJIS, conv, otto) =>
        tagEvent(new OpenedEmojiKeyboardEvent(otto))
    case (EPHEMERAL, conv, otto) =>
        if (conv.ephemeral == EphemeralExpiration.NONE) {
          tagEvent(OpenedMediaActionEvent.ephemeral(conv, otto, false))
        }
    case _ => //ignore
  }

  withConv(cursorController.onMessageSent).on(Threading.Ui) { case (_, conv, otto) =>
    TrackingUtils.onSentTextMessage(global, conv, otto)
  }

  cursorController.onMessageEdited.on(Threading.Ui) { msg =>
    tagEvent(new EditedMessageEvent(msg))
  }

  withConv(cursorController.onEphemeralExpirationSelected) { case (_, conv, otto) =>
    tagEvent(OpenedMediaActionEvent.ephemeral(conv, otto, true))
  }

  private def durationInSeconds(a: AssetData): Int = (a match {
    case AssetData.WithDuration(d) => d
    case _ => Duration.ZERO
  }).toMillis.toInt / 1000

  private def messageTrackingData(messageId: MessageId): Future[MessageTrackingData] = {
    for {
      zms        <- zMessaging.head
      Some(msg)  <- zms.messagesStorage.get(messageId)
      Some(conv) <- zms.convsContent.convById(msg.convId)
      withOtto   <- isOtto(conv, zms.usersStorage)
      lastMsgs   <- zms.messagesStorage.lastMessageFromSelfAndFromOther(conv.id).head
      fromSelf    = msg.userId == zms.selfUserId
      convType    = conv.convType
      isLastMsg   = lastMsgs._1.exists(_.id == msg.id) || lastMsgs._2.exists(_.id == msg.id)
    } yield MessageTrackingData(convType, msg.msgType, withOtto, fromSelf, isLastMsg)
  }

  private def convTrackingData(convId: ConvId): Future[ConversationTrackingData] = {
    for {
      zms        <- zMessaging.head
      Some(conv) <- zms.convsContent.convById(convId)
      withOtto   <- isOtto(conv, zms.usersStorage)
    } yield ConversationTrackingData(conv.convType, withOtto)
  }
}

object UiTrackingController {

  case class MessageTrackingData(convType: ConversationType, msgType: Message.Type, withOtto: Boolean, msgFromSelf: Boolean, isLastMsg: Boolean)

  case class ConversationTrackingData(convType: ConversationType, withOtto: Boolean)

}
