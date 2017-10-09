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
 */
package com.waz.zclient.core.controllers.tracking.events.media;

import android.support.annotation.NonNull;
import com.waz.api.IConversation;
import com.waz.model.ConversationData;
import com.waz.zclient.core.controllers.tracking.attributes.Attribute;
import com.waz.zclient.core.controllers.tracking.attributes.ConversationType;
import com.waz.zclient.core.controllers.tracking.attributes.OpenedMediaAction;
import com.waz.zclient.core.controllers.tracking.events.Event;

public class OpenedMediaActionEvent extends Event {

    public static OpenedMediaActionEvent cursorAction(OpenedMediaAction action,
                                                      ConversationData conversation,
                                                      boolean isOtto) {
        return new OpenedMediaActionEvent(action, ConversationType.getValue(conversation.convType()), isOtto, "");
    }

    public static OpenedMediaActionEvent ephemeral(ConversationData conv,
                                                   boolean isOtto,
                                                   boolean fromDoubleTap) {
        return new OpenedMediaActionEvent(OpenedMediaAction.EPHEMERAL,
                                          ConversationType.getValue(conv.convType()),
                                          isOtto,
                                          fromDoubleTap ? "double_tap" : "single_tap");
    }

    private OpenedMediaActionEvent(OpenedMediaAction action,
                                   ConversationType conversationType,
                                   boolean isOtto,
                                   String method) {
        attributes.put(Attribute.TARGET, action.nameString);
        attributes.put(Attribute.TYPE, conversationType.name);
        attributes.put(Attribute.WITH_OTTO, String.valueOf(isOtto));
        attributes.put(Attribute.METHOD, method);
    }

    @NonNull
    @Override
    public String getName() {
        return "media.opened_action";
    }
}
