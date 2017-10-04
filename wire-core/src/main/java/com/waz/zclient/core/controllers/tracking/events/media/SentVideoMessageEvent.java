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
import com.waz.api.EphemeralExpiration;
import com.waz.api.IConversation;
import com.waz.model.ConversationData;
import com.waz.zclient.core.controllers.tracking.attributes.Attribute;
import com.waz.zclient.core.controllers.tracking.attributes.RangedAttribute;
import com.waz.zclient.core.controllers.tracking.events.Event;

public class SentVideoMessageEvent extends Event {

    public enum Source {
        CURSOR_BUTTON("cursor_button"),
        KEYBOARD("keyboard");

        public final String nameString;

        Source(String nameString) {
            this.nameString = nameString;
        }
    }

    public SentVideoMessageEvent(int durationAsSec, ConversationData conversation, SentVideoMessageEvent.Source source) {
        rangedAttributes.put(RangedAttribute.VIDEO_AND_AUDIO_MESSAGE_DURATION, durationAsSec);
        attributes.put(Attribute.SOURCE, source.nameString);
        if (conversation != null) {
            attributes.put(Attribute.WITH_BOT, "false");
            attributes.put(Attribute.CONVERSATION_TYPE, conversation.convType().name());
            attributes.put(Attribute.IS_EPHEMERAL, String.valueOf(conversation.ephemeral() != EphemeralExpiration.NONE));
            attributes.put(Attribute.EPHEMERAL_EXPIRATION, String.valueOf(conversation.ephemeral().duration().toSeconds()));
        }
    }

    @NonNull
    @Override
    public String getName() {
        return "media.sent_video_message";
    }
}
