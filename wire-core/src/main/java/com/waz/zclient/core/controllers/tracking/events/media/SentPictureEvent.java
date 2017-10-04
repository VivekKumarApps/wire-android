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
import com.waz.zclient.core.controllers.tracking.attributes.Attribute;
import com.waz.zclient.core.controllers.tracking.events.Event;

public class SentPictureEvent extends Event {

    public enum Source {
        CAMERA("camera"),
        GALLERY("gallery"),
        GIPHY("giphy"),
        SKETCH("sketch"),
        SHARING("sharing"),
        CLIP("clip"),
        ;

        public final String nameString;
        Source(String nameString) {
            this.nameString = nameString;
        }
    }

    public enum Method {
        KEYBOARD("keyboard"),
        FULL_SCREEN("full_screen"),
        TABLET("tablet"),
        DEFAULT("default");

        public final String nameString;

        Method(String nameString) {
            this.nameString = nameString;
        }
    }

    public enum SketchSource {
        SKETCH_BUTTON("sketch_button"),
        CAMERA_GALLERY("camera_gallery"),
        IMAGE_FULL_VIEW("image_full_view"),
        NONE("none");

        public final String nameString;

        SketchSource(String nameString) {
            this.nameString = nameString;
        }
    }

    public SentPictureEvent(Source source, String conversationType, Method method, SketchSource sketchSource, boolean withBot, boolean isEphemeral, String ephemeralExpiration) {
        attributes.put(Attribute.SOURCE, source.nameString);
        attributes.put(Attribute.CONVERSATION_TYPE, conversationType);
        attributes.put(Attribute.WITH_BOT, String.valueOf(withBot));
        if (source == Source.CAMERA || source == Source.GALLERY) {
            attributes.put(Attribute.METHOD, method.toString());
        }
        if (sketchSource != SketchSource.NONE) {
            attributes.put(Attribute.SKETCH_SOURCE, sketchSource.nameString);
        }
        attributes.put(Attribute.IS_EPHEMERAL, String.valueOf(isEphemeral));
        attributes.put(Attribute.EPHEMERAL_EXPIRATION, ephemeralExpiration);
    }


    @NonNull
    @Override
    public String getName() {
        return "media.sent_picture_event";
    }
}
