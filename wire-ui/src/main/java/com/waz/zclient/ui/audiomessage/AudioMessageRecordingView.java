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
package com.waz.zclient.ui.audiomessage;

import android.animation.ObjectAnimator;
import android.animation.ValueAnimator;
import android.content.Context;
import android.content.DialogInterface;
import android.graphics.LightingColorFilter;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.LayerDrawable;
import android.os.Handler;
import android.support.v4.content.ContextCompat;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.SeekBar;
import android.widget.TextView;
import com.waz.api.AssetFactory;
import com.waz.api.AudioAssetForUpload;
import com.waz.api.AudioEffect;
import com.waz.api.AudioOverview;
import com.waz.api.PlaybackControls;
import com.waz.api.RecordingCallback;
import com.waz.api.RecordingControls;
import com.waz.zclient.core.api.scala.ModelObserver;
import com.waz.zclient.ui.R;
import com.waz.zclient.ui.utils.CursorUtils;
import com.waz.zclient.utils.StringUtils;
import com.waz.zclient.utils.ViewUtils;
import org.threeten.bp.Duration;
import org.threeten.bp.Instant;
import timber.log.Timber;

public class AudioMessageRecordingView extends FrameLayout implements View.OnClickListener {

    private static final int RECORDING_TIMER_INTERVAL = 100;
    private static final int RECORDING_INDICATOR_VISIBLE_INTERVAL = 750;
    private static final int RECORDING_INDICATOR_HIDDEN_INTERVAL = 350;
    private PlaybackControls playbackControls;

    private enum SlideControlState {
        RECORDING, SEND_FROM_RECORDING, PREVIEW
    }

    private View closeButtonContainer;
    private TextView hintTextView;
    private View recordingIndicatorDotView;
    private View recordingIndicatorContainerView;
    private View cancelButton;
    private View bottomButton;
    private TextView bottomButtonTextView;
    private View sendButton;
    private TextView sendButtonTextView;
    private View slideControl;
    private View slideControlContainer;
    private TextView timerTextView;
    private SeekBar recordingSeekBar;
    private boolean darkTheme;

    private int actionUpMinY;
    private boolean sendImmediately;
    private AudioAssetForUpload audioAssetForUpload;

    private SlideControlState slideControlState;
    private RecordingControls recordingControls;
    private Callback callback;

    private Drawable defaultSlideControlBackground;
    private Drawable colorSlideControlBackground;

    private long recordingStartEpochMillis;
    private Handler recordingTimerHandler;
    private final Runnable recordingTimerRunnable = new Runnable() {
        @Override
        public void run() {
            int durationMillis = (int) (System.currentTimeMillis() - recordingStartEpochMillis);
            timerTextView.setText(StringUtils.formatTimeMilliSeconds(durationMillis));
            recordingTimerHandler.postDelayed(recordingTimerRunnable, RECORDING_TIMER_INTERVAL);
        }
    };

    private final ModelObserver<PlaybackControls> playbackControlsModelObserver = new ModelObserver<PlaybackControls>() {
        @Override
        public void updated(PlaybackControls model) {
            if (model.isPlaying()) {
                bottomButtonTextView.setText(R.string.glyph__pause);
            } else {
                bottomButtonTextView.setText(R.string.glyph__play);
            }
            recordingSeekBar.setMax((int) model.getDuration().toMillis());
            recordingSeekBar.setProgress((int) model.getPlayhead().toMillis());
        }
    };

    private final RecordingCallback recordingCallback = new RecordingCallback() {
        @Override
        public void onStart(Instant instant) {
            setRecordingStart(instant.toEpochMilli());
        }

        @Override
        public void onComplete(final AudioAssetForUpload audioAssetForUpload, boolean fileSizeLimitReached, AudioOverview overview) {
            if (fileSizeLimitReached) {
                ViewUtils.showAlertDialog(getContext(),
                                          R.string.audio_message__recording__limit_reached__title,
                                          R.string.audio_message__recording__limit_reached__message,
                                          R.string.audio_message__recording__limit_reached__confirmation,
                                          R.string.confirmation_menu__cancel,
                                          new DialogInterface.OnClickListener() {
                                              @Override
                                              public void onClick(DialogInterface dialogInterface, int i) {
                                                  if (callback != null) {
                                                      callback.onSendAudioMessage(audioAssetForUpload, null, true);
                                                  }
                                              }
                                          },
                                          null);
                return;
            }

            if (sendImmediately) {
                if (callback != null) {
                    callback.onSendAudioMessage(audioAssetForUpload, null, true);
                }
            } else {
                AudioMessageRecordingView.this.audioAssetForUpload = audioAssetForUpload;
                Timber.d("setting up playback controls");
                playbackControls = audioAssetForUpload.getPlaybackControls();
                playbackControlsModelObserver.setAndUpdate(playbackControls);
            }
        }

        @Override
        public void onCancel() {
            playbackControlsModelObserver.clear();
            Timber.d("playback controls set to null");
            playbackControls = null;
        }
    };

    private final SeekBar.OnSeekBarChangeListener seekBarChangeListener = new SeekBar.OnSeekBarChangeListener() {
        @Override
        public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
            if (!fromUser || playbackControls == null) {
                return;
            }
            playbackControls.setPlayhead(Duration.ofMillis(progress));
        }

        @Override
        public void onStartTrackingTouch(SeekBar seekBar) {

        }

        @Override
        public void onStopTrackingTouch(SeekBar seekBar) {

        }
    };

    private ObjectAnimator recordingIndicatorDotAnimator;

    public AudioMessageRecordingView(Context context) {
        this(context, null);
    }

    public AudioMessageRecordingView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public AudioMessageRecordingView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

        LayoutInflater.from(getContext()).inflate(R.layout.voice_message_recording, this, true);

        slideControlContainer = ViewUtils.getView(this, R.id.fl__audio_message__recording__slide_control_container);
        closeButtonContainer = ViewUtils.getView(this, R.id.ll__audio_message_recording__close_button_container);

        hintTextView = ViewUtils.getView(this, R.id.ttv__audio_message_recording__hint);
        recordingIndicatorDotView = ViewUtils.getView(this, R.id.fl__audio_message__recording__indicator_dot);
        recordingIndicatorContainerView = ViewUtils.getView(this, R.id.fl__audio_message__recording__indicator_container);
        cancelButton = ViewUtils.getView(this, R.id.fl__audio_message__recording__cancel_button_container);
        bottomButton = ViewUtils.getView(this, R.id.fl__audio_message__recording__bottom_button_container);
        bottomButtonTextView = ViewUtils.getView(this, R.id.gtv__audido_message__recording__bottom_button);
        sendButton = ViewUtils.getView(this, R.id.fl__audio_message__recording__send_button_container);
        sendButtonTextView  = ViewUtils.getView(this, R.id.gtv__audio_message__recording__send_button);
        slideControl = ViewUtils.getView(this, R.id.fl__audio_message__recording__slide_control);
        timerTextView = ViewUtils.getView(this, R.id.ttv__audio_message__recording__duration);
        recordingSeekBar = ViewUtils.getView(this, R.id.sb__voice_message__recording__seekbar);
        recordingSeekBar.setVisibility(GONE);
        recordingSeekBar.setOnSeekBarChangeListener(seekBarChangeListener);

        defaultSlideControlBackground = ContextCompat.getDrawable(getContext(), R.drawable.audio_message__slide_control__background);
        colorSlideControlBackground = ContextCompat.getDrawable(getContext(), R.drawable.audio_message__slide_control__background_accent__green);

        sendButtonTextView.setTextColor(ContextCompat.getColor(getContext(), R.color.accent_green));

        actionUpMinY = getResources().getDimensionPixelOffset(R.dimen.audio_message_recording__slide_control__height) - 2 * getResources().getDimensionPixelOffset(R.dimen.audio_message_recording__slide_control__width) - getContext().getResources().getDimensionPixelSize(R.dimen.wire__padding__8);

        cancelButton.setOnClickListener(this);
        sendButton.setOnClickListener(this);
        bottomButton.setOnClickListener(this);

        recordingTimerHandler = new Handler();

        setSlideControlState(SlideControlState.RECORDING);
        sendImmediately = false;
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);

        int width = MeasureSpec.getSize(widthMeasureSpec);
        int cancelContainerWidth = CursorUtils.getDistanceOfAudioMessageIconToLeftScreenEdge(getContext(), width);
        ViewUtils.setWidth(closeButtonContainer, cancelContainerWidth);
        ViewUtils.setMarginLeft(cancelButton, CursorUtils.getMarginBetweenCursorButtons(getContext()));
    }

    @Override
    public void onClick(View view) {
        Timber.d("onClick");
        if (view.getId() == R.id.fl__audio_message__recording__cancel_button_container) {
            Timber.d("cancel button, slideControlState = " + slideControlState + ", callback is " + (callback != null? "set":"null"));
            // CANCEL BUTTON
            if (slideControlState == SlideControlState.RECORDING ||
                callback == null) {
                return;
            }
            if (playbackControls != null && playbackControls.isPlaying()) {
                playbackControls.stop();
            }
            callback.onCancelledAudioMessageRecording();
        } else if (view.getId() == R.id.fl__audio_message__recording__send_button_container) {
            Timber.d("send button");
            // SEND BUTTON
            if (recordingControls == null ||
                callback == null ||
                playbackControls == null) {
                return;
            }
            if (playbackControls.isPlaying()) {
                playbackControls.stop();
            }
            callback.onSendAudioMessage(audioAssetForUpload, null, false);
        } else if (view.getId() == R.id.fl__audio_message__recording__bottom_button_container) {
            Timber.d("bottom button");
            if (playbackControls == null) {
                return;
            }
            if (playbackControls.isPlaying()) {
                playbackControls.stop();
            } else {
                playbackControls.play();
                if (callback != null) {
                    callback.onPreviewedAudioMessage();
                }
            }
        }
    }

    public void setCallback(Callback callback) {
        this.callback = callback;
    }

    public void prepareForRecording() {

        recordingControls = AssetFactory.recordAudioAsset(recordingCallback);
        setSlideControlState(SlideControlState.RECORDING);
    }

    public void setRecordingStart(long recordingStartEpochMillis) {
        this.recordingStartEpochMillis = recordingStartEpochMillis;
        recordingTimerHandler.post(recordingTimerRunnable);
    }

    public void onMotionEventFromAudioMessageButton(MotionEvent motionEvent) {
        switch (motionEvent.getAction()) {
            case MotionEvent.ACTION_MOVE:
                if (slidedUpToSend(motionEvent)) {
                    setSlideControlState(SlideControlState.SEND_FROM_RECORDING);
                } else {
                    setSlideControlState(SlideControlState.RECORDING);
                }
                break;
            case MotionEvent.ACTION_CANCEL:
            case MotionEvent.ACTION_OUTSIDE:
            case MotionEvent.ACTION_UP:
                if (recordingControls == null) {
                    // recordingControls might be gone while user is long pressing e.g. when cancel is clicked at same time
                    break;
                }
                if (slideControlState == SlideControlState.SEND_FROM_RECORDING) {
                    // SLIDE UP TO INSTANT SEND
                    sendImmediately = true;
                } else if (slideControlState == SlideControlState.RECORDING) {
                    sendImmediately = false;
                    setSlideControlState(SlideControlState.PREVIEW);
                }
                if (recordingControls != null) {
                    recordingControls.stop();
                }
                break;
        }
    }

    public void reset() {
        playbackControlsModelObserver.clear();
        playbackControls = null;
        audioAssetForUpload = null;
        sendImmediately = false;
        if (recordingControls != null) {
            recordingControls.cancel();
        }
        recordingControls = null;
        timerTextView.setText("");
        stopRecordingTimer();
        stopRecordingIndicator();
    }

    public void setDarkTheme(boolean dark) {
        darkTheme = dark;
    }

    public void setAccentColor(int color) {
        Drawable drawable = recordingSeekBar.getProgressDrawable();
        if (drawable == null) {
            return;
        }
        if (drawable instanceof LayerDrawable) {
            LayerDrawable layerDrawable = (LayerDrawable) drawable;
            Drawable progress = layerDrawable.findDrawableByLayerId(android.R.id.progress);
            if (progress != null) {
                drawable = progress;
            }
        }
        drawable.setColorFilter(new LightingColorFilter(0xFF000000, color));
        drawable = recordingSeekBar.getThumb();
        drawable.setColorFilter(new LightingColorFilter(0xFF000000, color));
    }

    private boolean slidedUpToSend(MotionEvent motionEvent) {
        if (slideControlState != SlideControlState.RECORDING &&
            slideControlState != SlideControlState.SEND_FROM_RECORDING) {
            return false;
        }

        return motionEvent.getY() <= actionUpMinY;
    }

    private void setSlideControlState(SlideControlState state) {
        if (slideControlState == state) {
            return;
        }
        slideControlState = state;
        switch (state) {
            case RECORDING:
                slideControl.setBackground(defaultSlideControlBackground);
                hintTextView.setText(getResources().getString(R.string.audio_message__recording__slide_control__slide_hint));
                sendButtonTextView.setTextColor(ContextCompat.getColor(getContext(), R.color.accent_green));
                if (darkTheme) {
                    bottomButtonTextView.setTextColor(ContextCompat.getColor(getContext(),
                                                                             R.color.wire__text_color_primary_dark_selector));
                } else {
                    bottomButtonTextView.setTextColor(ContextCompat.getColor(getContext(),
                                                                             R.color.wire__text_color_primary_light_selector));
                }
                bottomButtonTextView.setText(getContext().getResources().getText(R.string.glyph__microphone_on));
                recordingIndicatorContainerView.setVisibility(VISIBLE);
                recordingSeekBar.setVisibility(GONE);
                startRecordingIndicator();
                if (callback != null) {
                    callback.onStartedRecordingAudioMessage();
                }
                break;
            case SEND_FROM_RECORDING:
                slideControl.setBackground(colorSlideControlBackground);
                sendButtonTextView.setTextColor(ContextCompat.getColor(getContext(), R.color.wire__text_color_primary_dark_selector));
                bottomButtonTextView.setTextColor(ContextCompat.getColor(getContext(), R.color.wire__text_color_primary_dark_selector));
                break;
            case PREVIEW:
                if (darkTheme) {
                    bottomButtonTextView.setTextColor(ContextCompat.getColor(getContext(),
                                                                             R.color.wire__text_color_primary_dark_selector));
                } else {
                    bottomButtonTextView.setTextColor(ContextCompat.getColor(getContext(),
                                                                             R.color.wire__text_color_primary_light_selector));
                }
                slideControl.setBackground(defaultSlideControlBackground);
                sendButtonTextView.setTextColor(ContextCompat.getColor(getContext(), R.color.accent_green));
                bottomButtonTextView.setText(getContext().getResources().getText(R.string.glyph__play));
                recordingSeekBar.setVisibility(VISIBLE);
                hintTextView.setText(getResources().getString(R.string.audio_message__recording__slide_control__tap_hint));
                recordingIndicatorContainerView.setVisibility(GONE);
                stopRecordingIndicator();
                stopRecordingTimer();
                break;
        }
    }

    private void startRecordingIndicator() {
        if (recordingIndicatorDotAnimator == null) {
            recordingIndicatorDotAnimator = ObjectAnimator.ofFloat(recordingIndicatorDotView, View.ALPHA, 0f);
            recordingIndicatorDotAnimator.setRepeatCount(ValueAnimator.INFINITE);
            recordingIndicatorDotAnimator.setRepeatMode(ValueAnimator.REVERSE);
            recordingIndicatorDotAnimator.setDuration(RECORDING_INDICATOR_HIDDEN_INTERVAL);
            recordingIndicatorDotAnimator.setStartDelay(RECORDING_INDICATOR_VISIBLE_INTERVAL);
        }
        recordingIndicatorDotAnimator.start();
    }

    private void stopRecordingIndicator() {
        recordingIndicatorDotAnimator.cancel();
    }

    private void stopRecordingTimer() {
        recordingTimerHandler.removeCallbacks(recordingTimerRunnable);
    }

    public interface Callback {
        void onSendAudioMessage(AudioAssetForUpload audioAssetForUpload,
                                AudioEffect appliedAudioEffect,
                                boolean sentWithQuickAction);

        void onCancelledAudioMessageRecording();

        void onPreviewedAudioMessage();

        void onStartedRecordingAudioMessage();
    }
}
