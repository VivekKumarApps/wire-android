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
package com.waz.zclient.pages.main.participants.views;

import android.content.Context;
import android.support.v4.view.PagerAdapter;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.view.ViewGroup;
import com.waz.utils.WhereAmI;
import com.waz.zclient.R;
import com.waz.zclient.utils.ViewUtils;
import com.waz.zclient.views.ParticipantDetailsTab;
import timber.log.Timber;

public class TabbedParticipantPagerAdapter extends PagerAdapter implements RecyclerView.RecyclerListener {

    private Callback callback;
    private Context context;
    private ParticipantOtrDeviceAdapter participantOtrDeviceAdapter;

    public enum ParticipantTabs {
        DETAILS(R.string.otr__participant__tab_details),
        DEVICES(R.string.otr__participant__tab_devices);

        private final int label;

        ParticipantTabs(int label) {
            this.label = label;
        }

        int getLabel() {
            return label;
        }
    }

    public TabbedParticipantPagerAdapter(Context context, ParticipantOtrDeviceAdapter participantOtrDeviceAdapter, Callback callback) {
        this.context = context;
        this.callback = callback;
        this.participantOtrDeviceAdapter = participantOtrDeviceAdapter;
    }

    @Override
    public Object instantiateItem(ViewGroup container, int position) {
        View v;
        switch (ParticipantTabs.values()[position]) {
            case DETAILS:
                v = new ParticipantDetailsTab(context);
                v.setTag(ParticipantTabs.DETAILS);
                break;
            case DEVICES:
                RecyclerView rv = new RecyclerView(context);
                rv.setLayoutManager(new LinearLayoutManager(context));
                rv.setHasFixedSize(true);
                rv.setAdapter(participantOtrDeviceAdapter);
                rv.setRecyclerListener(this);
                ViewUtils.setPaddingBottom(rv, context.getResources().getDimensionPixelSize(R.dimen.participants__otr_device__padding_bottom));
                rv.setClipToPadding(false);
                v = rv;
                break;
            default:
                throw new RuntimeException("Unexpected ViewPager position");
        }
        container.addView(v);
        return v;
    }

    @Override
    public void destroyItem(ViewGroup container, int position, Object view) {
        container.removeView((View) view);
    }


    @Override
    public int getCount() {
        return ParticipantTabs.values().length;
    }

    @Override
    public boolean isViewFromObject(View view, Object object) {
        return view == object;
    }

    @Override
    public CharSequence getPageTitle(int position) {
        return context.getString(ParticipantTabs.values()[position].getLabel());
    }

    @Override
    public void finishUpdate(ViewGroup container) {
        super.finishUpdate(container);
        if (callback != null) {
            callback.finishUpdate();
        }
    }

    @Override
    public void onViewRecycled(RecyclerView.ViewHolder holder) {
        if (holder instanceof ParticipantOtrDeviceAdapter.ViewHolder) {
            ((ParticipantOtrDeviceAdapter.ViewHolder) holder).recycle();
        }
    }

    public interface Callback {
        void finishUpdate();
    }
}
