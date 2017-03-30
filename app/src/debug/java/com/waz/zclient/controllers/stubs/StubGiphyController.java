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
package com.waz.zclient.controllers.stubs;

import com.waz.zclient.controllers.giphy.GiphyObserver;
import com.waz.zclient.controllers.giphy.IGiphyController;

/**
 * This class was AUTOGENERATED.
 *
 * All changes will be overwritten with the next build.
 */
public class StubGiphyController implements IGiphyController {
  @Override
  public void close() {

  }

  @Override
  public void cancel() {

  }

  @Override
  public void addObserver(GiphyObserver observer) {

  }

  @Override
  public void tearDown() {

  }

  @Override
  public void removeObserver(GiphyObserver observer) {

  }

  @Override
  public boolean handleInput(String text) {
    return false;
  }

  @Override
  public void search(String keyword) {

  }

    @Override
    public void searchTrending() {

    }

    @Override
  public void searchRandom() {

  }

  @Override
  public boolean isInputAllowedForGiphy(String input) {
    return false;
  }
}
