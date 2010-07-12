/*
 *  Clock.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
 *
 *  This file is part of the reversi program
 *  http://github.com/rcrr/reversi
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3, or (at your option) any
 *  later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 *  or visit the site <http://www.gnu.org/licenses/>.
 */


package rcrr.reversi;

public class Clock {

    private static final long DEFAULT_GAME_TIME_IN_MINUTES = 30;
    private static final long TIME_UNITS_PER_SECOND = 1000;
    private static final long SECONDS_PER_MINUTE = 60;

    private final long blackTime;
    private final long whiteTime;

    private Clock(final long blackTime, final long whiteTime) {
	this.blackTime = blackTime;
	this.whiteTime = whiteTime;
    }

    public static Clock initialClock(Long gameTimeInMinutes) {
	if (gameTimeInMinutes == null) gameTimeInMinutes = DEFAULT_GAME_TIME_IN_MINUTES;
	final long t = SECONDS_PER_MINUTE * TIME_UNITS_PER_SECOND * gameTimeInMinutes;
	return new Clock (t, t);
    }

    public Clock setTime(SquareState player, long deltaTime) throws GameOverException {
	switch(player) {
	case BLACK:
	    final long bRemainingTime = blackTime - deltaTime;
	    if (bRemainingTime < 0) throw new GameOverException("BLACK player exceded his game time limit.");
	    return new Clock(bRemainingTime, whiteTime);
	case WHITE:
	    final long wRemainingTime = whiteTime - deltaTime;
	    if (wRemainingTime < 0) throw new GameOverException("WHITE player exceded his game time limit.");
	    return new Clock(blackTime, wRemainingTime);
	}
	// throw new Exception("Only BLACK and WHITE players are supported by Clock.");
	System.out.println("Only BLACK and WHITE players are supported by Clock.");
	System.exit(1);
	return null;
    }

    private static String timeString(long time) {
	long rTime = Math.round(time / TIME_UNITS_PER_SECOND);
	long minutes = (long) Math.floor(rTime / SECONDS_PER_MINUTE);
	long seconds = rTime - (minutes * SECONDS_PER_MINUTE);
	return minutes + ":" + seconds;
    }

    @Override public String toString() {
	return "[ " + SquareState.BLACK + "=" + timeString(blackTime) + ", " + 
	    SquareState.WHITE + "=" + timeString(whiteTime) + " ]";
    }

}