/*
    Copyright (c) 2010 Roberto Corradini

    This file is part of the reversi program
    http://github.com/rcrr/reversi

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 3, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
    or visit the site <http://www.gnu.org/licenses/>.
*/

package rcrr.reversi.ui;

import java.net.URL;

import java.awt.Color;
import java.awt.Font;

import javax.swing.Icon;
import javax.swing.ImageIcon;


public class Constants {

    public static final Color BACKGROUND_COLOR = new Color(0, 0, 0);
    public static final Color LABEL_TEXT_COLOR = new Color(220, 220, 220);
    public static final Color BASE_COLOR = new Color(32, 142, 32);
    
    public static final String WHITE_DISK_ICON_FILE = "images/reversi-white-disk.png";
    public static final String BLACK_DISK_ICON_FILE = "images/reversi-black-disk.png";
    public static final String GRID_DOT_ICON_FILE = "images/reversi-grid-dot.png";
 
    public static final URL WHITE_DISK_ICON_URL = Constants.class.getResource(WHITE_DISK_ICON_FILE);
    public static final URL BLACK_DISK_ICON_URL = Constants.class.getResource(BLACK_DISK_ICON_FILE);
    public static final URL GRID_DOT_ICON_URL = Constants.class.getResource(GRID_DOT_ICON_FILE);

    public static final Icon WHITE_DISK_ICON = new ImageIcon(WHITE_DISK_ICON_URL);
    public static final Icon BLACK_DISK_ICON = new ImageIcon(BLACK_DISK_ICON_URL);
    public static final Icon GRID_DOT_ICON = new ImageIcon(GRID_DOT_ICON_URL);

    public static final int SQUARE_SIZE = 70;
    public static final int DOT_SIZE = 16;
    public static final int SQUARES_GAP = 2;
    public static final int LABELS_HEIGHT = 20; 
    public static final int LABELS_GAP = 8;

    public static final Font LABELS_FONT = new Font("Courier New", Font.ITALIC, 14);

}