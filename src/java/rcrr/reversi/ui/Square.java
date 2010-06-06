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

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.ImageIcon;

import java.net.URL;

class Square {

    private JPanel jp;
    private BoardSquareKey bs;
    private SquareColor c;

    private JLabel piece;

    public Square(BoardSquareKey bs) {
	this.bs = bs;
	this.jp = new JPanel(new BorderLayout());
	jp.setBackground(Constants.BASE_COLOR);
	this.c = SquareColor.EMPTY;
	this.piece = null;
    }

    public JPanel getJp() {return jp;}
    public BoardSquareKey getBs() {return bs;}
    public SquareColor getSc() {return c;}

    public void setSc(SquareColor c) {
	if (this.c != c) {
	    if (piece != null) {
		jp.remove(piece);
		this.piece = null;
	    }
	    URL iconURL = null;
	    switch (c) {
	    case WHITE: iconURL = Constants.WHITE_DISK_ICON_URL;
		break;
	    case BLACK: iconURL = Constants.BLACK_DISK_ICON_URL;
		break;
	    case EMPTY:
		break;
	    }
	    if (c != SquareColor.EMPTY) {
		this.piece = new JLabel(new ImageIcon(iconURL));
		jp.add(this.piece);
	    }
	    this.c = c;
	}
    }
}