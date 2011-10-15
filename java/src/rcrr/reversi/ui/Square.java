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
import javax.swing.Icon;
import javax.swing.ImageIcon;

import java.net.URL;

class Square {

    private JPanel jp;
    private BoardSquareKey bs;
    private SquareColor color;

    private JLabel piece;
    private Icon ico;

    public Square(BoardSquareKey bs) {
	this.bs = bs;
	this.jp = new JPanel(new BorderLayout());
	jp.setBackground(Constants.BASE_COLOR);
	this.color = SquareColor.EMPTY;
	this.ico = null;
	this.piece = new JLabel(ico);
	jp.add(piece);
    }

    public JPanel getJp() {return jp;}
    public BoardSquareKey getBs() {return bs;}
    public SquareColor getSc() {return color;}

    public void setSc(SquareColor c) {

	if (color != c) {
	    switch (c) {
	    case WHITE: ico = Constants.WHITE_DISC_ICON;
		break;
	    case BLACK: ico = Constants.BLACK_DISC_ICON;
		break;
	    case EMPTY: ico = null;
		break;
	    }
	    piece.setIcon(ico);
	    this.color = c;
	}
    }
}