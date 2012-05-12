/*
 *  SquarePanel.java
 *
 *  Copyright (c) 2010, 2011 Roberto Corradini. All rights reserved.
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

package rcrr.reversi.ui;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.Icon;
import javax.swing.ImageIcon;

import rcrr.reversi.board.Square;
import rcrr.reversi.board.SquareState;

class SquarePanel extends JPanel {

    private Square square;
    private SquareState state;

    private JLabel disc;
    private Icon icon;

    public SquarePanel(final Square square) {
        super(new BorderLayout());
        setBackground(Constants.BASE_COLOR);
        this.square = square;
        this.state = SquareState.EMPTY;
        this.icon = null;
        this.disc = new JLabel(this.icon);
        add(this.disc);
    }

    public Square getSquare() { return this.square; }
    public SquareState getSquareState() { return this.state; }

    public void setSquareState(final SquareState state) {
        if (this.state != state) {
            switch (state) {
            case WHITE: this.icon = Constants.WHITE_DISC_ICON;
                break;
            case BLACK: this.icon = Constants.BLACK_DISC_ICON;
                break;
            case EMPTY: this.icon = null;
                break;
            default: throw new RuntimeException("Unsupported SquareState: " + state);
            }
            this.disc.setIcon(this.icon);
            this.state = state;
        }
    }
}
