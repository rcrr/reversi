/*
 *  TranscriptFrame.java
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
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 *  or visit the site <http://www.gnu.org/licenses/>.
 */

package rcrr.reversi.ui;

import java.util.List;
import java.util.Arrays;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Line2D;
import java.awt.geom.Ellipse2D;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;

import javax.swing.JFrame;
import javax.swing.JPanel;

import rcrr.reversi.Game;
import rcrr.reversi.Square;
import rcrr.reversi.SquareState;

public class TranscriptFrame extends JFrame {

    private final class BoardTranscriptPanel extends JPanel {

	private final Game game;

	public BoardTranscriptPanel(final Game game) {
	    this.game = game;
	}

	@Override public void  paint(Graphics g) {
	    super.paint(g);
	    final Graphics2D g2 = (Graphics2D) g;
	    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
	    g2.setRenderingHint(RenderingHints.KEY_RENDERING,
				RenderingHints.VALUE_RENDER_QUALITY);

	    drawBoardFrame(g2);
	    drawBoardVerticalLines(g2);
	    drawBoardHorizontalLines(g2);
	    drawBoardDots(g2);
	    drawBoardLabels(g2);

	    // Must be inserted the disc drawing method.
	    drawGamePosition(g2);

	}

	private void drawBoardFrame(final Graphics2D g2) {
	    for (int i=0; i<3; i++) {
		final Rectangle2D rectangle = new Rectangle2D.Double(BOARD_GAP - i,
								     BOARD_GAP - i,
								     BOARD_SIZE + (2 * i),
								     BOARD_SIZE + (2 * i));
		g2.draw(rectangle);
	    }
	}

	private void drawBoardVerticalLines(final Graphics2D g2) {
	    for (int i=1; i<8; i++) {
		final Line2D line = new Line2D.Double(BOARD_GAP + (i * (SQUARE_SIDE + 1)),
						      BOARD_GAP,
						      BOARD_GAP + (i * (SQUARE_SIDE + 1)),
						      BOARD_GAP + BOARD_SIZE);
		g2.draw(line);
	    }
	}

	private void drawBoardHorizontalLines(final Graphics2D g2) {
	    for (int i=1; i<8; i++) {
		final Line2D line = new Line2D.Double(BOARD_GAP,
						      BOARD_GAP + (i * (SQUARE_SIDE + 1)),
						      BOARD_GAP + BOARD_SIZE,
						      BOARD_GAP + (i * (SQUARE_SIDE + 1)));
		g2.draw(line);
	    }
	}

	private void drawBoardDots(final Graphics2D g2) {
	    final float dia = (float) (1. * DOT_DIA);
	    final int dotP1 = BOARD_GAP + (2 * (SQUARE_SIDE + 1));
	    final int dotP2 = BOARD_GAP + (6 * (SQUARE_SIDE + 1));
	    final List<Point> dots = Arrays.asList(new Point(dotP1, dotP1),
						   new Point(dotP1, dotP2),
						   new Point(dotP2, dotP2),
						   new Point(dotP2, dotP1));
	    for (final Point c : dots) {
		final Ellipse2D dot = new Ellipse2D.Double((float) (c.getX() - .5 * dia),
							   (float) (c.getY() - .5 * dia),
							   (float) dia,
							   (float) dia);
		g2.draw(dot);
		g2.fill(dot);
	    }
	}

	private void drawBoardLabels(final Graphics2D g2) {
	    g2.setColor(LABEL_TEXT_COLOR);
	    for (String label : COL_LABELS) {
		final int ix1 = 1 + COL_LABELS.indexOf(label);
		final FontRenderContext frc = g2.getFontRenderContext();
		final TextLayout tl = new TextLayout(label, L_FONT, frc);
		final double x = BOARD_GAP - 0.25 * L_FONT_SIZE + ((ix1 - 0.5) * (LINE_THICKNESS + SQUARE_SIDE));
		final double y = .75 * BOARD_GAP;
		tl.draw(g2, (float) x, (float) y);
	    }
	    for (String label : ROW_LABELS) {
		final int iy1 = 1 + ROW_LABELS.indexOf(label);
		final FontRenderContext frc = g2.getFontRenderContext();
		final TextLayout tl = new TextLayout(label, L_FONT, frc);
		final double x = .5 * BOARD_GAP;
		final double y = BOARD_GAP + 0.20 * L_FONT_SIZE + ((iy1 - 0.5) * (LINE_THICKNESS + SQUARE_SIDE));
		tl.draw(g2, (float) x, (float) y);
	    }
	}

	private void drawGamePosition(final Graphics2D g2) {
	    for (Square square : Square.values()) {
		final SquareState color = game.board().get(square);
		final int ix = square.column().ordinal();
		final int iy = square.row().ordinal();
		final Ellipse2D disc = new Ellipse2D.Double(BOARD_GAP + (ix * (SQUARE_SIDE + 1)) + ((SQUARE_SIDE - DISK_R) / 2),
							    BOARD_GAP + (iy * (SQUARE_SIDE + 1)) + ((SQUARE_SIDE - DISK_R) / 2),
							    DISK_R,
							    DISK_R);
		switch (color) {
		case BLACK:
		    g2.draw(disc);
		    g2.fill(disc);
		    break;
		case WHITE:
		    g2.draw(disc);
		    break;
		case EMPTY: break;
		case OUTER: throw new RuntimeException("Unsopported disc color: OUTER.");
		}
	    }
	}



    }

    public static final int DISK_R = 32;
    public static final int SQUARE_SIDE = 40;
    public static final int LINE_THICKNESS = 1;
    public static final int BOARD_GAP = 32;
    public static final int DOT_DIA = 6;

    public static final int EMPTY = 0;
    public static final int BLACK = 1;
    public static final int WHITE = 2;
    public static final int OUTER = 3;

    public static final int L_FONT_SIZE = 12;
    public static final Font L_FONT = new Font("Courier New", Font.ITALIC, L_FONT_SIZE);
    public static final int MOVE_FONT_SIZE = 13;
    public static final Font MOVE_FONT = new Font("Lucida Console", Font.BOLD, MOVE_FONT_SIZE);

    public static final Color BG_COLOR = new Color(220, 220, 220);
    public static final Color DRAWING_COLOR = new Color(30, 30, 30);
    public static final Color LABEL_TEXT_COLOR = new Color(10, 10, 10);
    public static final Color BLACK_COLOR = new Color(0, 0, 0);
    public static final Color WHITE_COLOR = new Color(255, 255, 255);

    public static final java.util.List<String> COL_LABELS = java.util.Arrays.asList("A", "B", "C", "D", "E", "F", "G", "H");
    public static final java.util.List<String> ROW_LABELS = java.util.Arrays.asList("1", "2", "3", "4", "5", "6", "7", "8");

    public static final int BOARD_SIZE = (LINE_THICKNESS * 9) + (SQUARE_SIDE * 8);
    public static final int FRAME_H = (BOARD_GAP * 2) + BOARD_SIZE;
    public static final int FRAME_W = (BOARD_GAP * 2) + BOARD_SIZE;

    private final Game game;
    private final JPanel board;

    public TranscriptFrame(final Game game) {
	super("Game Transcript");
	this.game = game;
	setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	board = new BoardTranscriptPanel(game);
	getContentPane().add(board);
        final BorderLayout layout = new BorderLayout();
	final Dimension dim = new Dimension (FRAME_W, FRAME_H);
	board.setBackground(BG_COLOR);
	board.setPreferredSize(dim);
        board.setLayout(layout);
	board.setOpaque(true);
	setVisible(true);
        pack();
    }

}
