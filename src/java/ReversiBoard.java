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

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import java.awt.geom.*;

import java.net.URL;

enum SquareColor {EMPTY, BLACK, WHITE}

enum BoardRowKey {R1, R2, R3, R4, R5, R6, R7, R8}
enum BoardColKey {CA, CB, CC, CD, CE, CF, CG, CH}

enum BoardSquareKey {A1, A2, A3, A4, A5, A6, A7, A8,
	B1, B2, B3, B4, B5, B6, B7, B8,
	C1, C2, C3, C4, C5, C6, C7, C8,
	D1, D2, D3, D4, D5, D6, D7, D8,
	E1, E2, E3, E4, E5, E6, E7, E8,
	F1, F2, F3, F4, F5, F6, F7, F8,
	G1, G2, G3, G4, G5, G6, G7, G8,
	H1, H2, H3, H4, H5, H6, H7, H8;
}
 
public class ReversiBoard {

    private static final Color BACKGROUND_COLOR = new Color(0, 0, 0);
    private static final Color LABEL_TEXT_COLOR = new Color(220, 220, 220);
    protected static final Color BASE_COLOR = new Color(32, 142, 32);
    
    private static final String WHITE_DISK_ICON_FILE = "images/reversi-white-disk.png";
    private static final String BLACK_DISK_ICON_FILE = "images/reversi-black-disk.png";
    private static final String GRID_DOT_ICON_FILE = "images/reversi-grid-dot.png";

    private static final int squareSize = 70;
    private static final int dotSize = 16;
    private static final int squaresGap = 2;
    private static final int labelsHeight = 20; 
    private static final int labelsGap = 8;

    private static final Font labelsFont = new Font("Courier New", Font.ITALIC, 14);
 
    protected static final URL whiteIconURL = ReversiBoard.class.getResource(WHITE_DISK_ICON_FILE);
    protected static final URL blackIconURL = ReversiBoard.class.getResource(BLACK_DISK_ICON_FILE);
    private static final URL gridDotIconURL = ReversiBoard.class.getResource(GRID_DOT_ICON_FILE);

    Map<BoardSquareKey, Square> squares = new EnumMap<BoardSquareKey, Square>(BoardSquareKey.class);
    JFrame frm;
    JLayeredPane layp;
    JPanel grid;
    JPanel dots;
    JPanel labels;
    JPanel labelsCorner;
    JPanel rowLabels;
    JPanel colLabels;

    public ReversiBoard() {
	
	frm = new JFrame("Reversi Board");
	frm.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	
	final int gsd = 8 * squareSize;
	final int lsd = gsd + (labelsHeight + (2 * labelsGap) + squaresGap);
        final Dimension gridSize = new Dimension(gsd, gsd);
        final Dimension labelsSize = new Dimension(lsd, lsd);
	final int gs = labelsHeight + labelsGap + squaresGap;
	
        // Use a Layered Pane for this application.
	layp = new JLayeredPane();
        frm.getContentPane().add(layp);
        layp.setPreferredSize(labelsSize);
	
	// Add the labels panel to the Layered Pane.
	labels = new JPanel();
	layp.add(labels, new Integer(10));
        labels.setLayout(null);
        labels.setPreferredSize(labelsSize);
        labels.setBounds(0, 0, labelsSize.width, labelsSize.height);
	labels.setBackground(BACKGROUND_COLOR);
	labels.setOpaque(true);
	
	// Add the North-West corner to the Labels Panel.
	labelsCorner = new JPanel();
	labels.add(labelsCorner);
	labelsCorner.setLayout(new GridLayout(1, 1, 0, 0));
	labelsCorner.setBackground(Color.white);
        labelsCorner.setBounds(labelsGap, labelsGap, labelsHeight, labelsHeight);
	JPanel jplc = new JPanel(new BorderLayout());
	labelsCorner.add(jplc);
	jplc.setBackground(BASE_COLOR);
	
	// Add the Row-List to the Label Panel.
	rowLabels = new JPanel();
	labels.add(rowLabels);
	rowLabels.setLayout(new GridLayout(8, 1, 0, squaresGap));
        rowLabels.setBounds(labelsGap, gs, labelsHeight, gsd);
	rowLabels.setBackground(BACKGROUND_COLOR);
	for (BoardRowKey brk : BoardRowKey.values()) {
	    JPanel jp = new JPanel(new BorderLayout());
	    jp.setBackground(ReversiBoard.BASE_COLOR);
            rowLabels.add(jp);
	    JLabel jl = new JLabel(brk.toString().substring(1, 2), JLabel.CENTER);
	    jl.setFont(labelsFont);
	    jl.setForeground(LABEL_TEXT_COLOR);
	    jp.add(jl);
	}
	
	// Add the Column-List to the Label Panel.
	colLabels = new JPanel();
	labels.add(colLabels);
	colLabels.setLayout(new GridLayout(1, 8, squaresGap, 0));
        colLabels.setBounds(gs, labelsGap, gsd, labelsHeight);
	colLabels.setBackground(BACKGROUND_COLOR);
	for (BoardColKey bck : BoardColKey.values()) {
	    JPanel jp = new JPanel(new BorderLayout());
	    jp.setBackground(ReversiBoard.BASE_COLOR);
            colLabels.add(jp);
	    JLabel jl = new JLabel(bck.toString().substring(1, 2), JLabel.CENTER);
	    jl.setFont(labelsFont);
	    jl.setForeground(LABEL_TEXT_COLOR);
	    jp.add(jl);
	}

        // Add the grid panel to the Layered Pane. 
        grid = new JPanel();
        layp.add(grid, new Integer(20));
        grid.setLayout(new GridLayout(8, 8, squaresGap, squaresGap));
        grid.setPreferredSize(gridSize);
        grid.setBounds(gs, gs, gridSize.width, gridSize.height);
	grid.setBackground(BACKGROUND_COLOR);

	// Ad the dots panel to the Layered Pane.
	dots = new JPanel();
        layp.add(dots, new Integer(30));
        dots.setLayout(null);
        dots.setPreferredSize(gridSize);
        dots.setBounds(gs, gs, gridSize.width, gridSize.height);
	dots.setBackground(BACKGROUND_COLOR);
	dots.setOpaque(false);
	setDot(2, 2);
	setDot(6, 2);
	setDot(6, 6);
	setDot(2, 6);

	for (BoardSquareKey bsk : BoardSquareKey.values()) {
	    Square square = new Square(bsk);
            grid.add(square.getJp());
	    squares.put(bsk, square);
	}

	frm.pack();
	frm.setResizable(true);
	frm.setLocationRelativeTo(null);
	frm.setVisible(true);
    }

    public void setSquareColor(BoardSquareKey sk, SquareColor c) {
	squares.get(sk).setSc(c);
    }

    public void resetBoard() {
	for (BoardSquareKey bsk : BoardSquareKey.values()) {
	    setSquareColor(bsk, SquareColor.EMPTY);
	}
	setSquareColor(BoardSquareKey.D4, SquareColor.WHITE);
	setSquareColor(BoardSquareKey.E5, SquareColor.WHITE);
	setSquareColor(BoardSquareKey.D5, SquareColor.BLACK);
	setSquareColor(BoardSquareKey.E4, SquareColor.BLACK);
    }

    private void setDot(int x, int y) {
	JLabel dot = new JLabel(new ImageIcon(gridDotIconURL));
        dots.add(dot);
	int xDotCenter = (squareSize * x) - dotSize / 2;
	int yDotCenter = (squareSize * y) - dotSize / 2;
	dot.setBounds(xDotCenter, yDotCenter, dotSize, dotSize);    
    }
    
    public static void main(String[] args) {
	// Create the frame on the event dispatching thread.
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    ReversiBoard b = new ReversiBoard();
		    b.resetBoard();
		}
	    });
    }

}

class Square {

    private JPanel jp;
    private BoardSquareKey bs;
    private SquareColor c;

    private JLabel piece;

    public Square(BoardSquareKey bs) {
	this.bs = bs;
	this.jp = new JPanel(new BorderLayout());
	jp.setBackground(ReversiBoard.BASE_COLOR);
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
	    case WHITE: iconURL = ReversiBoard.whiteIconURL;
		break;
	    case BLACK: iconURL = ReversiBoard.blackIconURL;
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