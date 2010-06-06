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
import java.awt.GridLayout;
import java.awt.Color;
import java.awt.Dimension;

import java.util.EnumMap;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

import java.net.URL;
 
public class ReversiBoard {

    private Map<BoardSquareKey, Square> squares = new EnumMap<BoardSquareKey, Square>(BoardSquareKey.class);
    private JFrame frm;
    private JLayeredPane layp;
    private JPanel grid;
    private JPanel dots;
    private JPanel labels;
    private JPanel labelsCorner;
    private JPanel rowLabels;
    private JPanel colLabels;

    private boolean initialized = false;

    public ReversiBoard() {}

    private synchronized void init() throws Exception {

	if (initialized) throw new Exception("ReversiBoard instance already initialized.");
	
	frm = new JFrame("Reversi Board");
	frm.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	
	final int gsd = 8 * Constants.SQUARE_SIZE;
	final int lsd = gsd + (Constants.LABELS_HEIGHT + (2 * Constants.LABELS_GAP) + Constants.SQUARES_GAP);
        final Dimension gridSize = new Dimension(gsd, gsd);
        final Dimension labelsSize = new Dimension(lsd, lsd);
	final int gs = Constants.LABELS_HEIGHT + Constants.LABELS_GAP + Constants.SQUARES_GAP;
	
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
	labels.setBackground(Constants.BACKGROUND_COLOR);
	labels.setOpaque(true);
	
	// Add the North-West corner to the Labels Panel.
	labelsCorner = new JPanel();
	labels.add(labelsCorner);
	labelsCorner.setLayout(new GridLayout(1, 1, 0, 0));
	labelsCorner.setBackground(Color.white);
        labelsCorner.setBounds(Constants.LABELS_GAP, Constants.LABELS_GAP, Constants.LABELS_HEIGHT, Constants.LABELS_HEIGHT);
	JPanel jplc = new JPanel(new BorderLayout());
	labelsCorner.add(jplc);
	jplc.setBackground(Constants.BASE_COLOR);
	
	// Add the Row-List to the Label Panel.
	rowLabels = new JPanel();
	labels.add(rowLabels);
	rowLabels.setLayout(new GridLayout(8, 1, 0, Constants.SQUARES_GAP));
        rowLabels.setBounds(Constants.LABELS_GAP, gs, Constants.LABELS_HEIGHT, gsd);
	rowLabels.setBackground(Constants.BACKGROUND_COLOR);
	for (BoardRowKey brk : BoardRowKey.values()) {
	    JPanel jp = new JPanel(new BorderLayout());
	    jp.setBackground(Constants.BASE_COLOR);
            rowLabels.add(jp);
	    JLabel jl = new JLabel(brk.toString().substring(1, 2), JLabel.CENTER);
	    jl.setFont(Constants.LABELS_FONT);
	    jl.setForeground(Constants.LABEL_TEXT_COLOR);
	    jp.add(jl);
	}
	
	// Add the Column-List to the Label Panel.
	colLabels = new JPanel();
	labels.add(colLabels);
	colLabels.setLayout(new GridLayout(1, 8, Constants.SQUARES_GAP, 0));
        colLabels.setBounds(gs, Constants.LABELS_GAP, gsd, Constants.LABELS_HEIGHT);
	colLabels.setBackground(Constants.BACKGROUND_COLOR);
	for (BoardColKey bck : BoardColKey.values()) {
	    JPanel jp = new JPanel(new BorderLayout());
	    jp.setBackground(Constants.BASE_COLOR);
            colLabels.add(jp);
	    JLabel jl = new JLabel(bck.toString().substring(1, 2), JLabel.CENTER);
	    jl.setFont(Constants.LABELS_FONT);
	    jl.setForeground(Constants.LABEL_TEXT_COLOR);
	    jp.add(jl);
	}

        // Add the grid panel to the Layered Pane. 
        grid = new JPanel();
        layp.add(grid, new Integer(20));
        grid.setLayout(new GridLayout(8, 8, Constants.SQUARES_GAP, Constants.SQUARES_GAP));
        grid.setPreferredSize(gridSize);
        grid.setBounds(gs, gs, gridSize.width, gridSize.height);
	grid.setBackground(Constants.BACKGROUND_COLOR);

	// Ad the dots panel to the Layered Pane.
	dots = new JPanel();
        layp.add(dots, new Integer(30));
        dots.setLayout(null);
        dots.setPreferredSize(gridSize);
        dots.setBounds(gs, gs, gridSize.width, gridSize.height);
	dots.setBackground(Constants.BACKGROUND_COLOR);
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

	this.initialized = true;
    }

    public synchronized boolean isInitialized() {
	return this.initialized;
    }

    public static ReversiBoard initDisplay() {
	final ReversiBoard rb = new ReversiBoard();
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    try {
			rb.init();
		    } catch (Exception e) {
			System.out.println("Exiting on exception: " + e);
			System.exit(1);
		    }
		}
	    });
	return rb;
    }
    
    public void setSquareColor(final BoardSquareKey sk, final SquareColor c) {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    Square sq = squares.get(sk);
		    sq.setSc(c);
		}
	    });
    }

    public void drawInitialBoard() {
	for (BoardSquareKey bsk : BoardSquareKey.values()) {
	    setSquareColor(bsk, SquareColor.EMPTY);
	}
	setSquareColor(BoardSquareKey.D4, SquareColor.WHITE);
	setSquareColor(BoardSquareKey.E5, SquareColor.WHITE);
	setSquareColor(BoardSquareKey.D5, SquareColor.BLACK);
	setSquareColor(BoardSquareKey.E4, SquareColor.BLACK);
    }

    private void setDot(int x, int y) {
	JLabel dot = new JLabel(Constants.GRID_DOT_ICON);
        dots.add(dot);
	int xDotCenter = (Constants.SQUARE_SIZE * x) - Constants.DOT_SIZE / 2;
	int yDotCenter = (Constants.SQUARE_SIZE * y) - Constants.DOT_SIZE / 2;
	dot.setBounds(xDotCenter, yDotCenter, Constants.DOT_SIZE, Constants.DOT_SIZE);    
    }

    public static void main(String[] args) {
	ReversiBoard rb = ReversiBoard.initDisplay();
	rb.drawInitialBoard();
    }

}