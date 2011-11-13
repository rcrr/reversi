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

import java.awt.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.awt.image.*;
import javax.swing.*;

import java.io.*;
import javax.imageio.ImageIO;

import java.util.List;

import static rcrr.reversi.ui.TestPrint.*;

public class TestPrint {

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

    protected final java.util.List<Integer> squares = java.util.Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                                                                              3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                                                                              3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                                                                              3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                                                                              3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                                                                              3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                                                                              3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                                                                              3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                                                                              3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                                                                              3, 3, 3, 3, 3, 3, 3, 3, 3, 3);
    
    protected final java.util.List<Integer> gameMoves = emptyMoves();

    JFrame frame;
    JPanel p;

    public TestPrint() {
        initComponents();
    }

    private void initComponents() {
        frame = new JFrame("Reversi Board");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        p = new TestPrintPanel(this);
        frame.getContentPane().add(p);

        BorderLayout layout = new BorderLayout();
        Dimension dim = new Dimension (FRAME_W, FRAME_H);
        p.setBackground(BG_COLOR);
        p.setPreferredSize(dim);
        p.setLayout(layout);
        p.setOpaque(true);

        frame.pack();
    }

    public void saveImage() {
        BufferedImage image = new BufferedImage(FRAME_H, FRAME_W, BufferedImage.TYPE_INT_RGB);
        Graphics2D g2 = (Graphics2D)image.createGraphics();
        p.paint(g2);
        try {
            File f = new File("/home/rcrr/lisp/prj/myimage.png");
            ImageIO.write(image, "png", f);
        } catch (IOException ioe) {System.out.println("IO error: " + ioe); System.exit(1);}
    }

    public static java.util.List<Integer> emptyMoves() {
        java.util.List<Integer> list = java.util.Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        return list;
    }

    public static int convCol(int idx) {
        int col = idx % 10;
        return col;
    }

    public static int convRow(int idx) {
        int row = idx / 10;
        return row;
    }

    public static int convArray(int col, int row) {
        int idx = 10 * row + col;
        return idx;
    }

    public void setSquare(int ix, int iy, int color) {
        setSquare(convArray(ix, iy), color);
    }

    public void setSquare(int idx, int color) {
        squares.set(idx, color);
        p.repaint();
    }

    public java.util.List<Integer> getMoves() {
        return gameMoves;
    }

    public void setMoves(java.util.List<Integer> moves) {
        for (int i=0; i<100; i++) {
            gameMoves.set(i, moves.get(i));
        }
        p.repaint();
    }

    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new  Runnable() {
            public void run() {
                TestPrint tp = new TestPrint();
                tp.frame.setVisible(true);
                java.util.List<Integer> moves = emptyMoves();
                tp.setSquare(44, WHITE);
                tp.setSquare(45, BLACK);
                tp.setSquare(54, BLACK);
                tp.setSquare(55, WHITE);
                tp.setSquare(56, BLACK);
                // 31st World Othello Championship: Athens 2007 - final: first game
                // Nicolet 35-29 Tominaga K. 
                moves.set(56, 1);
                tp.setSquare(64, WHITE);
                moves.set(64, 2);
                tp.setSquare(53, BLACK);
                moves.set(53, 3);
                tp.setSquare(46, WHITE);
                moves.set(46, 4);
                tp.setSquare(35, BLACK);
                moves.set(35, 5);
                tp.setSquare(63, WHITE);
                moves.set(63, 6);
                tp.setSquare(34, BLACK);
                moves.set(34, 7);
                tp.setSquare(66, WHITE);
                moves.set(66, 8);
                tp.setSquare(65, BLACK);
                moves.set(65, 9);
                tp.setSquare(74, WHITE);
                moves.set(74, 10);
                tp.setMoves(moves);
                tp.saveImage();
            }
        });
    }
}

class TestPrintPanel extends JPanel {
    
    private TestPrint tp;

    public TestPrintPanel(TestPrint tp) {
        this.tp = tp;
    }

    public void  paint(Graphics g) {

        super.paint(g);
        Graphics2D g2 = (Graphics2D) g;
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                   RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_RENDERING,
                                   RenderingHints.VALUE_RENDER_QUALITY);

        g2.setColor(TestPrint.DRAWING_COLOR);

        for (int i=0; i<3; i++) {
            Rectangle2D rectangle = new Rectangle2D.Double(TestPrint.BOARD_GAP - i,
                                                           TestPrint.BOARD_GAP - i,
                                                           TestPrint.BOARD_SIZE + (2 * i),
                                                           TestPrint.BOARD_SIZE + (2 * i));
            g2.draw(rectangle);
        }
        
        for (int i=1; i<8; i++) {
            Line2D line = new Line2D.Double(TestPrint.BOARD_GAP + (i * (TestPrint.SQUARE_SIDE + 1)),
                                            TestPrint.BOARD_GAP,
                                            TestPrint.BOARD_GAP + (i * (TestPrint.SQUARE_SIDE + 1)),
                                            TestPrint.BOARD_GAP + TestPrint.BOARD_SIZE);
            g2.draw(line);
        }

        for (int i=1; i<8; i++) {
            Line2D line = new Line2D.Double(TestPrint.BOARD_GAP,
                                            TestPrint.BOARD_GAP + (i * (TestPrint.SQUARE_SIDE + 1)),
                                            TestPrint.BOARD_GAP + TestPrint.BOARD_SIZE,
                                            TestPrint.BOARD_GAP + (i * (TestPrint.SQUARE_SIDE + 1)));
            g2.draw(line);
        }

        float dia = (float) (1. * TestPrint.DOT_DIA);
        int dotP1 = TestPrint.BOARD_GAP + (2 * (TestPrint.SQUARE_SIDE + 1));
        int dotP2 = TestPrint.BOARD_GAP + (6 * (TestPrint.SQUARE_SIDE + 1));
        java.util.List<Point> dots = java.util.Arrays.asList(new Point(dotP1, dotP1), new Point(dotP1, dotP2), new Point(dotP2, dotP2), new Point(dotP2, dotP1));
        for (Point c : dots) {
            Ellipse2D dot = new Ellipse2D.Double((float) (c.getX() - .5*dia), (float) (c.getY() - .5*dia), (float) dia, (float) dia);
            g2.draw(dot);
            g2.fill(dot);
        }

        g2.setColor(TestPrint.LABEL_TEXT_COLOR);

        for (String label : TestPrint.COL_LABELS) {
            int ix1 = 1 + TestPrint.COL_LABELS.indexOf(label);
            FontRenderContext frc = g2.getFontRenderContext();
            TextLayout tl = new TextLayout(label, TestPrint.L_FONT, frc);
            double x = TestPrint.BOARD_GAP - 0.25 * TestPrint.L_FONT_SIZE + ((ix1 - 0.5) * (TestPrint.LINE_THICKNESS + TestPrint.SQUARE_SIDE));
            double y = .75 * TestPrint.BOARD_GAP;
            tl.draw(g2, (float) x, (float) y);
        }

        for (String label : TestPrint.ROW_LABELS) {
            int iy1 = 1 + TestPrint.ROW_LABELS.indexOf(label);
            FontRenderContext frc = g2.getFontRenderContext();
            TextLayout tl = new TextLayout(label, TestPrint.L_FONT, frc);
            double x = .5 * TestPrint.BOARD_GAP;
            double y = TestPrint.BOARD_GAP + 0.20 * TestPrint.L_FONT_SIZE + ((iy1 - 0.5) * (TestPrint.LINE_THICKNESS + TestPrint.SQUARE_SIDE));
            tl.draw(g2, (float) x, (float) y);
        }

        // from here must still be inserted into Transcript frame ....
        for (int idx = 0; idx<100; idx++) {
            Integer square = tp.squares.get(idx);
            int ix = convCol(idx);
            int iy = convRow(idx);
            Ellipse2D disk = new Ellipse2D.Double(TestPrint.BOARD_GAP + ((ix - 1) * (TestPrint.SQUARE_SIDE + 1)) + ((TestPrint.SQUARE_SIDE - TestPrint.DISK_R) / 2),
                                                  TestPrint.BOARD_GAP + ((iy - 1) * (TestPrint.SQUARE_SIDE + 1)) + ((TestPrint.SQUARE_SIDE - TestPrint.DISK_R) / 2),
                                                  TestPrint.DISK_R,
                                                  TestPrint.DISK_R);
            switch (square) {
            case BLACK: g2.draw(disk);
                g2.fill(disk);
                break;
            case WHITE: g2.draw(disk);
                break;
            case EMPTY: break;
            case OUTER: break;
            }
            int move = tp.gameMoves.get(idx);
            if (move != 0) {
                paintMove(g2, idx, move, square);
            }
        }
    }

    private void paintMove(Graphics2D g2, int idx, int move, int color) {
        String strMove = String.valueOf(move);
        int nchar = strMove.length();
        FontRenderContext frc = g2.getFontRenderContext();
        TextLayout tl = new TextLayout(strMove, MOVE_FONT, frc);
        int ix = convCol(idx);
        int iy = convRow(idx);
        double x = BOARD_GAP + ((ix - 0.74) * (LINE_THICKNESS + SQUARE_SIDE)) - (0.40 * (nchar - 2) * MOVE_FONT_SIZE);
        double y = BOARD_GAP + ((iy - 0.45) * (LINE_THICKNESS + SQUARE_SIDE));
        Color tmpColor = g2.getColor();
        if (color == BLACK) {
            g2.setColor(WHITE_COLOR);
        } else {
            g2.setColor(BLACK_COLOR);
        }
        tl.draw(g2, (float) x, (float) y);
        g2.setColor(tmpColor);
    }

}
