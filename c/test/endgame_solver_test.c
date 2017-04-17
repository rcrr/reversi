/**
 * @file
 *
 * @brief Endgame solvers unit test suite.
 * @details Collects tests and helper methods for all the solvers under the endgame_solver umbrella.
 *
 * @par endgame_solver_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2017 Roberto Corradini. All rights reserved.
 *
 * @par License
 * <tt>
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * \n
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 * </tt>
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include <glib.h>

#include "board.h"
#include "game_position_db.h"

#include "exact_solver.h"
#include "improved_fast_endgame_solver.h"
#include "minimax_solver.h"


/**
 * @brief GamePositionDb fixture
 */
typedef struct {
  GamePositionDb *db;
} GamePositionDbFixture;

/**
 * @brief A test case is used to automate the execution of a set of test game position.
 */
typedef struct {
  gchar *gpdb_label;       /**< @brief The game position label used to acces the gpdb database. */
  int    best_move_count;  /**< @brief The count of best moves. */
  int    outcome;          /**< @brief The expected game position value. */
  Square best_move[64];    /**< @brief The expected best move array. */
} TestCase;

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, number 01.
 */
const TestCase ffo_01_simplified_4[] =
  {
    { "ffo-01-simplified-4", 1, +18, { B6 } }, // ffo-01-simplified-4;..bbbbb..wbwbb.w.bwwbbwwbbbbbbbwwbbbwwbww.bwbwwwwwwwwwwwbbbbbbb.;b;From position ffo-01-simplified-3 executing A7;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, number 05.
 */
const TestCase ffo_05[] =
  {
    { "ffo-05", 1, +32, { G8 } }, // ffo-05;.wwwww....wbbw.bbbwbwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bwww...bbbbb..;b; G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 1 to 19.
 */
const TestCase ffo_01_19[] =
  {
    { "ffo-01", 1, +18, { G8 } }, // ffo-01;..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;b; G8:+18. H1:+12. H7:+6. A2:+6. A3:+4. B1:-4. A4:-22. G2:-24.;
    { "ffo-02", 1, +10, { A4 } }, // ffo-02;.bbbbbb...bwwww..bwbbwwb.wwwwwwwwwwwbbwwwwwbbwwb..bbww....bbbbb.;b; A4:+10. B2:+0. A3:-6. G7:-8. A7:-12. H7:-14. B7:-14. H2:-24.;
    { "ffo-03", 1,  +2, { D1 } }, // ffo-03;....wb....wwbb...wwwbb.bwwbbwwwwwbbwbbwwwbbbwwwwwbbbbwbw..wwwwwb;b; D1:+2. G3:+0. B8:-2. B1:-4. C1:-4. A2:-4. A3:-6. B2:-12.;
    { "ffo-04", 1,  +0, { H8 } }, // ffo-04;.bbbbbb.b.bbbww.bwbbbwwbbbwbwwwb.wbwwbbb..wwwbbb..wwbb....bwbbw.;b; H8:+0. A5:+0. B6:-4. B7:-4. A6:-8. B2:-12. H2:-26.;
    { "ffo-05", 1, +32, { G8 } }, // ffo-05;.wwwww....wbbw.bbbwbwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bwww...bbbbb..;b; G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.;
    { "ffo-06", 1, +14, { A1 } }, // ffo-06;..wbbb..wwwbbb..wwwbwbw.wwbwwwb.wwbbbbbbbwwbbwb..wwwwb...bbbbbb.;b; A1:+14. H3:+14. A8:+12. H2:+8. G2:+8. H4:+4. G7:+4. A7:-22. B1:-24.;
    { "ffo-07", 1,  +8, { A6 } }, // ffo-07;..wbbw..bwbbbb..bwwwbbbbbwwbbbbbbwwwwbbb.bbbbbbb..bbwww....bbww.;b; A6:+8. G1:+0. A1:-2. H8:-6. H7:-14. B1:-30.;
    { "ffo-08", 1,  +8, { E1 } }, // ffo-08;...b.b..b.bbbb..bbbbwbbbbbbwwwwwbbwbbbw.bwbbbbw.bwwbbb..bwwbbw..;w; E1:+8. H2:+4. G2:+4. B2:+4. G7:+4. B1:+2. G1:-6. C1:-8.;
    { "ffo-09", 1,  -8, { A4 } }, // ffo-09;..bwbb..w.wwbbbb.wwwbbbb.bwbbbwbbbwbwwwbwbbwbwbb..wbww....wwww..;w; G7:-8. A4:-8. B1:-16. A7:-16. B7:-26. A3:-30. G1:-38. H7:-40.;
    { "ffo-10", 1, +10, { B2 } }, // ffo-10;.bbbb.....wbbb..bwbwbwbbwbwbbwbbwbbwbwwwbbbwbwwb..wbbw...wwwww..;w; B2:+10. B7:+4. F1:+0. A7:-4. A2:-6. G2:-12. H2:-16. H7:-20.;
    { "ffo-11", 1, +30, { B3 } }, // ffo-11;...w.bwb....bbwb...bbwwbw.bbwbwbbbbwwbwb.bwwbbbbbwwwbb.bwwwwwww.;w; B3:+30. C2:+26. A6:+24. G7:+20. C3:+18. D2:+16. B4:+10. E1:+6.;
    { "ffo-12", 2,  -8, { A7, B7 } }, // ffo-12;..w..w..b.wwwwb.bbwwwbwwbbwbwbwwbbwbbwwwbbbbwwww..wbbb...bbbbb..;w; B7:-8. A7:-10. G7:-14. G8:-14. H2:-16. G1:-16. H1:-20.;
    { "ffo-13", 1, +14, { B7 } }, // ffo-13;..bbbbb..wwwbb...wwwbbbb.wbwbwbbwbbbwbbb..bwbwbb..wbwww..wwwww..;b; B7:+14. A4:+0. A3:-8. B1:-18. G8:-20. H7:-20. A2:-24.;
    { "ffo-14", 1, +18, { A3 } }, // ffo-14;..bbbbb...wwwb...bwwbbbb.wwwwwwwwwwbbbwwwwwbbwwb..bbww....bbbbb.;b; A3:+18. A4:+12. B1:+8. G7:-4. H7:-14. A7:-24. B7:-24. B2:-28.;
    { "ffo-15", 1,  +4, { G3 } }, // ffo-15;....w......wwb...wwwbb.bwwwbwwwwwbbwbbwwwbbbwwwwwbbbwwbw..wwwwwb;b; G3:+4. B8:+4. F1:+0. C1:+0. C2:-2. D1:-4. B2:-8. A3:-8.;
    { "ffo-16", 1, +24, { F8 } }, // ffo-16;.bbbbbb.b.bbbww.bwbbbwwbbwwbbbwb.wwwbbbb..wwbbbb...www....bwb.w.;b; F8:+24. C7:+20. A5:+6. H1:+6. B6:+0. B7:-2. A6:-6. H2:-26.;
    { "ffo-17", 1,  +8, { F8 } }, // ffo-17;.wwwww....wbbw.bbbwwwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bww....bbbb...;b; F8:+8. G2:+6. G6:-24. G1:-32. F7:-32. G7:-34. B2:-38.;
    { "ffo-18", 1,  -2, { G2 } }, // ffo-18;.bbb......wwwb..bwwwwwbbwbwbwwbbwbbwwwwwbbbwbwwb..wbbw...wwwww..;b; G2:-2. B7:-6. F1:-8. E1:-10. H7:-12. G8:-14. G7:-14. A2:-18. B2:-18.;
    { "ffo-19", 1,  +8, { B6 } }, // ffo-19;..wbbw..bwbbbb..bwwwwbbbbwwwbbbbb.wwwbbb..wwwwbb..bbwww....bbww.;b; B6:+8. H8:+4. B7:+0. G1:-6. B5:-16. H7:-16. B1:-24.;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 20 to 29.
 */
const TestCase ffo_20_29[] =
  {
    {"ffo-20", 1,  +6, { H5 } }, // ffo-20;bbbwbbbbwbbbbbbbwwbbbbbbwwwbbbbbwwwbbww.wwwww...wwwwwww.wwwwwww.;b; H5:+6. G6:-2. F6:-4. H6:-10;
    {"ffo-21", 1,  +0, { G5 } }, // ffo-21;wwwwwwwwbwwbbb..bbwwbww.bwbwww..bwwwwb..bwwbww..bwwwww..bbbb....;w; G5:+0. G2:-2. G4:-4. G6:-6;
    {"ffo-22", 1,  +2, { G8 } }, // ffo-22;..wwww..b.wwwww.bbwwbwbbbwbwbbbbbbbwbbbb.bbwbwbb..wbbb.b....b...;w; G8:+2. A6:+0. F8:-4. A7:-4. H2:-4. B2:-6. D8:-8. B7:-14. G7:-26;
    {"ffo-23", 1,  +4, { A2 } }, // ffo-23;..w.......wwb...wwwbbbw.wwwwbwbbbbbwwbwbbbbbbwwbb.bbbbwb..bbbb..;b; A2:+4. D1:-20. H3:-20. B1:-30. G2:-30. E1:-30. F2:-34. G8:-34. B2:-36. H2:-38;
    {"ffo-24", 1,  +0, { C3 } }, // ffo-24;..w..w.....wwwb..b.bwbww..bbbwwwbbbbwwwwbbbwbbwwbbbbbb..bwbb.w..;w; C3:+0. B4:-4. C2:-8. E8:-12. G7:-14. H2:-16. G1:-24;
    {"ffo-25", 2,  +0, { G1, A5 } }, // ffo-25;....b......bbbw..wwwbbbbbwwwwbbw.bbwwbbwwwbwbbbbwwwbb...b.bbbb..;w; G1:+0. A5:+0. F1:-4. D1:-6. F7:-8. C2:-10. G7:-10. H2:-12. H7:-16;
    {"ffo-26", 1,  +0, { D8 } }, // ffo-26;.wwwww....wbbw...wwwwbbw.wwwbwbb.wwbwwbb.bwbbwbb..w.bbbb..w....w;b; D8:+0. A6:-2. A4:-6. B7:-6. A5:-12. G1:-16. A2:-16. A3:-18. H2:-18. B8:-20. G2:-20. B2:-26;
    {"ffo-27", 1,  -2, { B7 } }, // ffo-27;..bw.w....wwww..wwbwbbw.wwwwbbwwwwwbbwb.wbwbbbbb..bbbb....b.w.b.;b; B7:-2. E1:-4. B1:-6. H2:-10. H5:-10. B2:-12. A2:-14. H3:-28. G1:-28. G2:-28;
    {"ffo-28", 3,  +0, { F1, B2, E1 } }, // ffo-28;..w.......www..b.bwwwwbbbbbbwbwb.bbwbwwbbbwbwwbb.wwwww.b...www..;b; F1:+0. B2:+0. E1:+0. B1:-4. F2:-6. G7:-6. D1:-12. C8:-20. G8:-22. B8:-28;
    {"ffo-29", 1, +10, { G2 } }, // ffo-29;.wbbbb....wbbw..bbwwbwwwbbbwwbwwbbwwbwwwbbbbww.bb.bbw...........;b; G2:+10. A1:+4. G6:-10. H2:-12. F8:-12. E8:-12. G7:-24. G1:-24. B2:-30. F7:-34;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 30 to 39.
 */
const TestCase ffo_30_39[] =
  {
    {"ffo-30", 1,  +0, { G3 } }, // ffo-30;.bbb....b.bww...bbwbww..bwbwbw..bwwbwbbbbwwbbwb...wwwww..bbbbb..;b; G3:+0. G2:-12. E1:-16. F2:-18. F1:-22. G4:-22. H6:-24. B7:-24. G8:-28;
    {"ffo-31", 1,  -2, { G6 } }, // ffo-31;.wwwww....wwww..wbbwww...bbbww..bbbbbbw.bbbwww.wb.wwww...wwwww..;b; G6:-2. G3:-4. G4:-8. G7:-14. H5:-14. G2:-16. G1:-30. G8:-32;
    {"ffo-32", 1,  -4, { G3 } }, // ffo-32;..bb....w.bbwb..wwbww...wbwbwww.wwbbwwwbwwbbbwwb..bbbbwb..b..b.b;b; G3:-4. B7:-6. E1:-8. H4:-10. F3:-10. H3:-10. B2:-14. A7:-22;
    {"ffo-33", 2,  -8, { E7, A3 } }, // ffo-33;.bbbbbbb..bwww....wbwwbb.wwbbwbb.wwwwwbb.b.bwwbb...w.b.b..wwww..;b; E7:-8. A3:-8. A6:-12. B2:-12. G7:-12. G2:-12. A4:-14. C6:-20. A5:-22. B3:-28;
    {"ffo-34", 1,  -2, { C2 } }, // ffo-34;.............w.w.wwwwwwwwwwwwbwwwbbwwwbw.bbbwbww..bbbwbw..wbbbbw;b; C2:-2. D2:-6. E2:-6. A3:-10. A2:-10. F1:-12. G2:-14. G1:-16. B2:-20. B8:-26;
    {"ffo-35", 1,  +0, { C7 } }, // ffo-35;..bbb.....bbbb.wwwbbwwwwwwwwwwbw.wwbbbbw.wwwbbbw...bwbb...b.....;w; C7:+0. D8:-8. H8:-8. B2:-12. G1:-14. E8:-20. B1:-20. F8:-24. F1:-32. H7:-32. G8:-38;
    {"ffo-36", 1,  +0, { B7 } }, // ffo-36;...b.w....bbbw.bbbbbbbbbbwwbbwwbbwbwwwbbbbwwww.bb..wwww.........;w; B7:+0. B1:-2. E1:-4. C1:-6. G6:-8. G2:-10. A2:-22. B2:-24;
    {"ffo-37", 1, -20, { G2 } }, // ffo-37;..wwww..w.wwww..wbbbwww.wbbwbw..wwbbwbb.wwbbbb..w.bbb.....bb.w..;b; G2:-20. G4:-22. B7:-22. H3:-22. G1:-30. H2:-42. B1:-48;
    {"ffo-38", 1,  +4, { B2 } }, // ffo-38;..wwww....wwww...bwbbwwbwwbwwwwb.wwwwwbbbwwbbbbb..b.b...........;b; B2:+4. A5:+0. H2:-4. A3:-10. A7:-18. G2:-20. B7:-22. G1:-24. B1:-26;
    {"ffo-39", 9, +64, { A8, B1, G1, G5, G6, C8, H3, E8, H4 } }, // ffo-39;w.wwww..bwbbwb..bwwwbbb.bwwwbb..bwwbwb..bwbbb...b.bb............;w; A8:+64. B1:+64. G1:+64. G5:+64. G6:+64. C8:+64. H3:+64. E8:+64. H4:+64. F7:+62. D8:+62. E7:+62. H2:+62. B8:+62. G2:+60. G4:+60. F6:+32;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 40 to 49.
 */
const TestCase ffo_40_49[] =
  {
    {"ffo-40", 1, +38, { A2 } }, // ffo-40;w..wwwwb.wwwwwwbwwbbwwwbwwbwwwbbwwwwwwbb...wwwwb....w..b........;b;FFO position 40, black to move, Turner vs Monnom, Bruxelles 1997.;
    {"ffo-41", 1,  +0, { H4 } }, // ffo-41;.wwwww....wwwwb..wwwwww.bbbbbww..bbwwb..wwbwbb....wbbw...www..w.;b;FFO position 41, black to move, Eclipse vs Logistello, Internet (7-12) 1996.;
    {"ffo-42", 1,  +6, { G2 } }, // ffo-42;..www.......bb.wwwwwwbww.wwwwbwwb.wwwbbw...wwbww...wwwbw..wwww..;b;FFO position 42, black to move, Penloup vs Shaman, Cambridge 1998.;
    {"ffo-43", 2, -12, { G3, C7 } }, // ffo-43;..bbbbb...bbbb...wwwbb...wwbbbb..wwbbbw.wwwwbww....bwb....bbbbb.;w;FFO position 43, white to move, Brightwell vs Suekuni, WC 1997 (final).;
    {"ffo-44", 2, -14, { D2, B8 } }, // ffo-44;..w.b.w...w.bw.w.wwbbbwwwwwwbbbwwwwwbb..bbwwbw....bbbb.....bbb..;w;FFO position 44, white to move, Shaman vs Tastet, WC 1995.;
    {"ffo-45", 1,  +6, { B2 } }, // ffo-45;...bbbb.b.bbbw..bbwbww..bbbwbw..bbwbbw...wbbbww.w.wwww......ww..;b;FFO position 45, black to move, Tamenori vs Shaman, WC 1995 (final).;
    {"ffo-46", 1,  -8, { B3 } }, // ffo-46;...bbb....wwwb....wwwbb..wwwwbbb..wwwwbb..wbwbbb..bbww...bbbb.w.;b;FFO position 46, black to move, Caspard vs Juhem, FC 1994.;
    {"ffo-47", 1,  +4, { G2 } }, // ffo-47;.wwwww....wwww...wwwwb..bbbbbb...wbwwb..wwwbwb....wwbb....bbbb..;w;FFO position 47, white to move, Brightwell vs Tastet, WC 1997.;
    {"ffo-48", 1, +28, { F6 } }, // ffo-48;.....b..b.bbb...bbbbww..bwbwwbb.bwwbbb..bwwbb.....wwwb...bbbbbb.;w;FFO position 48, white to move, Brightwell vs Tastet, Paris Open 1997.;
    {"ffo-49", 1, +16, { E1 } }, // ffo-49;..wb.w....bbww..wwwwwbb.wwwwwb..wwwbwbb.wwwwbb.....wwb....b.w...;b;FFO position 49, black to move, Lazard vs Cali, FC 1991.;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 50 to 59.
 */
const TestCase ffo_50_59[] =
  {
    {"ffo-50", 1, +10, { D8 } }, // ffo-50;....b.....bbb....wwwbwww.wwwbwww.wbwbwbw.wwbbwww..wwbw....w..w..;b; D8:+10. H7:+6. A4:+2. B2:+2. G7:-2. G8:-2. A2:-4. A5:-6. A7:-6. G2:-6. H2:-6. A6:-8. A3:-10. B8:-14. B7:-18.;
    {"ffo-51", 2,  +6, { E2, A3 } }, // ffo-51;....w.b......b.....bbbw.wbbbbbww.bbwwbwwbbwbbbww..wwww.w....ww..;w; E2:+6. A3:+6. F1:+4. G7:+2. C2:+0. B3:-2. D2:-4. G2:-6. C3:-10. A5:-14.;
    {"ffo-52", 1,  +0, { A3 } }, // ffo-52;...b.......wb..b..bwwbbbbbbwbbbbbbbwwbbbbbbwwwbb..bw...b........;w; A3:+0. E1:-2. B3:-2. F2:-8. F1:-12. B8:-12. G2:-16. B7:-18. B2:-22. A7:-26.;
    {"ffo-53", 2,  -2, { D8 } }, // ffo-53;....ww.....www...bbbbwww..bbwwbw.bbbbbww..wwwbww..b.wb.w.....b..;b; D8:-2. C1:-4. E8:-6. D1:-8. G1:-8. G7:-12. B6:-12. B7:-14. G2:-18. H2:-20. D7:-22.;
    {"ffo-54", 2,  -2, { C7 } }, // ffo-54;..www...bbww....bbbbwwwwbbbbwb..bbbwbb..bbwww......www.....w....;b; C7:-2. F8:-4. F2:-4. C8:-4. F6:-10. G8:-12. B1:-14. E2:-16. H2:-20. E8:-22.;
    {"ffo-55", 4,  +0, { G6, B7, E2, G4 } }, // ffo-55;........b.b......bbbbwwwwwbwbb..wwwbbbb.wwbbbb..w.wwwb.....ww...;w; G6:+0. B7:+0. E2:+0. G4:+0. F2:-2. H4:-2. D2:-2. C1:-4. H5:-4. F8:-6. D1:-12. G7:-16. A3:-16. B2:-28.;
    {"ffo-56", 1,  +2, { H5 } }, // ffo-56;..bbbbb...bbbb...wwwbb...wwbwb...wbbbbb.wwwwwbw....wbb..........;w; H5:+2. F8:-8. G3:-12. E8:-14. G4:-20. G8:-28. G7:-32. G2:-34. B1:-40.;
    {"ffo-57", 1, -10, { A6 } }, // ffo-57;...................bbwww..bbbwww..bbwbww.wwwbbbw..wbww.w.wwwww..;b; A6:-10. F2:-12. B5:-16. A7:-18. G7:-20. G2:-20. B7:-22. G8:-52.;
    {"ffo-58", 1,  +4, { G1 } }, // ffo-58;..bwww....www....wwwbww..wwwwbw..wbwbbb.wwbbbb....b.bb..........;b; G1:+4. A5:-6. B2:-10. H2:-18. A4:-22. A3:-22. F2:-22. A7:-24. H4:-26. B1:-26. A2:-28. H3:-36. G2:-38.;
    {"ffo-59", 3, +64, { H4, G8, E8 } }, // ffo-59;.......................w..wwwww...wwwwwbwwwwbbbb..bbwwbb..bb.w.b;b; H4:+64. G8:+64. E8:+64. A5:+62. B3:+54. F3:+52. B5:+44. D3:+38. E3:+36. G3:+34. C3:+30.;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 60 to 69.
 */
const TestCase ffo_60_69[] =
  {
    {"ffo-60", 1, +20, { C2 } }, // ffo-60;...wwww....www....bwbwbb..bwwbbb..bwwbbb..bwwwbb..wbbb.b..bbbb..;b; C2:+20. B8:+18. C1:+10. B6:+0. G7:-10. G2:-12. B7:-18.;
    {"ffo-61", 2, -14, { H3, G1 } }, // ffo-61;.bbbb...b.bbwb..bbbbwbb.bwwbwwwwbwwwwww.bbwwww..b...w...........;w; H3:-14. G1:-14. F1:-16. B7:-22. H2:-30. B2:-34. G2:-46.;
    {"ffo-62", 1, +28, { E8 } }, // ffo-62;..wwww....wwbb....wbbbbbbbwbbwww.bbbbww.wbbbbbbw....b...........;w; E8:+28. D7:+24. H7:+24. F7:+18. C7:+14. D8:+10. A5:-2. G7:-4. H2:-8. G2:-20.;
    {"ffo-63", 1,  -2, { F2 } }, // ffo-63;..b.......b.b....wbbbb...wbbbbw.wwbwbwwwwwwwwbw...bwbb.....bbbb.;w; F2:-2. B8:-4. D2:-8. C8:-10. E1:-10. B7:-12. F1:-14. G3:-16. D1:-18. G2:-18. G7:-24. B1:-28.;
    {"ffo-64", 1, +20, { B4 } }, // ffo-64;..w..b....w..b.w.wwbbbww..bbbbbw..bbwwww.bbbbbb...bbb....b.www..;w; B4:+20. E1:+18. E2:+18. B5:+18. D2:+10. A5:+8. F7:+6. C8:-6. G7:-12. H7:-16.;
    {"ffo-65", 1, +10, { G1 } }, // ffo-65;....ww....wwwwb...wbbbb.w.wbbbb..wwbbwb.bbwbbbb...wwww.......w..;b; G1:+10. C8:+8. A5:+8. B7:+6. D8:+4. B3:+2. D1:+0. B1:+0. E8:-4. B4:-6. C1:-8. B8:-10. G8:-14. B2:-30.;
    {"ffo-66", 1, +30, { H3 } }, // ffo-66;.www....b.wbb...bbwbbww.bwbbww..bbwwww..bbwwww....www.....w.....;b; H3:+30. G4:+28. F8:+26. G5:+18. F7:+16. G6:+12. G7:+10. D8:+6. B2:-14. E8:-16.;
    {"ffo-67", 1, +22, { H3 } }, // ffo-67;.bbbbb....bwbb..wwwbwbw..wwwbwww.wwwbbw...wwwb.w...wb...........;b; H3:+22. C8:+20. A6:+20. B6:+14. D8:+14. H5:+12. B2:+12. A4:+12. C7:+6. B7:+2. A5:-4. H2:-14.;
    {"ffo-68", 1, +28, { E8 } }, // ffo-68;...www....wwww....wbbwwb.wwbbwb..wwbbbb..bwwbb....www........w..;b; E8:+28. A5:+26. A6:+24. A4:+24. H2:+22. C8:+22. B1:+22. C1:+20. B3:+18. G2:+10. B2:+8. B7:+8. B8:+8. D8:+4. G1:-4.;
    {"ffo-69", 1,  +0, { H3 } }, // ffo-69;..wwww.....www...wwwww..bbwbbww..wbwbww.wbbbbbb...b.b...........;b; H3:+0. H5:-2. A2:-2. A5:-4. G2:-4. C2:-12. H4:-12. B2:-12. G3:-12. G1:-26.;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 70 to 79.
 */
const TestCase ffo_70_79[] =
  {
    {"ffo-70", 1, -24, { E3 } }, // ffo-70;...b....b.bbb...bbbb....bbbwww..bbbbww..bbwwbbb.b.wwbb....w.....;b; E3:-24. E8:-26. D8:-26. G5:-26. F3:-44. B7:-48. G4:-56.;
    {"ffo-71", 2, +20, { D2 } }, // ffo-71;..................bbbbb..bbbbbw..wbbbwwb..wbwbbb..wwbb.b...bbbb.;w; D2:+20. F2:+18. B3:+16. A4:+10. H2:+10. E2:+4. G2:-22. G7:-22. C2:-30.;
    {"ffo-72", 1, +24, { E1 } }, // ffo-72;...w......wwbb...bbwbbb.bbbbwwbb.bbbbww...bbbww....bb.......b...;w; E1:+24. A3:+22. A6:+22. C8:+18. F1:+16. B2:+16. C7:+16. H3:+16. B6:+14. D8:+10. A5:+10. B7:+6. H5:+4. H2:+4. G2:-14.;
    {"ffo-73", 2,  -4, { G4 } }, // ffo-73;..b..b....bbb...wwbbbb...wwbbb...wbwbbw.wwwbbbb.w..wbw..........;w; G4:-4. D8:-6. H5:-6. H6:-8. F8:-8. E1:-12. G3:-12. G2:-12. F2:-20. G7:-22. D1:-22.;
    {"ffo-74", 2, -30, { F1 } }, // ffo-74;....b.....wbbw.b..wbwwbb.wwbbwbb..wbbw.b..bbww....bwww.......w..;w; F1:-30. B5:-32. C1:-32. B6:-36. D1:-42. B8:-44. B7:-44. C8:-44.;
    {"ffo-75", 4, +14, { D2 } }, // ffo-75;....w.......ww....bbwb.w.bbbwbww..wwwww...wwwbwb..wwwwb......w..;b; D2:+14. H5:+12. D1:+8. B6:+4. F1:+0. C8:+0. H7:-2. D8:-4. G1:-6. G8:-6. B8:-10. G3:-16. B7:-34.;
    {"ffo-76", 1, +32, { A3 } }, // ffo-76;...w......ww.w.....wwwb.wwwwwwb..bbbbwbb..wwwwww..www.......w...;b; A3:+32. F7:+30. E1:+30. C1:+30. H7:+28. F8:+28. E2:+20. C3:+14. D8:+14. G7:+14. B8:+12. G2:+12. B7:+12. B3:+12. C8:+10. G1:-4.;
    {"ffo-77", 1, +34, { B7 } }, // ffo-77;..w.wb..b.www...bbwww...bbwbwwww.wwwww..w.b.w.....wb............;b; B7:+34. C8:+30. B6:+26. D6:+24. B2:+22. D1:+14. F2:+12. F6:+12. F3:+4. H3:+0. F7:-4.;
    {"ffo-78", 1,  +8, { F1 } }, // ffo-78;....w.....wwww...wwwb.b.wwbwbbbb.bwwb...bwww.b....ww.......w....;b; F1:+8. A7:+6. C8:+4. A2:+2. A3:+2. E8:-2. C1:-2. G1:-8. E6:-10. B8:-10. B7:-18. B2:-20. B1:-22.;
    {"ffo-79", 3, +64, { D7 } }, // ffo-79;..............b.....w.bb...wwwb.wwwwbwbb..wwwwww..w.ww.w....ww..;b; D7:+64. D8:+62. H8:+56. C3:+30. B8:+16. C4:+14. E2:+12. D2:+12. G7:-2.;
    {NULL, 0, 0, {A1}}
  };


/* Test function prototypes. */

static void
game_position_es_solve_test (GamePositionDbFixture *fixture,
                             gconstpointer test_data);

static void
game_position_ifes_solve_test (GamePositionDbFixture *fixture,
                               gconstpointer test_data);

static void
game_position_minimax_solve_test (GamePositionDbFixture *fixture,
                                  gconstpointer test_data);

static void
game_position_ab_solve_test (GamePositionDbFixture *fixture,
                             gconstpointer test_data);



/* Helper function prototypes. */

static GamePositionDb *
gpdb_setup (gchar *source);

static void
gpdb_ffo_fixture_setup (GamePositionDbFixture *fixture,
                        gconstpointer test_data);

static void
gpdb_sample_games_fixture_setup (GamePositionDbFixture *fixture,
                                 gconstpointer test_data);

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer test_data);

static GamePosition *
get_gp_from_db (GamePositionDb *db,
                gchar *id);

static void
run_test_case_array (GamePositionDb *db,
                     const TestCase tca[],
                     ExactSolution* (*solver)(const GamePositionX *const gpx,
                                              const endgame_solver_env_t *const env));

static bool
is_move_part_of_array (const Square move,
                       const Square move_array[],
                       const int move_array_length);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  board_module_init();

  g_test_add("/es/ffo_05",
             GamePositionDbFixture,
             (gconstpointer) ffo_05,
             gpdb_ffo_fixture_setup,
             game_position_es_solve_test,
             gpdb_fixture_teardown);

  g_test_add("/ifes/ffo_05",
             GamePositionDbFixture,
             (gconstpointer) ffo_05,
             gpdb_ffo_fixture_setup,
             game_position_ifes_solve_test,
             gpdb_fixture_teardown);

  g_test_add("/minimax/ffo_01_simplified_4",
             GamePositionDbFixture,
             (gconstpointer) ffo_01_simplified_4,
             gpdb_sample_games_fixture_setup,
             game_position_minimax_solve_test,
             gpdb_fixture_teardown);

  g_test_add("/ab/ffo_05",
             GamePositionDbFixture,
             (gconstpointer) ffo_05,
             gpdb_ffo_fixture_setup,
             game_position_ab_solve_test,
             gpdb_fixture_teardown);

  if (g_test_slow ()) {
    g_test_add("/minimax/ffo_05",
               GamePositionDbFixture,
               (gconstpointer) ffo_05,
               gpdb_ffo_fixture_setup,
               game_position_minimax_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/es/ffo_01_19",
               GamePositionDbFixture,
               (gconstpointer) ffo_01_19,
               gpdb_ffo_fixture_setup,
               game_position_es_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/ifes/ffo_01_19",
               GamePositionDbFixture,
               (gconstpointer) ffo_01_19,
               gpdb_ffo_fixture_setup,
               game_position_ifes_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/ab/ffo_01_19",
               GamePositionDbFixture,
               (gconstpointer) ffo_01_19,
               gpdb_ffo_fixture_setup,
               game_position_ab_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/es/ffo_20_29",
               GamePositionDbFixture,
               (gconstpointer) ffo_20_29,
               gpdb_ffo_fixture_setup,
               game_position_es_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/ifes/ffo_20_29",
               GamePositionDbFixture,
               (gconstpointer) ffo_20_29,
               gpdb_ffo_fixture_setup,
               game_position_ifes_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/es/ffo_30_39",
               GamePositionDbFixture,
               (gconstpointer) ffo_30_39,
               gpdb_ffo_fixture_setup,
               game_position_es_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/ifes/ffo_30_39",
               GamePositionDbFixture,
               (gconstpointer) ffo_30_39,
               gpdb_ffo_fixture_setup,
               game_position_ifes_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/es/ffo_40_49",
                GamePositionDbFixture,
               (gconstpointer) ffo_40_49,
               gpdb_ffo_fixture_setup,
               game_position_es_solve_test,
               gpdb_fixture_teardown);
    g_test_add("/ifes/ffo_40_49",
               GamePositionDbFixture,
               (gconstpointer) ffo_40_49,
               gpdb_ffo_fixture_setup,
               game_position_ifes_solve_test,
               gpdb_fixture_teardown);
  }

  return g_test_run();
}



/*
 * Test functions.
 */

static void
game_position_es_solve_test (GamePositionDbFixture *fixture,
                             gconstpointer test_data)
{
  GamePositionDb *db = fixture->db;
  TestCase *tcap = (TestCase *) test_data;
  run_test_case_array(db, tcap, game_position_es_solve);
}

static void
game_position_ifes_solve_test (GamePositionDbFixture *fixture,
                               gconstpointer test_data)
{
  GamePositionDb *db = fixture->db;
  TestCase *tcap = (TestCase *) test_data;
  run_test_case_array(db, tcap, game_position_ifes_solve);
}

static void
game_position_minimax_solve_test (GamePositionDbFixture *fixture,
                                  gconstpointer test_data)
{
  GamePositionDb *db = fixture->db;
  TestCase *tcap = (TestCase *) test_data;
  run_test_case_array(db, tcap, game_position_minimax_solve);
}

static void
game_position_ab_solve_test (GamePositionDbFixture *fixture,
                             gconstpointer test_data)
{
  GamePositionDb *db = fixture->db;
  TestCase *tcap = (TestCase *) test_data;
  run_test_case_array(db, tcap, game_position_ab_solve);
}



/*
 * Internal functions.
 */

static GamePositionDb *
gpdb_setup (gchar *source)
{
  GamePositionDb               *db;
  GamePositionDbSyntaxErrorLog *syntax_error_log;
  FILE                         *fp;
  GError                       *error;

  /* Loads the game position database. */
  fp = fopen(source, "r");
  if (!fp) {
    g_test_message("Unable to open database test file \"%s\" for reading.\n", source);
    g_test_fail();
  }
  g_assert(fp);
  db = gpdb_new(g_strdup(source));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, source, db, &syntax_error_log, &error);
  fclose(fp);
  g_free(source);

  /* Removes the tmp file, frees the resources. */
  g_free(error);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);

  g_assert (db);
  return db;
}

static void
gpdb_ffo_fixture_setup (GamePositionDbFixture *fixture,
                        gconstpointer test_data)
{
  gchar *source = g_strdup("db/gpdb-ffo.txt");
  fixture->db = gpdb_setup(source);
}

static void
gpdb_sample_games_fixture_setup (GamePositionDbFixture *fixture,
                                 gconstpointer test_data)
{
  gchar *source = g_strdup("db/gpdb-sample-games.txt");
  fixture->db = gpdb_setup(source);
}

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer test_data)
{
  g_assert(fixture->db != NULL);
  gpdb_free(fixture->db, TRUE);
}

static GamePosition *
get_gp_from_db (GamePositionDb *db,
                gchar *id)
{
  GamePositionDbEntry *entry = gpdb_lookup(db, id);
  if (!entry) {
    g_test_message("The entry \"%s\" is missing from game position database.\n", id);
    g_test_fail();
  }
  g_assert(entry);
  return entry->game_position;
}

static void
run_test_case_array (GamePositionDb *db,
                     const TestCase tca[],
                     ExactSolution* (*solver)(const GamePositionX *const gpx,
                                              const endgame_solver_env_t *const env))
{
  endgame_solver_env_t endgame_solver_env =
    { .log_file = NULL,
      .pve_dump_file = NULL,
      .repeats = 0
    };

  const TestCase *tc = NULL;
  for (int i = 0;; i++) {
    tc = &tca[i];
    if (tc->gpdb_label == NULL) break;
    if (g_test_verbose()) {
      gchar *moves_to_s = square_array_to_string(tc->best_move, tc->best_move_count);
      printf("Test #%3d: data[position=%s, expected_value=%+03d, expected_best_moves={%s}]; ", i, tc->gpdb_label, tc->outcome, moves_to_s);
      g_free(moves_to_s);
    }
    const GamePosition *const gp = get_gp_from_db(db, tc->gpdb_label);
    GamePositionX *const gpx = game_position_x_gp_to_gpx(gp);
    ExactSolution *const solution = solver(gpx, &endgame_solver_env);
    if (g_test_verbose())
      printf("result[outcome=%+03d, move=%s]", solution->outcome, square_to_string(solution->pv[0]));
    const bool ok_value = tc->outcome == solution->outcome;
    const bool ok_move = is_move_part_of_array(solution->pv[0], tc->best_move, tc->best_move_count);
    if (!ok_value || !ok_move) g_test_fail();
    exact_solution_free(solution);
    free(gpx);
    if (g_test_verbose()) printf(": %s\n", (ok_value && ok_move) ? "OK": "KO");
  }
}

static bool
is_move_part_of_array (const Square move,
                       const Square move_array[],
                       const int move_array_length)
{
  for (int i = 0; i < move_array_length; i++) {
    if (move == move_array[i]) return true;
  }
  return false;
}
