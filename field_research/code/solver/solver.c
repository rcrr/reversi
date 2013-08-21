/*!\mainpage
 *
 * solver.c : a program to solve Othello endgame script.
 * - copyleft (c) 2001-2004
 * - version 1.4 (2004-04-12 18:00:00)
 * - author: Richard A Delorme
 * - e-mail: abulmo@club-internet.fr
 * - web site: http://perso.club-internet.fr/abulmo/resources/
 *
 *
 * \section lic Licence
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 * \section dl Download
 * You can download the source with its documentation here:
 * http://perso.club-internet.fr/abulmo/resources/solver/solver.1-4.zip
 *
 *
 * \section pres Presentation
 *
 *      "The important decisions in design are not what to put in but what to
 *       leave out."
 *                  Tony Hoare
 *
 * At ftp://ftp.nj.nec.com/pub/igord/IOS/src, an Othello archive site, there
 * is a program about endgame solving firstly written by Jean-Christophe Weill,
 * then improved by Warren D Smith and further improved by Gunnar Anderson.
 * Although my work was greatly influenced by these programs, the solver I
 * present here is not another improvement but a complete re-implementation,
 * done in a few hours by cutting and pasting code from my own program Edax.
 * I tried to write an optimized but readable code, two incompatible things. Do
 * not forget that the C language is a write-only language :-(
 * Compared to their previous works:
 *    - more general user interface. In addition to the best score, this solver
 * also provides the best move and (if hash table is ON) the best principal
 * variation, some performance data and, on demand, the tested board. The
 * test positions (too easy, IMHO) are no more included into the code but are
 * present within an external standardized script file "endgame.scr". Moreover,
 * you may try "fforum-1-19.scr", "fforum-20-39.scr", and "fforum-40-59.scr",
 * that contain problems published in the magazine of the Federation Francaise
 * d'Othello: FFORUM.
 *    - strict ansi-C89 conformance.
 *    - more modular code. I put into functions many things sparsed into the
 * same function.
 *    - avoidance of global variables. Instead I organize them into structures
 * passed to functions as pointers. Structure and function naming was carefully
 * chosen to give an object-oriented-programming feeling.
 *    - to keep code clear but fast, I overuse macro inlining.
 *    - more and better algorithms used:
 *         - fast (due to inlining and loop-unrolling) move generators.
 *         - Principal Variation Search (an enhanced alphabeta) [1].
 *         - move sorting according to the type of the squares.
 *         - move sorting to play fastest & stablest lines first.
 *         - move sorting based on approximated parity.
 *         - hash table [2] used to:
 *               - anticipate cutoff at present level (if position is stored).
 *               - anticipate cutoff at the next level (enhanced transposition
 *                 cutoff [3]).
 *               - replay known bestmove first.
 *
 * A faster endgame solver would need:
 *    - a good evaluation function.
 *    - iterative deepening.
 *    - move sorting using shallow search.
 *    - "hard coding" of move generation, ...
 *    - 64 bit board representation (bitboard) on 64 bit machine.
 *    - other heuristics: conspiracy number, ...
 *
 * but the following code is already much too long. Whatever, this solver is
 * very fast for positions with less than 15 empties and still quiet good until
 * 20 empties. The source has been kept in a single file to mimic other similar
 * but older othello endgame solvers ; however, any clever implementation
 * should split it in smaller files gathering the same logical content.
 *
 *
 * \section comp Compilation
 * I recommend to compile it with very aggressive optimizing settings, for
 * example with the gnu compiler:
 *
 *    gcc -O3 -fomit-frame-pointer -funroll-loops -march=<your-CPU> solver.c -o solver
 *
 * A few #define at the top of the code may be changed to test/tune
 * the effect of different parameters.
 *
 *
 * \section us Usage
 * Usage: solver [options] script_file
 *
 * Options:
 *    - -v            verbose mode.
 *    - -vv           very verbose mode.
 *    - -wdl          search for win/draw/loss.
 *    - -wd           search for win/(draw-loss).
 *    - -dl           search for (win-draw)/loss.
 *    - -h <nbits>    set hash table size.
 *
 * Note: the best value for the '-h' option, i.e. the  hash table size in bit
 * number is approximately the number of empty squares of the position.
 * Example: solver -v -h 20 fforum-20-39.scr
 *
 *
 * \section hist History
 * - version 1.0: 2001-10-31
 * - version 1.1: 2001-12-19
 *      - added stablest square based sorting
 *      - node counter as Macros
 *      - cosmetic enhancements.
 * - version 1.2: 2003-12-06
 *      - added parity based sorting
 *      - plain alphabeta near the leaves
 *      - better comments & documentation (compatible with doxygen).
 *      - suppression of annoying statistic routines.
 *      - minor bug removals.
 *      - minor speed enhancements.
 *      - cosmetic enhancements.
 * - version 1.3: 2004-02-18
 *      - major bug removals: parity missing initialisation.
 *      - minor bug removals.
 * - version 1.4: 2004-04-12
 *      - major bug removal: wrong score returned from game-over positions.
 *      - minor speed enhancements: board update/restore unrolling.
 *
 *
 * \section ref Reference
 * -# Marsland T.A. (1983) Relative efficiency of alpha-beta implementations.
 *     8th IIJCAI. p 763-766.
 * -# Breuker D.M., Uiterwijk J.W.H.M. & van den Herik H.J. (1996) Replacement
 *     Schemes and Two-Level Tables. ICCA J 19-3 p 183-193.
 * -# Plaat A, Schaeffer J., Wirn P. & de Bruin A.(1996) Exploiting Graph
 *     Properties of Game Trees.
 *
 *
 * \section thk Thanks
 *     - to Christophe Lanuit, for the stablest sorting heuristics.
 *     - to Bruno Causse, who discovered an illogical bug when passing.
 *     - to Gunnar Anderson, who gave me the idea about the parity based on
 * quadrant, as a fast sorting algorithm at shallow depth.
 *     - to Björn Lindberg, who reported a bug in version 1.3.
 *     - to people at GGS for their invaluable comments about this file.
 *
 */

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#endif

/*!
 * \defgroup mac Macros, constants, globals & declarations
 */
/*@{*/

/* YOU MAY MODIFY FOLLOWING DATA TO CONTROL THE USAGE OF SOME HEURISTICS */

/*! depth switch from a slow but efficient PVS near the root to a fast PVS
  near the leaves. (default = 7) */
#define EMPTIES_DEEP_TO_SHALLOW_SEARCH 7

/*! use the hash table heuristics. 1 = on, 0 = off. (default = 1) */
#define USE_HASH_TABLE 1

/*! use the enhanced transposition cutoff heuristic. 1 = on, 0 = off.
  (default = 1) */
#define USE_ENHANCED_TRANSPOSITION_CUTOFF (1 && USE_HASH_TABLE)

/*! play odd square first   1 = on, 0 = off. (default = 1) */
#define PLAY_ODD_SQUARE_FIRST 1

/*! step through the 'smallest' subtree first to find the solution faster.
  1 = on, 0 = off. (default = 1) */
#define PLAY_FAST_SUBTREE_FIRST 1

/*! among equally 'small' subtree choose the one with more stable squares.
  1 = on, 0 = off. (default = 1) */
#define PLAY_STABLEST_SUBTREE (1 && PLAY_FAST_SUBTREE_FIRST)

/*! use the best move stored in the hash table. 1 = on, 0 = off. (default = 1) */
#define PLAY_BEST_MOVE_IN_MEMORY_FIRST (1 && USE_HASH_TABLE)

/*! use presorted squares. 1 = on, 0 = off. (default = 1) */
#define USE_PRESORTED_SQUARES 1

/*! node counter 0 = off. 1 = internal_nodes, 2 = internal_nodes + leave_nodes,
  3 = all. (default = 3, more spectacular!) */
#define COUNT_NODES 3

/* FOLLOWING DATA SHOULD NOT BE CHANGED */
/*! infinite score: a huge value unreachable as a score and fitting in a char */
#define INF_SCORE 127

/*! size of the board */
#define BOARD_SIZE 91

/*! maximal number of moves */
#define MAX_MOVE 32

/*! maximal number of squares flipped + 1 */
#define MAX_FLIP 20

/*! maximal score */
#define MAX_SCORE 64

/*! constants for square coordinates */
enum {
  NOMOVE = 0, PASS,
  A1 = 10, B1, C1, D1, E1, F1, G1, H1,
  A2 = 19, B2, C2, D2, E2, F2, G2, H2,
  A3 = 28, B3, C3, D3, E3, F3, G3, H3,
  A4 = 37, B4, C4, D4, E4, F4, G4, H4,
  A5 = 46, B5, C5, D5, E5, F5, G5, H5,
  A6 = 55, B6, C6, D6, E6, F6, G6, H6,
  A7 = 64, B7, C7, D7, E7, F7, G7, H7,
  A8 = 73, B8, C8, D8, E8, F8, G8, H8
};

/*! flipping directions */
enum {
  NW = -10,
  N  =  -9,
  NE =  -8,
  W =  -1,
  E =  +1,
  SW =  +8,
  S  =  +9,
  SE = +10
};

/*! a flipping direction ID for each square */
const int FLIPPING_DIRECTION_ID[BOARD_SIZE] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 2, 2, 2, 2, 3, 3,
  0, 1, 1, 2, 2, 2, 2, 3, 3,
  0, 4, 4, 5, 5, 5, 5, 6, 6,
  0, 4, 4, 5, 5, 5, 5, 6, 6,
  0, 4, 4, 5, 5, 5, 5, 6, 6,
  0, 4, 4, 5, 5, 5, 5, 6, 6,
  0, 7, 7, 8, 8, 8, 8, 9, 9,
  0, 7, 7, 8, 8, 8, 8, 9, 9,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

/*! a quadrant id for each square */
const int QUADRANT_ID[BOARD_SIZE] = {
  4, 4, 4, 4, 4, 4, 4, 4, 4,
  4, 0, 0, 0, 0, 1, 1, 1, 1,
  4, 0, 0, 0, 0, 1, 1, 1, 1,
  4, 0, 0, 0, 0, 1, 1, 1, 1,
  4, 0, 0, 0, 0, 1, 1, 1, 1,
  4, 2, 2, 2, 2, 3, 3, 3, 3,
  4, 2, 2, 2, 2, 3, 3, 3, 3,
  4, 2, 2, 2, 2, 3, 3, 3, 3,
  4, 2, 2, 2, 2, 3, 3, 3, 3,
  4, 4, 4, 4, 4, 4, 4, 4, 4, 4
};

/*! constants for colors */
enum {
  BLACK = 0,
  WHITE,
  EMPTY,
  OFF_SIDE
};

/*! constants for search mode */
enum {
  EXACT_SCORE,
  WDL_SCORE,
  WD_SCORE,
  DL_SCORE
};

/*! hashing global data */
static unsigned long hash_code_set_disc[BOARD_SIZE][3][2];
static unsigned long hash_code_flip_disc[BOARD_SIZE][2];
static unsigned long hash_code_swap_player[2];

/*! move representation */
typedef struct Move {
  int position[MAX_FLIP];      /*< affected square position */
  int n;                       /*< number of flipped discs */
  int score;                   /*< score for this move */
  unsigned long hash_code[2];  /*< 64 bit hash code */
} Move;

/*! (double linked) list of squares */
typedef struct SquareList {
  int position;                 /*!< square position */
  struct SquareList *previous;  /*!< link to previous square */
  struct SquareList *next;      /*!< link to next square */
} SquareList;

/*! (simple) list of a legal moves */
typedef struct MoveList {
  Move move;                /*!< a legal move */
  struct MoveList *next;    /*!< link to next legal move */
} MoveList;

/*! board representation */
typedef struct Board {
  char square[BOARD_SIZE];           /*!< square content */
  char player;                       /*!< player to move */
  int n_discs[2];                    /*!< disc numbers */
  int n_empties;                     /*!< number of empty squares */
  double n_nodes;                    /*!< node counter */
  unsigned long hash_code[2];        /*!< 64 bit hash code */
  unsigned char parity[4];           /*!< quadrant parity */
  SquareList empties[MAX_SCORE - 2]; /*!< list of empty squares */
  SquareList *position_to_empties[BOARD_SIZE]; /*!< link position to the list */
} Board;

/*! Hash : item stored in the hash table */
typedef struct Hash {
  unsigned long lock;       /*!< hash lock */
  signed char lower;        /*!< lower bound of the position score */
  signed char upper;        /*!< upper bound of the position score */
  unsigned char move;       /*!< best move */
  unsigned char depth;      /*!< depth of the analysis ( = board->n_empties) */
} Hash;

/*! HashEntry: an entry, with two items, of the hash table */
typedef struct HashEntry {
  Hash deepest; /*!< entry for the highest cost search */
  Hash newest;  /*!< entry for the most recent search */
} HashEntry;

/*! HashTable : hash table */
typedef struct HashTable {
  HashEntry *hash_entry;  /*!< array of entry */
  unsigned long hash_mask; /*!< a bit mask */
} HashTable;

#if COUNT_NODES > 0
/*! node counter for internal nodes */
#define BOARD_UPDATE_INTERNAL_NODES() board->n_nodes++;
#else
/*! no node counter for internal nodes */
#define BOARD_UPDATE_INTERNAL_NODES()
#endif
#if COUNT_NODES > 1
/*! node counter for terminal nodes (leaves) */
#define BOARD_UPDATE_TERMINAL_NODES() board->n_nodes++;
/*! node counter correction for terminal nodes */
#define BOARD_CORRECT_TERMINAL_NODES() board->n_nodes--;
#else
/*! no node counter */
#define BOARD_UPDATE_TERMINAL_NODES()
/*! node counter correction for terminal nodes */
#define BOARD_CORRECT_TERMINAL_NODES() board->n_nodes -= 2;
#endif
#if COUNT_NODES > 2
/*! more general node counter */
#define BOARD_UPDATE_ALL_NODES() board->n_nodes++
#else
/*! no general node counter */
#define BOARD_UPDATE_ALL_NODES()
#endif

/*! macros to check if a move (<= 6 flips) is correct along a direction */
#define BOARD_CHECK_MOVE_6(dir)                       \
  ( square[dir] == o &&                               \
  ( square[2 * dir] == p || (square[2 * dir] == o &&  \
  ( square[3 * dir] == p || (square[3 * dir] == o &&  \
  ( square[4 * dir] == p || (square[4 * dir] == o &&  \
  ( square[5 * dir] == p || (square[5 * dir] == o &&  \
  ( square[6 * dir] == p || (square[6 * dir] == o &&  \
    square[7 * dir] == p)))))))))))

/*! macros to check if a move (<= 4 flips) is correct along a direction */
#define BOARD_CHECK_MOVE_4(dir)                             \
        ( square[dir] == o &&                               \
        ( square[2 * dir] == p || (square[2 * dir] == o &&  \
        ( square[3 * dir] == p || (square[3 * dir] == o &&  \
        ( square[4 * dir] == p || (square[4 * dir] == o &&  \
          square[5 * dir] == p)))))))

  /*! macro to count (at most 6) discs flipped along a direction */
    #define BOARD_COUNT_FLIPS_6(dir)                                     \
        if (square[dir] == o) {                                          \
            if (square[2 * dir] == p) n_flips++;                         \
            else if (square[2 * dir] == o) {                             \
                if (square[3 * dir] == p) n_flips += 2;                  \
                else if (square[3 * dir] == o) {                         \
                    if (square[4 * dir] == p) n_flips += 3;              \
                    else if (square[4 * dir] == o) {                     \
                            if (square[5 * dir] == p) n_flips += 4;      \
                            else if (square[5 * dir] == o) {             \
                                if (square[6 * dir] == p) n_flips += 5;  \
                                else if (square[6 * dir] == o            \
                                 && square[7 * dir] == p) n_flips += 6;  \
        }}}}}

    /*! macro to count (at most 4) discs flipped along a direction */
    #define BOARD_COUNT_FLIPS_4(dir)                                \
        if (square[dir] == o) {                                     \
            if (square[2 * dir] == p) n_flips++;                    \
            else if (square[2 * dir] == o) {                        \
                if (square[3 * dir] == p) n_flips += 2;             \
                else if (square[3 * dir] == o) {                    \
                    if (square[4 * dir] == p) n_flips += 3;         \
                    else if (square[4 * dir] == o                   \
                             && square[5 * dir] == p) n_flips += 4; \
        }}}

    /*! macro to flip discs along a direction */
    #define BOARD_FLIP(dir, max_flip)           \
        if (BOARD_CHECK_MOVE_##max_flip(dir)) { \
            int z = x + (dir);                  \
            do {                                \
                board->square[z] = p;           \
                move->position[++move->n] = z;  \
                z += (dir);                     \
            } while (board->square[z] == o);    \
        }

    /*! macro to get a move along a direction */
    #define BOARD_GET_MOVE(dir, max_flip)                        \
        if (BOARD_CHECK_MOVE_##max_flip(dir)) {                  \
            int z = x + (dir);                                   \
            do {                                                 \
                move->position[++move->n] = z;                   \
                move->hash_code[0] ^= hash_code_flip_disc[z][0]; \
                move->hash_code[1] ^= hash_code_flip_disc[z][1]; \
                z += (dir);                                      \
            } while (board->square[z] == o);                     \
        }

    /*! update board */
    #define BOARD_UPDATE_SQUARE(board, move) switch((move)->n) {  \
        case 18: board->square[(move)->position[18]] = p;  \
        case 17: board->square[(move)->position[17]] = p;  \
        case 16: board->square[(move)->position[16]] = p;  \
        case 15: board->square[(move)->position[15]] = p;  \
        case 14: board->square[(move)->position[14]] = p;  \
        case 13: board->square[(move)->position[13]] = p;  \
        case 12: board->square[(move)->position[12]] = p;  \
        case 11: board->square[(move)->position[11]] = p;  \
        case 10: board->square[(move)->position[10]] = p;  \
        case  9: board->square[(move)->position[ 9]] = p;  \
        case  8: board->square[(move)->position[ 8]] = p;  \
        case  7: board->square[(move)->position[ 7]] = p;  \
        case  6: board->square[(move)->position[ 6]] = p;  \
        case  5: board->square[(move)->position[ 5]] = p;  \
        case  4: board->square[(move)->position[ 4]] = p;  \
        case  3: board->square[(move)->position[ 3]] = p;  \
        case  2: board->square[(move)->position[ 2]] = p;  \
        case  1: board->square[(move)->position[ 1]] = p;  \
        case  0: board->square[(move)->position[ 0]] = p;  \
    }

    /*! restore board */
    #define BOARD_RESTORE_SQUARE(board, move) switch((move)->n) {  \
        case 18: board->square[(move)->position[18]] = o;  \
        case 17: board->square[(move)->position[17]] = o;  \
        case 16: board->square[(move)->position[16]] = o;  \
        case 15: board->square[(move)->position[15]] = o;  \
        case 14: board->square[(move)->position[14]] = o;  \
        case 13: board->square[(move)->position[13]] = o;  \
        case 12: board->square[(move)->position[12]] = o;  \
        case 11: board->square[(move)->position[11]] = o;  \
        case 10: board->square[(move)->position[10]] = o;  \
        case  9: board->square[(move)->position[ 9]] = o;  \
        case  8: board->square[(move)->position[ 8]] = o;  \
        case  7: board->square[(move)->position[ 7]] = o;  \
        case  6: board->square[(move)->position[ 6]] = o;  \
        case  5: board->square[(move)->position[ 5]] = o;  \
        case  4: board->square[(move)->position[ 4]] = o;  \
        case  3: board->square[(move)->position[ 3]] = o;  \
        case  2: board->square[(move)->position[ 2]] = o;  \
        case  1: board->square[(move)->position[ 1]] = o;  \
        case  0: board->square[(move)->position[ 0]] = EMPTY;  \
    }


    /*! macro to update player */
    #define BOARD_UPDATE_PLAYER(board) (board->player = o)

    /*! macro to restore player */
    #define BOARD_RESTORE_PLAYER(board) (board->player = p)

    /*! macro to update disc counter */
    #define BOARD_UPDATE_DISCS(board, n_flips)     \
        board->n_discs[(int)p] += n_flips + 1;     \
        board->n_discs[(int)o] -= n_flips;         \
        board->n_empties--;

    /* macro to restore disc counter */
    #define BOARD_RESTORE_DISCS(board, n_flips)     \
        board->n_discs[(int)p] -= n_flips + 1;      \
        board->n_discs[(int)o] += n_flips;          \
        board->n_empties++;

    #if PLAY_ODD_SQUARE_FIRST
        /*! macro to update parity */
        #define BOARD_UPDATE_PARITY(board, x) (board->parity[QUADRANT_ID[x]] ^= 1)
        /*! macro to restore parity */
        #define BOARD_RESTORE_PARITY(board, x) (board->parity[QUADRANT_ID[x]] ^= 1)
    #else
        #define BOARD_UPDATE_PARITY(board, x)
        #define BOARD_RESTORE_PARITY(board, x)
    #endif

    /*! macro to update the list of empty squares */
    #define BOARD_UPDATE_EMPTIES(board, empties, x) \
        empties = board->position_to_empties[x];    \
        empties->previous->next = empties->next;    \
        empties->next->previous = empties->previous;

    /*! macro to restore the list of empty squares */
    #define BOARD_RESTORE_EMPTIES(board, empties, x) \
        empties = board->position_to_empties[x];     \
        empties->previous->next = empties;           \
        empties->next->previous = empties;

    /*! macro to update for the last time the list of empty squares */
    #define BOARD_LAST_UPDATE_EMPTIES(board, empties, x) \
        empties = board->position_to_empties[x];         \
        empties->previous->next = empties->next;

    /*! macro to restore for the last time the list of empty squares */
    #define BOARD_LAST_RESTORE_EMPTIES(board, empties, x) \
        empties = board->position_to_empties[x];          \
        empties->previous->next = empties;

    /*! macro to update the hash codes */
    #define BOARD_UPDATE_HASH_CODE(board, code) \
        board->hash_code[0] ^= code[0];         \
        board->hash_code[1] ^= code[1];

    /*! macro to restore the hash codes */
    #define BOARD_RESTORE_HASH_CODE(board, code) \
        board->hash_code[0] ^= code[0];          \
        board->hash_code[1] ^= code[1];

    /*! macro to swap colors */
    #define OPPONENT(p) ((p) ^ 1)

    /*! test if hash table exists */
    #define HASH_TABLE_OK(hash_table) (hash_table->hash_mask != 0)

    /*! convert clock ticks to hours */
    #define TICK_TO_H(t) (int)((t) / 3600 / CLOCKS_PER_SEC)

    /*! convert clock ticks to minutes */
    #define TICK_TO_M(t) (int)(((t) / 60 / CLOCKS_PER_SEC) % 60)

    /*! convert clock ticks to seconds */
    #define TICK_TO_S(t) (int)(((t) / CLOCKS_PER_SEC) % 60)

    /*! convert clock ticks to tenth of seconds */
    #define TICK_TO_DS(t) (int)(((t) / (CLOCKS_PER_SEC / 10)) % 10)

/*@}*/
/*!
 * \defgroup hash Hash table module
 * The hash table is an efficient memory system to remember the previously
 * analysed positions and re-use the collected data when needed.
 * The hash table contains entries of analysed data where the board is uniquely
 * identified through a 32-bit key and the results of the analysis recorded are
 * two score bounds, the depth of the analysis and the best move found.
 *
 * For more information about how this hash table implementation works, you may
 * read: Breuker D.M., Uiterwijk J.W.H.M. & van den Herik H.J. (1996) Replacement
 * Schemes and Two-Level Tables. ICCA J 19-3 p 183-193.
 */
/*@{*/

/*!
 * \brief Pseudo-random number generator.
 *
 * A good random generator (similar to rand48 or Java's one) to set the hash codes.
 * \return a 32 bits random unsigned long integer.
 */
unsigned long hash_random(void)
{
  static unsigned long x[3] = {0xe66du, 0xdeecu, 0x5u};
  const unsigned long MASK = 0x0000ffffu;
  const unsigned long A[3] = {0xe66du, 0xdeecu, 0x5u};
  const unsigned long B = 0xBu;
  unsigned long product[3];

  product[0] = A[0] * x[0] + B;
  product[1] = A[1] * x[0] + (product[0] >> 16);
  product[2] = A[1] * x[1] + A[0] * x[2] + A[2] * x[0] + (product[1] >> 16);
  product[1] = A[0] * x[1] + (product[1] & MASK);
  product[2] += (product[1] >> 16);
  x[0] = (product[0] & MASK);
  x[1] = (product[1] & MASK);
  x[2] = (product[2] & MASK);

  return x[1] + (x[2] << 16);
}

/*!
 * \brief Initialise the hashtable.
 *
 * Allocate the hash table entries and initialise the hash masks and codes.
 * \param hash_table hash table to setup.
 * \param n_bits     requested size for the hash table in number of bits.
 */
void hash_init(HashTable *hash_table, int n_bits)
{
  int i,j;
  unsigned long hash_mask[2], size = 1u << n_bits;

  if (hash_table->hash_entry != NULL) free(hash_table->hash_entry);
  hash_table->hash_entry = malloc(size * sizeof (HashEntry));
  if (hash_table->hash_entry == NULL) {
    fprintf(stderr, "hash_init: cannot allocate the hash table\n");
    exit(EXIT_FAILURE);
  }
  hash_mask[0] = hash_table->hash_mask = size - 1;
  hash_mask[1] = 0xffffffff;

  for (j = 0; j < 2; j++) {
    hash_code_swap_player[j] = (hash_random() & hash_mask[j]);
    for (i = 0; i < BOARD_SIZE; i++) {
      hash_code_set_disc[i][BLACK][j] = (hash_random() & hash_mask[j]);
      hash_code_set_disc[i][WHITE][j] = (hash_random() & hash_mask[j]);
      hash_code_set_disc[i][EMPTY][j] = 0;
      hash_code_flip_disc[i][j] = hash_code_set_disc[i][BLACK][j] ^
        hash_code_set_disc[i][WHITE][j];
      hash_code_set_disc[i][BLACK][j] ^= hash_code_swap_player[j];
      hash_code_set_disc[i][WHITE][j] ^= hash_code_swap_player[j];
    }
  }
}

/*!
 * \brief Clear the hashtable.
 *
 * Set all hash table entries to zero.
 * \param hash_table hash table to clear.
 */
void hash_clear(HashTable *hash_table)
{
  unsigned long i;
  static const HashEntry init_entry = {
    {0, -INF_SCORE, +INF_SCORE, 0, 0},
    {0, -INF_SCORE, +INF_SCORE, 0, 0}};

  if (HASH_TABLE_OK(hash_table)) {
    for (i = 0; i <= hash_table->hash_mask; i++) {
      hash_table->hash_entry[i] = init_entry;
    }
  }
}

/*!
 * \brief Free the hashtable.
 *
 * Free the memory allocated by the hash table entries
 * \param hash_table hash_table to free.
 */
void hash_free(HashTable *hash_table)
{
  free(hash_table->hash_entry);
  hash_table->hash_entry = NULL;
}

/*!
 * \brief Update an hashtable entry
 *
 * Find an hash table entry according to the evaluated board hash codes. Then
 * update the entry if it already exists otherwise create a new one. Collisions
 * are managed in such a way that better existing entries are always preserved
 * and the new evaluated data is always added. Lower and  upper score bounds
 * are then updated/set from the alpha, beta and score values according to the
 * following alphabeta property (where alpha < beta):
 *     -if (score >= beta) score is a lower bound of the real score
 *     -if (score <= alpha) score is an upper bound of the real score
 *     -if (alpha < score && score < beta) score equals the real score
 * So:
 *     -if (score < beta) update the upper bound of the hash entry
 *     -if (score > alpha) update the lower bound of the hash entry
 * The best move is also stored.
 * \param hash_table hash table to update.
 * \param board      evaluated board.
 * \param alpha      alpha bound when calling the alphabeta function.
 * \param beta       beta bound when calling the alphabeta function.
 * \param score      best score found.
 * \param move       best move found.
 */
void hash_update(HashTable *hash_table, const Board *board, int alpha, int beta, int score, int move)
{
  HashEntry *hash_entry;
  Hash *deepest, *newest;

  if (!HASH_TABLE_OK(hash_table)) return;

  hash_entry = hash_table->hash_entry + board->hash_code[0];
  deepest = &(hash_entry->deepest);
  newest = &(hash_entry->newest);
  /* try to update deepest entry */
  if (board->hash_code[1] == deepest->lock) {
    if (score < beta && score < deepest->upper)
      deepest->upper = (char) score;
    if (score > alpha && score > deepest->lower)
      deepest->lower = (char) score;
    deepest->move = (char) move;
    /* else try to update newest entry */
  } else if (board->hash_code[1] == newest->lock) {
    if (score < beta && score < newest->upper)
      newest->upper = (char) score;
    if (score > alpha && score > newest->lower)
      newest->lower = (char) score;
    newest->move = (char) move;
    /* else try to add to deepest entry */
  } else if (deepest->depth < board->n_empties) {
    if (newest->depth < deepest->depth) *newest = *deepest;
    deepest->lock = board->hash_code[1];
    deepest->depth = (char) board->n_empties;
    deepest->lower = -INF_SCORE;
    deepest->upper = +INF_SCORE;
    if (score < beta) deepest->upper = (char) score;
    if (score > alpha) deepest->lower = (char) score;
    deepest->move = (char) move;
    /* else add to newest entry */
  } else {
    newest->lock = board->hash_code[1];
    newest->depth = (char) board->n_empties;
    newest->lower = -INF_SCORE;
    newest->upper = +INF_SCORE;
    if (score < beta) newest->upper = (char) score;
    if (score > alpha) newest->lower = (char) score;
    newest->move = (char) move;
  }
}

/*!
 * \brief Find an hash table entry according to the evaluated board hash codes.
 *
 * The data recorded within the entry will then be used to reframe the alpha
 * beta bounds and set the move to search first. In some cases, an alphabeta
 * cut will be immediately found so avoiding the entire search and gaining a
 * (lot of) time.
 *
 * \param hash_table : hash table.
 * \param board      : evaluated board.
 * \return : an hash table entry if the board was found, NULL otherwise.
 */
Hash *hash_get(HashTable *hash_table, const Board *board)
{
  HashEntry *hash_entry;

  if (HASH_TABLE_OK(hash_table)) {
    hash_entry = hash_table->hash_entry + board->hash_code[0];
    if (board->hash_code[1] == hash_entry->deepest.lock)
      return &(hash_entry->deepest);
    if (board->hash_code[1] == hash_entry->newest.lock)
      return &(hash_entry->newest);
  }
  return NULL;
}

/*@}*/
/*!
 * \defgroup board Board module
 *
 * This module deals with the Board management.
 *
 * The Board is represented with a structure containing the following data:
 *  - a 1-D array with the square contents.
 *  - the player to move.
 *  - the disc number for each players.
 *  - the number of remaining empty squares.
 *  - a list of empty squares for a fast navigation through them.
 *  - hash table codes identifying the board in the hash table.
 *  - a node counter.
 *
 * High level functions are provided to set/modify the board data or to compute
 * some board properties. Most of the functions are optimized to be as fast as
 * possible, while remaining readable.
 *
 */
/*@{*/

/*!
 * \brief Set a board from a string description.
 *
 * Read a standardized string (See http://www.nada.kth.se/~gunnar/download2.html
 * for details) and translate it into our internal Board structure.
 * \param board : the board to set
 * \param string : string describing the board
 */
void board_set(Board *board, const char *string)
{
  int i, j;
  SquareList *empties;
#if USE_PRESORTED_SQUARES
  const int presorted_position[] = {
    A1, A8, H1, H8,                    /* Corner */
    A3, A6, C1, C8, F1, F8, H3, H6,    /* A */
    C3, C6, F3, F6,                    /* D */
    A4, A5, D1, D8, E1, E8, H4, H5,    /* B */
    C4, C5, D3, D6, E3, E6, F4, F5,    /* E */
    B4, B5, D2, D7, E2, E7, G4, G5,    /* G */
    B3, B6, C2, C7, F2, F7, G3, G6,    /* F */
    A2, A7, B1, B8, G1, G8, H2, H7,    /* C */
    B2, B7, G2, G7,                    /* X */
  };
#else
  const int presorted_position[] = {     /* A1 -> H8 */
    A1, A2, A3, A4, A5, A6, A7, A8,
    B1, B2, B3, B4, B5, B6, B7, B8,
    C1, C2, C3, C4, C5, C6, C7, C8,
    D1, D2, D3,         D6, D7, D8,
    E1, E2, E3,         E6, E7, E8,
    F1, F2, F3, F4, F5, F6, F7, F8,
    G1, G2, G3, G4, G5, G6, G7, G8,
    H1, H2, H3, H4, H5, H6, H7, H8
  };
#endif

  for (i=0; i < BOARD_SIZE; i++) board->square[i] = OFF_SIDE;
  board->n_discs[BLACK] = board->n_discs[WHITE] = board->n_empties = 0;
  for (i = A1; i <= H8; i += 9)
    for (j = i; j < i + 8; j++) {
      if (*string == '\0') break;
      switch (tolower(*string)) {
      case 'b':
      case 'x':
      case '*':
        board->square[j] = BLACK;
        board->n_discs[BLACK]++;
        break;
      case 'o':
      case 'w':
        board->square[j] = WHITE;
        board->n_discs[WHITE]++;
        break;
      case '-':
      case '.':
        board->square[j] = EMPTY;
        board->n_empties++;
        break;
      default:
        j--;
        break;
      }
      string++;
    }

  board->player = EMPTY;
  for (;*string != '\0' && board->player == EMPTY; string++) {
    switch (tolower(*string)) {
    case 'b':
    case 'x':
    case '*':
      board->player = BLACK;
      break;
    case 'o':
    case 'w':
      board->player = WHITE;
      break;
    default:
      break;
    }
  }
  if (board->player == EMPTY) {
    fprintf(stderr, "board_set: incorrect player value '%c'\n", *string);
    exit(EXIT_FAILURE);
  }

  /* init Empties */
  empties = board->empties;
  empties->position = NOMOVE; /* sentinel */
  empties->previous = NULL;
  empties->next = empties + 1;
  empties = empties->next;
  for (i = 0; i < 60; i++) {    /* add empty squares */
    if (board->square[presorted_position[i]] == EMPTY) {
      empties->position = presorted_position[i];
      empties->previous = empties - 1;
      empties->next = empties + 1;
      board->position_to_empties[presorted_position[i]] = empties;
      empties = empties->next;
    }
  }
  empties->position = NOMOVE; /* sentinel */
  empties->previous = empties - 1;
  empties->next = NULL;

  /* set hash_code */
  board->hash_code[0] = board->hash_code[1] = 0;
  if ((board->player == WHITE) ^ ((board->n_empties & 1) == 1)) {
    board->hash_code[0] ^= hash_code_swap_player[0];
    board->hash_code[1] ^= hash_code_swap_player[1];
  }
  for (i = A1; i <= H8; i += 9)
    for (j = i; j < i + 8; j++) {
      board->hash_code[0] ^= hash_code_set_disc[j][(int)board->square[j]][0];
      board->hash_code[1] ^= hash_code_set_disc[j][(int)board->square[j]][1];
    }

  /* init parity */
  board->parity[0] = board->parity[1] = board->parity[2] = board->parity[3] = 0;
  for (empties = board->empties->next; empties->position != NOMOVE; empties = empties->next) {
    board->parity[QUADRANT_ID[empties->position]] ^= 1;
  }

  /* nodes */
  board->n_nodes = 0;
}

/*!
 * \brief Check move validity.
 *
 * Check if a legal move exists on square 'x' for 'player'
 * \param board   board to test
 * \param x       square on which to move.
 * \param player  player to move.
 * \return        1 if a legal move exists, 0 otherwise.
 */
int board_check_move(const Board *board, int x, int player)
{
  const char p = player;
  const char o = OPPONENT(p);
  const char *square = board->square + x;

  switch (FLIPPING_DIRECTION_ID[x]) {
  case 1:
    return  (BOARD_CHECK_MOVE_6(SE) ||
             BOARD_CHECK_MOVE_6(S ) ||
             BOARD_CHECK_MOVE_6( E));
  case 3:
    return  (BOARD_CHECK_MOVE_6(SW) ||
             BOARD_CHECK_MOVE_6( W) ||
             BOARD_CHECK_MOVE_6(S ));
  case 7:
    return  (BOARD_CHECK_MOVE_6(NE) ||
             BOARD_CHECK_MOVE_6(N ) ||
             BOARD_CHECK_MOVE_6( E));
  case 9:
    return  (BOARD_CHECK_MOVE_6(NW) ||
             BOARD_CHECK_MOVE_6(N ) ||
             BOARD_CHECK_MOVE_6( W));
  case 2:
    return  (BOARD_CHECK_MOVE_6(S ) ||
             BOARD_CHECK_MOVE_4(SE) ||
             BOARD_CHECK_MOVE_4(SW) ||
             BOARD_CHECK_MOVE_4( E) ||
             BOARD_CHECK_MOVE_4( W));
  case 4:
    return  (BOARD_CHECK_MOVE_6( E) ||
             BOARD_CHECK_MOVE_4(NE) ||
             BOARD_CHECK_MOVE_4(SE) ||
             BOARD_CHECK_MOVE_4(N ) ||
             BOARD_CHECK_MOVE_4(S ));
  case 6:
    return  (BOARD_CHECK_MOVE_6( W) ||
             BOARD_CHECK_MOVE_4(NW) ||
             BOARD_CHECK_MOVE_4(SW) ||
             BOARD_CHECK_MOVE_4(S ) ||
             BOARD_CHECK_MOVE_4(N ));
  case 8:
    return  (BOARD_CHECK_MOVE_6(N ) ||
             BOARD_CHECK_MOVE_4(NE) ||
             BOARD_CHECK_MOVE_4(NW) ||
             BOARD_CHECK_MOVE_4( E) ||
             BOARD_CHECK_MOVE_4( W));
  case 5:
    return  (BOARD_CHECK_MOVE_4(N ) ||
             BOARD_CHECK_MOVE_4( E) ||
             BOARD_CHECK_MOVE_4(S ) ||
             BOARD_CHECK_MOVE_4( W) ||
             BOARD_CHECK_MOVE_4(NE) ||
             BOARD_CHECK_MOVE_4(SE) ||
             BOARD_CHECK_MOVE_4(NW) ||
             BOARD_CHECK_MOVE_4(SW));
  }
  return 0;
}

/*!
 * \brief Count flippable discs.
 *
 * Count how many discs can be flipped (without flipping them).
 * \param board  board to test
 * \param x      square on which to move.
 * \param player player to move.
 * \return       the number of disc(s) flipped.
 */
int board_count_flips(const Board *board, int x, int player)
{
  const char p = player;
  const char o = OPPONENT(p);
  const char *square = board->square + x;
  int n_flips = 0;

  switch (FLIPPING_DIRECTION_ID[x]) {
  case 1:
    BOARD_COUNT_FLIPS_6( E);
    BOARD_COUNT_FLIPS_6(S );
    BOARD_COUNT_FLIPS_6(SE);
    break;
  case 3:
    BOARD_COUNT_FLIPS_6( W);
    BOARD_COUNT_FLIPS_6(SW);
    BOARD_COUNT_FLIPS_6(S );
    break;
  case 7:
    BOARD_COUNT_FLIPS_6(N );
    BOARD_COUNT_FLIPS_6(NE);
    BOARD_COUNT_FLIPS_6( E);
    break;
  case 9:
    BOARD_COUNT_FLIPS_6(NW);
    BOARD_COUNT_FLIPS_6(N );
    BOARD_COUNT_FLIPS_6( W);
    break;
  case 2:
    BOARD_COUNT_FLIPS_6(S );
    BOARD_COUNT_FLIPS_4( W);
    BOARD_COUNT_FLIPS_4( E);
    BOARD_COUNT_FLIPS_4(SW);
    BOARD_COUNT_FLIPS_4(SE);
    break;
  case 4:
    BOARD_COUNT_FLIPS_6( E);
    BOARD_COUNT_FLIPS_4(N );
    BOARD_COUNT_FLIPS_4(NE);
    BOARD_COUNT_FLIPS_4(S );
    BOARD_COUNT_FLIPS_4(SE);
    break;
  case 6:
    BOARD_COUNT_FLIPS_6( W);
    BOARD_COUNT_FLIPS_4(NW);
    BOARD_COUNT_FLIPS_4(N );
    BOARD_COUNT_FLIPS_4(SW);
    BOARD_COUNT_FLIPS_4(S );
    break;
  case 8:
    BOARD_COUNT_FLIPS_6(N );
    BOARD_COUNT_FLIPS_4(NW);
    BOARD_COUNT_FLIPS_4(NE);
    BOARD_COUNT_FLIPS_4( W);
    BOARD_COUNT_FLIPS_4( E);
    break;
  case 5:
    BOARD_COUNT_FLIPS_4(NW);
    BOARD_COUNT_FLIPS_4(N );
    BOARD_COUNT_FLIPS_4(NE);
    BOARD_COUNT_FLIPS_4( W);
    BOARD_COUNT_FLIPS_4( E);
    BOARD_COUNT_FLIPS_4(SW);
    BOARD_COUNT_FLIPS_4(S );
    BOARD_COUNT_FLIPS_4(SE);
    break;
  }
  return n_flips;
}

/*!
 * \brief Flip discs on the board.
 *
 * Modify a board by flipping its discs. Only the square[] and player members
 * are modified.
 * \param board board to modify
 * \param x     square on which to move.
 * \param move  a Move structure remembering the modification.
 * \return      the number of disc(s) flipped.
 */
int board_do_flip(Board *board, int x, Move *move)
{
  const char p = board->player;
  const char o = OPPONENT(p);
  char *square = board->square + x;

  move->n = 0;
  switch (FLIPPING_DIRECTION_ID[x]) {
  case 1:
    BOARD_FLIP(E , 6);
    BOARD_FLIP( S, 6);
    BOARD_FLIP(SE, 6);
    break;
  case 3:
    BOARD_FLIP( W, 6);
    BOARD_FLIP(SW, 6);
    BOARD_FLIP(S , 6);
    break;
  case 7:
    BOARD_FLIP(N , 6);
    BOARD_FLIP(NE, 6);
    BOARD_FLIP( E, 6);
    break;
  case 9:
    BOARD_FLIP(NW, 6);
    BOARD_FLIP(N , 6);
    BOARD_FLIP( W, 6);
    break;
  case 2:
    BOARD_FLIP(S , 6);
    BOARD_FLIP( W, 4);
    BOARD_FLIP( E, 4);
    BOARD_FLIP(SW, 4);
    BOARD_FLIP(SE, 4);
    break;
  case 4:
    BOARD_FLIP(N , 4);
    BOARD_FLIP(NE, 4);
    BOARD_FLIP( E, 6);
    BOARD_FLIP(S , 4);
    BOARD_FLIP(SE, 4);
    break;
  case 6:
    BOARD_FLIP(NW, 4);
    BOARD_FLIP(N , 4);
    BOARD_FLIP( W, 6);
    BOARD_FLIP(SW, 4);
    BOARD_FLIP(S , 4);
    break;
  case 8:
    BOARD_FLIP(NW, 4);
    BOARD_FLIP(N , 6);
    BOARD_FLIP(NE, 4);
    BOARD_FLIP( W, 4);
    BOARD_FLIP( E, 4);
    break;
  case 5:
    BOARD_FLIP(NW, 4);
    BOARD_FLIP(N , 4);
    BOARD_FLIP(NE, 4);
    BOARD_FLIP( W, 4);
    BOARD_FLIP( E, 4);
    BOARD_FLIP(SW, 4);
    BOARD_FLIP(S , 4);
    BOARD_FLIP(SE, 4);
    break;
  }
  if (move->n > 0) {
    *move->position = x;
    *square = p;
    BOARD_UPDATE_PLAYER(board);
    BOARD_UPDATE_INTERNAL_NODES();
  }
  return move->n;
}

/*!
 * \brief Compute a move.
 *
 * Compute how the board will be modified by a move without playing it.
 * \param board board
 * \param x     square on which to move.
 * \param move  a Move structure remembering the modification.
 * \return      the number of disc(s) flipped.
 */
int board_get_move(const Board *board, int x, Move *move)
{
  const char p = board->player;
  const char o = OPPONENT(p);
  const char *square = board->square + x;

  move->n = 0;
  move->hash_code[0] = hash_code_set_disc[x][(int)p][0];
  move->hash_code[1] = hash_code_set_disc[x][(int)p][1];
  switch (FLIPPING_DIRECTION_ID[x]) {
  case 1:
    BOARD_GET_MOVE( E, 6);
    BOARD_GET_MOVE(S , 6);
    BOARD_GET_MOVE(SE, 6);
    break;
  case 3:
    BOARD_GET_MOVE( W, 6);
    BOARD_GET_MOVE(SW, 6);
    BOARD_GET_MOVE(S , 6);
    break;
  case 7:
    BOARD_GET_MOVE(N , 6);
    BOARD_GET_MOVE(NE, 6);
    BOARD_GET_MOVE( E, 6);
    break;
  case 9:
    BOARD_GET_MOVE(NW, 6);
    BOARD_GET_MOVE(N , 6);
    BOARD_GET_MOVE( W, 6);
    break;
  case 2:
    BOARD_GET_MOVE( W, 4);
    BOARD_GET_MOVE( E, 4);
    BOARD_GET_MOVE(SW, 4);
    BOARD_GET_MOVE(S , 6);
    BOARD_GET_MOVE(SE, 4);
    break;
  case 4:
    BOARD_GET_MOVE(N , 4);
    BOARD_GET_MOVE(NE, 4);
    BOARD_GET_MOVE( E, 6);
    BOARD_GET_MOVE(S , 4);
    BOARD_GET_MOVE(SE, 4);
    break;
  case 6:
    BOARD_GET_MOVE(NW, 4);
    BOARD_GET_MOVE(N , 4);
    BOARD_GET_MOVE( W, 6);
    BOARD_GET_MOVE(SW, 4);
    BOARD_GET_MOVE(S , 4);
    break;
  case 8:
    BOARD_GET_MOVE(NW, 4);
    BOARD_GET_MOVE(N , 6);
    BOARD_GET_MOVE(NE, 4);
    BOARD_GET_MOVE( W, 4);
    BOARD_GET_MOVE( E, 4);
    break;
  case 5:
    BOARD_GET_MOVE(NW, 4);
    BOARD_GET_MOVE(N , 4);
    BOARD_GET_MOVE(NE, 4);
    BOARD_GET_MOVE( W, 4);
    BOARD_GET_MOVE( E, 4);
    BOARD_GET_MOVE(SW, 4);
    BOARD_GET_MOVE(S , 4);
    BOARD_GET_MOVE(SE, 4);
    break;
  }
  if (move->n>0) {
    *move->position = x;
  }

  return move->n;
}

/*!
 * \brief Update a board.
 *
 * Update a board by flipping its discs and updating every other data,
 * according to the 'move' description.
 * \param board : the board to modify
 * \param move  : A Move structure describing the modification.
 */
void board_update_move(Board *board, const Move *move)
{
  const char p = board->player;
  const char o = OPPONENT(p);
  SquareList *empties;

  BOARD_UPDATE_SQUARE(board, move);
  BOARD_UPDATE_PLAYER(board);
  BOARD_UPDATE_DISCS(board, move->n);
  BOARD_UPDATE_INTERNAL_NODES();
  BOARD_UPDATE_HASH_CODE(board, move->hash_code);
  BOARD_UPDATE_PARITY(board, *move->position);
  BOARD_UPDATE_EMPTIES(board, empties, *move->position);
}

/*!
 * \brief Restore a board.
 *
 * Restore a board by un-flipping its discs and restoring every other data,
 * according to the 'move' description, in order to cancel a board_update_move.
 * \param board : board to restore.
 * \param move  : a Move structure describing the modification.
 */
void board_restore_move(Board *board, const Move *move)
{
  const char o = board->player;
  const char p = OPPONENT(o);
  SquareList *empties;

  BOARD_RESTORE_SQUARE(board, move);
  BOARD_RESTORE_PLAYER(board);
  BOARD_RESTORE_DISCS(board, move->n);
  BOARD_RESTORE_HASH_CODE(board, move->hash_code);
  BOARD_RESTORE_PARITY(board, *move->position);
  BOARD_RESTORE_EMPTIES(board, empties, *move->position);
}

/*!
 * \brief Passing move
 *
 * Modify a board by passing player's turn.
 * \param board : board to update.
 */
void board_update_pass(Board *board)
{
  const char o = OPPONENT(board->player);
  BOARD_UPDATE_PLAYER(board);
  BOARD_UPDATE_INTERNAL_NODES();
  BOARD_UPDATE_HASH_CODE(board, hash_code_swap_player);
}

/*!
 * \brief Un-passing
 *
 * Restore a board by un-passing player's turn.
 * \param board : board to restore.
 */
void board_restore_pass(Board *board)
{
  const char p = OPPONENT(board->player);
  BOARD_RESTORE_PLAYER(board);
  BOARD_RESTORE_HASH_CODE(board, hash_code_swap_player);
}

/*!
 * \brief Get a list of legal moves.
 *
 * Compute the complete list of legal moves and store it into a simple linked
 * list, to fasten ulterior move sorting.
 * \param board : the board to check
 * \param start : start point of the linked-list that will received the legal
 *                moves.
 */
void board_get_movelist(const Board *board, MoveList *start)
{
  MoveList *list = start + 1, *previous = start;
  SquareList *empties;

  for (empties = board->empties->next; empties->position != NOMOVE; empties = empties->next) {
    if (board_get_move(board, empties->position, &(list->move))) {
      previous = previous->next = list;
      list++;
    }
  }
  previous->next = NULL;
}

/*!
 * \brief Estimate the mobility.
 *
 * Count a number of legal moves for a player. The mobility is an
 * important concept in Othello, used here for move sorting.
 * \param board : the board to test.
 * \param player : the player to test.
 * \return : the number of legal moves.
 */
int board_get_mobility(const Board *board, int player)
{
  SquareList *empties;
  int n_moves = 0;

  for (empties = board->empties->next; empties->position != NOMOVE; empties = empties->next) {
    if (board_check_move(board, empties->position, player))
      n_moves++;
  }

  return n_moves;
}

/*!
 * \brief Estimate corner stability.
 *
 * Count the number of stable discs around the corner. Limiting the count
 * to the corner keep the function fast but still get this information,
 * particularly important at Othello. Corner stability will be used for
 * move sorting.
 * \param board : the board.
 * \param player : the player.
 * \return : the number of stable discs around the corner.
 */
int board_get_corner_stability(Board *board, int player)
{
  const char *square = board->square;
  const char p = player;
  int n_stables = 0;

  if (square[A1] == p) {
    n_stables++;
    if (square[B1] == p) n_stables++;
    if (square[A2] == p) n_stables++;
  }
  if (square[A8] == p) {
    n_stables++;
    if (square[B8] == p) n_stables++;
    if (square[A7] == p) n_stables++;
  }
  if (square[H1] == p) {
    n_stables++;
    if (square[G1] == p) n_stables++;
    if (square[H2] == p) n_stables++;
  }
  if (square[H8] == p) {
    n_stables++;
    if (square[G8] == p) n_stables++;
    if (square[H7] == p) n_stables++;
  }

  return n_stables;
}

/*!
 * \brief Initialize the local parity counters.
 *
 * Approximate the local parity by computing it on each board quadrant. This
 * will be used at the very end of game for move sorting.
 * \param board : the board.
 */
/*#if PLAY_ODD_SQUARE_FIRST
  void board_init_parity(Board *board)
  {
  unsigned char *parity = board->parity;
  SquareList *empties;

  parity[0] = parity[1] = parity[2] = parity[3] = 0;
  for (empties = board->empties->next; empties->position != NOMOVE; empties = empties->next) {
  parity[QUADRANT_ID[empties->position]] ^= 1;
  }
  }
  #else
  #define board_init_parity(board)
  #endif
*/

/*!
 * \brief Print out the board.
 *
 * Print an ASCII representation of the board to an output stream.
 * \param board : board to print.
 * \param f     : output stream.
 */
void board_print(Board *board, FILE *f)
{
  int i, j, square, x;
  char *color = "*O-.?";

  fputs("  A B C D E F G H\n", f);
  for (i = 1; i <= 8; i++) {
    fputc(i + '0', f);
    fputc(' ', f);
    for (j = 1; j <= 8; j++) {
      x = i * 9 + j;
      square = (int)board->square[x];
      if (square == EMPTY && board_check_move(board, x, board->player))
        square++;
      fputc(color[square], f);
      fputc(' ', f);
    }
    fputc(i+'0', f);
    if (i == 2)
      fprintf(f, " %c to move", color[(int)board->player]);
    else if (i == 4)
      fprintf(f, " *: discs = %2d    moves = %2d",
              board->n_discs[BLACK], board_get_mobility(board, BLACK));
    else if (i == 5)
      fprintf(f, " O: discs = %2d    moves = %2d",
              board->n_discs[WHITE], board_get_mobility(board, WHITE));
    else if (i == 6)
      fprintf(f, "  empties = %2d      ply = %2d",
              board->n_empties, 61-board->n_empties);
    fputc('\n', f);
  }
  fputs("  A B C D E F G H\n", f);
}

/*@}*/
/*!
 * \defgroup move Move module
 * Functions to print a move or a list of moves and to sort list of moves.
 */
/*@{*/

/*!
 * \brief Print out a move
 *
 * Print the move, using letter case to distinguish player's color,
 * to an output stream.
 * \param x      square coordinate to print.
 * \param player player color.
 * \param f      output stream.
 */
void move_print(int x, int player, FILE *f)
{
  int s[2];

  if (x == NOMOVE) {
    s[0] = ' ';
    s[1] = ' ';
  } else if (x == PASS) {
    s[0] = 'p';
    s[1] = 's';
  } else if (x >= A1 && x <= H8 && x % 9 > 0) {
    s[0] = x % 9 + 'a' - 1;
    s[1] = x / 9 + '1' - 1;
  } else {
    s[0] = '?';
    s[1] = '?';
  }

  if (player == BLACK) {
    s[0] = toupper(s[0]);
    s[1] = toupper(s[1]);
  }
  fputc(s[0], f);
  fputc(s[1], f);
}

/*!
 * \brief Print a PV.
 *
 * Get the Principal Variation (best move sequence) from the hash table,
 * and print it to the output stream.
 * \param board      starting position.
 * \param hash_table hash table where to retrieve the moves.
 * \param depth      maximum number of moves to print.
 * \param f          output stream.
 */
void line_print(Board *board, HashTable *hash_table, int depth, FILE *f)
{
  const char p = board->player;
  const char o = OPPONENT(p);
  Hash *hash = hash_get(hash_table, board);
  Move move[1];

  if (depth <= 0) return;
  if (hash != NULL) {
    move_print(hash->move, board->player, f);
    fputc(' ',f);
    if (board_get_move(board, hash->move, move)) {
      BOARD_UPDATE_SQUARE(board, move);
      BOARD_UPDATE_PLAYER(board);
      BOARD_UPDATE_HASH_CODE(board, move->hash_code);
      line_print(board, hash_table, depth - 1, f);
      BOARD_RESTORE_SQUARE(board, move);
      BOARD_RESTORE_PLAYER(board);
      BOARD_RESTORE_HASH_CODE(board, move->hash_code);
    } else if (hash->move == PASS) {
      BOARD_UPDATE_PLAYER(board);
      BOARD_UPDATE_HASH_CODE(board, hash_code_swap_player);
      line_print(board, hash_table, depth - 1, f);
      BOARD_RESTORE_PLAYER(board);
      BOARD_RESTORE_HASH_CODE(board, hash_code_swap_player);
    } else {
      line_print(board, hash_table, depth - 1, f);
    }
  } else {
    fputs("-- ", f);
    line_print(board, hash_table, depth - 1, f);
  }
}

/*!
 * \brief Sort a move as best.
 *
 * Put a move at the head of the list.
 * \param movelist   list of moves to sort.
 * \param move       best move to to set first.
 */
void movelist_sort_bestmove(MoveList *movelist, int move)
{
  MoveList *iter, *previous;

  for (iter = (previous = movelist)->next; iter != NULL; iter = (previous = iter)->next) {
    if (*iter->move.position == move) {
      previous->next = iter->next;
      iter->next = movelist->next;
      movelist->next = iter;
      break;
    }
  }
}

/*!
 * \brief Sort a list of move, fastest-first.
 *
 * Sort a list to accelerate the alphabeta research. The moves that minimize
 * opponent mobility and increase player's relative stability will be played
 * first.
 * \param movelist   : list of moves to sort.
 * \param board      : board on which the moves applied.
 */
void movelist_sort_fastfirst(MoveList *movelist, Board *board)
{
  MoveList *iter, *best, *previous_best, *previous;
  SquareList *empties;
  const char p = board->player;
  const char o = OPPONENT(p);

  for (iter = movelist->next; iter != NULL; iter = iter->next) {
    BOARD_UPDATE_SQUARE(board, &(iter->move));
    BOARD_LAST_UPDATE_EMPTIES(board, empties, *iter->move.position);
    BOARD_UPDATE_ALL_NODES();
#if PLAY_STABLEST_SUBTREE
    iter->move.score = (board_get_mobility(board, o) << 4) - board_get_corner_stability(board, p);
#else
    iter->move.score = board_get_mobility(board, o);
#endif
    BOARD_RESTORE_SQUARE(board, &(iter->move));
    BOARD_LAST_RESTORE_EMPTIES(board, empties, *iter->move.position);
  }
  for (iter = movelist; iter->next != NULL; iter = iter->next) {
    previous_best = iter;
    for (previous = previous_best->next; previous->next != NULL;
         previous = previous->next) {
      if (previous_best->next->move.score >  previous->next->move.score) {
        previous_best = previous;
      }
    }
    if (previous_best != iter) {
      best = previous_best->next;
      previous_best->next = best->next;
      best->next = iter->next;
      iter->next = best;
    }
  }
}

/*@}*/
/*!
 * \defgroup search Search module
 * Functions that evaluate a board with different methods depending on the
 * position in the tree search and/or that finds the best move of a given
 * board.
 *
 * At the end of the game, some trivial functions are used to compute the score.
 * Optimization here is reached by maintaining a disc number record that is
 * updated each time a move is made. At the end of the game, computing the score
 * consists only to a simple substraction. For further optimization, the last
 * move is not made and only the number of flipped discs is evaluated. Special
 * and optimized functions are used when one or two empty squares remain on the
 * board, in order to speed up the search.
 *
 * The search of the best move is driven with the Principal Variation Search
 * algorithm (PVS) [1], an enhanced variation of the alphabeta algorithm. The
 * alphabeta algorithm is known to visit less nodes when the alphabeta window is
 * reduced. PVS takes this property into account. From a set of sibling nodes,
 * the first node is searched using a plain alpha beta window. Then the sibling
 * nodes are only searched with minimal windows (where beta = alpha + 1), just
 * to refute the best (first) score. In rare cases the first move is actually
 * refuted, then the current move is re-searched a second time in order to
 * determinate its score more accurately. On highly ordered tree, very few
 * re-searches will be done. Moreover, thanks to the hash table, a second search
 * is usually faster than a first search. So the seldom and fast re-searches
 * will not impact too much the benefit of using minimal windows. Aspiration
 * windows have been added as another improvement, so that even the first
 * search is done with a reduced window.
 * During the 1990s, several re-re-search algorithm based on null-window
 * alphabeta have been proposed : SSS*ab [2], Dual*ab[2], NegaC*[3], MDT[2],
 * negascout[4]. Some (unpublished) experimental tests I made with them did not
 * show any significant improvement compare to the PVS algorithm with
 * aspiration-windows used here.
 *
 * To be efficient PVS need highly ordered tree. The following ordering have
 * been made :
 *       - fixed square ordering : square usually leading to a good move are
 * visited first, ie from corner squares to X and C squares.
 *       - most stable ordering : a crude evaluation of stability at the corner
 * (corner, X and C squares) to order the moves.
 *       - fast first ordering : the moves leading to the most reduced mobility
 * for the opponent are played first.
 *       - best move previously found : If the position have been previously
 * searched, the best move that was found is replayed as the first move.
 *
 * -# Campbell MS & Marsland TA (1983) A Comparison of Minimax Trees Search
 *    Algorithms. Artificial Intelligence, 20, pp. 347-367.
 * -# Plaat A. et al. (1996) Best-First Fixed Depth Minimax Algorithms. Artificial
 *    Intelligence, 84, pp. 255-293.
 * -# Weill JC. (1992) Experiments With The NegaC* Search. An alternative for Othello
 *    Endgame Search. ICCA journal, 15(1), pp. 3-7.
 * -# Reinsfeld A. (1983) An Improvement Of the Scout Tree-Search Algorithm. ICCA
 *    journal, 6(4), pp. 4-14.
*/
/*@{*/

/*!
 * \brief Get the final score.
 *
 * Get the final score, when no move can be made.
 * \param board  board.
 * \return       the final score, as a disc difference.
 */
int board_get_final_score(const Board *board)
{
  const int p = board->player;
  const int o = OPPONENT(p);
  int score = board->n_discs[p] - board->n_discs[o];

  if (score < 0) score -= board->n_empties;
  else if (score > 0) score += board->n_empties;

  return score;
}

/*!
 * \brief Get the final score.
 *
 * Get the final score, when the board is full.
 * \param board   board.
 * \return        the final score, as a disc difference.
 */
int board_get_final_score_0(const Board *board)
{
  const int p = board->player;
  const int o = OPPONENT(p);
  int score = board->n_discs[p] - board->n_discs[o];

  return score;
}

/*!
 * \brief Get the final score.
 *
 * Get the final score, when 1 empty square remains.
 * \param board   board.
 * \return        the final score, as a disc difference.
 */
int board_get_final_score_1(Board *board)
{
  const int p = board->player;
  const int o = OPPONENT(p);
  int score = board->n_discs[p] - board->n_discs[o];
  int n_flips;
  const int x = board->empties->next->position;

  if ((n_flips = board_count_flips(board, x, p)) > 0) {
    score += 2 * n_flips + 1;
    BOARD_UPDATE_TERMINAL_NODES();
  } else if ((n_flips = board_count_flips(board, x, o)) > 0) {
    score -= 2 * n_flips + 1;
    BOARD_UPDATE_TERMINAL_NODES();
  } else if (score < 0) score--;
  else if (score > 0) score++;

  return score;
}

/*!
 * \brief Get the final score.
 *
 * Get the final score, when 2 empty squares remain.
 * \param board  the board to evaluate.
 * \param alpha  upper score value.
 * \param beta   lower score value.
 * \param passed  a flag indicating if previous move was a pass.
 * \return       the final score, as a disc difference.
 */
int board_get_final_score_2(Board *board, int alpha, int beta, int passed)
{
  const char p = board->player;
  const char o = OPPONENT(p);
  SquareList *empties = board->empties->next;
  int x1 = empties->position;
  int x2 = empties->next->position;
  int score, bestscore, n1_flips, n2_flips, diff_discs;
  Move move;

  diff_discs = board->n_discs[(int)p] - board->n_discs[(int)o];
  bestscore = -INF_SCORE;

  /* try to play on the first available square */
  if ((n1_flips = board_do_flip(board, x1, &move)) > 0) {
    if ((n2_flips = board_count_flips(board, x2, o)) > 0) {
      bestscore = diff_discs + 2 * (n1_flips - n2_flips);
      BOARD_UPDATE_TERMINAL_NODES();
    } else if ((n2_flips = board_count_flips(board, x2, p)) > 0) {
      bestscore = diff_discs + 2 * (n1_flips + n2_flips) + 2;
      BOARD_UPDATE_TERMINAL_NODES();
    } else {
      bestscore = diff_discs + 2 * n1_flips + 1;
      if (bestscore > 0) bestscore++;
      else if (bestscore < 0) bestscore--;
    }
    BOARD_RESTORE_SQUARE(board, &move);
    BOARD_RESTORE_PLAYER(board);
  }
  /* if needed, try to play on the second & last available square */
  if (bestscore < beta && (n1_flips = board_do_flip(board, x2, &move)) > 0) {
    if ((n2_flips = board_count_flips(board, x1, o)) > 0) {
      score = diff_discs + 2 * (n1_flips - n2_flips);
      BOARD_UPDATE_TERMINAL_NODES();
    } else if ((n2_flips = board_count_flips(board, x1, p)) > 0) {
      score = diff_discs + 2 * (n1_flips + n2_flips) + 2;
      BOARD_UPDATE_TERMINAL_NODES();
    } else {
      score = diff_discs + 2 * n1_flips + 1;
      if (score > 0) score++;
      else if (score < 0) score--;
    }
    BOARD_RESTORE_SQUARE(board, &move);
    BOARD_RESTORE_PLAYER(board);
    if (score > bestscore) bestscore = score;
  }
  /* if no move were available */
  if (bestscore == -INF_SCORE) {
    if (passed) { /* game is over */
      BOARD_CORRECT_TERMINAL_NODES();
      bestscore = diff_discs;
      if (bestscore > 0) bestscore += 2;
      else if (bestscore < 0) bestscore -= 2;
    } else { /* pass... */
      BOARD_UPDATE_PLAYER(board);
      BOARD_UPDATE_INTERNAL_NODES();
      bestscore = -board_get_final_score_2(board, -beta, -alpha, 1);
      BOARD_RESTORE_PLAYER(board);
    }
  }

  return bestscore;
}

/*!
 * \brief  Evaluate a position using a shallow Alphabeta.
 *
 * This function is used when there are few empty squares on the board. Here,
 * optimizations are in favour of speed instead of efficiency. A simple
 * alphabeta is used because of the low branching factor that makes PVS less
 * efficient.
 * \param board   board.
 * \param alpha   lower bound.
 * \param beta    upper bound.
 * \param passed  a flag indicating if previous move was a pass.
 * \return        the final score, as a disc difference.
 */
int alphabeta_shallow(Board *board, int alpha, int beta, int passed)
{
  const char p = board->player;
  const char o = OPPONENT(p);
  int score, bestscore = -INF_SCORE;
  SquareList *empties;
  Move move;

#if PLAY_ODD_SQUARE_FIRST
  int parity;
  for (parity = 1; parity >= 0; parity--) {
    for (empties = board->empties->next; empties->position != NOMOVE; empties = empties->next) {
      if (board->parity[QUADRANT_ID[empties->position]] == parity
          && board_do_flip(board, empties->position, &move)) {
#else
  {
    for (empties = board->empties->next; empties->position != NOMOVE; empties = empties->next) {
      if (board_do_flip(board, empties->position, &move)) {
#endif
        BOARD_UPDATE_PLAYER(board);
        BOARD_UPDATE_DISCS(board, move.n);
        BOARD_UPDATE_PARITY(board, *move.position);
        BOARD_UPDATE_EMPTIES(board, empties, *move.position);
        if (board->n_empties == 2) {
          score = -board_get_final_score_2(board, -beta, -alpha, 0);
        } else {
          score = -alphabeta_shallow(board, -beta, -alpha, 0);
        }
        BOARD_RESTORE_SQUARE(board, &move);
        BOARD_RESTORE_PLAYER(board);
        BOARD_RESTORE_DISCS(board, move.n);
        BOARD_RESTORE_PARITY(board, *move.position);
        BOARD_RESTORE_EMPTIES(board, empties, *move.position);
        if (score > bestscore) {
          bestscore = score;
          if (bestscore > alpha) {
            alpha = bestscore;
            if (alpha >= beta) return bestscore;
          }
        }
      }
    }
  }

  /* no move */
  if (bestscore == -INF_SCORE) {
    if (passed) { /* game over */
      BOARD_CORRECT_TERMINAL_NODES();
      bestscore = board_get_final_score(board);
    } else { /* pass */
      BOARD_UPDATE_PLAYER(board);
      BOARD_UPDATE_INTERNAL_NODES();
      bestscore = -alphabeta_shallow(board, -beta, -alpha, 1);
      BOARD_RESTORE_PLAYER(board);
    }
  }
  return bestscore;
}

/*!
 * \brief Evaluate a position with a deep Principal Variation Search algorithm.
 *
 * This function is used when there are still many empty squares on the board. Move
 * ordering, hash table cutoff, enhanced transposition cutoff, etc. are used in
 * order to diminish the size of the tree to analyse, but at the expense of a
 * slower speed.
 *
 * \param board      board.
 * \param hash_table hash_table.
 * \param alpha      lower bound.
 * \param beta       upper bound.
 * \param passed     a flag indicating if previous move was a pass.
 * \return the final score, as a disc difference.
 */
int PVS_deep(Board *board, HashTable *hash_table, int alpha, int beta, int passed)
{
  int score, bestscore, lower, upper, bestmove;
  unsigned long hash_lock, hash_index;
  MoveList movelist[MAX_MOVE + 2], *iter;
  Move *move;
  Hash *hash;
  HashEntry *hash_entry;

  bestmove = NOMOVE;
  lower = alpha;
  upper = beta;
  /* transposition cutoff ? */
#if USE_HASH_TABLE
  hash = hash_get(hash_table, board);
  if (hash != NULL) {
    if (upper > hash->upper) {
      upper = hash->upper;
      if (upper <= lower) return upper;
    }
    if (lower < hash->lower) {
      lower = hash->lower;
      if (lower >= upper) return lower;
    }
    bestmove = hash->move;
  }
#endif
  board_get_movelist(board, movelist);
  if (movelist->next == NULL) {
    if (passed) {
      BOARD_CORRECT_TERMINAL_NODES();
      alpha = -(beta = +INF_SCORE);
      bestscore = board_get_final_score(board);
      bestmove = NOMOVE;
    } else {
      board_update_pass(board);
      bestscore = -PVS_deep(board, hash_table, -upper, -lower, 1);
      bestmove = PASS;
      board_restore_pass(board);
    }
  } else {
    /* enhanced transposition cutoff */
#if USE_ENHANCED_TRANSPOSITION_CUTOFF
    if (board->n_empties > EMPTIES_DEEP_TO_SHALLOW_SEARCH &&
        HASH_TABLE_OK(hash_table)) {
      if (bestmove != NOMOVE) movelist_sort_bestmove(movelist, bestmove);
      for (iter = movelist->next; iter != NULL; iter = iter->next) {
        move = &(iter->move);
        hash_index = (board->hash_code[0] ^ move->hash_code[0]);
        hash_lock = (board->hash_code[1] ^ move->hash_code[1]);
        hash_entry = hash_table->hash_entry + hash_index;
        BOARD_UPDATE_ALL_NODES();
        if (hash_entry->deepest.lock ==  hash_lock && -hash_entry->deepest.upper >= upper)
          return -hash_entry->deepest.upper;
        if (hash_entry->newest.lock ==  hash_lock && -hash_entry->newest.upper >= upper)
          return -hash_entry->newest.upper;
      }
    }
#endif
    /* move sorting */
#if PLAY_FAST_SUBTREE_FIRST
    if (board->n_empties > EMPTIES_DEEP_TO_SHALLOW_SEARCH)
      movelist_sort_fastfirst(movelist, board);
#endif
#if PLAY_BEST_MOVE_IN_MEMORY_FIRST
    if (bestmove != NOMOVE && bestmove != *movelist->next->move.position)
      movelist_sort_bestmove(movelist, bestmove);
#endif

    /* first move */
    iter = movelist->next;
    move = &(iter->move);
    board_update_move(board, move);
    if (board->n_empties < EMPTIES_DEEP_TO_SHALLOW_SEARCH) {
      bestscore = -alphabeta_shallow(board, -upper, -lower, 0);
    } else {
      bestscore = -PVS_deep(board, hash_table, -upper, -lower, 0);
    }
    bestmove = *move->position;
    if (bestscore > lower) lower = bestscore;
    board_restore_move(board, move);

    /* other moves : try to refute the first/best one */
    for (iter = iter->next; lower < upper && iter != NULL; iter = iter->next) {
      move = &(iter->move);
      board_update_move(board, move);
      if (board->n_empties < EMPTIES_DEEP_TO_SHALLOW_SEARCH) {
        score = -alphabeta_shallow(board, -lower - 1, -lower, 0);
        if (lower < score && score < upper)
          score = -alphabeta_shallow(board, -upper, -score, 0);
      } else {
        score = -PVS_deep(board, hash_table, -lower - 1, -lower, 0);
        if (lower < score && score < upper)
          score = -PVS_deep(board, hash_table, -upper, -score, 0);
      }
      board_restore_move(board, move);
      if (score > bestscore) {
        bestscore = score;
        bestmove = *move->position;
        if (bestscore > lower) lower = bestscore;
      }
    }
  }
#if USE_HASH_TABLE
  hash_update(hash_table, board, alpha, beta, bestscore, bestmove);
#endif

  return bestscore;
}

/*!
 * \brief Principal Variation Search algorithm at the root of the tree.
 *
 * This function solves the position provided within the limits set by the alpha
 * and beta bounds. The movelist parameter is updated so that the bestmove is the
 * first of the list when the search ended.
 *
 * \param board      board.
 * \param hash_table hash table to memorize the analysis.
 * \param alpha      lower bound.
 * \param beta       upper bound.
 * \param movelist   List of legal moves (should actually contain moves !).
 */
void PVS_root(Board *board, HashTable *hash_table, int alpha, int beta,
              MoveList *movelist)
{
  int lower, upper;
  MoveList *iter;
  Move *move, *bestmove;

  lower = alpha;
  upper = beta;
  board->n_nodes++;

  /* first move */
  iter = movelist->next;
  bestmove = &(iter->move);
  board_update_move(board, bestmove);
  if (board->n_empties == 0) {
    bestmove->score = -board_get_final_score_0(board);
  } else if (board->n_empties == 1) {
    bestmove->score = -board_get_final_score_1(board);
  } else if (board->n_empties == 2) {
    bestmove->score = -board_get_final_score_2(board, -upper, -lower, 0);
  } else if (board->n_empties < EMPTIES_DEEP_TO_SHALLOW_SEARCH) {
    bestmove->score = -alphabeta_shallow(board, -upper, -lower, 0);
  } else {
    bestmove->score = -PVS_deep(board, hash_table, -upper, -lower, 0);
  }
  if (bestmove->score > lower) lower = bestmove->score;
  board_restore_move(board, bestmove);

  /* other moves : try to refute the first/best one */
  for (iter = iter->next; lower < upper && iter != NULL; iter = iter->next) {
    move = &(iter->move);
    board_update_move(board, move);
    if (board->n_empties == 0) {
      move->score = -board_get_final_score_0(board);
    } else if (board->n_empties == 1) {
      move->score = -board_get_final_score_1(board);
    } else if (board->n_empties == 2) {
       move->score = -board_get_final_score_2(board, -upper, -lower, 0);
    } else if (board->n_empties < EMPTIES_DEEP_TO_SHALLOW_SEARCH) {
      move->score = -alphabeta_shallow(board, -lower - 1, -lower, 0);
      if (lower < move->score && move->score < upper)
        move->score = -PVS_deep(board, hash_table, -upper, -move->score, 0);
    } else {
      move->score = -PVS_deep(board, hash_table, -lower - 1, -lower, 0);
      if (lower < move->score && move->score < upper)
        move->score = -PVS_deep(board, hash_table, -upper, -move->score, 0);
    }
    board_restore_move(board, move);
    if (move->score > bestmove->score) {
      bestmove = move;
      if (bestmove->score > lower) lower = bestmove->score;
    }
  }
  movelist_sort_bestmove(movelist, *bestmove->position);
#if USE_HASH_TABLE
  hash_update(hash_table, board, alpha, beta, bestmove->score, *bestmove->position);
#endif
}

/*!
 * \brief Search the bestmove of a given board.
 *
 * Depending on the mode, the function will try to solve for a draw (DL_SCORE),
 * a win (WD_SCORE), the exact outcome (WDL_SCORE) or the exact score. For the
 * latter case, aspiration windows have been implemented. Thus, the search
 * does not start with a plain alphabeta window [-INF_SCORE, +INF_SCORE], but
 * with a reduce window [-1, +1] that will just find who is the winner. In case
 * the game outcome is a draw, the search will stop here otherwise another
 * search with a [+1, +8] window for the winner side will start. If the winning
 * score falls within these bounds, the search is stopped else it is restarted
 * with a last window of [+8, +64].
 * The search is then made within the PVS_root function.
 *
 * \param board       board to solve.
 * \param hash_table  hash table to memorize the analysis.
 * \param mode        search mode: exact or win/draw/loss.
 * \param bestmove    the bestmove found.
 */
void solve(Board *board, HashTable *hash_table, int mode, Move *bestmove)
{
  int score, bound;
  MoveList movelist[MAX_MOVE + 2];

  board->n_nodes = 0;
  board_get_movelist(board, movelist);
  if (movelist->next != NULL) { /* normal play ? */
    if (board->n_empties > EMPTIES_DEEP_TO_SHALLOW_SEARCH)
      movelist_sort_fastfirst(movelist, board);
    switch (mode) {
    case DL_SCORE:
      /* look for a draw or a loss */
      PVS_root(board, hash_table, -1,  0, movelist);
      break;
    case WD_SCORE:
      /* look for a win or a draw */
      PVS_root(board, hash_table,  0, +1, movelist);
      break;
    case WDL_SCORE:
      /* look for a win or a draw or a loss */
      PVS_root(board, hash_table, -1, +1, movelist);
      break;
    default:
      /* look for the exact best score */
      /* start to look for a win or a draw or a loss */
      PVS_root(board, hash_table, -1, +1, movelist);
      score = movelist->next->move.score;
      if (score > 0) {
        /* if a win look for a score between [+2 +8] */
        bound = score + 8;
        PVS_root(board, hash_table, score, bound, movelist);
        score = movelist->next->move.score;
        if (score >= bound) {
          /* failed -> look for a score between [+8, +64] */
          PVS_root(board, hash_table, score, +MAX_SCORE, movelist);
        }
      } else if (score < 0) {
        /* if a loss look for a score between [-8 -2] */
        bound = score - 8;
        PVS_root(board, hash_table, bound, score, movelist);
        score = movelist->next->move.score;
        if (score <= bound) {
          /* failed -> look for a score between [-64, -8] */
          PVS_root(board, hash_table, -MAX_SCORE, score, movelist);
        }
      }
      break;
    }
    *bestmove = movelist->next->move;
  } else { /* pass ? */
    bestmove->n = 0;
    board_update_pass(board);
    board_get_movelist(board, movelist);
    if (movelist->next == NULL) { /* game over ? */
      bestmove->score = -board_get_final_score(board);
      *bestmove->position = NOMOVE;
    } else {
      if (board->n_empties > EMPTIES_DEEP_TO_SHALLOW_SEARCH)
        movelist_sort_fastfirst(movelist, board);
      switch (mode) {
      case DL_SCORE:
        PVS_root(board, hash_table,  0, +1, movelist);
        break;
      case WD_SCORE:
        PVS_root(board, hash_table, -1,  0, movelist);
        break;
      case WDL_SCORE:
        PVS_root(board, hash_table, -1, +1, movelist);
        break;
      default:
        PVS_root(board, hash_table, -1, +1, movelist);
        score = movelist->next->move.score;
        if (score > 0) {
          bound = score + 8;
          PVS_root(board, hash_table, score, bound, movelist);
          score = movelist->next->move.score;
          if (score >= bound)
            PVS_root(board, hash_table, score, +MAX_SCORE, movelist);
        } else if (score < 0) {
          bound = score - 8;
          PVS_root(board, hash_table, bound, score, movelist);
          score = movelist->next->move.score;
          if (score <= bound)
            PVS_root(board, hash_table, -MAX_SCORE, score, movelist);
        }
        break;
      }
      bestmove->score = -movelist->next->move.score;
      *bestmove->position = PASS;
    }
    board_restore_pass(board);
  }
}

/*@}*/
/*!
 * \defgroup main Main Program Module
 * The main function and a usage function explaining the program options are
 * defined here.
 *
 */
/*@{*/


/*!
 * \brief Print program usage.
 *
 * Explain the program command line parameters.
 */
void usage(void)
{
  fprintf(stderr,"solver [options] <script_file>\n");
  fprintf(stderr,"\nSolve a set of othello engdame problems\n");
  fprintf(stderr,"\nOptions:\n");
  fprintf(stderr,"  -v            verbose mode.\n");
  fprintf(stderr,"  -vv           very verbose mode.\n");
  fprintf(stderr,"  -wdl          search for win/draw/loss.\n");
  fprintf(stderr,"  -wd           search for win/(draw-loss).\n");
  fprintf(stderr,"  -dl           search for (win-draw)/loss.\n");
  fprintf(stderr,"  -h <nbits>    set hash table size.\n");
#ifdef _WIN32
  fprintf(stderr,"  -critical     run at critical time priority.\n");
  fprintf(stderr,"  -normal       run at normal time priority (default).\n");
  fprintf(stderr,"  -idle         run at idle time priority.\n");
#endif
  fprintf(stderr,"\nExample:\n");
  fprintf(stderr,"solver -v -h 20 fforum-20-39.scr\n\n");
  exit(EXIT_FAILURE);
}

/*!
 * \brief Program main function.
 *
 * Read optional parameters and a script file name from the command line.
 * Then loop through the positions provided in the script file, solving each one
 * according to the chosen options.
 * \param argc   number of arguments.
 * \param argv   arguments.
 * \return       SUCCESS_EXIT or SUCCESS_FAILURE.
 */
int main(int argc, char **argv)
{
  Board board;
  Move bestmove;
  char *file_name = NULL, problem[256];
  FILE *file;
  clock_t t, T = 0;
  int i, verbose = 0, mode = EXACT_SCORE;
  double nodes = 0.0;
  HashTable hash_table = {NULL, 0};
#if COUNT_NODES > 0
  const char *HEADER = "\n # |depth|score|   principal variation   |   time    | nodes (N)  | speed (N/s)";
  const char *SEPARATOR = "-------------------------------------------------------------------------------";
#else
  const char *HEADER = "\n # |depth|score|   principal variation   |   time";
  const char *SEPARATOR = "-----------------------------------------------------";
#endif
#ifdef _WIN32
  HANDLE hthread;
  int priority = THREAD_PRIORITY_NORMAL;
#endif

  /* parse arguments */
  for (i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-vv") == 0) verbose = 2;
    else if (strcmp(argv[i], "-v") == 0) verbose = 1;
    else if (strcmp(argv[i], "-wdl") == 0) mode = WDL_SCORE;
    else if (strcmp(argv[i], "-wd") == 0) mode = WD_SCORE;
    else if (strcmp(argv[i], "-dl") == 0) mode = DL_SCORE;
    else if (strcmp(argv[i], "-h") == 0 && i + 1 < argc)
      hash_init(&hash_table, atoi(argv[++i]));
#ifdef _WIN32
    else if (strcmp(argv[i], "-critical") == 0)
      priority = THREAD_PRIORITY_TIME_CRITICAL;
    else if (strcmp(argv[i], "-idle") == 0)
      priority = THREAD_PRIORITY_IDLE;
    else if (strcmp(argv[i], "-normal") == 0)
      priority = THREAD_PRIORITY_NORMAL;
#endif
    else if (file_name == NULL) file_name = argv[i];
    else usage();
  }

  /* open script file */
  if (file_name == NULL) usage();
  file = fopen(file_name, "r");
  if (file == NULL) {
    fprintf(stderr, "solver: cannot open script file %s\n", file_name);
    exit(EXIT_FAILURE);
  }

  /* priority control for win32 systems */
#ifdef _WIN32
   hthread = GetCurrentThread();
   if (!SetThreadPriority(hthread, priority)) {
     fputs("solver: cannot change the thread priority\n", stderr);
   }
#endif

   /* loop through problems */
   if (verbose == 1) {
     puts(HEADER);
     puts(SEPARATOR);
   }
   for (i = 0; fgets(problem, 256, file) ; i++) {
     if (*problem == '%') continue;
     board_set(&board, problem);
     if (verbose == 2) {
       board_print(&board, stdout);
       puts(HEADER);
       puts(SEPARATOR);
     }
     t = clock();
#if USE_HASH_TABLE
     hash_clear(&hash_table);
#endif
     solve(&board,&hash_table,mode,&bestmove);
     t = clock() - t;
     T += t;
     nodes += board.n_nodes;
     if (verbose) {
       printf("%3d|", i + 1);
       printf("  %2d |", board.n_empties);
       printf(" %+03d | ", bestmove.score);
       if (hash_table.hash_mask != 0) {
         line_print(&board, &hash_table, 8, stdout);
       } else {
         move_print(*bestmove.position, board.player, stdout);
         printf(" -- -- -- -- -- -- -- ");
       }
       printf("| %d:%02d:%02d.%1d ", TICK_TO_H(t), TICK_TO_M(t), TICK_TO_S(t), TICK_TO_DS(t));
#if COUNT_NODES > 0
       printf("|%11.0f |", board.n_nodes);
       if (t > 0) printf(" %9.0f", board.n_nodes / t * CLOCKS_PER_SEC);
#endif
       putchar('\n');
       if (verbose == 2) putchar('\n');
       fflush(stdout);
     }
#ifdef _WIN32
     if (priority == THREAD_PRIORITY_TIME_CRITICAL) {
       Sleep(100);
     }
#endif
   }
   if (verbose) puts(SEPARATOR);
#if COUNT_NODES > 0
   printf("%.30s : %.0f nodes in ",file_name, nodes);
   printf("%d:%02d:%02d.%1d",
          TICK_TO_H(T), TICK_TO_M(T), TICK_TO_S(T), TICK_TO_DS(T));
   if (T > 0) printf(" %.0f nodes/s.", nodes * CLOCKS_PER_SEC / T);
   printf("\n\n");
#else
   printf("%.30s : %d:%02d:%02d.%1d\n\n",
          file_name, TICK_TO_H(T), TICK_TO_M(T), TICK_TO_S(T), TICK_TO_DS(T));
#endif
   /* close files & free allocated data */
   fclose(file);
   hash_free(&hash_table);

   return EXIT_SUCCESS;
}

/*@}*/
