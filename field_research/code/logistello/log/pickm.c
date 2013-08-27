// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// pick a number of paths from the book / 5.97

#include "main.h"
#include "playm.h"
#include "pmove.h"
#include "move.h"
#include "eval.h"
#include "sboard.h"
#include "pcstat.h"

#include "goodies.h"
#include "filecom.h"
#include "crt.h"
#include "playgm.h"
#include "lib.h"
#include "book.h"
#include "fpatt.h"
#include "hash.h"
#include "killer.h"

static int  bookmode = Book::Mode::DEV_FIRST;
static Book *p_book=0;

const bool RND_DRAW_MODE = false;  
const int  draw_mode = Book::DrawMode::GOOD_FOR_BLACK; // if not random ...
// normal setting: Book::DrawMode::NORMAL;

const bool RND_2ND_MOVE  = false;  // c3 seems to be inferior

const int N = 1000;

char *book_file = "newbook.oko";
char *alt_file  = "newbook.alt";

void _abort() { exit(0); }

int main()
{
  int i;

  p_book = new Book;

  p_book->create_from_files(book_file, alt_file, 0);

  p_book->set_mode(bookmode);

  FOR (i, N) {

    NewGame ngame;
    PackedMove pm, pm_end;
    int     path_value, player = BLACK;
    ValType path_val_type;

    int dm = draw_mode;

    if (RND_DRAW_MODE) {

      switch (random() % 2) {
      case 0: dm = Book::DrawMode::GOOD_FOR_BLACK; break;
      case 1: dm = Book::DrawMode::BAD_FOR_BLACK; break;
 	//      case 0: dm = Book::DrawMode::NORMAL; break;
      default: Error("here?");
      }
    }

    cout << " >>>>>>>> dm=" << dm << endl;

    p_book->set_draw_mode(dm);
    p_book->set_public_draw_mode(dm);

    pm.set(BLACK, D3);
    ngame.set_pm(0, pm);
    ngame.set_move_num(1);

    player = -player;

    if (RND_2ND_MOVE) {

      if (random() & 0x1000) {
	pm.set(WHITE, C3);
      } else {
	pm.set(WHITE, C5);
      }
      ngame.set_pm(1, pm);
      ngame.set_move_num(2);

      player = -player;
    }

    ngame.set_pm(ngame.get_move_num(), pm_end);


    FOREVER {

      int value;
      ValType val_type;
      
      int move = p_book->find_move(ngame, player, value, val_type);
    
      if (move ==  ZUG_UNBEKANNT) break;

      // append move

      pm.set(player, move);
      ngame.set_pm(ngame.get_move_num(), pm);
      ngame.set_move_num(ngame.get_move_num()+1);

      player = -player;

      path_value = value;
      path_val_type = val_type;
    }

    p_book->delete_alt_leaf(ngame);

    ngame.unique();
    cout << endl << endl << "### "; 
    ngame.f_write(stdout); fflush(stdout);
    cout << " " << path_value << " " << path_val_type << endl << endl;

  }

  return 0;
}
