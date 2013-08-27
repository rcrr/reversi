// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2


// Log. code fragment which illustrates how to handle
// JOIN and UPDATE

// pvc is of type Client*
// Message(s) sends s+'\n' to the server


...

#include <vector.h>


if (type == Client::JOIN || type == Client::UPDATE) {

  printf("SIG_JOIN | SIG_UPDATE\n");

  String id;
  
  if (type == Client::JOIN) id = pvc->join.id; else id = pvc->update.id;
  
  const ClientGame *cgame = pvc->get_game(id);
  
  if (!cgame) Error("SIG_JOIN: game not found!");
  
  if (cgame->pinf[0].name != pvc->login() && cgame->pinf[1].name != pvc->login())
    goto reset;
  
  String msg = "tell /os break ";
  msg += id;
  
  // not 8x8 or anti -> break
  // how the hell did you accept those games in the first place?

  if (cgame->type.get_board_type() != 8 || cgame->type.is_anti_game()) {
    Message(msg);
    goto reset;
  }
  
  // player on reject list -> break
  
  if (RejectCond) {
    
    // resist root
    
    FOR (i, (int)RejectNames.size())
      if (cgame->pinf[0].name == RejectNames[i] ||
	  cgame->pinf[1].name == RejectNames[i]) {
	Message(msg);
	goto reset;
      }
  }
  
  if (type == Client::JOIN && !cgame->type.is_rand_game()) {

    // regular game started -> convert ClientGame into ancient format (Game)
    
    Board tab;
    StartPos(&tab); tab.to_move = BLACK; 
    
    uint4 i;
    
    FOR (i, cgame->get_moves().size()) {
      sint4 x, y;
      cgame->get_moves()[i].to_xy(x, y);
      sint4 ind = x+1 + 10 * (y+1);
      tab.p[ind] = i+1 + NUM_DISP;
    }
    
    Board2Game(&tab, &Game);
    fWriteGame(stdout, &Game);
  }
  
  if (!cgame->get_moves().size() == 0 ||
      !cgame->type.is_komi_game()) 
    if (cgame->name_to_move() != pvc->login()) goto reset;  // not my turn
  
  // convert current position into ancient board format (board)
  
  const ClientBoard &bo = cgame->get_curr_pos();
  sint4 x, y;
  
  board = board0;  // empty board (10x10 bytes)
  
  FOR (y, 8) {
    FOR (x, 8) {
      
      sint4 ind = ClientMove::xy_to_ind(x, y);
      sint4 i = Tab8to10[x+y*8];
      
      switch (bo.get_square(ind)) {
      case ClientBoard::BLACK : board.p[i] = BLACK; break;
      case ClientBoard::WHITE : board.p[i] = WHITE; break;
      case ClientBoard::EMPTY : board.p[i] = EMPTY;  break;         
      case ClientBoard::BORDER: board.p[i] = BORDER;  break;         
      default: Error("illegal board entry"); break;
      }
    }
  }
  

  // convert last move 

  const vector<ClientMove> &vm = cgame->get_moves();
  
  if (vm.size() > 0) {
    
    ClientMove cm = vm.back();
    
    if (cm.is_pass()) last_move = MOVE_PASS;
    else {
      sint4 x, y;
      cm.to_xy(x, y);
      last_move = x+1 + 10* (y+1);
    }
    
  } else {
    
    last_move = MOVE_UNKNOWN;
    
  }
  
  // which color is to move?

  if (bo.get_to_move() == ClientColor::BLACK) to_move = BLACK; else to_move = WHITE;
  
  printf(">>> board_komi=%f board_rand=%d\n",
             cgame->komi,
	 cgame->type.get_rand_discs()
	 );
  
  // update book parameters

  handle_komi(cgame->komi,
	      cgame->type.get_rand_discs(),
	      p_book);
  
  // remaining thinking time

  int ind = cgame->name_index(pvc->login());
  remaining_seconds = cgame->pinf[ind].clock.time_left(false) / uSec;

  received_msg = true; n = SIG_BOARD;

  pvc->reset();
}

