// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// handle library / 12.93, 3.94, 6.96

#include "main.h"
#include "lib.h"
#include "crt.h"
#include "goodies.h"
#include "trans.h"
#include "killer.h"
#include "playm.h"
#include "pmove.h"
#include "game.h"
#include "newgame.h"


#define TEST		false
#define ALLOW_RAND_VAL  false   // doesn't work: errors add up

#define MAX_LIB_DISCS	64
#define MAX_ALT_DISCS	39
#define MIN_DEPTH	4
#define CHANCE_FAC	100
#define DIFF_FAC	100	// value1 = difference * DIFF_FAC + offset

#define DIFF_ALT	(100*DIFF_FAC)	// valid entries are less 
#define SURE_VAL	(51*CHANCE_FAC)	// (>50 <128)*CHANCE_FAC


// +PERC% in opening, +0% at game end (linear) => use earlier alternative 

#define PERC		0
#define VAL(val,discs)  \
 (val + (discs <= MAX_ALT_DISCS ? my_round(PERC*CHANCE_FAC*(MAX_ALT_DISCS-discs)/(float)MAX_ALT_DISCS) : 0))


#define MOVE_POOL_SIZE	50000


// result first

int compSGNMOVEDATA(const void *a, const void *b)
{
  int    sa, sb, d;


  if      (((MOVEDATA*) a)->Value > 0) sa =  1;
  else if (((MOVEDATA*) a)->Value < 0) sa = -1;
  else				       sa =  0;

  if      (((MOVEDATA*) b)->Value > 0) sb =  1;
  else if (((MOVEDATA*) b)->Value < 0) sb = -1;
  else				       sb =  0;


  d = sb - sa;

  if (d) return d;

  d = my_round(((float)((MOVEDATA*) b)->Value2)/CHANCE_FAC) - 
      my_round(((float)((MOVEDATA*) a)->Value2)/CHANCE_FAC); 

  if (d) return d;

  // if signs of modified difference & chances are equal use difference
 
  return ((MOVEDATA*) b)->Value - ((MOVEDATA*) a)->Value;
}



// chance first

int compSGNMOVEDATA2(const void *a, const void *b)
{
  int  d;

  d = my_round(((float)((MOVEDATA*) b)->Value2)/CHANCE_FAC) - 
      my_round(((float)((MOVEDATA*) a)->Value2)/CHANCE_FAC); 

  if (d) return d;

  return ((MOVEDATA*) b)->Value - ((MOVEDATA*) a)->Value;
}



/*  add small percentage if position is a "win" 
 *  subtract if it is a "loss",
 *  do nothing in case of a "draw" or deviation
 */

#define WIN_ADD 2

int G(int r, int v)
{
  int d;

  d = my_round(((float)v)/CHANCE_FAC);
  if (r > 0) d += WIN_ADD;
  else if (r < 0 && r != -DIFF_ALT) d -= WIN_ADD;

  return d;
}


int compSGNMOVEDATA3(const void *a, const void *b)
{
  return G(((MOVEDATA*)b)->Value, ((MOVEDATA*)b)->Value2) - 
         G(((MOVEDATA*)a)->Value, ((MOVEDATA*)a)->Value2);
}



/***********  library stuff   ****************/


// allocate new node

LIBNODE *NewLibNode(int SonNum)
{
  int     i;
  LIBNODE *pL;

  if (SonNum < 0) Error("SonNum < 0");

  pL = (LIBNODE*) malloc(sizeof(LIBNODE));

  if (!pL) Error("NewLIBNODE: no memory");

  pL->SonNum = SonNum;

  if (!SonNum) SonNum = 1;

  pL->Sons = (MOVENODE*) malloc(SonNum * sizeof(MOVENODE));

  if (!pL->Sons) Error("no mem");

  FOR (i, SonNum) pL->Sons[i].Flags = LIB_NEW;

  return pL;
}




void InitMoveNode(LIBRARY *pT, MOVENODE *pMN, NewGame &Game, int MoveIndex)
{
  pMN->Value   = Game.get_value();
  pMN->Flags   = LIB_NEW;

  if (Game.is_alternative())      pMN->Flags |= LIB_ALT;
  if (Game.is_finished()) pMN->Flags |= LIB_FIN;

  pMN->Next    = NULL;
  pMN->MoveNum = Game.get_move_num()-MoveIndex;


  if (pT->MoveNum + pMN->MoveNum >= MOVE_POOL_SIZE) {

    pT->Moves = (PackedMove*) malloc(MOVE_POOL_SIZE * sizeof(PackedMove));

    if (!pT->Moves) Error("InitMoveNode: no memory");

    pT->MoveNum = 0;

  }

  pMN->Moves = &pT->Moves[pT->MoveNum];

  pT->MoveNum += pMN->MoveNum;
   
  memcpy(pMN->Moves, &Game.get_pm(MoveIndex), pMN->MoveNum);
}




// allocate new library

LIBRARY *NewLibrary(void)
{
  LIBRARY *pT;

  pT = (LIBRARY*) malloc(sizeof(LIBRARY));

  if (!pT) Error("NewLIBRARY: no memory");

  pT->GameNum = 0;

  pT->MoveNum =  MOVE_POOL_SIZE;
  pT->Moves   = NULL;

  pT->pRoot = NewLibNode(0);

  pT->DepthBound   = 64;
  pT->GameNumBound = 1;

  pT->DrawBad = pT->DrawGood = pT->DrawBadBlack = pT->DrawGoodBlack = false;
  pT->PublicDrawBad = pT->PublicDrawGood = 
  pT->PublicDrawBadBlack = pT->PublicDrawGoodBlack = false;

  pT->Mode = LIBMODE_RESFIRST;

  pT->path_randomization = false;

  pT->PrivDiscMax = 100;

  ResetMax(pT);
  pT->max_delta = 0;

  return pT;
}



// append game, return false <=> game exists

bool AppendGame(LIBRARY *pT, LIBNODE *pL, NewGame &Game, int MoveIndex)
{
  int i, j;


  if (MoveIndex >= Game.get_move_num()) return false;


  FOR (i, pL->SonNum) {

    if (pL->Sons[i].Moves[0] == Game.get_pm(MoveIndex)) break;

  }


  if (i >= pL->SonNum) {			// new path

#if 1
    if (Game.is_alternative()) {
// printf(">>> %d\n", Game.get_move_num() - MoveIndex);

      if (Game.get_move_num() - MoveIndex != 1) {
	printf("no game!\n");
        return false;
      }
    }
#endif

    if (Game.is_public_draw()) {

      // public draws must be in book-skeleton!
      printf("n"); fflush(stdout);
      return false;
    }

    pL->Sons = (MOVENODE*) 
      realloc((char*)pL->Sons, (pL->SonNum+1) * sizeof(MOVENODE));

    if (!pL->Sons) Error("realloc?");

    InitMoveNode(pT, &pL->Sons[pL->SonNum], Game, MoveIndex);

    pL->SonNum++;


  } else {					// old path


    FOR (j, pL->Sons[i].MoveNum) 
      if (pL->Sons[i].Moves[j] != Game.get_pm(MoveIndex+j)) break;

    if (!Game.get_pm(MoveIndex+j).get_raw()) { 

      // path included 

      if (Game.is_public_draw()) {  
	if ((pL->Sons[i].Flags & LIB_ALT)) {   // obsolete
	  printf("*** alt -> public draw?\n");
	  return false;
	}

        // change flag: public draw

	if (Game.get_value() != 0) {
	  printf("*** public draw  diff != 0\n");
	  return false;
	}

	pL->Sons[i].Flags |= LIB_PUBLIC_DRAW;	
	return true;

      } else {

	// printf("*** path included\n");

	return false;
      }
    }


    if (j >= pL->Sons[i].MoveNum && !pL->Sons[i].Next) { 

      // library path is included in new game

      if (Game.is_public_draw()) {
	printf("i");
	return false;
      }

      // extend existing line (for alternatives which lie *on* paths)
      // note: alternatives are generated starting at root => extension is OK
      // printf("*** longer path\n");  

      InitMoveNode(pT, &pL->Sons[i], Game, MoveIndex);

      return true;
    }


    if (Game.is_alternative()) {

      if ((pL->Sons[i].Flags & LIB_ALT)) {

	printf("#"); fflush(stdout);

        return false;
      }

    }



    if (j >= pL->Sons[i].MoveNum) {		// next node

      if (AppendGame(pT, pL->Sons[i].Next, Game, MoveIndex+j)) {

        pL->Sons[i].Flags |= LIB_NEW;
	return true;
	
      } else return false;


    } else {					// split move sequence


      if (Game.is_public_draw()) {

	// public draws must be in book-skeleton!

	printf("p"); fflush(stdout);
	
	return false;
      }


      if (Game.is_alternative()) {

	// split only at the end of alternative-path (remove error in book)

        if (MoveIndex + j != Game.get_move_num()-1) {

	  printf("%%"); fflush(stdout);

          return false;
        }

      }


      LIBNODE *pN = NewLibNode(2);

      // old part starting from difference point

      pN->Sons[0].MoveNum = pL->Sons[i].MoveNum - j;  // old part
      pN->Sons[0].Moves   = pL->Sons[i].Moves+j;
      pN->Sons[0].Flags   = pL->Sons[i].Flags;	      // flag of old path!

      pN->Sons[0].Value   = pL->Sons[i].Value;
      pN->Sons[0].Next    = pL->Sons[i].Next;

      // new sequence starting from difference point

      InitMoveNode(pT, &pN->Sons[1], Game, MoveIndex+j); // new part

      // old sequence is now shorter and new

      pL->Sons[i].Next    = pN;
      pL->Sons[i].MoveNum = j;
      pL->Sons[i].Flags  |= LIB_NEW;

    }
  }

  return true;
}




// append game to game tree, return false <=> game exists

bool AppendGameToLibrary(LIBRARY *pT, NewGame &Game)
{
  if (Game.is_alternative() && Game.get_move_num()+4 > MAX_ALT_DISCS+1) {
    return false;  // discard obsolete alternatives beyond endgame horizon
  }

  if (AppendGame(pT, pT->pRoot, Game, 0)) {
    pT->GameNum++;
    return true;
  }

  return false;
}



LIBRARY *MakeLibrary(char *libfile, char *evalfile, char *drawsfile)
{
  FILE     *fp;
  LIBRARY *pT;
  NewGame  Game;
  int      GameNum=0;

  pT = NewLibrary();

  fp = fopen(libfile, "r");

  if (fp) {

    printf("[ reading library '%s' ...", libfile); fflush(stdout);

    FOREVER {

      if (!Game.f_read_packed(fp)) break;

      if (!AppendGameToLibrary(pT, Game)) { printf("."); fflush(stdout); }
      else GameNum++;

    }

    printf("OK - %d game(s) ]\n", GameNum);

    fclose(fp);

  } else printf("[ can't read library!\a ]\n");


  if (GameNum) {
  
    int value;
    bool is_pd;

    if (evalfile) AddEvalsToLibrary(evalfile, pT);

    printf("[ MiniMax ..."); fflush(stdout);

    GameNum = EvalLibNode(pT, pT->pRoot, &value, &is_pd);

    printf("OK - root value %d ]\n", value);


    if (drawsfile) {

      // mark public draws

      fp = fopen(drawsfile, "r");

      GameNum = 0;

      if (fp) {

	printf("[ reading draws '%s' ...", drawsfile); fflush(stdout);

	FOREVER {
	  
	  if (!Game.f_read_packed(fp)) break;

	  Game.is_public_draw(true);

	  if (!AppendGameToLibrary(pT, Game)) { printf("."); fflush(stdout); }
	  else GameNum++;

	}

	printf("OK - %d draws(s) ]\n", GameNum);

	fclose(fp);

      } else printf("[ can't read draws-file!\a ]\n");
    }
  }

  return pT;
}



void AddEvalsToLibrary(char *file, LIBRARY *pT)
{
  FILE     *fp;
  NewGame  Game;
  int      EvalNum=0;

  if (!pT) Error("no library");


  fp = fopen(file, "r");
  if (fp) {

    printf("[ reading eval-file '%s' ...", file); fflush(stdout);

    FOREVER {

      if (!Game.f_read_packed(fp)) break;

      if (!Game.is_finished()) {
	Game.is_alternative(true);

	if (!AppendGameToLibrary(pT, Game)) { printf("."); fflush(stdout); }
	else EvalNum++;

      } else {

	printf("finished game as alternative!");

      }
    }

    fclose(fp);

  } else printf("[ can't read eval-file! ]\a\n");

  printf("OK - %d evaluation(s) ]\n", EvalNum);
}



// mark all subsequent nodes as old

void MarkOld(LIBRARY *pT, LIBNODE *pL)
{
  int i;

  
  if (!pL) return;

  FOR (i, pL->SonNum) {

    pL->Sons[i].Flags &= ~LIB_NEW;

    MarkOld(pT, pL->Sons[i].Next);

  }
}




#if 0
int CollectMoves(LIBRARY *pT, LIBNODE *pL, bool Alt, MOVEDATA *MoveData)
{
  int i, MoveNum=0;


  FOR (i, pL->SonNum) {

    if (((pL->Sons[i].Flags & LIB_ALT) != 0) == (Alt != 0)) {

      MoveData[MoveNum].Move    = SMOVE_MOVE(pL->Sons[i].Moves[0]);
      MoveData[MoveNum].Value   = pL->Sons[i].Value;
      MoveData[MoveNum].GameNum = pL->Sons[i].GameNum;

      if (SMOVE_PLAYER(pL->Sons[i].Moves[0]) == WHITE)

	MoveData[MoveNum].Value = - MoveData[MoveNum].Value; 

      MoveNum++;

    }

  }


  return MoveNum;
}
#endif





// evaluate nodes recursively (minimax value, game number, and public draws)

int EvalLibNode(LIBRARY *pT, LIBNODE *pL, int *pValue, bool *is_pd_p)
{
  int     i, val, maxval=-100, player=0, gamenum=0;
  static  int discs=3;
  bool    is_pd, father_pd = true;


  discs++;  // !!! upon return, discs must be decremented !!!

  if (!pL) Error("pL == NULL");
  if (pL->SonNum == 0) Error("no son?");


  FOR (i, pL->SonNum) {

    if (!player) player = pL->Sons[i].Moves[0].get_player();
    else if (player != pL->Sons[i].Moves[0].get_player()) Error("player?");

    if (pL->Sons[i].Next) {

      pL->Sons[i].GameNum = EvalLibNode(pT, pL->Sons[i].Next, &val, &is_pd);
      pL->Sons[i].Value = val;

      gamenum += pL->Sons[i].GameNum;
      val     =  pL->Sons[i].Value;
 
    } else {		// leaf sequence

      if (!(pL->Sons[i].Flags & LIB_ALT)) {

        gamenum++;
        val = pL->Sons[i].Value;
        pL->Sons[i].GameNum = 1;
	is_pd = (pL->Sons[i].Flags & LIB_PUBLIC_DRAW) != 0;

      } else {

/*

// choose ValueRand in [Value - RandAddMax, Value + RandAddMax]

        pL->Sons[i].ValueRand = 
	  pL->Sons[i].Value +
          (MyRand() % (2*pT->RandAddMax+1)) - pT->RandAddMax;
*/

#if 0
printf("%d %d %d\n", pL->Sons[i].Value, pL->Sons[i].ValueRand, pT->RandAddMax);
#endif

        continue;
      }

    }


    // handle public draw recursion

    if (val == 0 && is_pd) pL->Sons[i].Flags |=  LIB_PUBLIC_DRAW;
    else                   pL->Sons[i].Flags &= ~LIB_PUBLIC_DRAW;

    if (val == 0 && !is_pd) {

      // there is a private drawing successor

// cout << "ptdiscmax=" << pT->PrivDiscMax << endl;

      if (discs <= pT->PrivDiscMax) 

	// only private if #discs is not too large
	// (this prevents getting into analyzed drawing lines)
	
	father_pd = false;

    }



    if (player == WHITE) val = -val;

    if (val > maxval) maxval = val;

  }

  if (!gamenum) { 
    printf("%d %d ", discs, pL->SonNum); 
    Error("no normal game!\a\n");
  }

  if (player == WHITE) maxval = -maxval;

  *pValue = maxval;


  // no public draw if max. value is != 0

  if (maxval != 0) father_pd = false;

  *is_pd_p = father_pd;

  discs--;

  // printf("%d: %d %d\n", discs, maxval, gamenum);

  return gamenum;
}





// search move alternatives recursively and save paths

void FindAlt(FILE *fp, LIBRARY *pT, LIBNODE *pL, NewGame &Game, int movenum)
{
  int i, j;


  if (!pL) Error("pL == NULL");

  if (movenum > MAX_ALT_DISCS-4) return;


  FOR (i, pL->SonNum) 

    if (!pL->Sons[i].Next && (pL->Sons[i].Flags & LIB_ALT)) break;


  if (i >= pL->SonNum) {	// no alternative in LIBNODE => save new

    Game.set_move_num(movenum);
    Game.f_write_packed(fp);
  }


  FOR (i, pL->SonNum) {

    if (!(pL->Sons[i].Flags & LIB_ALT)) {	// normal path

      FOR (j, pL->Sons[i].MoveNum) {

        Game.get_pm(movenum+j) = pL->Sons[i].Moves[j];
        Game.set_move_num(movenum+j+1);

        if (movenum+j+1+4 > MAX_ALT_DISCS) break;

        if (j < pL->Sons[i].MoveNum-1) Game.f_write_packed(fp);

      }

      if (j >= pL->Sons[i].MoveNum && pL->Sons[i].Next) 

	FindAlt(fp, pT, pL->Sons[i].Next, Game, movenum+pL->Sons[i].MoveNum);

    }
  }
}



/* search good move alternatives in lost positions recursively and save paths 
 * with values 
 *
 */

void FindGood(FILE *fp, LIBRARY *pT, LIBNODE *pL, NewGame &Game, int movenum)
{
  int i, j, player=0, altval=0, altmove=0, val=0, maxval=-100;
  bool alt=false;


  if (!pL) Error("pL == NULL");

  if (movenum > MAX_ALT_DISCS-4) return;


  FOR (i, pL->SonNum) {

    if (!player) player = pL->Sons[i].Moves[0].get_player();
    else if (player != pL->Sons[i].Moves[0].get_player()) Error("player?");


    if (!(pL->Sons[i].Flags & LIB_ALT)) {

      val =  pL->Sons[i].Value;
 
    } else {

      alt = true; 

      altmove = pL->Sons[i].Moves[0].get_move();
      altval  = pL->Sons[i].Value;

      if (player == WHITE) altval = -altval;

      continue; 
    }

    if (player == WHITE) val = -val;

    if (val > maxval) maxval = val;
  }

  if (val <= 0 && alt) {

    Game.get_pm(movenum).set(player, altmove);
    Game.set_value(altval);
    Game.set_move_num(movenum+1);
    Game.f_write_packed(fp);

  }

  FOR (i, pL->SonNum) {

    if (!(pL->Sons[i].Flags & LIB_ALT)) {	// normal path

      FOR (j, pL->Sons[i].MoveNum) {

        Game.get_pm(movenum+j) = pL->Sons[i].Moves[j];
        Game.set_move_num(movenum+j+1);
        if (movenum+j+1+4 > MAX_ALT_DISCS) break;

      }

      if (j >= pL->Sons[i].MoveNum && pL->Sons[i].Next) 

	FindGood(fp, pT, pL->Sons[i].Next, Game, movenum+pL->Sons[i].MoveNum);

    }
  }
}


#if 0
#define EO(x) x;
#else
#define EO(x) 
#endif


// evaluate chances recursively, NegaMax with different strategies


// root information 

MOVEDATA GlobalMoveData[65];
int      GlobalMoveNum;


class RandPathInfo {
public:
  bool determine_path_num;  // do so?
  int  path_num;            // returned path-number
  int  root_player;         // player to move at current root
  int  alpha, beta;         // window
};


#define OUT false


int EvalChance(
  LIBRARY *pT, LIBNODE *pL, int discs, int depth, RandPathInfo &rpi
)
{
  int  i, val, val2, player, movenum, offset, public_offset;
  MOVEDATA MoveData[65];
  RandPathInfo lrpi = rpi;  // copy info

  if (!pL) Error("pL=0");

  if ((movenum=pL->SonNum) < 1) Error("<1 sons?");

  player = pL->Sons[0].Moves[0].get_player();

  offset = pT->BlackOffset;
  if (player == WHITE) offset = -offset;

  public_offset = pT->PublicBlackOffset;
  if (player == WHITE) public_offset = -public_offset;


  EO(printf("depth=%d discs=%d %d\n", depth, discs, player))


  // for each move ...

  FOR (i, movenum) {

    MoveData[i].Move = pL->Sons[i].Moves[0].get_move();
    MoveData[i].public_draw = (pL->Sons[i].Flags & LIB_PUBLIC_DRAW) != 0;

    val = pL->Sons[i].Value;


    // minimax game result

    MoveData[i].Value = pL->Sons[i].Value * DIFF_FAC;

    if (pL->Sons[i].Moves[0].get_player() == WHITE) {
      MoveData[i].Value = - MoveData[i].Value; 
      val = -val;
    }

    if (pL->Sons[i].Next) {			// branch follows

      val2 = EvalChance(
	       pT, pL->Sons[i].Next, discs+pL->Sons[i].MoveNum, depth+1, lrpi
	     );

      if (pL->Sons[i].Next->Sons[0].Moves[0].get_player() != player) 
	val2 = -val2;

      MoveData[i].path_num = lrpi.path_num;
      MoveData[i].GameNum = pL->Sons[i].GameNum;

    } else { 

      if (!(pL->Sons[i].Flags & LIB_FIN)) {

	// not finished => ALT value

        val2 = val * CHANCE_FAC;
	val2 = VAL(val2, discs);

	MoveData[i].Value = -DIFF_ALT;
	MoveData[i].GameNum = 0;

      } else {

	// finished => game result

	MoveData[i].GameNum = 1;

	if      (MoveData[i].Value > 0) val2 = +SURE_VAL;
	else if (MoveData[i].Value < 0) val2 = -SURE_VAL;
	else 			        val2 =  0;
	
	if ((pL->Sons[i].Flags & LIB_PUBLIC_DRAW) &&
	    MoveData[i].Value == 0 && pT->PublicBlackOffset) {

//KoorAus(MoveData[i].Move);
//printf("\a (%d) public draw reached\n", depth);

	  printf("p"); 

	  // drawing moves are good or bad

	  if (public_offset > 0) val2 = +(SURE_VAL-1);//-1=>choose winning move if ex.
	  if (public_offset < 0) val2 = -(SURE_VAL-1);//+1=>choose drawing move if ex.
	    
	} else {

	  if (MoveData[i].Value == 0 && pT->BlackOffset) {

	    // drawing moves are good or bad

	    if (offset > 0) val2 = +(SURE_VAL-1);//-1=>choose winning move if ex.
	    if (offset < 0) val2 = -(SURE_VAL-1);//+1=>choose drawing move if ex.
	  }
	}
      }
      
      if (rpi.determine_path_num) {

	// determine path-number for leaves/alternatives

	int root_val = val2;

	if (player != rpi.root_player) root_val = -root_val;

	// root_val = chance from point of view of root player
	// root_val in [alpha,beta] => #=1, otherwise #=0
	
	if (rpi.alpha <= root_val && root_val <= rpi.beta)
	  MoveData[i].path_num = 1;
	else 
	  MoveData[i].path_num = 0;
      }
    }

    // deviation value

    MoveData[i].Value2 = val2;
  }


  if (offset || public_offset) {

// change draw values according to offset

    FOR (i, movenum) 
      if (MoveData[i].Value == 0) {

	if ((pL->Sons[i].Flags & LIB_PUBLIC_DRAW) && public_offset) {

//	  printf("::: %d ", depth); KoorAus(MoveData[i].Move); printf("\n");
 	  MoveData[i].Value = public_offset;

	} else if (offset)
	  MoveData[i].Value = offset;

      }
  }


  int num_path_sons = 0;  // number of sons which path_num counts

#if 0
  FOR (i, movenum) {
    MOVEDATA &md = MoveData[i];

    cout << "[" << flush;
    KoorAus(md.Move); fflush(stdout);
    cout << " e=" << md.Value << " h=" << md.Value2 << "] " 
         << " n=" << md.GameNum << flush;
  }
  cout << endl;
#endif

  if (pT->Mode == LIBMODE_DEVFIRST) {

    // second approach: deviation value first 
    // find move with best chance (result is tiebreaker)

    qsort(MoveData, (size_t) movenum, sizeof(MOVEDATA), compSGNMOVEDATA2);
    num_path_sons = movenum;

  } else if (pT->Mode == LIBMODE_RESFIRST) {

    // first approach: game result first 
    // find move with maximum value and chance

    qsort(MoveData, (size_t) movenum, sizeof(MOVEDATA), compSGNMOVEDATA);

    if (MoveData[0].Value <= 0) {

      // look also at move-alternative in positions <= draw

      FOR (i, movenum)
	if (MoveData[i].Value == -DIFF_ALT)
	  MoveData[i].Value = MoveData[0].Value;
    }

    // find move with maximum value and chance according offset & alternative

    qsort(MoveData, (size_t) movenum, sizeof(MOVEDATA), compSGNMOVEDATA);


    // determine number of sons whose path_num must be considered
    // = # of relevant sons

    FOR (i, movenum) 
      if (sgn(MoveData[0].Value) != sgn(MoveData[i].Value)) break;

    num_path_sons = i;

if (pT->path_randomization) Error("randomization doesn't work with resfirst");
// since deviation value is not monotone! (e.g.  0 60 -> +2 40)
// means some work for max-update ... 


  } else if (pT->Mode == LIBMODE_ADD) {

    
// third approach: deviation first but add value dependend on result

    Error("LIBMODE_ADD not up-to-date");

#define TEST2 0

#if TEST2
    MOVEDATA md;
    qsort(MoveData, (size_t) movenum, sizeof(MOVEDATA), compSGNMOVEDATA2);
    md = MoveData[0];
#endif

    qsort(MoveData, (size_t) movenum, sizeof(MOVEDATA), compSGNMOVEDATA3);
    num_path_sons = movenum;   

#if TEST2
    if (md.Move != MoveData[0].Move) {

      printf("DEVFIRST:\n");

      qsort(MoveData, (size_t) movenum, sizeof(MOVEDATA), compSGNMOVEDATA2);

      FOR (i, movenum) { 
	KoorAus(MoveData[i].Move); 
	printf(":%d/%d ", MoveData[i].Value, MoveData[i].Value2); 
      }

      printf("\nADD:\n");

      qsort(MoveData, (size_t) movenum, sizeof(MOVEDATA), compSGNMOVEDATA3);

      FOR (i, movenum) { 
	KoorAus(MoveData[i].Move); 
	printf(":%d/%d ", MoveData[i].Value, MoveData[i].Value2); 
      }
      printf("\n");

    }
#endif

  } else Error("unknown libmode");


  if (depth == 0) {

    // copy information of all root-successors for further processing
 
    FOR (i, movenum) GlobalMoveData[i] = MoveData[i];
    GlobalMoveNum = movenum;

  }

EO(printf("depth=%d: ", depth))
EO(FOR(i, movenum) { KoorAus(MoveData[i].Move); printf(":%d/%d/%d ", MoveData[i].Value, MoveData[i].Value2, MoveData[i].path_num); })

EO(printf("\n(%d) max-return %d\n", depth, MoveData[0].Value2))


  
  if (rpi.determine_path_num) {

    int p_num = 0;

#if OUT

   cout << "depth=" << depth << " " << movenum << " sons " << 
       " same players:" <<  (player == rpi.root_player) << endl;

FOR (i, num_path_sons) {
  cout << "[" << MoveData[i].Value2 << " " << MoveData[i].path_num << "] ";
}
cout << endl;
#endif


    if (player == rpi.root_player) {

      // root player to move => 
      //   # = sum of all path_nums for sons with values in [alpha,beta] 

      FOR (i, num_path_sons) {

	if (rpi.alpha <= MoveData[i].Value2 && MoveData[i].Value2 <= rpi.beta)
	  p_num += MoveData[i].path_num;
      }

    } else {

      // root player not to move => 
      // # = min of path_nums for sons with maximum deviation 
      //     value (in [alpha,beta]) 
      
      p_num = MoveData[0].path_num;

      for (i=1; i < num_path_sons; i++) {

	if (MoveData[0].Value2 != MoveData[i].Value2) break;

	p_num = min(p_num, MoveData[i].path_num);
      }
    }

#if OUT
cout << "-->" << p_num << endl;
#endif

    rpi.path_num = p_num;

  }

  return MoveData[0].Value2;
}



// ********** search stuff *********** 



#define LMSG(s) printf(">>> lib: %s\n", s);


// return true <=> found

bool SearchPosition(LIBRARY *pT, NewGame &Game, POSINFO *pPosI)
{
  int     i, tr, MoveIndex, PathIndex, j;
  SPFELD  sf, sf1, sf2, sfelder[8];
  NewGame gameu;
  LIBNODE *pN;


  if (!pT->GameNum) return false;

  gameu = Game;
  
  gameu.unique();


// compute transformation

  Game.play(Game.get_move_num(), sf1);
  gameu.play(gameu.get_move_num(), sf2);

  Transform(&sf2, sfelder);

  FOR (tr, 8) {

    FOR (i, 100) if (sf1.p[i] != sfelder[tr].p[i]) break;
    if (i >= 100) break;
  }

  if (tr >= 8) Error("SearchLibNode: no transformation?");

//sprintf(s, "Trans: %d", tr); LMSG(s);

  pPosI->Trans = tr;

  SfGrund(&sf);

  pN        = pT->pRoot;
  MoveIndex = -1;
  PathIndex = -1;

  for (i=0; gameu.get_pm(i).get_raw(); i++) {

    if (MoveIndex < 0) {			// search move

      FOR (j, pN->SonNum)

        if (pN->Sons[j].Moves[0] == gameu.get_pm(i)) break;


      if (j >= pN->SonNum) return false;	// not found

      MoveIndex = j;
      PathIndex = 0;
    }


    // compare with path move

    if (gameu.get_pm(i) != pN->Sons[MoveIndex].Moves[PathIndex]) return false;

    // next tree position

    PathIndex++;

    if (PathIndex >= pN->Sons[MoveIndex].MoveNum) {

      pN = pN->Sons[MoveIndex].Next;
      MoveIndex = -1;

      if (!pN) return false;	// no more moves, e.g. alternative

      if (!pN) Error("pN=0");
    }
  }

  pPosI->pNode     = pN;
  pPosI->MoveIndex = MoveIndex;
  pPosI->PathIndex = PathIndex;

  return true;
}



int NewGameTreeMove(
  LIBRARY  *pT, 
  NewGame  &Game,
  ZUGIO	   *pzio,
  int      *pValue,
  ValType  *pType
)
{
  char   s[400], s1[400];
  int    i, MoveNum, Player, discs, bestmove;
  int    val;
  SPFELD Board;
  SFPOS   Moves[65];
  POSINFO PI;

  Game.set_value(0);
  Game.s_write(s1); 
  sprintf(s, "looking for %s", s1);
  LMSG(s);


  if (!pT) {
    LMSG("empty library");
    return ZUG_UNBEKANNT;
  }

  if (pT->path_randomization) {
    pT->old_max_val = pT->curr_max_val;	
    printf("old curr_max=%d\n", pT->old_max_val);
  }

  if (Game.get_move_num() > pT->DepthBound) {
    LMSG("depth > DepthBound");
    return ZUG_UNBEKANNT;
  }

  if (!(Player=Game.play(Game.get_move_num(), Board))) {
    LMSG("GameEnd");
    return ZUG_UNBEKANNT;
  }

  if (Player != pzio->cio.Partei) {
    LMSG("No Move");
    return ZUG_UNBEKANNT;
  }

  if (SfAnz(&Board) > MAX_LIB_DISCS) {
    LMSG("too many discs");
    return ZUG_UNBEKANNT;
  }

  MoveNum = SfMoeglZuegeE(&Board, Player, Moves);

  if (!MoveNum) Error("No Move - Can not be!\a");

  if (MoveNum == 1) { 

    LMSG("One Move");

    *pValue = 0;
    *pType  = ENDGAME;
    return Moves[0]; 
  }

  if      (pT->DrawBad)       pT->BlackOffset =  -Player;
  else if (pT->DrawGood)      pT->BlackOffset =   Player;
  else if (pT->DrawBadBlack)  pT->BlackOffset =  -1;
  else if (pT->DrawGoodBlack) pT->BlackOffset =   1;
  else			      pT->BlackOffset =   0;

  if      (pT->PublicDrawBad)       pT->PublicBlackOffset =  -Player;
  else if (pT->PublicDrawGood)      pT->PublicBlackOffset =   Player;
  else if (pT->PublicDrawBadBlack)  pT->PublicBlackOffset =  -1;
  else if (pT->PublicDrawGoodBlack) pT->PublicBlackOffset =   1;
  else			            pT->PublicBlackOffset =   0;

  // search game in GameTree and collect game moves and values in MoveData

  if (SearchPosition(pT, Game, &PI)) {

    if (PI.MoveIndex < 0) {		// branch

      RandPathInfo rpi;


      discs = 4 + Game.get_move_num();

      rpi.determine_path_num = false;
      
      val = EvalChance(pT, PI.pNode, discs, 0, rpi);

      if (GlobalMoveNum < 1) Error("no moves?");


      if (pT->path_randomization) {

	// determine new maximum

        if (val > pT->curr_max_val) {

	  pT->curr_max_val = val;
	  printf(">>> new curr_max=%d\n", val);
        }

	rpi.determine_path_num = true;
	rpi.root_player = Player;

#if 1
	rpi.alpha = pT->curr_max_val - pT->max_delta * CHANCE_FAC;
	rpi.beta  = val+1;
#else

// test

	rpi.alpha = -10000000;
	rpi.beta  = +10000000;
#endif

	printf("[ pathrand: val=%d player=%d al=%d be=%d ]\n",
	  val, rpi.root_player, rpi.alpha, rpi.beta
	);

        EvalChance(pT, PI.pNode, discs, 0, rpi);

	if (pT->Mode == LIBMODE_RESFIRST) {

	  // clear path_nums of irrelevant moves

	  FOR (i, GlobalMoveNum) {
	    if (sgn(GlobalMoveData[i].Value) != sgn(GlobalMoveData[0].Value)) 
	      GlobalMoveData[i].path_num = 0;
	  }
	}
      }

      // determine transpositions

      FOR (i, GlobalMoveNum) 
	GlobalMoveData[i].Move = Trans[PI.Trans](GlobalMoveData[i].Move);  

      // print move-data

      printf("\n");

      FOR (i, GlobalMoveNum) {

	printf("["); KoorAus(GlobalMoveData[i].Move);

	printf(": vt=%.1f ch=%d gn=%d", 
			(REAL)GlobalMoveData[i].Value/DIFF_FAC,
			my_round((REAL)GlobalMoveData[i].Value2/CHANCE_FAC)+50,
			GlobalMoveData[i].GameNum);

	if (GlobalMoveData[i].public_draw)
	  printf(" pud");

	if (pT->path_randomization) {

	  printf(" pa=%d", GlobalMoveData[i].path_num);

	}

	printf("] ");
      }

      printf("\n");


      if (pT->path_randomization) {

	// choose move randomly according to path_num

	int num_paths = 0;

	FOR (i, GlobalMoveNum) num_paths += GlobalMoveData[i].path_num;

	if (num_paths <= 0) Error("num_paths <= 0");

	int num_rand = (IRAN % num_paths) + 1;

        int sum = 0;

        FOR (i, GlobalMoveNum) {
	  sum += GlobalMoveData[i].path_num;
	  if (sum >= num_rand) break;
	}

	if (i >= GlobalMoveNum) Error("no move found");

	// swap contents of 0 and i

	{ 
	  MOVEDATA t = GlobalMoveData[0];
	  GlobalMoveData[0] = GlobalMoveData[i];
	  GlobalMoveData[i] = t;
	}

        printf("randomly chosen move: ");
        KoorAus(GlobalMoveData[0].Move);
	printf("\n");

      }

      // compute output-values

      { int chance, diff;
        double val;

        chance = my_round(GlobalMoveData[0].Value2 / CHANCE_FAC) + 50;
        if (chance == 0)   chance = 1;
        if (chance == 100) chance = 99;

        diff = my_round(GlobalMoveData[0].Value/DIFF_FAC);

        if (diff < -64) diff = 0;  // alternative chosen

        if (diff < 0) val = diff - chance * 0.01; 
	else          val = diff + chance * 0.01;

	printf("val= %+.2f\n", val); 

        *pValue = my_round(val*MIX_CONST);
        *pType  = MIX;

      }


      if (GlobalMoveData[0].Value >= 0) {
 
	LMSG("good position");

      } else {

	LMSG("bad position");

      }

      return GlobalMoveData[0].Move;


    } else {				// path


      if (PI.pNode->Sons[PI.MoveIndex].Flags & LIB_ALT) 
	Error("**** ALT???\a\a\a");


      LMSG("path");


      bestmove = Trans[PI.Trans](
        PI.pNode->Sons[PI.MoveIndex].Moves[PI.PathIndex].get_move());

      val = PI.pNode->Sons[PI.MoveIndex].Value;

      if (PI.pNode->Sons[PI.MoveIndex].Moves[PI.PathIndex].get_player() == WHITE)

	val = -val;

      if (val >= 0) {

	LMSG("good position => play move");

        *pValue = val;
        *pType  = ENDGAME;

	return bestmove;


      } else {

	LMSG("bad position => search");

	return ZUG_UNBEKANNT;
      }
    }

  } else {

    LMSG("game not found");

    return ZUG_UNBEKANNT;
  }

  Error("NewGameTreeMove: what?");

  return ZUG_UNBEKANNT;
}


void ResetMax(LIBRARY *pT)
{
  if (pT) {
    pT->curr_max_val = pT->old_max_val = pT->min_val = -(SURE_VAL+1);   
  }
}


void RestoreMax(LIBRARY *pT)
{
  if (pT) {
    pT->curr_max_val = pT->old_max_val;
  }
}

void SetOldMax(LIBRARY *pT)
{
  if (pT) {
    pT->old_max_val = pT->curr_max_val;
  }
}

