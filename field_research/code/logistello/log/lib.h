// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef LIB_H
#define LIB_H

#include "main.h"
#include "sboard.h"
#include "move.h"
#include "newgame.h"


#define  LIB_NEW	 1
#define  LIB_ALT	 2
#define  LIB_PUBLIC_DRAW 4
#define  LIB_FIN         8


typedef struct _MOVENODE {

  int		  GameNum;
  sint1		  Value;
  uint1		  Flags;

  uint1  	  MoveNum;
  PackedMove      *Moves;
  struct _LIBNODE *Next;

} MOVENODE;



typedef struct _LIBNODE {

  uint1            SonNum;
  struct _MOVENODE *Sons;

} LIBNODE;



typedef struct {

  LIBNODE *pNode;
  int     MoveIndex;
  int     PathIndex;
  int     Trans;

} POSINFO;



typedef enum { LIBMODE_RESFIRST, LIBMODE_DEVFIRST, LIBMODE_ADD } LIBMODE; 


typedef struct {

  LIBNODE *pRoot;
  NewGame *Games;
  int    GameNum;

  PackedMove *Moves;                     // move pool
  int    MoveNum;

  int    DepthBound;
  int    GameNumBound;

  LIBMODE Mode;

  bool	 DrawBad, DrawGood, DrawBadBlack, DrawGoodBlack;
  bool   PublicDrawBad, PublicDrawGood, 
         PublicDrawBadBlack, PublicDrawGoodBlack;

  int    BlackOffset;	             // cf. NewGameTreeMove
  int    PublicBlackOffset;

  int    PrivDiscMax;                // if #discs is less or equal =>
                                     // if there is a private drawing
                                     // successor and the value is 0 then 
                                     // the current node is a private draw

  // path-randomization variables

  bool   path_randomization;         // active?
  int    curr_max_val;               // maximum reachable deviation value
  int    old_max_val;                // old maximum reachable deviation value
  int    min_val;                    // for initialization
  int    max_delta;                  // maximum deviation from best value

} LIBRARY;





#define NOT_PLAYED	(-(INT_MAX-1))


typedef struct {

  int    Move, GameNum, Value, Value2;

  int    path_num; // for path-randomization
  bool   public_draw;

} MOVEDATA;



typedef struct { int TreeVal, Chance; } VALPAIR;


extern bool	AppendGameToLibrary(LIBRARY *pT, NewGame &Game);
extern int	EvalLibNode(LIBRARY *pT, LIBNODE *pV, int *pVal, bool *is_pd_p);
extern LIBRARY *NewLibrary(void);
extern LIBRARY *MakeLibrary(char *libfile, char *evalfile, char *drawsfile);
extern void     AddEvalsToLibrary(char *file, LIBRARY *pT);
extern void     MarkOld(LIBRARY *pT, LIBNODE *pL);
extern void     RandomizeLibrary(LIBRARY *pT);

extern int NewGameTreeMove(
  LIBRARY *pT, 
  NewGame  &Game,
  ZUGIO	   *pzio,
  int      *pValue,
  ValType  *pType
);

extern void ResetMax(LIBRARY *pT);
extern void RestoreMax(LIBRARY *pT);
extern void SetOldMax(LIBRARY *pT);

extern bool SearchPosition(LIBRARY *pT, NewGame &Game, POSINFO *pPosI);
extern void FindAlt(FILE *fp, LIBRARY *pT, LIBNODE *pL, NewGame &Game, int movenum);
extern void FindGood(FILE *fp, LIBRARY *pT, LIBNODE *pL, NewGame &Game, int movenum);

#endif
