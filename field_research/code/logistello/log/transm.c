// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* transformation of game representations, 4,7.93 */

#include "main.h"
#include "crt.h" 
#include "sboard.h"
#include "game.h"
#include "goodies.h"
#include "trans.h"
#include "newgame.h"
#include <sys/stat.h>

void _abort(void) { exit(1); }


int main(int argc, char **argv)
{
  char	    name[200];
  SPFELD    sf;
  FILE	    *fpin, *fpout;
  NewGame   ngame;
  int       i, firstnum;


  if (argc != 3 && argc !=4) {

  Fehler:
    Error("call: otrans option file\n\
            options are:  okoosp ospoko\n\
                          okogam newokogam gamoko\n\
                          ospsfk\n\
                          iososp\n\
                          srvgam\n\
                          ospuniq gamuniq\n\
			  osphalb sfkmittel\n\
                          sfknum num\n\
                          sfkasc sfkperm\n\
                          sfkrnd prob\n\
                          sfksplit prob\n\
                          sfkexp win-perc\n\
                          sfklabeltest\n\
                          sfksetlabel n\n\
                          testoko newoko firstnum\n");
  }


  if (sscanf(argv[1], "%d", &firstnum) == 1) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.osp", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    i = 0;

    FOREVER {

      if (fTabEin(fpin, &sf)) break;

      i++;

      if (i >= firstnum) fTabAus(fpout, &sf);
 
    }  

    fclose(fpin);
    fclose(fpout);


  } else if (!strcmp(argv[1], "okogam")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.gam", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      if (!ngame.f_read_packed(fpin)) break;
      if (!ngame.f_write(fpout)) Error("write error");
 
    }  

    fclose(fpin);
    fclose(fpout);

  } else if (!strcmp(argv[1], "gamoko")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.oko", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      if (!ngame.f_read(fpin)) break;
      if (!ngame.f_write_packed(fpout)) Error("write error");
    }

    fclose(fpin);
    fclose(fpout);

  } else if (!strcmp(argv[1], "okoosp")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.osp", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    FOREVER {

      if (!ngame.f_read_packed(fpin)) break;
      ngame.to_tab(sf);
      fTabAus(fpout, &sf);
    }  

    fclose(fpin);
    fclose(fpout);

#if 1

  } else if (!strcmp(argv[1], "iosgames")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.gam", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    FOREVER {

      int  i, m, player, diffBW, time;
      NewGame ngame;
      char s[400];
      char playerB[400], playerW[400], moves[400], *p;

      if (!fgets(s, 399, fpin)) break;

      if (sscanf(s, "%s %s %d %d %s", playerB, playerW, &diffBW, &time, moves) == 5) {

        p = moves;

	for (i=0;; i++) {

	  if      (*p == '+') player = BLACK; 
	  else if (*p == '-') player = WHITE;
	  else break;

	  p++;

	  m = (p[0] - '0')*10 + (p[1] - '0');

	  p += 2;
 
	  ngame.get_pm(i).set(player, m);

	}

	ngame.set_move_num(i);

	if (!ngame.play(i, sf)) {

	  ngame.is_finished(true);
	  ngame.set_value(SfAnzBLACK(&sf) - SfAnzWHITE(&sf));
	  
	  if (ngame.get_value() != diffBW) {

	    puts(s);
	    printf("%d %d\n", ngame.get_value(), diffBW);

	    SfAus(&sf, 0, 0);
	  }

	  ngame.f_write(fpout);
  
        } else printf("game not finished");

      }
    }  

    fclose(fpin);
    fclose(fpout);

 } else if (!strcmp(argv[1], "iosresign")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.games", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    FOREVER {

      int  i, m, player;
      GAME game;
      char s[400];
      char time[400], playerB[400], playerW[400], numB[400], numW[400],
	   min[400], flag[400], moves[400], end[400], *p;

      if (!fgets(s, 399, fpin)) break;

      if (sscanf(s, "%s %s %s %s %s %s %s %s %s", 
	time, playerB, playerW, numB, numW, flag, min, moves, end) == 9 &&
	!strcmp(flag, "r")) {


        p = moves;

	for (i=0;; i++) {

	  if      (*p == '+') player = BLACK; 
	  else if (*p == '-') player = WHITE;
	  else break;

	  p++;

	  m = (p[0] - '0')*10 + (p[1] - '0');

	  p += 2;
 
	  game.Moves[i] = SMOVE_GEN(m, player);

	}

	game.MoveNum = i;
	game.DiscDiffBW = 0;

	fprintf(fpout, "%s %s %s %s ", time, playerB, playerW, min);
	fWriteGame(fpout, &game);

      }
    }  

    fclose(fpin);
    fclose(fpout);

#endif

  } else if (!strcmp(argv[1], "iosgames")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.gam", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    FOREVER {

      int  i, m, player, diffBW, time;
      NewGame ngame;
      char s[400];
      char playerB[400], playerW[400], moves[400], *p;

      if (!fgets(s, 399, fpin)) break;

      if (sscanf(s, "%s %s %d %d %s", playerB, playerW, &diffBW, &time, moves) == 5) {

        p = moves;

	for (i=0;; i++) {

	  if      (*p == '+') player = BLACK; 
	  else if (*p == '-') player = WHITE;
	  else break;

	  p++;

	  m = (p[0] - '0')*10 + (p[1] - '0');

	  p += 2;
 
	  ngame.get_pm(i).set(player, m);

	}

	ngame.set_move_num(i);

	if (!ngame.play(i, sf)) {

	  ngame.is_finished(true);
	  ngame.set_value(SfAnzBLACK(&sf) - SfAnzWHITE(&sf));
	  
	  if (ngame.get_value() != diffBW) {

	    puts(s);
	    printf("%d %d\n", ngame.get_value(), diffBW);

	    SfAus(&sf, 0, 0);
	  }

	  ngame.f_write(fpout);
  
        } else printf("game not finished");

      }
    }  

    fclose(fpin);
    fclose(fpout);

  } else if (!strcmp(argv[1], "iososp")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.osp", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    FOREVER {

      int  i;
      char s[400];

      if (!fgets(s, 399, fpin)) break;
      if (strlen(s) != 202) { printf("len != 202 -> skip"); continue; }

      char *t = &s[73];
      SPFELD sf;

      SfGrund(&sf);
      int discs = 0;
      
      for (i=0; i < 128; i+=2) {
	int j = Tab8to10[i/2];

	if      (t[i] == '#') { sf.p[j] = BLACK; discs++; }
	else if (t[i] == ' ') { sf.p[j] = LEER;           }
	else if (t[i] == '(') { sf.p[j] = WHITE; discs++; }
	else {
	  int num = (t[i]-'0')*10 + (t[i+1]-'0');

	  sf.p[j] = num + NUM_DISP;
	}

      }

      if (discs & 1) sf.Marke = WHITE; else sf.Marke = BLACK;

      fTabAus(fpout, &sf);
    }  

    fclose(fpin);
    fclose(fpout);


  } else if (!strcmp(argv[1], "iosresign")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.games", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    FOREVER {

      int  i, m, player;
      GAME game;
      char s[400];
      char time[400], playerB[400], playerW[400], numB[400], numW[400],
	   min[400], flag[400], moves[400], end[400], *p;

      if (!fgets(s, 399, fpin)) break;

      if (sscanf(s, "%s %s %s %s %s %s %s %s %s", 
	time, playerB, playerW, numB, numW, flag, min, moves, end) == 9 &&
	!strcmp(flag, "r")) {


        p = moves;

	for (i=0;; i++) {

	  if      (*p == '+') player = BLACK; 
	  else if (*p == '-') player = WHITE;
	  else break;

	  p++;

	  m = (p[0] - '0')*10 + (p[1] - '0');

	  p += 2;
 
	  game.Moves[i] = SMOVE_GEN(m, player);

	}

	game.MoveNum = i;
	game.DiscDiffBW = 0;

	fprintf(fpout, "%s %s %s %s ", time, playerB, playerW, min);
	fWriteGame(fpout, &game);

      }
    }  

    fclose(fpin);
    fclose(fpout);

 } else if (!strcmp(argv[1], "sfklabeltest")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.ok", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    int num = 0, corrupt = 0;

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;

      num++;

      if (sf.Marke >= MA_DIFF && sf.Marke <= MA_DIFF+128) {

	if (fSfWrite(fpout, &sf, 1)) Error("write error");

      }	else {

	corrupt++;

        printf("label not a result\n");
      }
    }

    fclose(fpin);
    fclose(fpout);

    printf("%d board(s) read, %d corrupt\n", num, corrupt);

  } else if (!strcmp(argv[1], "sfksetlabel")) {

    if (argc != 4) goto Fehler;

    int label = atoi(argv[2]);

    if (label < -64 || label > 64) goto Fehler;

    fpin = fopen(argv[3], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.lab", argv[3]);
    printf("%s -> %s\n\n", argv[3], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;
      sf.Marke = MA_DIFF + 64 + label;
      if (fSfWrite(fpout, &sf, 1)) Error("write error");
    }

    fclose(fpin);
    fclose(fpout);

  } else if (!strcmp(argv[1], "sfkmittel")) {

    int anz=0;
    float sudiff=0;

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.mit", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;

      if (sf.Marke >= MA_DIFF && sf.Marke <= MA_DIFF+128) 

        sudiff += sf.Marke - MA_DIFF - 64;

      else

        Error("value not a difference!");

      anz++;

    }  

    fclose(fpin);

    if (!anz) Error("no board!");

    printf("E=%.2f\n", sudiff/anz);

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;

/*printf("%d\n", sf.Marke-MA_DIFF-64);*/

      if      (sf.Marke-MA_DIFF-64 > sudiff/anz) sf.Marke = MA_WKEIT + 99;
      else if (sf.Marke-MA_DIFF-64 < sudiff/anz) sf.Marke = MA_WKEIT + 1;
      else 			                 sf.Marke = MA_WKEIT + 50;

      fSfWrite(fpout, &sf, 1);
    }  

    fclose(fpout);


  } else if (!strcmp(argv[1], "sfknum")) {

    int anz=0, anzw=0;

    if (argc != 4) goto Fehler;

    fpin = fopen(argv[3], "r");
    if (!fpin) Error("file not found");

    int num = atoi(argv[2]);

    if (num <= 4 || num > 64) goto Fehler;

    sprintf(name, "%s.%d", argv[3], num);
    printf("%s -> %s\n\n", argv[3], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;

      anz++;

      if (SfAnz(&sf) == num) {

        fSfWrite(fpout, &sf, 1);
	anzw++;
      }
    }  

    fclose(fpin);
    fclose(fpout);

    printf("%d of %d boards written\n", anzw, anz);

  } else if (!strcmp(argv[1], "sfkrnd")) {

    int anz=0, anzw=0;

    if (argc != 4) goto Fehler;

    fpin = fopen(argv[3], "r");
    if (!fpin) Error("file not found");

    float prob = atof(argv[2]);

    if (prob <= 0 || prob >= 1) goto Fehler;

    sprintf(name, "%s.%.2f", argv[3], prob);
    printf("%s -> %s\n\n", argv[3], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;

      anz++;

      if (FRAN <= prob) {
        fSfWrite(fpout, &sf, 1);
	anzw++;
      }
    }  

    fclose(fpin);
    fclose(fpout);

    printf("%d of %d boards written\n", anzw, anz);

  } else if (!strcmp(argv[1], "sfksplit")) {

    int anz=0;
    FILE *fpout1, *fpout2;
    char name1[200], name2[200];
  
    if (argc != 4) goto Fehler;

    fpin = fopen(argv[3], "r");
    if (!fpin) Error("file not found");

    float prob = atof(argv[2]);

    if (prob <= 0 || prob >= 1) goto Fehler;

    sprintf(name1, "%s.%.2f", argv[3], prob);
    sprintf(name2, "%s.%.2fc", argv[3], prob);
    printf("%s -> %s %s\n\n", argv[3], name1, name2);

    fpout1 = fopen(name1, "w");
    if (!fpout1) Error("write error");

    fpout2 = fopen(name2, "w");
    if (!fpout2) Error("write error");

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;

      anz++;

      if (FRAN <= prob) {
        fSfWrite(fpout1, &sf, 1);
      } else {
        fSfWrite(fpout2, &sf, 1);
      }
    }  

    fclose(fpin);
    fclose(fpout1);
    fclose(fpout2);
    printf("%d boards written\n", anz);

  } else if (!strcmp(argv[1], "sfkperm")) {

    if (argc != 3) goto Fehler;

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.perm", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    // read boards

    struct stat buf;

    stat(argv[2], &buf);

    int len = buf.st_size / sizeof(SFCODE);
    SFCODE *boards = new SFCODE[len+1];

    int n;

    for (n=0;; n++) {
      if (fread((char*)&boards[n], sizeof(SFCODE), (size_t)1, fpin) < 1) break; 
      if (n >= len) Error("too many boards");
    }

#if 0

    printf("n=%d len=%d\n", n, len);

    int sum=0;
    FOR (i, n) {
      int j;
      FOR (j, sizeof(SFCODE)) {
	sum += *((char*)&boards[i]+j);
      }
    }

    printf("sum=%d\n", sum);

#endif

    // permute boards
    
    time_t ti;
    time(&ti);
    srandom(ti);

    FOR (i, n) {

      int r = random() % n;

      SFCODE t = boards[i];
      boards[i] = boards[r];
      boards[r] = t;
    }

    // write boards

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    if (fwrite((char*)boards, sizeof(SFCODE), (size_t)n, fpout) < n) 
      Error("write error");

    fclose(fpin);
    fclose(fpout);

    printf("OK\n");

  } else if (!strcmp(argv[1], "sfkexp")) {

    int anz=0, anzw=0;

    if (argc != 4) goto Fehler;

    fpin = fopen(argv[3], "r");
    if (!fpin) Error("file not found");

    int win_perc = atoi(argv[2]);

    if (win_perc <= 0 || win_perc >= 100) goto Fehler;

    sprintf(name, "%s.%d", argv[3], win_perc);
    printf("%s -> %s\n\n", argv[3], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;

      anz++;

      SFPOS m;
      int valb;

      // transform wld,val -> prob

      if (MoveWldDecode(sf.Marke, m, valb)) { // not wld-move

	if (MoveValDecode(sf.Marke, m, valb)) // not val-move

	  Error("label in rnd-file corrupt"); 

	if      (valb <= -(win_perc-50)) valb = -1;
	else if (valb >=   win_perc-50)  valb = +1;
	else                             valb =  0;

	//SfAus(&sf, 0, 0);
	//printf("%d\n", valb);

      }

      if      (valb == 0) sf.Marke = MA_WKEIT+50;
      else if (valb >  0) sf.Marke = MA_WKEIT+99;
      else           	  sf.Marke = MA_WKEIT+1; 

      // write first board

      fSfWrite(fpout, &sf, 1);
      anzw++;


      // make move

      if (m > 0) {

	if (!SfSetzen(&sf, BLACK, m)) Error("illegal move");

	// move possible 

      } else {

	SFPOS moves[65];

	if (SfMoeglZuege(&sf, BLACK, moves) != 0) Error("move exists!");

      }


      // save second board

      SfInvert(&sf); // black to move
      valb = -valb;

      if      (valb == 0) sf.Marke = MA_WKEIT+50;
      else if (valb >  0) sf.Marke = MA_WKEIT+99;
      else           	  sf.Marke = MA_WKEIT+1; 

      fSfWrite(fpout, &sf, 1);
      anzw++;
      
    }  

    fclose(fpin);
    fclose(fpout);

    printf("%d of %d boards written\n", anzw, anz);

  } else if (!strcmp(argv[1], "sfkasc")) {

    SPFELD sfarray[8];
    int j;

    if (argc != 3) goto Fehler;

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    puts("-- bla --");
    printf("W D L\n");
    FOR_SFPOS10(i) { KoorAus(i); putchar(' '); }
    puts("");

    FOREVER {

      if (!fSfRead(fpin, &sf, 1)) break;

      Transform(&sf, sfarray);

      FOR (j, 8) {

        FOR_SFPOS10(i) {
	
	  if      (sfarray[j].p[i] > 0) printf(" x ");
	  else if (sfarray[j].p[i] < 0) printf(" o ");
	  else                          printf(" - ");

	}

	if (sf.Marke >= MA_WKEIT && sf.Marke <= MA_WKEIT + 100) {

	  if (sf.Marke < MA_WKEIT + 50) printf("L");
	  if (sf.Marke > MA_WKEIT + 50) printf("W");
	  if (sf.Marke == MA_WKEIT + 50) printf("D");

	} else printf("???");

	puts("");
      }

    }  

    fclose(fpin);

  } else if (!strcmp(argv[1], "ospoko")) {

    int n=1;

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.oko", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      if (fTabEin(fpin, &sf)) break;
      if (ngame.from_tab(sf)) ngame.f_write_packed(fpout);

#if 0
      { SPFELD sf;

	if ((n % 100) == 0) { printf("%6d\r", n); fflush(stdout); }

        if (PlayGame(Game.MoveNum, &Game, &sf)) 
          printf("*** game %d corrupt\n", n);

        if (SfMoeglZuege(&sf, BLACK, moves) || SfMoeglZuege(&sf, WHITE, moves))
	  printf("*** game %d not finished\n", n);

      }

#endif

      n++;

    }

    fclose(fpin);
    fclose(fpout);


  } else if (!strcmp(argv[1], "ospsfk")) {

    int nr;
    SPIELINFO Info[65];


    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.sfk", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      if (fTabEin(fpin, &sf)) break;
      if ((nr=TabToInfo(&sf, Info)) >= 1) {

        if (Info[nr].AmZug == WHITE) SfInvert(&Info[nr].Sf);

	for (i=1; i <= nr; i++) {
	  Info[i].Sf.Marke = MA_WEISS_NICHT;
	  fSfWrite(fpout, &Info[i].Sf, 1);
	}
      }
    }

    fclose(fpin);
    fclose(fpout);

  } else if (!strcmp(argv[1], "ospuniq")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.uniq", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    for (i=0;; i++) {

if (!(i % 100)) printf("%d\n", i);

      if (fTabEin(fpin, &sf)) break;

      if (TabEindeutig(&sf)) fTabAus(fpout, &sf);

    }  

    fclose(fpin);
    fclose(fpout);


  } else if (!strcmp(argv[1], "gamuniq")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.uniq", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");


    for (i=0;; i++) {

      int k;

if (!(i % 100)) { printf("%6d\r", i); fflush(stdout); }

      if (!ngame.f_read(fpin)) break;

      k = ngame.get_value();

      ngame.unique();

      ngame.set_value(k);

      ngame.f_write(fpout);
    }  

    fclose(fpin);
    fclose(fpout);


  } else if (!strcmp(argv[1], "osphalb")) {

    int anz=0, halb;
    FILE *fpout1, *fpout2;

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    FOREVER {

      if (fTabEin(fpin, &sf)) break;

      anz++;
    }  

    sprintf(name, "%s.halb1", argv[2]);
    printf("%s -> %s\n", argv[2], name);

    fpout1 = fopen(name, "w");
    if (!fpout1) Error("write error");

    sprintf(name, "%s.halb2", argv[2]);
    printf("%s -> %s\n", argv[2], name);

    fpout2 = fopen(name, "w");
    if (!fpout2) Error("write error");


    halb = anz/2;
    anz = 0;

    fclose(fpin);
    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    FOREVER {

      if (fTabEin(fpin, &sf)) break;

      if (anz <= halb) fTabAus(fpout1, &sf); else fTabAus(fpout2, &sf);

      anz++;
    }  

    fclose(fpin);
    fclose(fpout1);
    fclose(fpout2);

  } else if (!strcmp(argv[1], "testoko")) {

    int n=1;

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.ok", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      NewGame ngame;

      if (!ngame.f_read_packed(fpin)) break;

      if (!ngame.ok()) {
	printf("*** game %d corrupt\n", n); 
	ngame.f_write(stdout);
	goto next2;
      }

      ngame.f_write_packed(fpout);

next2:
      n++;
    }

    fclose(fpin);
    fclose(fpout);

  } else if (!strcmp(argv[1], "srvgam")) {

    int n;

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");

    sprintf(name, "%s.gam", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    for (n=1;; n++) {
      const int MAX_LEN = 500;
      char line[MAX_LEN+2], color[MAX_LEN+2], srv_gam[MAX_LEN+2], dummy[MAX_LEN+2];
      int res, level;
      NewGame ngame;

      if (!fgets(line, MAX_LEN, fpin)) break;

      if (sscanf(line, "%s %s %s %d %s", dummy, dummy, color, &level, srv_gam) != 5) {
	fprintf(stderr, line);
	fprintf(stderr, "srvgam line game");
	continue;
      }

      if (!ngame.s_read_srv(srv_gam)) {
	fprintf(stderr, line);
	fprintf(stderr, "game corrupt\n");
	continue;
      }

      res = ngame.play();

      if (res >= 65) {

	fprintf(stderr, "*** game %d corrupt\n", n); 
	ngame.f_write(stderr);

      } else {

	if ((color[0] == 'B' && res <= 0) || 
            (color[0] == 'W' && res >= 0)) {

	  ngame.set_flags(0);
	  ngame.is_finished(true);
	  ngame.set_value(res);
	  fprintf(fpout, "%c %d ", color[0], level);
	  ngame.f_write(fpout);
	}
      }
    }

    fclose(fpin);
    fclose(fpout);

  } else if (!strcmp(argv[1], "newoko")) {

    fpin = fopen(argv[2], "r");
    if (!fpin) Error("file not found");


    sprintf(name, "%s.newoko", argv[2]);
    printf("%s -> %s\n\n", argv[2], name);

    fpout = fopen(name, "w");
    if (!fpout) Error("write error");

    FOREVER {

      GAME game;
      NewGame newgam;

      if (!fReadPackedGame(fpin, &game)) break;
      newgam.from_old(game);
      newgam.f_write_packed(fpout);
    }
    fclose(fpin);
    fclose(fpout);

  } else goto Fehler;

  return 0;
}
