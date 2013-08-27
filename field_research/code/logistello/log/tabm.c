// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* table generation / 2.94 */

#ifndef PRECOMP
#include "main.h"
#endif

 
#include "tab.h"
#include "sboard.h"
#include "crt.h"
#include "fpatt.h"
#include "patt.h"
#include "eval.h"


void _abort(void) { exit(10); }


int main(int argc, char **argv)
{
  int  argi, i;
  DELTA_BUFFER *delta;
  bool  f_detail=false;
  TABTYPE *tab;
  TABLES tables;

  InitCrt();
  InitSetzen();


  if (argc == 1) {

error:
    Error("call: otab -show raw-file [-detail] | -make tab-file | -makela tab-file | -make4x3 tab-file | -make3x3 tab-file");
  }


  argi = 1;


  if (!strcmp(argv[argi], "-show")) {

    if (!argv[argi+1]) goto error;

    if (argv[argi+2]) {

      if (strcmp(argv[argi+2], "-detail")) goto error;
      else f_detail = true;
    }

    if (!(delta=GetDeltaBuffer(argv[argi+1]))) 
      Error("file not found, too short, or corrupt");


    if (f_detail) {

      f_tabout = true;

      if (delta->Big) tab = ComputeBigTab(delta); 
      else            tab = ComputeTab(delta);


    } else {

      int pnum = Pot3[delta->DiscNum];

      if (delta->Big) {

        tab = ComputeBigTab(delta);

        FOR (i, pnum) {

          PattOut(stdout, i, delta->DiscNum); printf("  ");
          printf("%+4d\n", tab[i]);
        }

      } else {

        tab = ComputeTab(delta);

        FOR (i, pnum) {

	  int j;

          PattOut(stdout, i, delta->DiscNum); printf("  ");

          FOR (j, INUM+1) printf("%+4d ", tab[j*pnum+i]);
          printf("\n");

        }
      }

    }

  } else if (!strcmp(argv[argi], "-make")) {

    if (!argv[argi+1]) goto error;

#define APPEND(x, f) \
  if (!(delta=GetDeltaBuffer(f))) { printf(f); fflush(stdout); Error(" can't open raw-file or file corrupt"); } \
  if (delta->Big) tables.x = ComputeBigTab(delta); else tables.x = ComputeTab(delta); \
  FreeDeltaBuffer(&delta);

    APPEND(r1,  "r1.raw");
    APPEND(hv1, "hv1.raw");
    APPEND(hv2, "hv2.raw");
    APPEND(hv3, "hv3.raw");
    APPEND(hv4, "hv4.raw");
    APPEND(d1,  "d1.raw");
    APPEND(d2,  "d2.raw");
    APPEND(d3,  "d3.raw");
    APPEND(d4,  "d4.raw");
    APPEND(d5,  "d5.raw");
    APPEND(d6,  "d6.raw");
    APPEND(b1,  "b1.raw");
    APPEND(b2,  "b2.raw");

    WriteTables(&tables, argv[argi+1]);

  } else if (!strcmp(argv[argi], "-makela")) {
  
    if (!argv[argi+1]) goto error;

    APPEND(la, "la.raw");

    FILE *fp = fopen(argv[argi+1], "w");

    BigTabWrite(fp, tables.la, 14);

    if (ferror(fp)) Error("write error");
    fclose(fp);

  } else if (!strcmp(argv[argi], "-make4x3")) {
  
    if (!argv[argi+1]) goto error;

    APPEND(t4x3, "4x3.raw");

    FILE *fp = fopen(argv[argi+1], "w");

    BigTabWrite(fp, tables.t4x3, 12);

    if (ferror(fp)) Error("write error");
    fclose(fp);

  } else if (!strcmp(argv[argi], "-make3x3")) {
  
    if (!argv[argi+1]) goto error;

    APPEND(t3x3, "3x3.raw");

    FILE *fp = fopen(argv[argi+1], "w");

    BigTabWrite(fp, tables.t3x3, 9);

    if (ferror(fp)) Error("write error");
    fclose(fp);

  } else goto error;

  return 0;
}
