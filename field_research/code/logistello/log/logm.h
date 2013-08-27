// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef LOGM_H
#define LOGM_H

#ifdef  AMIGA
#define PRECOMP 
#else
#define PRECOMP 
#endif


#define MAT_CHECK

#define VARMAX	200	/* Anzahl der Merkmale  */
#define LENMAX	30	/* Merkmalsnamenlänge   */
#define DATMAX	200	/* Dateinamenlänge      */

#ifdef UNIX
#define BEISP_MAX 130000	/* Anzahl der Beispiele */
#else
#define BEISP_MAX 10000		/* Anzahl der Beispiele */
#endif

#define P_NULL	0.01	/* nicht 0, sonst Konvergenzprobleme	*/
			/*  0.01 gibt bei Ladwig Probleme	*/
			/*  (Punktverteilung)			*/


#undef REAL
#undef R_F
#undef R_E
#undef REAL_MAX 
#undef REAL_IS_FLOAT
#undef REAL_IS_DOUBLE



#define REAL_IS_DOUBLE

#ifdef REAL_IS_FLOAT

#define REAL float
#define R_F "f"
#define R_E "e"
#define REAL_MAX  FLT_MAX

#else

#define REAL double
#define R_F "lf"
#define R_E "le"
#define REAL_MAX  DBL_MAX

#endif

/*
extern void Error(char *s);
extern void Chk_Abort(void);
*/



#endif
