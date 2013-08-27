/* -----------------------------------------------------------------------
   client_util.i
   -------------------
   (C) Igor Durdanovic
       Mark Brockington
   ----------------------------------------------------------------------- */

static int blitz_compare( t_who *p1, t_who *p2 )
{
  return p2->blitz - p1->blitz;
}

static int standard_compare( t_who *p1, t_who *p2 )
{
  return p2->standard - p1->standard;
}

typedef int (*t_qcmp) (const void *, const void *);

void ios_challenge( int time, int inc, int def, 
	            int color,int rnd_color,
                    int min_rating, int max_rating,
                    int must_rated, int best)
{
int  cx, rating, qu, blitz;
int  player[256];

   blitz = (time + inc/2 < 15);

   if ( blitz )
     qsort(v_ios.u.WHO.list, v_ios.u.WHO.no, 
	   sizeof(v_ios.u.WHO.list[0]), (t_qcmp)blitz_compare);
   else
     qsort(v_ios.u.WHO.list, v_ios.u.WHO.no, 
	   sizeof(v_ios.u.WHO.list[0]), (t_qcmp)standard_compare);

   qu = 0;

   if ( min_rating < 0 ) { /* then min_rating += my_rating */
     for ( cx = 0; cx < v_ios.u.WHO.no; cx++ ) {
       rating = ( blitz ? v_ios.u.WHO.list[cx].blitz :
			 v_ios.u.WHO.list[cx].standard );
       if (! strcmp(ios_login, v_ios.u.WHO.list[cx].name) ) {
	 min_rating += rating;
	 printf("min_rating=%d\n",min_rating);
	 break;
       }
     }
   }
   for( cx = 0; cx < v_ios.u.WHO.no; cx++) {
     rating = ( blitz ? v_ios.u.WHO.list[cx].blitz :
                       v_ios.u.WHO.list[cx].standard );
     if ( rating >= min_rating && 
          rating <= max_rating &&
         !v_ios.u.WHO.list[cx].playing &&
          v_ios.u.WHO.list[cx].open &&
          v_ios.u.WHO.list[cx].rated >= must_rated &&
          strcmp(v_ios.u.WHO.list[cx].name, ios_login) ) {
       player[qu++] = cx;
       printf("%8s %4d %4d\n", 
	  v_ios.u.WHO.list[cx].name,
	  v_ios.u.WHO.list[cx].blitz,
	  v_ios.u.WHO.list[cx].standard);
       }
     }

   if (qu > 0) {
     cx = player[best ? 0 : rand() % qu];
     v_ios.type = IOS_MATCH;
     v_ios.u.MATCH.my_time  = time;
     v_ios.u.MATCH.my_inc   = inc;
     v_ios.u.MATCH.my_def   = def;
     v_ios.u.MATCH.my_color = color;
     v_ios.u.MATCH.rnd_color = rnd_color;
     strcpy( v_ios.u.MATCH.op_name, v_ios.u.WHO.list[cx].name );
     ios_put();
   }
} 

/* -----------------------------------------------------------------------
   (eof) client_util.i
   ----------------------------------------------------------------------- */
