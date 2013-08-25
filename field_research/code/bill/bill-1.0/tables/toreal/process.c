/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* generates array initializaton for everything.  Uses work.data as stdin */

#define NO_MOVE -1
#include <ctype.h>

main()
{
    int             i, f1, l1, f2, l2;
    char            s[80], name[10], c, first1[3], last1[3], first2[3], last2[3];
    int             bit_map[8], size, incr;
    static int      incr_tab[5] = {0, 9, -7, 9, -7};

    printf("{");
    while (gets(s))		/* until EOF */
    {
	switch (s[0])
	{
	case 'H':		/* a simple case */
	case 'V':
	    {
		sscanf(s,
		       "%s%c%2s%c%2s",
		       name, &c, first1, &c, last1);
		f1 = cvt(first1);
		l1 = cvt(last1);
		f2 = l2 = 0;	/* not used */
		size = 8;
		if (s[0] == 'H')
		    incr = 1;
		else
		    incr = 8;
		break;
	    }
	case 'D':
	    {
		switch (s[1])
		{
		case '8':
		case '6':
		case '7':
		    {
			sscanf(s,
			       "%s%c%2s%c%2s",
			       name, &c, first1, &c, last1);
			f1 = cvt(first1);
			l1 = cvt(last1);
			f2 = l2 = 0;	/* not used */
			size = s[1] - '0';
			incr = incr_tab[s[4] - '0'];
			break;
		    }
		case '4':
		case '5':
		    {
			sscanf(s,
			       "%s%c%2s%c%2s%c%2s%c%2s",
			       name, &c, first1, &c, last1,
			       &c, first2, &c, last2);
			f1 = cvt(first1);
			l1 = cvt(last1);
			f2 = cvt(first2);
			l2 = cvt(last2);
			size = 8;
			incr = incr_tab[s[4] - '0'];
			break;
		    }
		}		/* switch s[1] */
		break;
	    }			/* case 'D' */
	}			/* switch s[0] */
	printf("\n {%c%s%c,{ ", '"', name, '"');
	if (!(l2 == f2))
	{
	    for (i = f2; i != l2; i += incr)	/* put squares down */
		printf("%3d, ", i);
	    printf("%3d, ",i);
	}
	for (i = f1; i != l1; i += incr)	/* put squares down */
	    printf("%3d, ", i);
	printf("%3d, ",i);
	printf("}, %2d },", size);
    }				/* while */
    printf("\n};\n");
}

cvt (str)
char *str;
{
  if (str[0] == '-' && str[1] == '-')
    return (NO_MOVE);
  if (islower (str[0]))
    return ((str[1] - '1') * 8 + (str[0] - 'a'));
  else
    return ((str[1] - '1') * 8 + (str[0] - 'A'));
}
