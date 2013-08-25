/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* added by JGK to compile without libcs.a */

#include <stdio.h>


FILE* fwantread(path, filename, buffer, unused)
    char* path;
    char* filename;
    char* buffer;
    char* unused;
{
    return fopen(filename, "r");
}


FILE* fwantwrite(path, filename, buffer, junk1, junk2)
    char* path;
    char* filename;
    char* buffer;
    char* junk1;
    int junk2;
{
    return fopen(filename, "w");
}


int getint(buffer, min, max, junk)
    char* buffer;
    int min;
    int max;
    int junk;
{
    gets(buffer);
    return atoi(buffer);
}
