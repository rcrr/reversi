/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#define EXTRACT_BIT(BASE, START) (((BASE) >> (START)) & 0x1)
#define EXTRACT_BYTE(BASE, START) ((unsigned char)((BASE) >> (START) * 8))
#define EXTRACT_WORD(BASE, START) ((unsigned short)((BASE) >> (START) * 16))
#define EXTRACT(BASE, START, LENGTH) (((BASE) << (32 - (START) - (LENGTH))) >> (32 - (LENGTH)))
