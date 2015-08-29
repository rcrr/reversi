/**
 * @file
 *
 * @brief Reads a PVE dump dat file.
 * @details This executable reads a file that contains the
 * PVE dump, then executes the actions dictated by the flags given in the command line.
 *
 * @par read_pve_dump.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015 Roberto Corradini. All rights reserved.
 *
 * @par License
 * <tt>
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * \n
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 * </tt>
 */

#include <stdio.h>
#include <stdlib.h>


/* Static constants. */



/* Static variables. */

int
read_file (void)
{
  FILE * pFile;
  long lSize;
  char * buffer;
  size_t result;

  pFile = fopen ("pve_dump.dat", "rb");
  if (pFile == NULL) {fputs ("File error", stderr); exit(1);}

  // obtain file size:
  fseek(pFile, 0, SEEK_END);
  lSize = ftell(pFile);
  rewind(pFile);

  // allocate memory to contain the whole file:
  buffer = (char *) malloc(sizeof(char) * lSize);
  if (buffer == NULL) {fputs ("Memory error", stderr); exit(2);}

  // copy the file into the buffer:
  result = fread(buffer, 1, lSize, pFile);
  if (result != lSize) {fputs ("Reading error", stderr); exit (3);}

  /* the whole file is now loaded in the memory buffer. */

  // terminate
  fclose(pFile);
  free(buffer);
  return 0;
}

/**
 * @brief Main entry for the PVE dump utility.
 */
int
main (int argc, char *argv[])
{
  printf("read_pve_dump: hello world!\n");

  FILE * pFile;
  char c_buffer[] = {'a' , 'b' , 'c'};
  size_t i_buffer[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

  pFile = fopen("pve_dump.dat", "wb");

  fwrite(c_buffer, sizeof(char), sizeof(c_buffer), pFile);
  fwrite(i_buffer, sizeof(size_t), sizeof(i_buffer), pFile);

  fclose(pFile);

  read_file();

  return 0;
}
