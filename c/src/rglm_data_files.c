/**
 * @file
 *
 * @brief Reversi Generalized Linear Model Data Files module implementation.
 *
 * @par rglm_data_files.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2018 Roberto Corradini. All rights reserved.
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
#include <string.h>
#include <assert.h>


#include "rglm_data_files.h"

void
rglmdf_general_data_init (rglmdf_general_data_t *gd)
{
  assert(gd);
  gd->file_creation_time = (time_t) 0; // "Thu Jan  1 00:0:00 1970 (UTC)"
  gd->batch_id_cnt = 0;
  gd->batch_ids = NULL;
}

void
rglmdf_set_file_creation_time (rglmdf_general_data_t *gd,
                               time_t t)
{
  assert(gd);
  gd->file_creation_time = t;
}

time_t
rglmdf_get_file_creation_time (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->file_creation_time;
}

void
rglmdf_get_file_creation_time_as_string (rglmdf_general_data_t *gd,
                                               char *buf)
{
  assert(gd);
  assert(buf);

  struct tm file_creation_time_tm;
  gmtime_r(&gd->file_creation_time, &file_creation_time_tm);
  asctime_r(&file_creation_time_tm, buf);
}

size_t
rglmdf_set_batch_id_cnt (rglmdf_general_data_t *gd,
                         size_t cnt)
{
  assert(gd);

  const size_t s = sizeof(uint64_t) * cnt;
  uint64_t *buf;
  buf = (uint64_t *) malloc(s);
  if (buf) {
    free(gd->batch_ids);
    gd->batch_ids = buf;
    gd->batch_id_cnt = cnt;
    memset(gd->batch_ids, 0, s);
    return cnt;
  } else {
    return 0;
  }
}

size_t
rglmdf_get_batch_id_cnt (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->batch_id_cnt;
}

uint64_t *
rglmdf_get_batch_ids (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->batch_ids;
}

void
rglmdf_batch_ids_to_text_stream (rglmdf_general_data_t *gd,
                                 FILE *stream)
{
  fprintf(stream, "Selected batch_id values: ");
  for (size_t i = 0; i < gd->batch_id_cnt; i++ ) {
    fprintf(stream, "%zu", gd->batch_ids[i]);
    fprintf(stream, "%s", (i < gd->batch_id_cnt - 1) ? ", ": "\n");
  }
}

void
rglmdf_general_data_release (rglmdf_general_data_t *gd)
{
  free(gd->batch_ids);
}
