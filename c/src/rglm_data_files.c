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
  gd->empty_count = 0;
  gd->position_status_cnt = 0;
  gd->position_status_buffer = NULL;
  gd->position_statuses = NULL;
  gd->pattern_cnt = 0;
  gd->patterns = NULL;

  gd->position_summary.ntuples = 0;
  gd->position_summary.records = NULL;
  gd->pattern_freq_summary.ntuples = 0;
  gd->pattern_freq_summary.records = NULL;
}

void
rglmdf_general_data_release (rglmdf_general_data_t *gd)
{
  free(gd->patterns);
  free(gd->position_statuses);
  free(gd->position_status_buffer);
  free(gd->batch_ids);
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

uint8_t
rglmdf_get_empty_count (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->empty_count;
}

void
rglmdf_set_empty_count (rglmdf_general_data_t *gd,
                        uint8_t empty_count)
{
  assert(gd);
  gd->empty_count = empty_count;
}

size_t
rglmdf_set_position_status_cnt (rglmdf_general_data_t *gd,
                                size_t cnt)
{
  assert(gd);

  const size_t sbuf = sizeof(char) * RGLM_POSITION_STATUS_BUF_SIZE * cnt;
  const size_t sarr = sizeof(char *) * cnt;
  char *buf;
  char **arr;
  buf = (char *) malloc(sbuf);
  arr = (char **) malloc(sarr);
  if (buf && arr) {
    free(gd->position_status_buffer);
    free(gd->position_statuses);
    gd->position_status_buffer = buf;
    gd->position_statuses = arr;
    gd->position_status_cnt = cnt;
    memset(gd->position_status_buffer, 0, sbuf);
    for (size_t i = 0; i < cnt; i++ ) {
      gd->position_statuses[i] = gd->position_status_buffer + RGLM_POSITION_STATUS_BUF_SIZE * i;
    }
    return cnt;
  } else {
    return 0;
  }
}

size_t
rglmdf_get_position_status_cnt (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->position_status_cnt;
}

char **
rglmdf_get_position_statuses (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->position_statuses;
}

void
rglmdf_position_statuses_to_text_stream (rglmdf_general_data_t *gd,
                                         FILE *stream)
{
  fprintf(stream, "Selected position_statuses values: ");
  for (size_t i = 0; i < gd->position_status_cnt; i++ ) {
    fprintf(stream, "%s", gd->position_statuses[i]);
    fprintf(stream, "%s", (i < gd->position_status_cnt - 1) ? ", ": "\n");
  }
}

size_t
rglmdf_set_pattern_cnt (rglmdf_general_data_t *gd,
                         size_t cnt)
{
  assert(gd);

  const size_t s = sizeof(board_pattern_id_t) * cnt;
  board_pattern_id_t *buf;
  buf = (board_pattern_id_t *) malloc(s);
  if (buf) {
    free(gd->patterns);
    gd->patterns = buf;
    gd->pattern_cnt = cnt;
    memset(gd->patterns, 0, s);
    return cnt;
  } else {
    return 0;
  }
}

size_t
rglmdf_get_pattern_cnt (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->pattern_cnt;
}

board_pattern_id_t *
rglmdf_get_patterns (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->patterns;
}

void
rglmdf_patterns_to_text_stream (rglmdf_general_data_t *gd,
                                FILE *stream)
{
  fprintf(stream, "Selected pattern values: ");
  for (size_t i = 0; i < gd->pattern_cnt; i++ ) {
    fprintf(stream, "%s", board_patterns[gd->patterns[i]].name);
    fprintf(stream, "%s", (i < gd->pattern_cnt - 1) ? ", ": "\n");
  }
}
