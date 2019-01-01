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
 * @copyright 2018, 2019 Roberto Corradini. All rights reserved.
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

extern bool
rglmdf_verify_type_sizes (void)
{
  if (sizeof(char) != 1) return false;

  if (sizeof( uint8_t) != 1) return false;
  if (sizeof(uint16_t) != 2) return false;
  if (sizeof(uint32_t) != 4) return false;
  if (sizeof(uint64_t) != 8) return false;
  if (sizeof(  int8_t) != 1) return false;
  if (sizeof( int16_t) != 2) return false;
  if (sizeof( int32_t) != 4) return false;
  if (sizeof( int64_t) != 8) return false;

  if (sizeof(rglmdf_position_summary_record_t) != 24) return false;
  if (sizeof(rglmdf_pattern_freq_summary_record_t) != 40) return false;
  if (sizeof(rglmdf_solved_and_classified_gp_record_t) != 48) return false;

  return true;
}

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

  gd->positions.ntuples = 0;
  gd->positions.n_index_values_per_record = 0;
  gd->positions.records = NULL;
  gd->positions.iarray = NULL;
}

void
rglmdf_general_data_release (rglmdf_general_data_t *gd)
{
  free(gd->positions.iarray);
  free(gd->positions.records);
  free(gd->position_summary.records);
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

size_t
rglmdf_set_position_summary_ntuples (rglmdf_general_data_t *gd,
                                     size_t ntuples)
{
  assert(gd);

  const size_t s = sizeof(rglmdf_position_summary_record_t) * ntuples;
  rglmdf_position_summary_record_t *arr;
  arr = (rglmdf_position_summary_record_t *) malloc(s);
  if (arr) {
    free(gd->position_summary.records);
    gd->position_summary.records = arr;
    memset(gd->position_summary.records, 0, s);
    gd->position_summary.ntuples = ntuples;
    return ntuples;
  } else {
    return 0;
  }
}

size_t
rglmdf_get_position_summary_ntuples (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->position_summary.ntuples;
}

rglmdf_position_summary_record_t *
rglmdf_get_position_summary_records (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->position_summary.records;
}

void
rglmdf_position_summary_cnt_to_text_stream (rglmdf_general_data_t *gd,
                                            FILE *stream)
{
  assert(gd);
  size_t solved_classified_gp_cnt = 0;
  for (size_t i = 0; i < gd->position_summary.ntuples; i++)
    solved_classified_gp_cnt += gd->position_summary.records[i].classified_cnt;
  fprintf(stream, "Position Summary Table: number of tuples = %zu; game position count = %zu\n",
          gd->position_summary.ntuples, solved_classified_gp_cnt);
}

size_t
rglmdf_set_pattern_freq_summary_ntuples (rglmdf_general_data_t *gd,
                                         size_t ntuples)
{
  assert(gd);

  const size_t s = sizeof(rglmdf_pattern_freq_summary_record_t) * ntuples;
  rglmdf_pattern_freq_summary_record_t *arr;
  arr = (rglmdf_pattern_freq_summary_record_t *) malloc(s);
  if (arr) {
    free(gd->pattern_freq_summary.records);
    gd->pattern_freq_summary.records = arr;
    memset(gd->pattern_freq_summary.records, 0, s);
    gd->pattern_freq_summary.ntuples = ntuples;
    return ntuples;
  } else {
    return 0;
  }
}

size_t
rglmdf_get_pattern_freq_summary_ntuples (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->pattern_freq_summary.ntuples;
}

rglmdf_pattern_freq_summary_record_t *
rglmdf_get_pattern_freq_summary_records (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->pattern_freq_summary.records;
}

void
rglmdf_pattern_freq_summary_cnt_to_text_stream (rglmdf_general_data_t *gd,
                                                FILE *stream)
{
  assert(gd);
  fprintf(stream, "Pattern Frequency Summary Table: number of tuples = %zu\n",
          gd->pattern_freq_summary.ntuples);

  int pattern_id = -1;
  int64_t total_cnt = -1;
  double relative_frequency = -1.;
  double theoretical_probability = -1.;
  for (size_t i = 0; i < gd->pattern_freq_summary.ntuples; i++) {
    rglmdf_pattern_freq_summary_record_t *rec = &gd->pattern_freq_summary.records[i];
    if (rec->pattern_id != pattern_id) {
      total_cnt = 0;
      relative_frequency = 0.;
      theoretical_probability = 0.;
      pattern_id = rec->pattern_id;
    }
    total_cnt += rec->total_cnt;
    relative_frequency += rec->relative_frequency;
    theoretical_probability += rec->theoretical_probability;
    if (i + 1 == gd->pattern_freq_summary.ntuples || (rec + 1)->pattern_id != pattern_id) {
      const char *pattern_name = board_patterns[pattern_id].name;
      const int64_t gp_cnt = total_cnt / board_patterns[pattern_id].n_instances;
      printf("  Pattern id: %2d [%6s], total_cnt = %8ld, gp_cnt = %8ld, cumulated relative frequency = %1.4f, cumulated theoretical probability = %1.4f\n",
             pattern_id, pattern_name, total_cnt, gp_cnt, relative_frequency, theoretical_probability);
    }
  }
}

// -------------

size_t
rglmdf_set_positions_ntuples (rglmdf_general_data_t *gd,
                              size_t ntuples)
{
  assert(gd);

  size_t n_index_values_per_record = 0;
  for (size_t i = 0; i < gd->pattern_cnt; i++) {
    board_pattern_id_t pid = gd->patterns[i];
    n_index_values_per_record += board_patterns[pid].n_instances;
  }
  const size_t si = sizeof(uint32_t) * n_index_values_per_record * ntuples;
  uint32_t *iarr;
  iarr = (uint32_t *) malloc(si);
  if (iarr) {
    free(gd->positions.iarray);
    gd->positions.iarray = iarr;
    memset(gd->positions.iarray, 0, si);
    ;
    const size_t s = sizeof(rglmdf_solved_and_classified_gp_record_t) * ntuples;
    rglmdf_solved_and_classified_gp_record_t *arr;
    arr = (rglmdf_solved_and_classified_gp_record_t *) malloc(s);
    if (arr) {
      free(gd->positions.records);
      gd->positions.records = arr;
      memset(gd->positions.records, 0, s);
      gd->positions.ntuples = ntuples;
      gd->positions.n_index_values_per_record = n_index_values_per_record;
      return ntuples;
    } else {
      free(iarr);
      return 0;
    }
  } else {
    return 0;
  }
}

size_t
rglmdf_get_positions_ntuples (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->positions.ntuples;
}

rglmdf_solved_and_classified_gp_record_t *
rglmdf_get_positions_records (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->positions.records;
}

size_t
rglmdf_get_positions_n_index_values_per_record (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->positions.n_index_values_per_record;
}

uint32_t *
rglmdf_get_positions_iarray (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->positions.iarray;
}
