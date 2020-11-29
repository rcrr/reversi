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
 * @copyright 2018, 2019, 2020 Roberto Corradini. All rights reserved.
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

#include "sha3.h"
#include "file_utils.h"
#include "rglm_data_files.h"

#define RGLM_MAX_PATTERN_CNT 1024
#define RGLM_MAX_PATTERN_INSTANCE_CNT 8

int
rglmdf_get_endianness (void)
{
  int ret;
  const uint32_t u32 = 1;
  const char *const c = (char *) &u32;
  if (*c) {
    assert(*(c+0) == 0x01);
    assert(*(c+1) == 0x00);
    assert(*(c+2) == 0x00);
    assert(*(c+3) == 0x00);
    ret = 0;
  } else {
    assert(*(c+0) == 0x00);
    assert(*(c+1) == 0x00);
    assert(*(c+2) == 0x00);
    assert(*(c+3) == 0x01);
    ret = 1;
  }
  return ret;
}

bool
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
  if (sizeof(rglmdf_pattern_freq_summary_record_t) != 48) return false;
  if (sizeof(rglmdf_solved_and_classified_gp_record_t) != 48) return false;

  if (sizeof(double) != 8) return false;

  return true;
}

void
rglmdf_general_data_init (rglmdf_general_data_t *gd)
{
  assert(gd);
  gd->file_creation_time = (time_t) 0; // "Thu Jan  1 00:0:00 1970 (UTC)"
  gd->format = RGLMDF_FILE_DATA_FORMAT_TYPE_IS_INVALID;
  gd->batch_id_cnt = 0;
  gd->batch_ids = NULL;
  gd->empty_count = 0;
  gd->position_status_cnt = 0;
  gd->position_status_buffer = NULL;
  gd->position_statuses = NULL;
  gd->feature_cnt = 0;
  gd->features = NULL;
  gd->pattern_cnt = 0;
  gd->patterns = NULL;

  gd->position_summary.ntuples = 0;
  gd->position_summary.records = NULL;
  gd->pattern_freq_summary.glm_f_variable_cnt = 0;
  gd->pattern_freq_summary.glm_p_variable_cnt = 0;
  gd->pattern_freq_summary.ntuples = 0;
  gd->pattern_freq_summary.records = NULL;

  gd->positions.iarray_data_type = RGLMDF_IARRAY_IS_INDEX;
  gd->positions.ntuples = 0;
  gd->positions.n_fvalues_per_record = 0;
  gd->positions.n_index_values_per_record = 0;
  gd->positions.records = NULL;
  gd->positions.farray = NULL;
  gd->positions.i0array = NULL;

  gd->reverse_map_a_f = NULL;
  gd->reverse_map_a_p = NULL;
  gd->reverse_map_b = NULL;
}

void
rglmdf_general_data_release (rglmdf_general_data_t *gd)
{
  assert(gd);
  free(gd->reverse_map_b);
  free(gd->reverse_map_a_f);
  free(gd->positions.farray);
  free(gd->positions.i0array);
  free(gd->positions.records);
  free(gd->pattern_freq_summary.records);
  free(gd->position_summary.records);
  free(gd->patterns);
  free(gd->features);
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
rglmdf_set_format (rglmdf_general_data_t *gd,
                   rglmdf_file_data_format_type_t f)
{
  assert(gd);
  gd->format = f;
}

rglmdf_file_data_format_type_t
rglmdf_get_format (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->format;
}

void
rglmdf_format_to_text_stream (rglmdf_general_data_t *gd,
                              FILE *stream)
{
  assert(gd);
  if (!stream) return;

  char *s;

  switch (gd->format) {
  case RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL:
    s = "GENERAL";
    break;
  case RGLMDF_FILE_DATA_FORMAT_TYPE_IS_POSITIONS:
    s = "POSITIONS";
    break;
  case RGLMDF_FILE_DATA_FORMAT_TYPE_IS_INVALID:
    s = "INVALID";
    break;
  default:
    abort();
  }

  fprintf(stream, "The format of the binary data file is: %s\n", s);
}

void
rglmdf_get_file_creation_time_as_string (rglmdf_general_data_t *gd,
                                         char *buf)
{
  assert(gd);
  assert(buf);

  struct tm file_creation_time_tm;
  gmtime_r(&gd->file_creation_time, &file_creation_time_tm);
  strftime(buf, 25,"%c", &file_creation_time_tm);
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

  const size_t sbuf = sizeof(char) * RGLMDF_POSITION_STATUS_BUF_SIZE * cnt;
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
      gd->position_statuses[i] = gd->position_status_buffer + RGLMDF_POSITION_STATUS_BUF_SIZE * i;
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
rglmdf_set_feature_cnt (rglmdf_general_data_t *gd,
                        size_t cnt)
{
  assert(gd);

  /* Only if the format is GENERAL, cnt could be different from zero. */
  if (gd->format != RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL && cnt > 0) {
    return 0;
  }

  if (cnt == 0) {
    free(gd->features);
    gd->features = NULL;
    gd->feature_cnt = 0;
    return 0;
  }

  const size_t s = sizeof(board_feature_id_t) * cnt;
  board_feature_id_t *buf;
  buf = (board_feature_id_t *) malloc(s);
  if (buf) {
    free(gd->features);
    gd->features = buf;
    gd->feature_cnt = cnt;
    memset(gd->features, 0, s);
    return cnt;
  } else {
    return 0;
  }
}

size_t
rglmdf_get_feature_cnt (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->feature_cnt;
}

board_feature_id_t *
rglmdf_get_features (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->features;
}

void
rglmdf_features_to_text_stream (rglmdf_general_data_t *gd,
                                FILE *stream)
{
  fprintf(stream, "Selected feature values: ");
  if (gd->feature_cnt == 0) fprintf(stream, "--\n");
  for (size_t i = 0; i < gd->feature_cnt; i++ ) {
    fprintf(stream, "%s", board_features[gd->features[i]].name);
    fprintf(stream, "%s", (i < gd->feature_cnt - 1) ? ", ": "\n");
  }
}

size_t
rglmdf_get_glm_f_variable_cnt (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->pattern_freq_summary.glm_f_variable_cnt;
}

size_t
rglmdf_set_pattern_cnt (rglmdf_general_data_t *gd,
                        size_t cnt)
{
  assert(gd);

  /* Only if the format is GENERAL, cnt could be different from zero. */
  if (gd->format != RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL && cnt > 0) {
    return 0;
  }

  if (cnt == 0) {
    free(gd->patterns);
    gd->patterns = NULL;
    gd->pattern_cnt = 0;
    return 0;
  }

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
  if (gd->pattern_cnt == 0) fprintf(stream, "--\n");
  for (size_t i = 0; i < gd->pattern_cnt; i++ ) {
    fprintf(stream, "%s", board_patterns[gd->patterns[i]].name);
    fprintf(stream, "%s", (i < gd->pattern_cnt - 1) ? ", ": "\n");
  }
}

size_t
rglmdf_get_glm_p_variable_cnt (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->pattern_freq_summary.glm_p_variable_cnt;
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
rglmdf_set_entity_freq_summary_ntuples (rglmdf_general_data_t *gd,
                                        size_t feature_ntuples,
                                        size_t pattern_ntuples,
                                        size_t ntuples)
{
  assert(gd);
  assert(feature_ntuples + pattern_ntuples == ntuples);
  assert(ntuples < RGLMDF_INVALID_GLM_VARIABLE_ID);

  /* Only when format is GENERAL the table is populated. */
  if (gd->format != RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL) {
    if (feature_ntuples != 0 || pattern_ntuples != 0 || ntuples != 0) {
      return 0;
    }
  }

  if (gd->feature_cnt + gd->pattern_cnt > RGLM_MAX_PATTERN_CNT) {
    fprintf(stderr, "Error: gd->feature_cnt + gd->pattern_cnt > RGLM_MAX_PATTERN_CNT\n");
    fprintf(stderr, "       gd->feature_cnt = %zu\n", gd->feature_cnt);
    fprintf(stderr, "       gd->pattern_cnt = %zu\n", gd->pattern_cnt);
    fprintf(stderr, "Aborting ... \n");
    assert(false);
  }

  uint32_t idx[RGLM_MAX_PATTERN_CNT];

  size_t k;

  size_t reverse_map_a_length = BOARD_FEATURE_COUNT + BOARD_PATTERN_COUNT;
  size_t reverse_map_b_length = 0;

  k = 0;
  for (size_t i = 0; i < gd->feature_cnt; i++) {
    const board_feature_id_t fid = gd->features[i];
    idx[k++] = reverse_map_b_length;
    reverse_map_b_length += board_features[fid].field_cnt;
  }
  for (size_t i = 0; i < gd->pattern_cnt; i++) {
    const board_pattern_id_t pid = gd->patterns[i];
    unsigned int n = board_patterns[pid].n_squares;
    uint32_t dim = 1; for (size_t j = 0; j < n; j++) dim *= 3;
    idx[k++] = reverse_map_b_length;
    reverse_map_b_length += dim;
  }

  /*
   * Allocates memory for the three vectors.
   */
  const size_t s_a = sizeof(uint64_t *) * reverse_map_a_length;
  const size_t s_b = sizeof(uint64_t) * reverse_map_b_length;
  const size_t s_c = sizeof(rglmdf_pattern_freq_summary_record_t) * ntuples;
  uint64_t **arr_a_f = (uint64_t **) malloc(s_a);
  if (!arr_a_f) return 0;
  uint64_t *arr_b = (uint64_t *) malloc(s_b);
  if (!arr_b) {
    free(arr_a_f);
    return 0;
  }
  uint64_t **arr_a_p = arr_a_f + BOARD_FEATURE_COUNT;
  rglmdf_pattern_freq_summary_record_t *arr_c = (rglmdf_pattern_freq_summary_record_t *) malloc(s_c);
  if (!arr_c) {
    free(arr_a_f);
    free(arr_b);
    return 0;
  }

  k = 0;
  memset(arr_a_f, 0, s_a);
  free(gd->reverse_map_a_f);
  for (size_t i = 0; i < gd->feature_cnt; i++)
    arr_a_f[gd->features[i]] = arr_b + idx[k++];
  for (size_t i = 0; i < gd->pattern_cnt; i++)
    arr_a_p[gd->patterns[i]] = arr_b + idx[k++];
  gd->reverse_map_a_f = arr_a_f;
  gd->reverse_map_a_p = arr_a_p;

  for (size_t i = 0; i < reverse_map_b_length; i++)
    arr_b[i] = RGLMDF_INVALID_GLM_VARIABLE_ID;
  gd->reverse_map_b = arr_b;

  memset(arr_c, 0, s_c);
  free(gd->pattern_freq_summary.records);
  gd->pattern_freq_summary.records = arr_c;
  gd->pattern_freq_summary.glm_f_variable_cnt = feature_ntuples;
  gd->pattern_freq_summary.glm_p_variable_cnt = pattern_ntuples;
  gd->pattern_freq_summary.ntuples = ntuples;

  return ntuples;
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
  if (!stream) return;
  fprintf(stream, "Feature and Pattern Frequency Summary Table: number of tuples = %zu\n",
          gd->pattern_freq_summary.ntuples);

  int16_t entity_class;
  int16_t entity_id;
  int64_t total_cnt;
  double relative_frequency;
  double theoretical_probability;
  int v_cnt, f_index_cnt, p_index_cnt;

  total_cnt = -1;
  relative_frequency = -1.;
  theoretical_probability = -1.;
  v_cnt = -1;

  f_index_cnt = p_index_cnt = 0;
  entity_class = BOARD_ENTITY_CLASS_INVALID;
  entity_id = -1;
  for (size_t i = 0; i < gd->pattern_freq_summary.ntuples; i++) {
    rglmdf_pattern_freq_summary_record_t *rec = &gd->pattern_freq_summary.records[i];
    if (rec->entity_class != entity_class || rec->entity_id != entity_id) {
      v_cnt = 0;
      total_cnt = 0;
      relative_frequency = 0.;
      theoretical_probability = 0.;
      entity_class = rec->entity_class;
      entity_id = rec->entity_id;
    }
    v_cnt++;
    total_cnt += rec->total_cnt;
    relative_frequency += rec->relative_frequency;
    theoretical_probability += rec->theoretical_probability;
    if (i + 1 == gd->pattern_freq_summary.ntuples || (rec + 1)->entity_class != entity_class || (rec + 1)->entity_id != entity_id) {
      if (entity_class == BOARD_ENTITY_CLASS_FEATURE) {
        const char *feature_name = board_features[entity_id].name;
        const int first_index = f_index_cnt;
        f_index_cnt += board_features[entity_id].field_cnt;
        fprintf(stream, "  Feature id: %2d [%10s][%6d][F_%03d:F_%03d], total_cnt = %8ld\n",
                entity_id, feature_name, v_cnt, first_index, f_index_cnt - 1, total_cnt);
      } else if (entity_class == BOARD_ENTITY_CLASS_PATTERN) {
        const char *pattern_name = board_patterns[entity_id].name;
        const int64_t gp_cnt = total_cnt / board_patterns[entity_id].n_instances;
        const int first_index = p_index_cnt;
        p_index_cnt += board_patterns[entity_id].n_instances;
        fprintf(stream, "  Pattern id: %2d     [%6s][%6d][I_%03d:I_%03d], total_cnt = %8ld, gp_cnt = %8ld, cumulated relative frequency = %1.4f, cumulated theoretical probability = %1.4f\n",
                entity_id, pattern_name, v_cnt, first_index, p_index_cnt - 1, total_cnt, gp_cnt, relative_frequency, theoretical_probability);
      } else {
        fprintf(stream, "Invalid entity_class value %d, it must be in the range [0..1]. Aborting ...\n", entity_class);
        abort();
      }
    }
  }
}

size_t
rglmdf_set_positions_ntuples (rglmdf_general_data_t *gd,
                              size_t ntuples,
                              rglmdf_iarray_data_type_t iarray_data_type)
{
  assert(gd);

  size_t n_fvalues_per_record = 0;
  for (size_t i = 0; i < gd->feature_cnt; i++) {
    board_feature_id_t fid = gd->features[i];
    n_fvalues_per_record += board_features[fid].field_cnt;
  }
  const size_t fi = sizeof(double) * n_fvalues_per_record * ntuples;
  double *farr;
  farr = (double *) malloc(fi);
  if (!farr)
    return 0;

  size_t n_index_values_per_record = 0;
  for (size_t i = 0; i < gd->pattern_cnt; i++) {
    board_pattern_id_t pid = gd->patterns[i];
    n_index_values_per_record += board_patterns[pid].n_instances;
  }
  const size_t si = sizeof(uint32_t) * n_index_values_per_record * ntuples;
  uint32_t *iarr;
  iarr = (uint32_t *) malloc(si);
  if (!iarr) {
    free(farr);
    return 0;
  }

  const size_t s = sizeof(rglmdf_solved_and_classified_gp_record_t) * ntuples;
  rglmdf_solved_and_classified_gp_record_t *arr;
  arr = (rglmdf_solved_and_classified_gp_record_t *) malloc(s);
  if (!arr) {
    free(iarr);
    free(farr);
    return 0;
  }

  free(gd->positions.farray);
  gd->positions.farray = farr;
  memset(gd->positions.farray, 0, fi);

  free(gd->positions.i0array);
  gd->positions.i0array = iarr;
  memset(gd->positions.i0array, 0, si);

  free(gd->positions.records);
  gd->positions.records = arr;
  memset(gd->positions.records, 0, s);

  gd->positions.iarray_data_type = iarray_data_type;
  gd->positions.ntuples = ntuples;
  gd->positions.n_index_values_per_record = n_index_values_per_record;
  gd->positions.n_fvalues_per_record = n_fvalues_per_record;

  return ntuples;
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
rglmdf_get_positions_n_fvalues_per_record (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->positions.n_fvalues_per_record;
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
  return gd->positions.i0array;
}

double *
rglmdf_get_positions_farray (rglmdf_general_data_t *gd)
{
  assert(gd);
  return gd->positions.farray;
}

void
rglmdf_build_reverse_map (rglmdf_general_data_t *gd)
{
  assert(gd);
  rglmdf_pattern_freq_summary_table_t *t = &gd->pattern_freq_summary;
  const size_t n = t->ntuples;

  for (size_t i = 0; i < n; i++) {
    if (i != t->records[i].glm_variable_id) {
      fprintf(stderr, "Error, inconsistent record order in table Pattern Frequency Summary. Aborting ...\n");
      abort();
    }
    const int16_t ec  = t->records[i].entity_class;
    const int16_t pid = t->records[i].entity_id;
    const int32_t piv = t->records[i].principal_index_value;
    if (ec == BOARD_ENTITY_CLASS_FEATURE) {
      gd->reverse_map_a_f[pid][piv] = i;
    } else if (ec == BOARD_ENTITY_CLASS_PATTERN) {
      gd->reverse_map_a_p[pid][piv] = i;
    } else {
      fprintf(stderr, "Error, inconsistent value of entity_class. Aborting ...\n");
      abort();
    }
  }
}

uint32_t
rglmdf_map_pid_and_piv_to_glm_vid (rglmdf_general_data_t *gd,
                                   uint16_t entity_class,
                                   uint16_t entity_id,
                                   uint32_t principal_index_value)
{
  assert(gd);
  assert(entity_class == BOARD_ENTITY_CLASS_FEATURE || entity_class == BOARD_ENTITY_CLASS_PATTERN);
  if (entity_class == BOARD_ENTITY_CLASS_FEATURE)
    return gd->reverse_map_a_f[entity_id][principal_index_value];
  else if (entity_class == BOARD_ENTITY_CLASS_PATTERN)
    return gd->reverse_map_a_p[entity_id][principal_index_value];
  else
    abort();
}

void
rglmdf_transform_piv_to_glm_variable_id (rglmdf_general_data_t *gd,
                                         bool first_step)
{
  assert(gd);
  assert(gd->positions.iarray_data_type == (first_step ? RGLMDF_IARRAY_IS_INDEX : RGLMDF_IARRAY_IS_PRINCIPAL_INDEX));

  uint32_t idx[RGLM_MAX_PATTERN_CNT * RGLM_MAX_PATTERN_INSTANCE_CNT];

  if (gd->pattern_cnt > RGLM_MAX_PATTERN_CNT) {
    fprintf(stderr, "gd->pattern_cnt > RGLM_MAX_PATTERN_CNT. gd->pattern_cnt = %zu. Aborting ...\n", gd->pattern_cnt);
    abort();
  }

  const uint16_t pattern_class_type = 1;

  size_t k = 0;
  for (size_t i = 0; i < gd->pattern_cnt; i++) {
    const board_pattern_id_t pid = gd->patterns[i];
    if (board_patterns[pid].n_instances > RGLM_MAX_PATTERN_INSTANCE_CNT) {
      fprintf(stderr, "board_patterns[pid].n_instances > RGLM_MAX_PATTERN_INSTANCE_CNT. Aborting ... \n");
      abort();
    }
    for (size_t j = 0; j < board_patterns[pid].n_instances; j++) {
      idx[k++] = pid;
    }
  }

  const size_t ni = rglmdf_get_positions_n_index_values_per_record(gd);
  for (size_t i = 0; i < gd->positions.ntuples; i++) {
    for (size_t j = 0; j < ni; j++) {
      uint32_t output;
      board_pattern_index_t index_value, principal_index_value;
      const uint32_t pattern_id = idx[j];
      if (first_step) {
        index_value = gd->positions.i0array[i * ni + j];
        board_pattern_compute_principal_indexes(&principal_index_value, &index_value, &board_patterns[pattern_id], true);
        output = principal_index_value;
      } else {
        principal_index_value = gd->positions.i0array[i * ni + j];
        output = rglmdf_map_pid_and_piv_to_glm_vid(gd, pattern_class_type, pattern_id, principal_index_value);
      }
      gd->positions.i0array[i * ni + j] = output;
    }
  }
  gd->positions.iarray_data_type = first_step ? RGLMDF_IARRAY_IS_PRINCIPAL_INDEX : RGLMDF_IARRAY_IS_GLM_VARIABLE_ID;
}

void
rglmdf_gp_table_to_csv_file (rglmdf_general_data_t *gd,
                             FILE *f)
{
  assert(gd);
  assert(f);
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(gd);
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(gd);
  fprintf(f, "       I;    ROW_N;      GP_ID;                MOVER;             OPPONENT; GAME_VALUE; GAME_VALUE_TRANSFORMED; EVALUATION_FUNCTION;         RESIDUAL");
  for (size_t j = 0; j < nf; j++) fprintf(f, ";           F_%03zu", j);
  for (size_t j = 0; j < ni; j++) fprintf(f, ";   I_%03zu", j);
  fprintf(f, "\n");
  for (size_t i = 0; i < gd->positions.ntuples; i++) {
    rglmdf_solved_and_classified_gp_record_t *r = &gd->positions.records[i];
    fprintf(f, "%8zu; %8zu; %10ld; %20ld; %20ld; %10d; %22.12f; %19.12f; %+16.12f",
            i, r->row_n, r->gp_id, r->mover, r->opponent, r->game_value,
            r->game_value_transformed, r->evaluation_function, r->residual);
    for (size_t j = 0; j < nf; j++)
      fprintf(f, ";%+16.6f", gd->positions.farray[i * nf + j]);
    for (size_t j = 0; j < ni; j++)
      fprintf(f, ";%8u", gd->positions.i0array[i * ni + j]);
    fprintf(f, "\n");
  }
}

void
rglmdf_ps_table_to_csv_file (rglmdf_general_data_t *gd,
                             FILE *f)
{
  assert(gd);
  rglmdf_position_summary_record_t *r = rglmdf_get_position_summary_records(gd);
  size_t n = rglmdf_get_position_summary_ntuples(gd);
  fprintf(f, " SEQ;BATCH_ID;STATUS; GAME_POSITION_CNT;    CLASSIFIED_CNT\n");
  for (size_t i = 0; i < n; i++) {
    fprintf(f, "%04zu;%8d;%6s;%18ld;%18ld\n", i, r[i].batch_id, r[i].status, r[i].game_position_cnt, r[i].classified_cnt);
  }
}

void
rglmdf_fpfs_table_to_csv_file (rglmdf_general_data_t *gd,
                               FILE *f)
{
  assert(gd);
  rglmdf_pattern_freq_summary_record_t *r = rglmdf_get_pattern_freq_summary_records(gd);
  size_t n = rglmdf_get_pattern_freq_summary_ntuples(gd);
  fprintf(f, "     SEQ; GLM_VARIABLE_ID;   ENTITY_CLASS;  ENTITY_ID; PRINCIPAL_INDEX_VALUE;   TOTAL_CNT; RELATIVE_FREQUENCY; THEORETICAL_PROBABILITY;              WEIGHT\n");
  for (size_t i = 0; i < n; i++) {
    fprintf(f, "%08zu;%16ld;%15d;%11d;%22d;%12ld;%19.6f;%24.6f;%+20.15f\n",
            i, r[i].glm_variable_id, r[i].entity_class, r[i].entity_id,
            r[i].principal_index_value, r[i].total_cnt, r[i].relative_frequency,
            r[i].theoretical_probability, r[i].weight);
  }
}

int
rglmdf_check_sha3_file_digest (char *file_name)
{
  assert(file_name);
  const char *hash_extension = "SHA3-256";
  int ccount;
  char hash_name[1024];
  char hash[65];
  size_t l;
  sha3_ctx_t ctx;

  static const size_t buf_size = 4 * 1024;
  char buf[buf_size];
  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_to_s[2 * sha3_256_digest_lenght + 1];

  FILE *hash_file, *data_file;

  memset(hash, '\0', sizeof(hash));

  ccount = snprintf(hash_name, sizeof hash_name, "%s.%s", file_name, hash_extension);
  if (!(ccount < sizeof hash_name)) {
    fprintf(stderr, "Error: the lenght of hash_name is larger than %zu characters.\n", sizeof hash_name);
    fprintf(stderr, "file_name: \"%s\".\n", file_name);
    fprintf(stderr, "hash_name: \"%s\".\n", hash_name);
    return EXIT_FAILURE;
  }

  if (!fut_file_exists(file_name)) {
    fprintf(stderr, "Error: data file \"%s\" does not exist.\n", file_name);
    return EXIT_FAILURE;
  }

  if (!fut_file_exists(hash_name)) {
    fprintf(stderr, "Error: hash file \"%s\" does not exist.\n", hash_name);
    return EXIT_FAILURE;
  }

  hash_file = fopen(hash_name, "r");
  if (!hash_file) {
    fprintf(stderr, "Error: not able to open hash file \"%s\".\n", hash_name);
    return EXIT_FAILURE;
  }

  l = fread(hash, 64, 1, hash_file);
  if (l != 1) {
    fprintf(stderr, "Error: not able to read the hash properly from file \"%s\".\n", hash_name);
    fprintf(stderr, "hash = \"%s\"\n", hash);
    return EXIT_FAILURE;
  }
  fclose(hash_file);

  sha3_init(&ctx, sha3_256_digest_lenght);
  data_file = fopen(file_name, "r");
  if (!data_file) {
    fprintf(stderr, "Error: not able to open data file \"%s\".\n", file_name);
    return EXIT_FAILURE;
  }
  do {
    l = fread(buf, 1, buf_size, data_file);
    sha3_update(&ctx, buf, l);
  } while (l != 0);
  fclose(data_file);
  sha3_final(&ctx, msg_digest);
  sha3_msg_digest_to_string(msg_digest_to_s, msg_digest, sha3_256_digest_lenght);

  if (strcmp(hash, msg_digest_to_s) != 0) {
    fprintf(stderr, "Error: computed hash does not match the expected one.\n");
    fprintf(stderr, "Expected hash = \"%s\"\n", hash);
    fprintf(stderr, "Computed hash = \"%s\"\n", msg_digest_to_s);
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
rglmdf_generate_sha3_file_digest (char *file_name)
{
  assert(file_name);

  static const size_t buf_size = 64 * 1024;
  static const size_t digest_file_name_size = 1024;
  char buf[buf_size];
  size_t re, file_name_len;
  sha3_ctx_t ctx;
  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_to_s[2 * sha3_256_digest_lenght + 1];
  FILE *ifp, *ofp;
  char digest_file_name[digest_file_name_size];
  const char *suffix = ".SHA3-256";

  memset(digest_file_name, '\0', sizeof(digest_file_name));
  file_name_len = snprintf(digest_file_name, digest_file_name_size, "%s%s", file_name, suffix);
  if (file_name_len + 1 > digest_file_name_size) {
    fprintf(stderr, "Error in function rglmdf_generate_sha3_file_digest, the buffer size for the file name is too small. Aborting ...\n");
    abort();
  }

  ifp = fopen(file_name, "r");
  if (!ifp) {
    return 1;
  }

  sha3_init(&ctx, sha3_256_digest_lenght);

  do {
    re = fread(buf, 1, buf_size, ifp);
    sha3_update(&ctx, buf, re);
  } while (re != 0);

  fclose(ifp);
  sha3_final(&ctx, msg_digest);
  sha3_msg_digest_to_string(msg_digest_to_s, msg_digest, sha3_256_digest_lenght);

  ofp = fopen(digest_file_name, "w");
  if (!ofp) {
    return 2;
  }
  fprintf(ofp, "%s  %s\n", msg_digest_to_s, file_name);
  fclose(ofp);
  return 0;
}

int
rglmdf_write_rglm_weights_to_binary_file (rglmdf_general_data_t *gd,
                                          char *filename)
{
  assert(gd);
  assert(filename);

  uint64_t u64;
  uint8_t u8;
  int16_t i16;
  double w;

  time_t current_time = (time_t) -1;

  FILE *ofp = fopen(filename, "w");
  if (!ofp) {
    fprintf(stderr, "Unable to open binary output file: %s\n", filename);
    return EXIT_FAILURE;
  }

  /* Obtains current time as seconds elapsed since the Epoch. */
  current_time = time(NULL);
  assert(current_time != ((time_t) -1));

  /* Writes current time to the binary file. */
  u64 = current_time;
  fwrite(&u64, sizeof(uint64_t), 1, ofp);

  /* Writes empty count to the binary file. */
  u8 = gd->empty_count;
  fwrite(&u8, sizeof(uint8_t), 1, ofp);

  /* Writes the feature count, and the array of feature id to the binary file. */
  u64 = gd->feature_cnt;
  fwrite(&u64, sizeof(uint64_t), 1, ofp);
  for (size_t i = 0; i < gd->feature_cnt; i++) {
    i16 = gd->features[i];
    fwrite(&i16, sizeof(int16_t), 1, ofp);
  }

  /* Writes the pattern count, and the array of pattern id to the binary file. */
  u64 = gd->pattern_cnt;
  fwrite(&u64, sizeof(uint64_t), 1, ofp);
  for (size_t i = 0; i < gd->pattern_cnt; i++) {
    i16 = gd->patterns[i];
    fwrite(&i16, sizeof(int16_t), 1, ofp);
  }

  /* Writes the weights to the binary file. */
  const uint16_t pattern_class_type = 1;
  for (size_t i = 0; i < gd->pattern_cnt; i++) {
    board_pattern_id_t pid = gd->patterns[i];
    for (board_pattern_index_t j = 0; j < board_patterns[pid].n_configurations; j++) {
      board_pattern_index_t principal;
      board_pattern_compute_principal_indexes(&principal, &j, &board_patterns[pid], true);
      uint32_t glm_variable_id = rglmdf_map_pid_and_piv_to_glm_vid(gd, pattern_class_type, pid, principal);
      if (glm_variable_id == RGLMDF_INVALID_GLM_VARIABLE_ID)
        w = 0.0;
      else
        w = gd->pattern_freq_summary.records[glm_variable_id].weight;
      fwrite(&w, sizeof(double), 1, ofp);
    }
  }

  return EXIT_SUCCESS;
}

int
rglmdf_read_general_data_from_binary_file (rglmdf_general_data_t *gd,
                                           char *filename,
                                           bool verbose)
{
  assert(gd);
  assert(filename);

  int ret;
  uint64_t u64, v64, *u64p, u64_a, u64_b;
  int16_t i16;
  uint8_t u8;
  char **cpp;
  board_feature_id_t *bfip;
  board_pattern_id_t *bpip;
  rglmdf_position_summary_record_t *psrp;
  rglmdf_pattern_freq_summary_record_t *pfsrp;
  rglmdf_solved_and_classified_gp_record_t *scgprp;
  uint32_t *iarrayp;
  double *farrayp;
  uint64_t data_chunk_size;
  size_t l, n;
  char buf[512];

  /* Checks that the file has not been corrupted. */
  ret = rglmdf_check_sha3_file_digest(filename);
  if (ret != EXIT_SUCCESS) return EXIT_FAILURE;

  /* Any previous data held by gd is discarded and deallocated. */
  rglmdf_general_data_release(gd);
  rglmdf_general_data_init(gd);

  /* Opens the binary file. */
  FILE *ifp = fopen(filename, "r");
  if (!ifp) {
    fprintf(stderr, "Unable to open binary input file: %s\n", filename);
    return EXIT_FAILURE;
  }

  /* Reads the A valid milestone. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1 || u64 != RGLMDF_VALID_A) {
    fprintf(stderr, "Error while reading the A valid milestone from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }

  /* Reads the version of the format used by the binary data file. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1 || u64 != RGLMDF_BINARY_DATA_FILE_FORMAT_VERSION) {
    fprintf(stderr, "Error while reading the RGLMDF_BINARY_DATA_FILE_FORMAT_VERSION from the input binary file.\n");
    fprintf(stderr, "  The file format version is %zu\n", u64);
    fprintf(stderr, "  The executable program is compatible with format version %zu\n",
            (uint64_t) RGLMDF_BINARY_DATA_FILE_FORMAT_VERSION);
    fclose(ifp);
    return EXIT_FAILURE;
  }

  /* Reads the file creation time field. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading time of write from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  rglmdf_set_file_creation_time(gd, u64);
  if (verbose) {
    rglmdf_get_file_creation_time_as_string(gd, buf);
    fprintf(stdout, "Input file started to be written on (UTC) %s\n", buf);
  }

  /* Reads the format data type. */
  l = fread(&u8, sizeof(uint8_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading format from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  rglmdf_set_format(gd, u8);
  if (verbose) rglmdf_format_to_text_stream(gd, stdout);

  /* Reads the batch_id_cnt, batch_ids input fields.*/
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading batch_id_cnt from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  v64 = rglmdf_set_batch_id_cnt(gd, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for batch_ids array.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  u64p = rglmdf_get_batch_ids(gd);
  l = fread(u64p, sizeof(uint64_t), u64, ifp);
  if (l != u64) {
    fprintf(stderr, "Error while reading batch_ids from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  if (verbose) {
    rglmdf_batch_ids_to_text_stream(gd, stdout);
  }

  /* Reads the empty_count input field.*/
  l = fread(&u8, sizeof(uint8_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading empty_count from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  rglmdf_set_empty_count(gd, u8);
  if (verbose) fprintf(stdout, "Selected empty_count value: %u\n", u8);

  /* Reads the position_status_cnt, position_statuses, position_status_buffer input fields.*/
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading position_status_cnt from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  v64 = rglmdf_set_position_status_cnt(gd, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for position_statuses array.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  cpp = rglmdf_get_position_statuses(gd);
  l = fread(*cpp, RGLMDF_POSITION_STATUS_BUF_SIZE, u64, ifp);
  if (l != u64) {
    fprintf(stderr, "Error while reading position_statuses from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  if (verbose) rglmdf_position_statuses_to_text_stream(gd, stdout);

  /* Reads the feature_cnt, features input fields.*/
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading feature_cnt from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  v64 = rglmdf_set_feature_cnt(gd, u64);
  if (v64 != u64) {
    fprintf(stderr, "Error in function rglmdf_set_feature_cnt() or unable to allocate memory for features array.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  bfip = rglmdf_get_features(gd);
  for (size_t i = 0; i < u64; i++) {
    l = fread(&i16, sizeof(int16_t), 1, ifp);
    if (l != 1) {
      fprintf(stderr, "Error while reading feature[%zu] from the input binary file.\n", i);
      fclose(ifp);
      return EXIT_FAILURE;
    }
    bfip[i] = i16;
  }
  if (verbose) rglmdf_features_to_text_stream(gd, stdout);

  /* Reads the pattern_cnt, patterns input fields.*/
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading pattern_cnt from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  v64 = rglmdf_set_pattern_cnt(gd, u64);
  if (v64 != u64) {
    fprintf(stderr, "Error in function rglmdf_set_pattern_cnt() or unable to allocate memory for patterns array.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  bpip = rglmdf_get_patterns(gd);
  for (size_t i = 0; i < u64; i++) {
    l = fread(&i16, sizeof(int16_t), 1, ifp);
    if (l != 1) {
      fprintf(stderr, "Error while reading pattern[%zu] from the input binary file.\n", i);
      fclose(ifp);
      return EXIT_FAILURE;
    }
    bpip[i] = i16;
  }
  if (verbose) rglmdf_patterns_to_text_stream(gd, stdout);

  /* Reads the B valid milestone. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1 || u64 != RGLMDF_VALID_B) {
    fprintf(stderr, "Error while reading the B valid milestone from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }

  /* Reads the position summary table. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading position_summary_ntuples from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  v64 = rglmdf_set_position_summary_ntuples(gd, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for the records of position summary table.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  psrp = rglmdf_get_position_summary_records(gd);
  l = fread(psrp, sizeof(rglmdf_position_summary_record_t), u64, ifp);
  if (l != u64) {
    fprintf(stderr, "Error while reading position_summary_table from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  if (verbose) rglmdf_position_summary_cnt_to_text_stream(gd, stdout);

  /* Reads the C valid milestone. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1 || u64 != RGLMDF_VALID_C) {
    fprintf(stderr, "Error while reading the C valid milestone from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }

  /* Reads the pattern frequency summary table. */
  l = fread(&u64_a, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading feature_ntuples from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  l = fread(&u64_b, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading pattern_ntuples from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading ntuples from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  v64 = rglmdf_set_entity_freq_summary_ntuples(gd, u64_a, u64_b, u64);
  if (v64 != u64) {
    fprintf(stderr, "Error in function rglmdf_set_entity_freq_summary_ntuples() or unable to allocate memory for the records of pattern freq summary table.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  pfsrp = rglmdf_get_pattern_freq_summary_records(gd);
  l = fread(pfsrp, sizeof(rglmdf_pattern_freq_summary_record_t), u64, ifp);
  if (l != u64) {
    fprintf(stderr, "Error while reading freq_summary_table from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  if (verbose) rglmdf_pattern_freq_summary_cnt_to_text_stream(gd, stdout);

  /* Reads the D valid milestone. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1 || u64 != RGLMDF_VALID_D) {
    fprintf(stderr, "Error while reading the D valid milestone from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }

  /* Creates the mapping betweeen (pattern_id, principal_index_value) --> glm_variable_id */
  rglmdf_build_reverse_map(gd);
  if (verbose) fprintf(stdout, "The reverse map \"(pattern_id, principal_index_value) --> glm_variable_id\" has been computed.\n");

  /* Reads the iarrai data type. */
  l = fread(&u8, sizeof(uint8_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading iarray_data_type from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }

  /* Reads the number of record for the solved and classified game position table. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1) {
    fprintf(stderr, "Error while reading positions_ntuples from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  v64 = rglmdf_set_positions_ntuples(gd, u64, u8);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for the positions table.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }
  n = u64;

  /* Reads the E valid milestone. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1 || u64 != RGLMDF_VALID_E) {
    fprintf(stderr, "Error while reading the E valid milestone from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }

  /* Reads the sequence of chunk of records. Each chunk is organized as: chunk size n, n records, n irecords. */
  scgprp = rglmdf_get_positions_records(gd);
  farrayp = rglmdf_get_positions_farray(gd);
  iarrayp = rglmdf_get_positions_iarray(gd);
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(gd);
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(gd);
  size_t n_record_read = 0;
  for (;;) {
    l = fread(&data_chunk_size, sizeof(uint64_t), 1, ifp);
    if (l != 1) {
      fprintf(stderr, "Error while reading positions table data_chunk_size value.\n");
      fclose(ifp);
      return EXIT_FAILURE;
    }
    n_record_read += data_chunk_size;
    if (n_record_read > n) {
      fprintf(stderr, "Data chunks cumulated so far are more than expected.\n");
      fclose(ifp);
      return EXIT_FAILURE;
    }
    if (data_chunk_size == 0) {
      if (n_record_read != n) {
        fprintf(stderr, "Data chunks being read are less than expected.\n");
        fprintf(stderr, "Expected = %lu, number of record read = %zu\n", n, n_record_read);
        fclose(ifp);
        return EXIT_FAILURE;
      }
      break;
    }
    l = fread(scgprp, sizeof(rglmdf_solved_and_classified_gp_record_t), data_chunk_size, ifp);
    if (l != data_chunk_size) {
      fprintf(stderr, "Error while reading positions_table data chunk from the input binary file.\n");
      fclose(ifp);
      return EXIT_FAILURE;
    }
    if (nf != 0) {
      l = fread(farrayp, sizeof(double) * nf, data_chunk_size, ifp);
      if (l != data_chunk_size) {
        fprintf(stderr, "Error while reading positions_table farray data chunk from the input binary file.\n");
        fclose(ifp);
        return EXIT_FAILURE;
      }
    }
    if (ni != 0) {
      l = fread(iarrayp, sizeof(uint32_t) * ni, data_chunk_size, ifp);
      if (l != data_chunk_size) {
        fprintf(stderr, "Error while reading positions_table iarray data chunk from the input binary file.\n");
        fclose(ifp);
        return EXIT_FAILURE;
      }
    }
    scgprp += data_chunk_size;
    farrayp += nf * data_chunk_size;
    iarrayp += ni * data_chunk_size;

    /* Reads the F valid milestone. */
    l = fread(&u64, sizeof(uint64_t), 1, ifp);
    if (l != 1 || u64 != RGLMDF_VALID_F) {
      fprintf(stderr, "Error while reading the F valid milestone from the input binary file.\n");
      fclose(ifp);
      return EXIT_FAILURE;
    }
  }
  if (verbose) fprintf(stdout, "All %zu solved and classified game positions has been read succesfully.\n", n_record_read);

  /* Reads the G valid milestone. */
  l = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (l != 1 || u64 != RGLMDF_VALID_G) {
    fprintf(stderr, "Error while reading the G valid milestone from the input binary file.\n");
    fclose(ifp);
    return EXIT_FAILURE;
  }

  /* Closes the binary file. */
  fclose(ifp);

  return EXIT_SUCCESS;
}

static size_t
fwrite2 (const void *ptr,
         size_t size,
         size_t nmemb,
         FILE *stream,
         size_t *written_byte_count)
{
  size_t n;
  n = fwrite(ptr, size, nmemb, stream);
  *written_byte_count = n * nmemb;
  return n;
}

int
rglmdf_write_general_data_to_binary_file (rglmdf_general_data_t *gd,
                                          char *filename,
                                          time_t time)
{
  assert(gd);
  assert(filename);

  uint8_t u8;
  uint64_t u64;
  int16_t i16;

  size_t ntuples_written;
  size_t ntuples_to_write;
  size_t fwn;

  rglmdf_solved_and_classified_gp_record_t *rp;
  double *fap;
  uint32_t *iap;

  FILE *ofp = fopen(filename, "w");
  if (!ofp) {
    fprintf(stderr, "Unable to open binary output file: %s\n", filename);
    return EXIT_FAILURE;
  }

  /* Progressive count of bytes written to file. */
  fwn = 0;

  /* Writes the A valid milestone. */
  u64 = RGLMDF_VALID_A;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);

  /* Writes the version of the format used by the binary data file. */
  u64 = RGLMDF_BINARY_DATA_FILE_FORMAT_VERSION;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);

  /* Writes time to the binary file. */
  u64 = time;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);

  /* Writes the format to binary file. */
  u8 = gd->format;
  fwrite2(&u8, sizeof(uint8_t), 1, ofp, &fwn);

  /* Writes count of batch ids, and the batch_ids array to the binary file. */
  u64 = gd->batch_id_cnt;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  fwrite2(gd->batch_ids, sizeof(uint64_t), gd->batch_id_cnt, ofp, &fwn);

  /* Writes empty count to the binary file. */
  u8 = gd->empty_count;
  fwrite2(&u8, sizeof(uint8_t), 1, ofp, &fwn);

  /* Writes the count of position status, and the text buffer containing the terminated strings. */
  u64 = gd->position_status_cnt;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  fwrite2(gd->position_status_buffer, RGLMDF_POSITION_STATUS_BUF_SIZE, gd->position_status_cnt, ofp, &fwn);

  /* Writes the feature count, and the array of feature id to the binary file. */
  u64 = gd->feature_cnt;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  for (size_t i = 0; i < gd->feature_cnt; i++) {
    i16 = gd->features[i];
    fwrite2(&i16, sizeof(int16_t), 1, ofp, &fwn);
  }

  /* Writes the pattern count, and the array of pattern id to the binary file. */
  u64 = gd->pattern_cnt;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  for (size_t i = 0; i < gd->pattern_cnt; i++) {
    i16 = gd->patterns[i];
    fwrite2(&i16, sizeof(int16_t), 1, ofp, &fwn);
  }

  /* Writes the B valid milestone. */
  u64 = RGLMDF_VALID_B;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);

  /* Writes the statistics for game positions to the binary file ( position_summary table ). */
  u64 = gd->position_summary.ntuples;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  fwrite2(gd->position_summary.records, sizeof(rglmdf_position_summary_record_t), gd->position_summary.ntuples, ofp, &fwn);

  /* Writes the C valid milestone. */
  u64 = RGLMDF_VALID_C;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);

  /* Writes the statistics for pattern indexes to the binary file ( pattern_freq_summary table ). */
  u64 = gd->pattern_freq_summary.glm_f_variable_cnt;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  u64 = gd->pattern_freq_summary.glm_p_variable_cnt;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  u64 = gd->pattern_freq_summary.ntuples;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  fwrite2(gd->pattern_freq_summary.records, sizeof(rglmdf_pattern_freq_summary_record_t), gd->pattern_freq_summary.ntuples, ofp, &fwn);

  /* Writes the D valid milestone. */
  u64 = RGLMDF_VALID_D;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);

  /* Writes the header data of the classified and resolved game positions to the binary file. */
  u8 = gd->positions.iarray_data_type;
  fwrite2(&u8, sizeof(uint8_t), 1, ofp, &fwn);
  u64 = gd->positions.ntuples;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  ntuples_written = 0;
  ntuples_to_write = gd->positions.ntuples;
  rp = gd->positions.records;
  fap = gd->positions.farray;
  iap = gd->positions.i0array;

  /* Writes the E valid milestone. */
  u64 = RGLMDF_VALID_E;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);

  /* Writes the classified and resolved game positions to the binary file. */
  for (;;) {
    u64 = ntuples_to_write > RGLMDF_GPS_DATA_CHUNK_SIZE ? RGLMDF_GPS_DATA_CHUNK_SIZE : ntuples_to_write;
    fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
    if (u64 == 0) {
      if (ntuples_written != gd->positions.ntuples) {
        fprintf(stderr, "The number of game positions being written doesn't match the value gd->positions.ntuples.\n");
        return EXIT_FAILURE;
      }
      break;
    }
    fwrite2(rp, sizeof(rglmdf_solved_and_classified_gp_record_t), u64, ofp, &fwn);
    fwrite2(fap, sizeof(double) * gd->positions.n_fvalues_per_record, u64, ofp, &fwn);
    fwrite2(iap, sizeof(uint32_t) * gd->positions.n_index_values_per_record, u64, ofp, &fwn);
    ntuples_written += u64;
    ntuples_to_write -= u64;
    rp += u64;
    fap += u64 * gd->positions.n_fvalues_per_record;
    iap += u64 * gd->positions.n_index_values_per_record;

    /* Writes the F valid milestone. */
    u64 = RGLMDF_VALID_F;
    fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);
  }

  /* Writes the G valid milestone. */
  u64 = RGLMDF_VALID_G;
  fwrite2(&u64, sizeof(uint64_t), 1, ofp, &fwn);

  /* Closes the binary file. */
  fclose(ofp);

  /* Computes the SHA3 digest. */
  int ret_code;
  ret_code = rglmdf_generate_sha3_file_digest(filename);
  if (ret_code != 0) {
    fprintf(stderr, "Unable to compute and write SHA3-256 digest file.\n");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
