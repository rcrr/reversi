/**
 * @file
 *
 * @brief Reversi Generalized Linear Model data files.
 * @details This module defines data files, tables, and records being read and written to/from binary files.
 *
 * The `regab` program may extract data from the REGAB database and generate an appropriate binary file.
 * This file may be read using the `rglm` program.
 * Please consult the documentation of the two programs for further details on their usage.
 *
 * The ` Solved and Patern Classified Set of Game Positions` is stored in a binary file using
 * the format here described.
 * <p>
 * --- Header ---
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `time_t` value.<br>
 *     Meaning: the file creation time.<br>
 *     Ref: `file_creation_time`, scalar.<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count for batch_id entries.<br>
 *     Ref: `batch_id_cnt`, scalar.<br>
 *   - `8 bytes` field x `batch_id_cnt` times, read/written as multiple times `uint64_t`, not converted.<br>
 *     Meaning: array of batch_id entries.<br>
 *     Ref: `barch_ids`, array.<br>
 *   - `1 byte` field, read/written as `uint8_t`, not converted.<br>
 *     Meaning: board empty square count.<br>
 *     Ref: `empty_count`, scalar.<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count of status entries.<br>
 *     Ref: `position_status_cnt`, scalar.<br>
 *   - `RGLM_POSITION_STATUS_BUF_SIZE bytes` field x `position_status_cnt` times, read/written as multiple times `char[RGLM_POSITION_STATUS_BUF_SIZE * position_status_cnt]`, not converted.<br>
 *     Meaning: buffer of position status entries.<br>
 *     Ref: `position_status_buffer`, array.<br>
 *     Macro `RGLM_POSITION_STATUS_BUF_SIZE` is defined as `4`<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count of patterns.<br>
 *     Ref: `pattern_cnt`, scalar.<br>
 *   - `2 bytes` field x `pattern_cnt` times, read/written as multiple times `int16_t`, converted to `enum`.<br>
 *     Meaning: array of pattern entries.<br>
 *     Ref: `patterns`, array.<br>
 * <p>
 * --- Summary Tables. Position Summary Table, Pattern Frequency Summary Table. ---
 *   - `8 bytes` field, read/written as `uint64_t`, converted to `size_t` value.<br>
 *     Meaning: count of records in the position summary table.<br>
 *     Ref: `position_summary.ntuples`<br>
 *   - `1 rglmdf_position_summary_record_t` field x `position_summary.ntuples` times.<br>
 *     Meaning: the set of records in the table.<br>
 *     Ref: `position_summary.records`, array.<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to `size_t` value.<br>
 *     Meaning: count of records in the pattern frequency summary table.<br>
 *     Ref: `pattern_freq_summary.ntuples`<br>
 *   - `1 rglmdf_pattern_freq_summary_record_t` field x `pattern_freq_summary.ntuples` times.<br>
 *     Meaning: the set of records in the table.<br>
 *     Ref: `pattern_freq_summary.records`, array.<br>
 * <p>
 * --- Game Positions ---
 *   - `8 bytes` field, read/written as `uint64_t`, converted to `size_t` value.<br>
 *     Meaning: count of records in the game position table.<br>
 *     Ref: `gps_data.ntuples`<br>
 *   - Sequence of data chunks:<br>
 *      - `8 bytes` field, read/written as `uint64_t`, converted to `size_t` value.<br>
 *        Meaning: data chunk size ( number of game positions in this data chunk ).<br>
 *        Ref: `data_chunk_size`
 *      - `1 rglmdf_solved_and_classified_gp_record_t` field x `data_chunk_size` times.<br>
 *        Meaning: the set of records in the data chunk.<br>
 *        Ref: `gps_data.records`, array ( each data chunk is collected in the array following the previos one ).<br>
 *      - ABC<br>
 *        tbd<br>
 *   - XYZ
 *
 * @todo Complete the verbose logging in rglm program
 *
 * @todo Change data types not written to file to size_t
 *
 * @todo Complete file format documentation
 *
 * @todo Complete documentation in rglm_data_files.h
 *
 * @todo Write INDEX_VALUE to RGLM_VARIABLE conversion function
 *
 * @todo Add output flags in rglm for different data tables:
 *       - game position
 *       - game position translated to RGLM_VARIABLES
 *       - Summary Tables
 *
 * @todo Write SHA hash signature for the files
 *
 * @todo Verify memory leaks
 *
 * @par rglm_data_files.h
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

#ifndef RGLM_DATA_FILES_H
#define RGLM_DATA_FILES_H

/* Position status has length 3, plus one char for proper termination. */
#define RGLM_POSITION_STATUS_BUF_SIZE 4

#include <inttypes.h>
#include <time.h>

#include "board_pattern.h"

/*
 * Structures used to write and read binary files relay on "fixed" size fields, this is to make the code
 * more robust in case of an architectural change. Saved files should be readable also by different
 * hardware and software platforms.
 */

/**
 * @brief Reversi GLM data file record definition for the position summary table.
 *
 * @details Each record is identified by the `batch_id` and the `status` as defined
 * in the REGAB data base in table `regab_prng_gp`.
 *
 * The field `game_position_cnt` is the count of game positions being available in the
 * REGAB database that are matching the criteria `(batch_id, status)`.
 * The field `classified_cnt` is the count of solved and classified game positions stored
 * in the file and categorized by `batch_id` and `status`.
 * The two count could differ when records in table `regab_prng_gp` do not have a relative
 * one into table `regab_prng_gp_pattern_class` (it means that the game position is not classified).
 *
 * The record has a fized size and needs 24 bytes.
 */
typedef struct rglmdf_position_summary_record_s {
  int32_t batch_id;                             /**< @brief Key field, maps the batch_id::regab_prng_gp field in the REGAB database. */
  char status[RGLM_POSITION_STATUS_BUF_SIZE];   /**< @brief Key field, maps the status::regab_prng_gp field in the REGAB database. */
  int64_t game_position_cnt;                    /**< @brief Count of game position matching the key values in the regab_prng_gp table. */
  int64_t classified_cnt;                       /**< @brief Count of classified game position in table regab_prng_pattern_class referenced by the selection. */
} rglmdf_position_summary_record_t;

/**
 * @brief Reversi GLM data file table holding the summary of game positions.
 *
 * @details The table contains the count of game positions grouped by batch_is and status.
 */
typedef struct rglmdf_position_summary_table_s {
  uint64_t ntuples;                            /**< @brief Number of records. */
  rglmdf_position_summary_record_t *records;   /**< @brief Records of the table. */
} rglmdf_position_summary_table_t;

/**
 * @brief Reversi GLM data file record definition for the pattern frequency summary table.
 *
 * @details Each record is identified by the `glm_variable_id`.
 *
 * The table has a unique key on the two fields  `pattern_id` and `principal_index_value`, each record
 * is a bi-directional map between `glm_variable_id` and the fields `(pattern_id, principal_index_value)`.
 *
 * The set of records is generated by a group by operation on the REGAB database table `regab_prng_gp_pattern_class`
 * on the records matching the selection criteria.
 *
 * The group by operation executes three sums collecting the aggregated data stored into the fields `total_cnt`,
 * `relative_frequency`, `theoretical_probability`.
 *
 * The record has a fized size and needs 40 bytes.
 */
typedef struct rglmdf_pattern_freq_summary_record_s {
  int64_t glm_variable_id;         /**< @brief It is the unique variable index for the GLM (Generalized Linear Model). */
  int32_t pattern_id;              /**< @brief Board Pattern Id, as defined by REGAB table regab_prng_patterns. */
  int32_t principal_index_value;   /**< @brief Principal Index Value for Pattern as defined by REGAB table regab_prng_pattern_ranges. */
  int64_t total_cnt;               /**< @brief Number of times that the pattern, or its mirror, is found  in the game position selection. */
  double relative_frequency;       /**< @brief Relative frequency of the pattern in the data. */
  double theoretical_probability;  /**< @brief Expected probability. */
} rglmdf_pattern_freq_summary_record_t;

/**
 * @brief Reversi GLM data file table holding the summary of pattern frequencies.
 *
 * @details The table contains the count of pattern occurrencies grouped by (pattern_id, principal_index_value).
 */
typedef struct rglmdf_pattern_freq_summary_table_s {
  uint64_t ntuples;                                /**< @brief Number of records. */
  rglmdf_pattern_freq_summary_record_t *records;   /**< @brief Records of the table. */
} rglmdf_pattern_freq_summary_table_t;

/**
 * @brief Reversi GLM data file record definition for the solved and classified game position table.
 *
 * @details Each record is identified by either the `row_n` counter or the `gp_id` field.
 *
 * NEEDS MORE EXPLANATION ....
 */
typedef struct rglmdf_solved_and_classified_gp_record_s {
  int64_t row_n;       /**< @brief Row number. */
  int64_t gp_id;       /**< @brief Game Position Id, as defined by REGAB table regab_prng_gp. */
  int64_t mover;       /**< @brief Game position board definition for mover. */
  int64_t opponent;    /**< @brief Game position board definition for opponent. */
  int8_t game_value;   /**< @brief Game value for the position. */
  uint32_t *ivalues;   /**< @brief Array of pattern index values, or glm variables. */
} rglmdf_solved_and_classified_gp_record_t;

/**
 * @brief Reversi GLM data file table holding the game positons being solved and classified.
 *
 * @details The table is build with a record having a fixed set of fields organized into the
 *          the type #rglmdf_solved_and_classified_gp_record_t, and a variable set of index values
 *          that depends on the pattern set.
 *          The index values are collected into the `iarray` field that is dynamically allocated with a size
 *          equal to `ntuples` multiplied by `n_index_values_per_record`.
 *          Each record has a pointer `ivalues` that for convinience references the first index value for the
 *          game position.
 *          Index values are ordered by `(pattern_id, instance_number)`.
 */
typedef struct rglmdf_solved_and_classified_gp_table_s {
  uint64_t ntuples;                                    /**< @brief Number of records. */
  size_t n_index_values_per_record;                    /**< @brief */
  rglmdf_solved_and_classified_gp_record_t *records;   /**< @brief Records of the table. */
  uint32_t *iarray;                                    /**< @brief */
} rglmdf_solved_and_classified_gp_table_t;

/**
 * @brief Reversi GLM data file general data structure.
 *
 * @details The type contains all the info saved/retrieved from a GLM Data File.
 */
typedef struct rglmdf_general_data_s {
  time_t file_creation_time;                                  /**< @brief Creation time of the data file. */
  size_t batch_id_cnt;                                        /**< @brief Count of batch id entries. */
  uint64_t *batch_ids;                                        /**< @brief Array of batch_id values. */
  uint8_t empty_count;                                        /**< @brief Empty square count. */
  size_t position_status_cnt;                                 /**< @brief Count of statuses of game positions. */
  char *position_status_buffer;                               /**< @brief Text buffer for status labels. */
  char **position_statuses;                                   /**< @brief Array of terminated strings representing status labels. */
  size_t pattern_cnt;                                         /**< @brief Count of patterns. */
  board_pattern_id_t *patterns;                               /**< @brief Array of patterns. */
  rglmdf_position_summary_table_t position_summary;           /**< @brief Aggregated data for positions. */
  rglmdf_pattern_freq_summary_table_t pattern_freq_summary;   /**< @brief Aggregated data for pattern frequencies. */
  rglmdf_solved_and_classified_gp_table_t positions;          /**< @brief TAble of game positions. */
} rglmdf_general_data_t;

extern bool
rglmdf_verify_type_sizes (void);

/**
 * @brief Initializes the general data structure.
 *
 * @details Variables are set as follow:
 *  - `file_creation_time` is set to zero (Thu Jan  1 00:0:00 1970 (UTC))
 *  - `batch_id_cnt` is set to zero
 *  - `batch_ids` is set to `NULL`
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] gd reference to the general data structure
 */
extern void
rglmdf_general_data_init (rglmdf_general_data_t *gd);

/**
 * @brief Sets the attribute field `file_creation_time`.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] gd reference to the general data structure
 * @param [in]     t  new updated value
 */
extern void
rglmdf_set_file_creation_time (rglmdf_general_data_t *gd,
                               time_t t);

/**
 * @brief Gets the attribute field `file_creation_time`.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the value of the field
 */
extern time_t
rglmdf_get_file_creation_time (rglmdf_general_data_t *gd);

/**
 * @brief Translates the field `file_creation_time` as a character string.
 *
 * @details Conversion happens as UTC value.
 *          The generated string is saved in the `buf` array.
 *          The buffer referenced by `buf` must be longer than `26` chars.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `buf` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  gd  reference to the general data structure
 * @param [out] buf reference to the character buffer
 */
extern void
rglmdf_get_file_creation_time_as_string (rglmdf_general_data_t *gd,
                                         char *buf);

/**
 * @brief Allocates memory for the batch_ids array.
 *
 * @details Allocates memory for the batch_id array calling malloc.
 *          If allocation is succesfull sets the batch_id_cnt value and returns it.
 *          Allocated memory is zeroed.
 *          If the batch_ids array was already allocated it is freed.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in, out] gd  reference to the general data structure
 * @param [in]      cnt batch id count
 * @return              on success `cnt` otherwise zero
 */
extern size_t
rglmdf_set_batch_id_cnt (rglmdf_general_data_t *gd,
                         size_t cnt);

extern size_t
rglmdf_get_batch_id_cnt (rglmdf_general_data_t *gd);

extern uint64_t *
rglmdf_get_batch_ids (rglmdf_general_data_t *gd);

extern void
rglmdf_batch_ids_to_text_stream (rglmdf_general_data_t *gd,
                                 FILE *stream);

extern void
rglmdf_general_data_release (rglmdf_general_data_t *gd);

extern uint8_t
rglmdf_get_empty_count (rglmdf_general_data_t *gd);

extern void
rglmdf_set_empty_count (rglmdf_general_data_t *gd,
                        uint8_t empty_count);

extern size_t
rglmdf_set_position_status_cnt (rglmdf_general_data_t *gd,
                                size_t cnt);

extern size_t
rglmdf_get_position_status_cnt (rglmdf_general_data_t *gd);

extern char **
rglmdf_get_position_statuses (rglmdf_general_data_t *gd);

extern void
rglmdf_position_statuses_to_text_stream (rglmdf_general_data_t *gd,
                                         FILE *stream);

extern size_t
rglmdf_set_pattern_cnt (rglmdf_general_data_t *gd,
                        size_t cnt);

extern size_t
rglmdf_get_pattern_cnt (rglmdf_general_data_t *gd);

extern board_pattern_id_t *
rglmdf_get_patterns (rglmdf_general_data_t *gd);

extern void
rglmdf_patterns_to_text_stream (rglmdf_general_data_t *gd,
                                FILE *stream);

extern size_t
rglmdf_set_position_summary_ntuples (rglmdf_general_data_t *gd,
                                     size_t ntuples);

extern size_t
rglmdf_get_position_summary_ntuples (rglmdf_general_data_t *gd);

extern rglmdf_position_summary_record_t *
rglmdf_get_position_summary_records (rglmdf_general_data_t *gd);

extern void
rglmdf_position_summary_cnt_to_text_stream (rglmdf_general_data_t *gd,
                                            FILE *stream);

extern size_t
rglmdf_set_pattern_freq_summary_ntuples (rglmdf_general_data_t *gd,
                                         size_t ntuples);

extern size_t
rglmdf_get_pattern_freq_summary_ntuples (rglmdf_general_data_t *gd);

extern rglmdf_pattern_freq_summary_record_t *
rglmdf_get_pattern_freq_summary_records (rglmdf_general_data_t *gd);

extern void
rglmdf_pattern_freq_summary_cnt_to_text_stream (rglmdf_general_data_t *gd,
                                                FILE *stream);

extern size_t
rglmdf_set_positions_ntuples (rglmdf_general_data_t *gd,
                              size_t ntuples);

extern size_t
rglmdf_get_positions_ntuples (rglmdf_general_data_t *gd);

extern rglmdf_solved_and_classified_gp_record_t *
rglmdf_get_positions_records (rglmdf_general_data_t *gd);

extern size_t
rglmdf_get_positions_n_index_values_per_record (rglmdf_general_data_t *gd);

extern uint32_t *
rglmdf_get_positions_iarray (rglmdf_general_data_t *gd);

#endif /* RGLM_DATA_FILES_H */
