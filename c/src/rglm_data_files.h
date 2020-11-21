/**
 * @file
 *
 * @brief Reversi Generalized Linear Model data files.
 * @details This module defines data files, tables, and records being read and written to/from binary files.
 *
 * The `regab` program can extract data from the REGAB database and generate an appropriate binary file.
 * This file can be read using the `rglm` program.
 * Please consult the documentation of the two programs for further details on their usage.
 *
 * Structures used to write and read binary files relay on "fixed" size fields, this is to make the code
 * more robust in case of an architectural change. Saved files should be readable also by different
 * hardware and software platforms.
 * The function #rglmdf_verify_type_sizes() checks that the data types have the expected storage definition.
 * The function #rglmdf_get_endianness() returns the "endianess" of the system, only little endian systems has been tested so far.
 *
 * The `Solved and Pattern Classified Set of Game Positions` is stored in a binary file using
 * the format here described.
 * <p>
 * --- Header ---
 *   - `8 bytes` field, read/written as `uint64_t`, not converted.<br>
 *     Meaning: the read value has to be equal to `RGLMDF_VALID_A`, it is a formal validity check, the value is not used.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, not converted.<br>
 *     Meaning: the read value has to be equal to `RGLMDF_BINARY_DATA_FILE_FORMAT_VERSION`, it is a formal validity check, the value is not used.<br>
 *<br>
 *   - `1 byte` field, read/written as `uint8_t`, converted to `rglmdf_file_data_format_type_t` value.<br>
 *     Meaning: the format of the binary data file.<br>
 *     Ref: `format`, scalar.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `time_t` value.<br>
 *     Meaning: the file creation time.<br>
 *     Ref: `file_creation_time`, scalar.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count for batch_id entries.<br>
 *     Ref: `batch_id_cnt`, scalar.<br>
 *<br>
 *   - `8 bytes` field x `batch_id_cnt` times, read/written as multiple times `uint64_t`, not converted.<br>
 *     Meaning: array of batch_id entries.<br>
 *     Ref: `batch_ids`, array.<br>
 *<br>
 *   - `1 byte` field, read/written as `uint8_t`, not converted.<br>
 *     Meaning: board empty square count.<br>
 *     Ref: `empty_count`, scalar.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count of status entries.<br>
 *     Ref: `position_status_cnt`, scalar.<br>
 *<br>
 *   - `RGLMDF_POSITION_STATUS_BUF_SIZE bytes` field x `position_status_cnt` times, read/written as multiple times `char[RGLMDF_POSITION_STATUS_BUF_SIZE * position_status_cnt]`, not converted.<br>
 *     Meaning: buffer of position status entries.<br>
 *     Ref: `position_status_buffer`, array.<br>
 *     Macro `RGLMDF_POSITION_STATUS_BUF_SIZE` is defined being equal to `4`<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count of features.<br>
 *     Ref: `feature_cnt`, scalar.<br>
 *<br>
 *   - `2 bytes` field x `feature_cnt` times, read/written as multiple times `int16_t`, converted to `enum`.<br>
 *     Meaning: array of feature entries.<br>
 *     Ref: `features`, array.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count of patterns.<br>
 *     Ref: `pattern_cnt`, scalar.<br>
 *<br>
 *   - `2 bytes` field x `pattern_cnt` times, read/written as multiple times `int16_t`, converted to `enum` #board_pattern_id_t.<br>
 *     Meaning: array of pattern entries.<br>
 *     Ref: `patterns`, array.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, not converted.<br>
 *     Meaning: the read value has to be equal to `RGLMDF_VALID_B`, it is a formal validity check, the value is not used.<br>
 * <p>
 * --- Summary Tables. Position Summary Table, Pattern Frequency Summary Table. ---
 *   - `8 bytes` field, read/written as `uint64_t`, converted to `size_t` value.<br>
 *     Meaning: count of records in the position summary table.<br>
 *     Ref: `position_summary.ntuples`, scalar.<br>
 *<br>
 *   - `1 rglmdf_position_summary_record_t` field x `position_summary.ntuples` times.<br>
 *     Meaning: the set of records in the table.<br>
 *     Ref: `position_summary.records`, array.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, not converted.<br>
 *     Meaning: the read value has to be equal to `RGLMDF_VALID_C`, it is a formal validity check, the value is not used.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count of GLM independent variables belonging to features.<br>
 *     Ref: `pattern_freq_summary.glm_f_variable_cnt`, scalar.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to a `size_t` value.<br>
 *     Meaning: count of GLM independent variables belonging to patterns.<br>
 *     Ref: `pattern_freq_summary.glm_p_variable_cnt`, scalar.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to `size_t` value.<br>
 *     Meaning: count of records in the pattern frequency summary table.<br>
 *     Ref: `pattern_freq_summary.ntuples`, scalar.<br>
 *<br>
 *   - `1 rglmdf_pattern_freq_summary_record_t` field x `pattern_freq_summary.ntuples` times.<br>
 *     Meaning: the set of records in the table.<br>
 *     Ref: `pattern_freq_summary.records`, array.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, not converted.<br>
 *     Meaning: the read value has to be equal to `RGLMDF_VALID_D`, it is a formal validity check, the value is not used.<br>
 * <p>
 * --- Game Positions ---
 *   - `1 byte` field, read/written as `uint8_t`, converted to `rglmdf_iarray_data_type_t` value.<br>
 *     Meaning: type of data stored into iarray fields.<br>
 *     Ref: `iarray_data_type`, scalar.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, converted to `size_t` value.<br>
 *     Meaning: count of records in the game position table.<br>
 *     Ref: `positions.ntuples`, scalar.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, not converted.<br>
 *     Meaning: the read value has to be equal to `RGLMDF_VALID_E`, it is a formal validity check, the value is not used.<br>
 *<br>
 *   - Sequence of data chunks:<br>
 *      - `8 bytes` field, read/written as `uint64_t`, converted to `size_t` value.<br>
 *        Meaning: data chunk size ( number of game positions in this data chunk ).<br>
 *        Ref: `data_chunk_size`, scalar.<br>
 *        Notice: when `data_chunk_size` is equal to zero the sequence is exhausted.<br>
 *<br>
 *      - `1 rglmdf_solved_and_classified_gp_record_t` field x `data_chunk_size` times.<br>
 *        Meaning: the set of records in the data chunk.<br>
 *        Ref: `positions.records`, array ( each data chunk is collected in the array following the previos one ).<br>
 *<br>
 *      - `double` field x `n_fvalues_per_record` x `data_chunk_size` times.<br>
 *        Meaning: the array of feature values in the data chunk.<br>
 *        Ref: `positions.farray`, array ( each data chunk is collected in the array following the previos one ).<br>
 *        Notice: the value `n_fvalues_per_record` is computed summing up the field count for each feature being selected.<br>
 *<br>
 *      - `uint32_t` field x `n_index_values_per_record` x `data_chunk_size` times.<br>
 *        Meaning: the array of pattern index values in the data chunk.<br>
 *        Ref: `positions.iarray`, array ( each data chunk is collected in the array following the previos one ).<br>
 *        Notice: the value `n_index_values_per_record` is computed summing up the field count for each pattern being selected.<br>
 *<br>
 *      - `8 bytes` field, read/written as `uint64_t`, not converted.<br>
 *        Meaning: the read value has to be equal to `RGLMDF_VALID_F`, it is a formal validity check, the value is not used.<br>
 *<br>
 *   - `8 bytes` field, read/written as `uint64_t`, not converted.<br>
 *     Meaning: the read value has to be equal to `RGLMDF_VALID_G`, it is a formal validity check, the value is not used.<br>
 *<br>
 *
 * @par rglm_data_files.h
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

#ifndef RGLM_DATA_FILES_H
#define RGLM_DATA_FILES_H

/*
 * The version of the format used by the binary data file.
 * Every time that the file format is modified, this value has to be incremented.
 * Adopting consistently this practice, enforce the consistency of existing files
 * with new version of the executables running the rglmdf_read_general_data_from_binary_file()
 * function.
 */
#define RGLMDF_BINARY_DATA_FILE_FORMAT_VERSION 1

/* 64 bits values used as a "sanity check" in reading and writing binary files. */
#define RGLMDF_VALID_A 0x289fab30715d828c
#define RGLMDF_VALID_B 0xa947d47ef2c6510e
#define RGLMDF_VALID_C 0x502774e7b1ac9730
#define RGLMDF_VALID_D 0x820cc6eab481d935
#define RGLMDF_VALID_E 0x07f9ba20206c8901
#define RGLMDF_VALID_F 0x686d3d28511bc982
#define RGLMDF_VALID_G 0x2976a5fa6c200816

/* Position status has length 3, plus one char for proper termination. */
#define RGLMDF_POSITION_STATUS_BUF_SIZE 4

/* This is a conventional out of range value. */
#define RGLMDF_INVALID_GLM_VARIABLE_ID UINT32_MAX

/* The number of records into a data chunk. */
#define RGLMDF_GPS_DATA_CHUNK_SIZE 4096

#include <inttypes.h>
#include <time.h>

#include "board_pattern.h"

/**
 * @enum rglmdf_file_data_format_type_t
 * @brief Format of the binary file.
 *
 * @details The data file can have two distinct formats:
 *          - The general format
 *          - The game positions format
 *
 *          The general format contains features or patterns, and by consequence it has glm variables.
 *          Variables then imply the summary tables.
 *
 *          The game position format doesn't have features, patterns, variables and the summary tables.
 */
typedef enum {
  RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL,              /**< File format having the RGLM variables. */
  RGLMDF_FILE_DATA_FORMAT_TYPE_IS_POSITIONS,            /**< File format with just the positions. */
  RGLMDF_FILE_DATA_FORMAT_TYPE_IS_INVALID               /**< Not a valid format type. */
} rglmdf_file_data_format_type_t;

/**
 * @enum rglmdf_iarray_data_type_t
 * @brief Meaning of the data stored into the iarray field.
 */
typedef enum {
  RGLMDF_IARRAY_IS_INDEX,              /**< Pattern configuration index value. */
  RGLMDF_IARRAY_IS_PRINCIPAL_INDEX,    /**< Pattern configuration principal index value. */
  RGLMDF_IARRAY_IS_GLM_VARIABLE_ID,    /**< Generalized linear model variable id. */
  RGLMDF_IARRAY_IS_MISSING             /**< There are no iarray fields in the record. */
} rglmdf_iarray_data_type_t;

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
 * The record has a fixed size and needs 24 bytes.
 */
typedef struct rglmdf_position_summary_record_s {
  int32_t batch_id;                               /**< @brief Key field, maps the batch_id::regab_prng_gp field in the REGAB database. */
  char status[RGLMDF_POSITION_STATUS_BUF_SIZE];   /**< @brief Key field, maps the status::regab_prng_gp field in the REGAB database. */
  int64_t game_position_cnt;                      /**< @brief Count of game position matching the key values in the regab_prng_gp table. */
  int64_t classified_cnt;                         /**< @brief Count of classified game position in table regab_prng_pattern_class referenced by the selection. */
} rglmdf_position_summary_record_t;

/**
 * @brief Reversi GLM data file table holding the summary of game positions.
 *
 * @details The table contains the count of game positions grouped by batch_is and status.
 */
typedef struct rglmdf_position_summary_table_s {
  size_t ntuples;                              /**< @brief Number of records. */
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
 * The weight is set to `0.0` when the model is not yet optimized.
 *
 * The record has a fixed size and needs 48 bytes.
 */
typedef struct rglmdf_pattern_freq_summary_record_s {
  int64_t glm_variable_id;         /**< @brief It is the unique variable index for the GLM (Generalized Linear Model). */
  int16_t entity_class;            /**< @brief It is a value in board_entity_class_tg enum. */
  int16_t pattern_id;              /**< @brief Board Pattern Id, as defined by REGAB table regab_prng_patterns. */
  int32_t principal_index_value;   /**< @brief Principal Index Value for Pattern as defined by REGAB table regab_prng_pattern_ranges. */
  int64_t total_cnt;               /**< @brief Number of times that the pattern, or its mirror, is found in the game position selection. */
  double relative_frequency;       /**< @brief Relative frequency of the pattern configuration in the data. */
  double theoretical_probability;  /**< @brief Expected probability of the pattern configuration. */
  double weight;                   /**< @brief Weight of the pattern configuration in the evaluation function. */
} rglmdf_pattern_freq_summary_record_t;

/**
 * @brief Reversi GLM data file table holding the summary of pattern frequencies.
 *
 * @details The table contains the count of pattern occurrencies grouped by (pattern_id, principal_index_value).
 *          The value of 'ntuples' must be equal to the sum of 'glm_f_variable_cnt' and 'glm_p_variable_cnt'.
 */
typedef struct rglmdf_pattern_freq_summary_table_s {
  size_t glm_f_variable_cnt;                       /**< @brief Count of GLM independent variables belonging to features.*/
  size_t glm_p_variable_cnt;                       /**< @brief Count of GLM independent variables belonging to patterns. */
  size_t ntuples;                                  /**< @brief Number of records. */
  rglmdf_pattern_freq_summary_record_t *records;   /**< @brief Records of the table. */
} rglmdf_pattern_freq_summary_table_t;

/* ### ### */

/**
 * @brief Reversi GLM data file record definition for the entity to glm variable id mapping table.
 *
 * @details Each record is identified by a unique key composed by `(entity_class,entity_id,index_value)`.
 *
 */
typedef struct rglmdf_entity_to_glm_var_id_map_record_s {
  int16_t entity_class;          /**< @brief It is 0 when feature and 1 when pattern. */
  int16_t entity_id;             /**< @brief Board Pattern Id, as defined by REGAB table regab_prng_patterns. */
  int32_t index_value;
  int32_t principal_index_value;
  int64_t glm_variable_id;
} rglmdf_entity_to_glm_var_id_map_record_t;

/**
 * @brief Reversi GLM data file table holding the mapping between entities and glm variable ids.
 *
 * @details There are two mappings .
 */
typedef struct rglmdf_entity_to_glm_var_id_map_table_s {
  size_t ntuples;
  rglmdf_entity_to_glm_var_id_map_record_t *records;
} rglmdf_entity_to_glm_var_id_map_table_t;

/* ### ### */

/**
 * @brief Reversi GLM data file record definition for the solved and classified game position table.
 *
 * @details Each record is identified by either the `row_n` counter or the `gp_id` field.
 *
 * The record has a fixed size and needs 48 bytes.
 */
typedef struct rglmdf_solved_and_classified_gp_record_s {
  int64_t row_n;                  /**< @brief Row number. */
  int64_t gp_id;                  /**< @brief Game Position Id, as defined by REGAB table regab_prng_gp. */
  int64_t mover;                  /**< @brief Game position board definition for mover. */
  int64_t opponent;               /**< @brief Game position board definition for opponent. */
  int8_t game_value;              /**< @brief Game value for the position. */
  float game_value_transformed;   /**< @brief Game value transformed as a float in the range (0..1). */
  float evaluation_function;      /**< @brief Game value, in the (0..1) range, as computed by the evaluation function. */
  float residual;                 /**< @brief Delta value between evaluation function and game value transformed. */
} rglmdf_solved_and_classified_gp_record_t;

/**
 * @brief Reversi GLM data file table holding the game positons being solved and classified.
 *
 * @details The table is build with a record having a fixed set of fields organized into the
 *          the type `rglmdf_solved_and_classified_gp_record_t`, and two variable set of values:
 *
 *            - Feature values
 *            - Pattern index values
 *
 *          that depends on the feature and pattern sets.
 *          <br>
 *          The feature values are collected into the `farray` field that is dynamically allocated with a size
 *          equal to `ntuples` multiplied by `n_fvalues_per_record`.
 *          Feature values are ordered by `(feature_id, instance_number)`.
 *          <br>
 *          The pattern index values are collected into the `iarray` field that is dynamically allocated with a size
 *          equal to `ntuples` multiplied by `n_index_values_per_record`.
 *          Pattern index values are ordered by `(pattern_id, instance_number)`.
 */
typedef struct rglmdf_solved_and_classified_gp_table_s {
  rglmdf_iarray_data_type_t iarray_data_type;          /**< @brief Specifies what is the meaning of the data in the iarray fields. */
  size_t ntuples;                                      /**< @brief Number of records. */
  size_t n_fvalues_per_record;                         /**< @brief Count of feature values fields. */
  size_t n_index_values_per_record;                    /**< @brief Count of pattern index values fields. */
  rglmdf_solved_and_classified_gp_record_t *records;   /**< @brief Records of the table. */
  double *farray;                                      /**< @brief Feature values records. */
  uint32_t *iarray;                                    /**< @brief Pattern index values records. */
} rglmdf_solved_and_classified_gp_table_t;

/**
 * @brief Reversi GLM data file general data structure.
 *
 * @details The type contains all the info saved/retrieved from a RGLM Data File.
 */
typedef struct rglmdf_general_data_s {
  time_t file_creation_time;                                  /**< @brief Creation time of the data file. */
  rglmdf_file_data_format_type_t format;                      /**< @brief The format of the file and of the general data associated to it. */
  size_t batch_id_cnt;                                        /**< @brief Count of batch id entries. */
  uint64_t *batch_ids;                                        /**< @brief Array of batch_id values. */
  uint8_t empty_count;                                        /**< @brief Empty square count. */
  size_t position_status_cnt;                                 /**< @brief Count of statuses of game positions. */
  char *position_status_buffer;                               /**< @brief Text buffer for status labels. */
  char **position_statuses;                                   /**< @brief Array of terminated strings representing status labels. */
  size_t feature_cnt;                                         /**< @brief Count of features. */
  board_feature_id_t *features;                               /**< @brief Array of features. */
  size_t pattern_cnt;                                         /**< @brief Count of patterns. */
  board_pattern_id_t *patterns;                               /**< @brief Array of patterns. */
  rglmdf_position_summary_table_t position_summary;           /**< @brief Aggregated data for positions. */
  rglmdf_pattern_freq_summary_table_t pattern_freq_summary;   /**< @brief Aggregated data for pattern frequencies. */
  rglmdf_solved_and_classified_gp_table_t positions;          /**< @brief Table of game positions. */
  uint64_t **reverse_map_a_f;                                 /**< @brief Maps feature_id to the first entry belonging to the feature in reverse_map_b. */
  uint64_t **reverse_map_a_p;                                 /**< @brief Maps pattern_id to the first entry belonging to the pattern in reverse_map_b. */
  uint64_t *reverse_map_b;                                    /**< @brief Maps pattern_id and principal index value to the glm variable id. */
} rglmdf_general_data_t;

/**
 * @brief Check the endianness of the architecture.
 *
 * @return 0 when little, 1 when big endian
 */
extern int
rglmdf_get_endianness (void);

/**
 * @brief Checks that data type have the expected sizeof.
 *
 * @return true when checks are ok
 */
extern bool
rglmdf_verify_type_sizes (void);

/**
 * @brief Initializes the general data structure.
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
 * @brief Sets the attribute field `format`.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] gd reference to the general data structure
 * @param [in]     f  new updated value
 */
extern void
rglmdf_set_format (rglmdf_general_data_t *gd,
                   rglmdf_file_data_format_type_t f);

/**
 * @brief Gets the attribute field `format`.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the value of the field
 */
extern rglmdf_file_data_format_type_t
rglmdf_get_format (rglmdf_general_data_t *gd);

/**
 * @brief Outputs to `stream` a text message with the file data format.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd     reference to the general data structure
 * @param [in] stream the output file handler
 */
extern void
rglmdf_format_to_text_stream (rglmdf_general_data_t *gd,
                              FILE *stream);

/**
 * @brief Translates the field `file_creation_time` as a character string.
 *
 * @details Conversion happens as UTC value.
 *          The generated string is saved in the `buf` array.
 *          The buffer referenced by `buf` must be long `25` chars or more.
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

/**
 * @brief Getter function for the `batch_id_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `batch_id_cnt` field
 */
extern size_t
rglmdf_get_batch_id_cnt (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `batch_ids` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `batch_ids` field
 */
extern uint64_t *
rglmdf_get_batch_ids (rglmdf_general_data_t *gd);

/**
 * @brief Outputs to `stream` a text message with the list of batch id.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd     reference to the general data structure
 * @param [in] stream the output file handler
 */
extern void
rglmdf_batch_ids_to_text_stream (rglmdf_general_data_t *gd,
                                 FILE *stream);

/**
 * @brief Frees all the allocated memory in the substructures of `gd`.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd     reference to the general data structure
 */
extern void
rglmdf_general_data_release (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `empty_count` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `empty_count` field
 */
extern uint8_t
rglmdf_get_empty_count (rglmdf_general_data_t *gd);

/**
 * @brief Setter function for the `empty_count` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd          reference to the general data structure
 * @param [in] empty_count new value for the `empty_count` field
 */
extern void
rglmdf_set_empty_count (rglmdf_general_data_t *gd,
                        uint8_t empty_count);

/**
 * @brief Setter function for the `position_status_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd  reference to the general data structure
 * @param [in] cnt new value for the `position_status_cnt` field
 */
extern size_t
rglmdf_set_position_status_cnt (rglmdf_general_data_t *gd,
                                size_t cnt);

/**
 * @brief Getter function for the `position_status_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `position_status_cnt` field
 */
extern size_t
rglmdf_get_position_status_cnt (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `position_statuses` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `position_statuses` field
 */
extern char **
rglmdf_get_position_statuses (rglmdf_general_data_t *gd);

/**
 * @brief Outputs to `stream` a text message with the list of statuses.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd     reference to the general data structure
 * @param [in] stream the output file handler
 */
extern void
rglmdf_position_statuses_to_text_stream (rglmdf_general_data_t *gd,
                                         FILE *stream);

/**
 * @brief Setter function for the `feature_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd  reference to the general data structure
 * @param [in] cnt new value for the `feature_cnt` field
 */
extern size_t
rglmdf_set_feature_cnt (rglmdf_general_data_t *gd,
                        size_t cnt);

/**
 * @brief Getter function for the `feature_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `feature_cnt` field
 */
extern size_t
rglmdf_get_feature_cnt (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `features` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `features` field
 */
extern board_feature_id_t *
rglmdf_get_features (rglmdf_general_data_t *gd);

/**
 * @brief Outputs to `stream` a text message with the list of features.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd     reference to the general data structure
 * @param [in] stream the output file handler
 */
extern void
rglmdf_features_to_text_stream (rglmdf_general_data_t *gd,
                                FILE *stream);

/**
 * @brief Getter function for the `glm_f_variable_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `glm_f_variable_cnt` field
 */
extern size_t
rglmdf_get_glm_f_variable_cnt (rglmdf_general_data_t *gd);

/**
 * @brief Setter function for the `pattern_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd  reference to the general data structure
 * @param [in] cnt new value for the `pattern_cnt` field
 */
extern size_t
rglmdf_set_pattern_cnt (rglmdf_general_data_t *gd,
                        size_t cnt);

/**
 * @brief Getter function for the `pattern_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `pattern_cnt` field
 */
extern size_t
rglmdf_get_pattern_cnt (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `patterns` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern board_pattern_id_t *
rglmdf_get_patterns (rglmdf_general_data_t *gd);

/**
 * @brief Outputs to `stream` a text message with the list of patterns.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd     reference to the general data structure
 * @param [in] stream the output file handler
 */
extern void
rglmdf_patterns_to_text_stream (rglmdf_general_data_t *gd,
                                FILE *stream);

/**
 * @brief Getter function for the `glm_p_variable_cnt` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `glm_p_variable_cnt` field
 */
extern size_t
rglmdf_get_glm_p_variable_cnt (rglmdf_general_data_t *gd);

/**
 * @brief Initializes the `position_summary` field.
 *
 * @details Appropriate memory is allocated for the requested number of records.
 *          If the table was already allocated it is freed.
 *          The field `position_summary.ntuples` is update with the new value if
 *          the allocation is succesful.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] gd      reference to the general data structure
 * @param [in]     ntuples the new capacity of the table
 * @return                 the number of allocated records
 */
extern size_t
rglmdf_set_position_summary_ntuples (rglmdf_general_data_t *gd,
                                     size_t ntuples);

/**
 * @brief Getter function for the `position_summary.ntuples` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern size_t
rglmdf_get_position_summary_ntuples (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `position_summary.records` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern rglmdf_position_summary_record_t *
rglmdf_get_position_summary_records (rglmdf_general_data_t *gd);

/**
 * @brief Outputs to `stream` a text message with a short description of the position summary table.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd     reference to the general data structure
 * @param [in] stream the output file handler
 */
extern void
rglmdf_position_summary_cnt_to_text_stream (rglmdf_general_data_t *gd,
                                            FILE *stream);

/**
 * @brief Initializes the `pattern_freq_summary` field.
 *
 * @details Appropriate memory is allocated for the requested number of records.
 *          If the table was already allocated it is freed.
 *          The function allocates memory also for the reverse map structures.
 *          The field `position_summary.ntuples` is update with the new value if
 *          the allocation is succesful.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] gd              reference to the general data structure
 * @param [in]     feature_ntuples the new capacity due to features
 * @param [in]     pattern_ntuples the new capacity due to patterns
 * @param [in]     ntuples         the new capacity of the table
 * @return                         the number of allocated records
 */
extern size_t
rglmdf_set_pattern_freq_summary_ntuples (rglmdf_general_data_t *gd,
                                         size_t feature_ntuples,
                                         size_t pattern_ntuples,
                                         size_t ntuples);

/**
 * @brief Getter function for the `pattern_freq_summary.ntuples` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern size_t
rglmdf_get_pattern_freq_summary_ntuples (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `pattern_freq_summary.records` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern rglmdf_pattern_freq_summary_record_t *
rglmdf_get_pattern_freq_summary_records (rglmdf_general_data_t *gd);

/**
 * @brief Outputs to `stream` a text message with a short description of the pattern frequency summary table.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd     reference to the general data structure
 * @param [in] stream the output file handler
 */
extern void
rglmdf_pattern_freq_summary_cnt_to_text_stream (rglmdf_general_data_t *gd,
                                                FILE *stream);

/**
 * @brief Initializes the `positions` table.
 *
 * @details Appropriate memory is allocated for the requested number of records.
 *          If the table was already allocated it is freed.
 *          The function allocates memory for two arrays of data, `positions.records`
 *          and `positions.iarray`.
 *          The field `positions.ntuples` is updated with the new value if
 *          the allocation is succesful, as well as the field `positions.n_index_values_per_record`.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] gd               reference to the general data structure
 * @param [in]     ntuples          the new capacity of the table
 * @param [in]     iarray_data_type type of data stored in iarray fields
 * @return                          the number of allocated records
 */
extern size_t
rglmdf_set_positions_ntuples (rglmdf_general_data_t *gd,
                              size_t ntuples,
                              rglmdf_iarray_data_type_t iarray_data_type);

/**
 * @brief Getter function for the `positions.ntuples` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern size_t
rglmdf_get_positions_ntuples (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `positions.records` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern rglmdf_solved_and_classified_gp_record_t *
rglmdf_get_positions_records (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `positions.n_fvalues_per_record` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern size_t
rglmdf_get_positions_n_fvalues_per_record (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `positions.n_index_values_per_record` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern size_t
rglmdf_get_positions_n_index_values_per_record (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `positions.iarray` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern uint32_t *
rglmdf_get_positions_iarray (rglmdf_general_data_t *gd);

/**
 * @brief Getter function for the `positions.farray` field.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @return        the `patterns` field
 */
extern double *
rglmdf_get_positions_farray (rglmdf_general_data_t *gd);

/**
 * @brief Computes and populates the reverse map structures.
 *
 * @details This function has to be called once just after having populated
 *          the Pattern Frequency Summary Table.
 *          The pocedure populates the data in the array `reverse_map_b`.
 *          The data in `reverse_map_a_f`, as well as the memory allocation are
 *          prepared before by the call to #rglmdf_set_pattern_freq_summary_ntuples.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 */
extern void
rglmdf_build_reverse_map (rglmdf_general_data_t *gd);

/**
 * @brief Queries the glm variable id.
 *
 * @details The function does not check input values to be in the proper ranges.
 *          Unexpected behavior may occur when out of range value are used.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd                    reference to the general data structure
 * @param [in] entity_class          class of the entity ( feature or pattern )
 * @param [in] pattern_id            pattern id key
 * @param [in] principal_index_value principal index value key
 * @return                           the corresponding value for the `glm_variable_id`
 */
extern uint32_t
rglmdf_map_pid_and_piv_to_glm_vid (rglmdf_general_data_t *gd,
                                   uint16_t entity_class,
                                   uint16_t pattern_id,
                                   uint32_t principal_index_value);

/**
 * @brief Transforms the `positions.iarray` values.
 *
 * @details This function has to be called two times, once with thw parameter `first_step`
 *          set to `true`, and a second time with the parameter set to `false`.
 *          The first call transforms the pattern index values to the principal values.
 *          The second call transforms the pattern principal index values to the `glm_variable_id`
 *          value.
 *          The function can be called after the `positions` table has been fully populated.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] gd         reference to the general data structure
 * @param [in]     first_step governs the transformation to apply
 */
extern void
rglmdf_transform_piv_to_glm_variable_id (rglmdf_general_data_t *gd,
                                         bool first_step);

/**
 * @brief Outputs to `f` the game positions table in CSV format.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @param [in] f  the output file handler
 */
extern void
rglmdf_gp_table_to_csv_file (rglmdf_general_data_t *gd,
                             FILE *f);

/**
 * @brief Outputs to `f` the position summary table in CSV format.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @param [in] f  the output file handler
 */
extern void
rglmdf_ps_table_to_csv_file (rglmdf_general_data_t *gd,
                             FILE *f);

/**
 * @brief Outputs to `f` the feature and pattern frequency summary table in CSV format.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd reference to the general data structure
 * @param [in] f  the output file handler
 */
extern void
rglmdf_fpfs_table_to_csv_file (rglmdf_general_data_t *gd,
                               FILE *f);

/**
 * @brief Checks sha3 digest.
 *
 * @details Opens the files named `file_name` and `file_name`.SHA3-256,
 *          then computes the file digest of the first file and comapres it
 *          with the one found in the second file.
 *          When the two digest are equal returs `0`, otherwise `1`.
 *          When one of the two files is not found returns `1`.
 *
 * @invariant Parameter `file_name` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] file_name name of the file being checked
 * @return              `0` on succesful execution.
 */
extern int
rglmdf_check_sha3_file_digest (char *file_name);

/**
 * @brief Computes sha3 digest and write it to a new file.
 *
 * @details Opens the file named `file_name`, computes the SHA3-256 digest,
 *          then write it to a new file having a name composed as `file_name`
 *          plus the suffix `SHA3-256`. Following the digest there are two spaces
 *          followed by the name of the imput file.
 *
 *          Running from the command line:<br>
 *          `rhash --sha3-256 file_name > file_name.SHA3-256`<br>
 *          it is possible to obtain the same result.
 *
 * @invariant Parameter `file_name` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] file_name name of the file being digested
 * @return              `0` on succesful execution.
 */
extern int
rglmdf_generate_sha3_file_digest (char *file_name);

/**
 * @brief Retrieves the genaral data structure from a binary file.
 *
 * @details Opens the file named `filename`, and retrieves the data
 *          from the file populating the `gd` general data structure.
 *          The function looks for a second file named `filename`.SHA3-256
 *          reads the hash from it, and compares it with the digest of `filename`.
 *          An error is rised if the sha3-256 file is not found , or in case the hash
 *          doesn't match.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `filename` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd       reference to the general data structure
 * @param [in] filename name of the file being retrieved
 * @param [in] verbose  when true send log output to stdout
 * @return              `0` on succesful execution.
 */
extern int
rglmdf_read_general_data_from_binary_file (rglmdf_general_data_t *gd,
                                           char *filename,
                                           bool verbose);

/**
 * @brief Dumps the RGLM weights to a binary file.
 *
 * @details Opens the file named `filename`, and stores the RGLM parameters in it.
 *          All parameters belonging to to the list of patterns considered by the model
 *          are saved. Parameters corresponding to missing pattern configurations are
 *          valued as zero.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `filename` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd       reference to the general data structure
 * @param [in] filename name of the file being digested
 * @return              `0` on succesful execution.
 */
extern int
rglmdf_write_rglm_weights_to_binary_file (rglmdf_general_data_t *gd,
                                          char *filename);

/**
 * @brief Dumps the RGLM weights to a binary file.
 *
 * @details Opens the file named `filename`, and stores the RGLM parameters in it.
 *          All parameters belonging to to the list of patterns considered by the model
 *          are saved. Parameters corresponding to missing pattern configurations are
 *          valued as zero.
 *          The time field saved in the file is the actual time.
 *
 * @invariant Parameter `gd` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `filename` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gd       reference to the general data structure
 * @param [in] filename name of the file being digested
 * @param [in] time     the value of the time field saved in the file
 * @return              `0` on succesful execution.
 */
extern int
rglmdf_write_general_data_to_binary_file (rglmdf_general_data_t *gd,
                                          char *filename,
                                          time_t time);


#endif /* RGLM_DATA_FILES_H */
