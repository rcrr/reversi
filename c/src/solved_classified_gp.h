/**
 * @file
 *
 * @brief Solved and classified game position module definitions.
 * @details This module defines tables and records read and written to binary files.
 *
 * @par solved_classified_gp.h
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

#ifndef SOLVED_CLASSIFIED_GP_H
#define SOLVED_CLASSIFIED_GP_H



typedef struct regab_ext_cnt_pos_record_s {
  int batch_id;
  char status[4];
  int64_t game_position_cnt;
  int64_t classified_cnt;
} regab_ext_cnt_pos_record_t;

typedef struct regab_ext_cnt_pos_table_s {
  size_t ntuples;
  regab_ext_cnt_pos_record_t *records;
} regab_ext_cnt_pos_table_t;

typedef struct regab_ext_cnt_pattern_freq_record_s {
  long long int glm_variable_id;
  int pattern_id;
  int principal_index_value;
  int64_t total_cnt;
  double relative_frequency;
  double theoretical_probability;
} regab_ext_cnt_pattern_freq_record_t;

typedef struct regab_ext_cnt_pattern_freq_table_s {
  size_t ntuples;
  regab_ext_cnt_pattern_freq_record_t *records;
} regab_ext_cnt_pattern_freq_table_t;

typedef struct regab_ext_solved_and_classified_gp_table_s {
  size_t ntuples;
} regab_ext_solved_and_classified_gp_table_t;


#endif /* SOLVED_CLASSIFIED_GP_H */
