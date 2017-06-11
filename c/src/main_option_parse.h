/**
 * @file
 *
 * @brief Main option parse module definitions.
 * @details This module defines a portable, reentrant, embeddable, getopt-like
 * option parser.
 *
 * @par main_option_parser.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2017 Roberto Corradini. All rights reserved.
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

#ifndef MAIN_OPTION_PARSE_H
#define MAIN_OPTION_PARSE_H

#include <stdbool.h>

/**
 * @brief Collects all the info required for parsing.
 */
typedef struct {
  int argc;                 /**< @brief . */
  char **argv;              /**< @brief . */
  bool permute;             /**< @brief . */
  int optind;               /**< @brief . */
  int optopt;               /**< @brief . */
  char *optarg;             /**< @brief . */
  char errmsg[64];          /**< @brief . */
  int subopt;               /**< @brief . */
} mop_options_t;

/**
 * @enum mop_argtype_t
 * @brief The argument type specifies if not requested, mandatory, or optional.
 */
typedef enum {
  MOP_NONE,
  MOP_REQUIRED,
  MOP_OPTIONAL
} mop_argtype_t;

/**
 * @enum mop_errtype_t
 * @brief Error type.
 */
typedef enum {
  MOP_ERR_INVALID,
  MOP_ERR_MISSING,
  MOP_ERR_TOO_MANY
} mop_errtype_t;

/**
 * @brief Collects the parsing rules for the long parser.
 */
typedef struct {
    const char *longname;     /**< @brief . */
    int shortname;            /**< @brief . */
    mop_argtype_t argtype;    /**< @brief . */
} mop_options_long_t;



extern void
mop_init (mop_options_t *options,
          int argc,
          char **argv);

extern int
mop_parse (mop_options_t *options,
           const char *optstring);

extern int
mop_parse_long (mop_options_t *options,
                const mop_options_long_t *longopts,
                int *longindex);

extern char *
mop_arg (mop_options_t *options);



#endif /* MAIN_OPTION_PARSE_H */
