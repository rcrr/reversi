/**
 * @file
 *
 * @brief A small library for loading .ini config files.
 *
 * @details The library has support for sections, comment lines and quoted string values (with escapes).
 *          Unquoted values and keys are trimmed of whitespace when loaded.
 *
 * @par cfg.h
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

#ifndef CFG_H
#define CFG_H

/**
 * @brief A config structure.
 *
 * @details Collects value/key pairs organized by sections.
 */
typedef struct cfg_s cfg_t;

/**
 * @brief Loads a config file into memory.
 *
 * @details `NULL` is returned if the file cannot be loaded.
 *
 * @param [in] filename path to the config file
 * @return              a newly allocated config object
 */
cfg_t *
cfg_load (const char *filename);

/**
 * @brief Free the memory used by the `cfg_t` object when we are done with it.
 *
 * @details Calling this function invalidates all string pointers returned by the library.
 *
 * @param [in,out] cfg the reference to the config object
 */
void
cfg_free (cfg_t *cfg);

/**
 * @brief Returns the value, if found, identified by `section`/`key`.
 *
 * @details Given a section and a key the corresponding value is returned if it exists.
 *          If the section argument is `NULL` then all sections are searched.
 *
 * @param [in] cfg     the reference to the config object
 * @param [in] section the section to search in
 * @param [in] key     the key to search for
 * @return             a reference to the value string
 */
const char *
cfg_get (cfg_t *cfg,
         const char *section,
         const char *key);

/**
 * @brief Populates `dst` with a reference to a formatted value identified by `section`/`key`.
 *
 * @details Stores the result of calling scanf on the value acquired by the config object by searching
 *          with `section`/`key` parameters.
 *          When the search is succesful `*dst` is altered and `1` is returned.
 *          When the search is not succesful `*dst` is unchanged and `0` is returned.
 *
 * @param [in]  cfg     the reference to the config object
 * @param [in]  section the section to search in
 * @param [in]  key     the key to search for
 * @param [in]  scanfmt scanf like format string
 * @param [out] dst     a reference to the output of scanf
 * @return              `1` when `section/key` has been found, otherwise `0`
 */
int
cfg_sget (cfg_t *cfg,
          const char *section,
          const char *key,
          const char *scanfmt,
          void *dst);

#endif
