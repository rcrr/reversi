/**
 * @file
 *
 * @brief Main option parse module implementation.
 *
 * This is a getopt-like option parser, this module differs from
 * POSIX getopt() option parser in three details:
 *
 * 1) Parser state is stored entirely in global variables, some of
 * which are static and inaccessible. This means only one thread can
 * use getopt(). It also means it's not possible to recursively parse
 * nested sub-arguments while in the middle of argument parsing.
 * Mop fixes this by storing all state on a local struct.
 *
 * 2) The POSIX standard provides no way to properly reset the parser.
 * This means for portable code that getopt() is only good for one
 * run, over one argv with one option string. It also means subcommand
 * options cannot be processed with getopt(). Most implementations
 * provide a method to reset the parser, but it's not portable.
 * Mop provides an mop_arg() function for stepping over
 * subcommands and continuing parsing of options with another option
 * string. The #mop_options_t type itself can be passed around to
 * subcommand handlers for additional subcommand option parsing. A
 * full reset can be achieved by with an additional mop_init().
 *
 * 3) Error messages are printed to stderr. This can be disabled with
 * opterr, but the messages themselves are still inaccessible.
 * Mop solves this by writing an error message in its errmsg
 * field. The downside to Mop is that this error message will
 * always be in English rather than the current locale.
 *
 * Mop has been derived from Optparse, a public domain, portable, reentrant, embeddable,
 * getopt-like option parser written by Christopher Wellons.
 * See the GitHub page: <a href="https://github.com/skeeto/optparse.git" target="_blank">
 * Optionparse</a>.
 *
 * @par main_option_parse.c
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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "main_option_parse.h"

/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static int
mop_error (mop_options_t *options,
           const mop_errtype_t et,
           const char *data);

static int
mop_is_dashdash (const char *arg);

static int
mop_is_shortopt (const char *arg);

static int
mop_is_longopt (const char *arg);

static void
mop_permute (mop_options_t *options,
             int index);

static int
mop_argtype (const char *optstring,
             char c);


static int
mop_longopts_end (const mop_options_long_t *longopts,
                  int i);

static void
mop_from_long(const mop_options_long_t *longopts,
              char *optstring);

static int
mop_longopts_match (const char *longname,
                    const char *option);

static char *
mop_longopts_arg (char *option);

static int
mop_long_fallback (mop_options_t *options,
                   const mop_options_long_t *longopts,
                   int *longindex);



/*
 * Internal variables and constants.
 */

static const char *const mop_msg_invalid = "invalid option";
static const char *const mop_msg_missing = "option requires an argument";
static const char *const mop_msg_toomany = "option takes no arguments";

/**
 * @endcond
 */



/**
 * @brief Initializes the parser state.
 *
 * @param [out] options a pointer to a structure that shall be initialized
 * @param [in]  argc    arguments count
 * @param [in]  argv    the vector of arguments
 */
void
mop_init (mop_options_t *options,
          int argc,
          char **argv)
{
  options->argc = argc;
  options->argv = argv;
  options->permute = true;
  options->optind = 1;
  options->subopt = 0;
  options->optarg = NULL;
  options->errmsg[0] = '\0';
}

/**
 * @brief Stepps over non-option arguments.
 *
 * @details Argument parsing can continue with mop_parse() after using this
 * function. That would be used to parse the options for the
 * subcommand returned by mop_arg(). This function allows you to
 * ignore the value of optind.
 *
 * @param [in] options a pointer to a structure initialized by a call to `mop_init()`
 * @return             the next non-option argument, or `NULL` for no more arguments
 */
char *
mop_arg (mop_options_t *options)
{
  char *option = options->argv[options->optind];
  options->subopt = 0;
  if (option != 0) options->optind++;
  return option;
}

/**
 * @brief Reads the next option in the argv array.
 *
 * @details The mop_parse() function is a command-line parser that can be
 * used by applications like the standard unix `getopt()` utility.
 *
 * Just like `getopt()`, a character followed by no colons means no
 * argument. One colon means the option has a required argument. Two
 * colons means the option takes an optional argument.
 *
 * mop_parse() should be familiar to anyone accustomed to getopt(), and
 * it could be a nearly drop-in replacement. The option string is the
 * same and the fields in the `options` structure have the same names as
 * the getopt() global variables (optarg, optind, optopt).
 *
 * Optparse also supports GNU-style long options with mop_parse_long().
 * The interface is slightly different and simpler than getopt_long().
 *
 * By default, argv is permuted as it is parsed, moving non-option
 * arguments to the end. This can be disabled by setting the `permute`
 * field to `false` after initialization.
 *
 * @param [in] options   a pointer to a structure initialized by a call to `mop_init()`
 * @param [in] optstring a getopt()-formatted option string.
 * @return               the next option character, `-1` for done, or '`?`' for error
 *
 */
int
mop_parse (mop_options_t *options,
           const char *optstring)
{
  int type;
  char *next;
  char *option = options->argv[options->optind];
  options->errmsg[0] = '\0';
  options->optopt = 0;
  options->optarg = NULL;
  if (option == 0) {
    return -1;
  } else if (mop_is_dashdash(option)) {
    options->optind++; /* consume "--" */
    return -1;
  } else if (!mop_is_shortopt(option)) {
    if (options->permute) {
      int index = options->optind++;
      int r = mop_parse(options, optstring);
      mop_permute(options, index);
      options->optind--;
      return r;
    } else {
      return -1;
    }
  }
  option += options->subopt + 1;
  options->optopt = option[0];
  type = mop_argtype(optstring, option[0]);
  next = options->argv[options->optind + 1];
  switch (type) {
  case -1: {
    char str[2] = {0, 0};
    str[0] = option[0];
    options->optind++;
    return mop_error(options, MOP_ERR_INVALID, str);
  }
  case MOP_NONE:
    if (option[1]) {
      options->subopt++;
    } else {
      options->subopt = 0;
      options->optind++;
    }
    return option[0];
  case MOP_REQUIRED:
    options->subopt = 0;
    options->optind++;
    if (option[1]) {
      options->optarg = option + 1;
    } else if (next != 0) {
      options->optarg = next;
      options->optind++;
    } else {
      char str[2] = {0, 0};
      str[0] = option[0];
      options->optarg = NULL;
      return mop_error(options, MOP_ERR_MISSING, str);
    }
    return option[0];
  case MOP_OPTIONAL:
    options->subopt = 0;
    options->optind++;
    if (option[1])
      options->optarg = option + 1;
    else
      options->optarg = NULL;
    return option[0];
  }
  return 0;
}

/**
 * @brief Handles GNU-style long options in addition to getopt() options.
 *
 * @details This works a lot like GNU's getopt_long(). The last option in
 * `longopts` must be all zeros, marking the end of the array. The
 * `longindex` argument may be `NULL`.
 *
 * @param [in]  options   a pointer to a structure initialized by a call to `mop_init()`
 * @param [in]  longopts  a long format option string
 * @param [out] longindex a reference where is saved the option index in `longopts`
 */
int
mop_parse_long (mop_options_t *options,
                const mop_options_long_t *longopts,
                int *longindex)
{
  int i;
  char *option = options->argv[options->optind];
  if (option == 0) {
    return -1;
  } else if (mop_is_dashdash(option)) {
    options->optind++; /* consume "--" */
    return -1;
  } else if (mop_is_shortopt(option)) {
    return mop_long_fallback(options, longopts, longindex);
    } else if (!mop_is_longopt(option)) {
    if (options->permute) {
      int index = options->optind++;
      int r = mop_parse_long(options, longopts, longindex);
      mop_permute(options, index);
      options->optind--;
      return r;
    } else {
      return -1;
    }
  }

  /* Parse as long option. */
  options->errmsg[0] = '\0';
  options->optopt = 0;
  options->optarg = NULL;
  option += 2; /* skip "--" */
  options->optind++;
  for (i = 0; !mop_longopts_end(longopts, i); i++) {
    const char *name = longopts[i].longname;
    if (mop_longopts_match(name, option)) {
      char *arg;
      if (longindex) *longindex = i;
      options->optopt = longopts[i].shortname;
      arg = mop_longopts_arg(option);
      if (longopts[i].argtype == MOP_NONE && arg != 0) {
        return mop_error(options, MOP_ERR_TOO_MANY, name);
      } if (arg != 0) {
        options->optarg = arg;
      } else if (longopts[i].argtype == MOP_REQUIRED) {
        options->optarg = options->argv[options->optind++];
        if (options->optarg == NULL)
          return mop_error(options, MOP_ERR_MISSING, name);
      }
      return options->optopt;
    }
  }
  return mop_error(options, MOP_ERR_INVALID, option);
}

/**
 * @cond
 */



/*
 * Internal functions.
 */

static int
mop_error (mop_options_t *options,
           const mop_errtype_t et,
           const char *data)
{
  unsigned p = 0;
  const char *sep = " -- '";
  char *msg;
  char ret;

  switch (et) {
  case MOP_ERR_INVALID:
    msg = (char *) mop_msg_invalid;
    ret = '?';
    break;
  case MOP_ERR_MISSING:
    msg = (char *) mop_msg_missing;
    ret = ':';
    break;
  case MOP_ERR_TOO_MANY:
    msg = (char *) mop_msg_toomany;
    ret = '?';
    break;
  default:
    abort();
  }

  while (*msg)
    options->errmsg[p++] = *msg++;
  while (*sep)
    options->errmsg[p++] = *sep++;
  while (p < sizeof(options->errmsg) - 2 && *data)
    options->errmsg[p++] = *data++;
  options->errmsg[p++] = '\'';
  options->errmsg[p++] = '\0';

  return ret;
}

static int
mop_is_dashdash (const char *arg)
{
  return arg != 0 && arg[0] == '-' && arg[1] == '-' && arg[2] == '\0';
}

static int
mop_is_shortopt (const char *arg)
{
  return arg != 0 && arg[0] == '-' && arg[1] != '-' && arg[1] != '\0';
}

static int
mop_is_longopt (const char *arg)
{
  return arg != 0 && arg[0] == '-' && arg[1] == '-' && arg[2] != '\0';
}

static void
mop_permute (mop_options_t *options,
             int index)
{
  char *non_option = options->argv[index];
  for (int i = index; i < options->optind - 1; i++)
    options->argv[i] = options->argv[i + 1];
  options->argv[options->optind - 1] = non_option;
}

static int
mop_argtype (const char *optstring,
             char c)
{
  int count = MOP_NONE;
  if (c == ':') return -1;
  for (; *optstring && c != *optstring; optstring++);
  if (!*optstring) return -1;
  if (optstring[1] == ':')
    count += optstring[2] == ':' ? 2 : 1;
  return count;
}


static int
mop_longopts_end (const mop_options_long_t *longopts,
                  int i)
{
  return !longopts[i].longname && !longopts[i].shortname;
}

static void
mop_from_long(const mop_options_long_t *longopts,
              char *optstring)
{
  char *p = optstring;
  int i;
  for (i = 0; !mop_longopts_end(longopts, i); i++) {
    if (longopts[i].shortname) {
      int a;
      *p++ = longopts[i].shortname;
      for (a = 0; a < (int)longopts[i].argtype; a++)
        *p++ = ':';
    }
  }
  *p = '\0';
}

/* Unlike strcmp(), handles options containing "=". */
static int
mop_longopts_match (const char *longname,
                    const char *option)
{
  const char *a = option, *n = longname;
  if (longname == 0) return 0;
  for (; *a && *n && *a != '='; a++, n++)
    if (*a != *n) return 0;
  return *n == '\0' && (*a == '\0' || *a == '=');
}

/* Return the part after "=", or NULL. */
static char *
mop_longopts_arg (char *option)
{
  for (; *option && *option != '='; option++);
  if (*option == '=') return option + 1;
  else return 0;
}

static int
mop_long_fallback (mop_options_t *options,
                   const mop_options_long_t *longopts,
                   int *longindex)
{
  int result;
  char optstring[96 * 3 + 1]; /* 96 ASCII printable characters */
  mop_from_long(longopts, optstring);
  result = mop_parse(options, optstring);
  if (longindex != NULL) {
    *longindex = -1;
    if (result != -1) {
      for (int i = 0; !mop_longopts_end(longopts, i); i++)
        if (longopts[i].shortname == options->optopt)
          *longindex = i;
    }
  }
  return result;
}

/**
 * @endcond
 */
