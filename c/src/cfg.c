/**
 * @file
 *
 * @brief Config ini files library implementation.
 *
 * @par cfg.c
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
#include <string.h>
#include <ctype.h>

#include "cfg.h"

struct cfg_s {
  char *data;
  char *end;
};

/* Case insensitive string compare */
static int
strcmpci (const char *a,
          const char *b)
{
  for (;;) {
    int d = tolower(*a) - tolower(*b);
    if (d != 0 || !*a) {
      return d;
    }
    a++, b++;
  }
}

/* Returns the next string in the split data */
static char *
next (cfg_t *cfg,
      char *p)
{
  p += strlen(p);
  while (p < cfg->end && *p == '\0') p++;
  return p;
}

static void
trim_back (cfg_t *cfg,
           char *p)
{
  while (p >= cfg->data && (*p == ' ' || *p == '\t' || *p == '\r'))
    *p-- = '\0';
}

static char *
discard_line (cfg_t *cfg,
              char *p)
{
  while (p < cfg->end && *p != '\n')
    *p++ = '\0';
  return p;
}


static char *
unescape_quoted_value (cfg_t *cfg,
                       char *p)
{
  /* Use `q` as write-head and `p` as read-head, `p` is always ahead of `q`
   * as escape sequences are always larger than their resultant data */
  char *q = p;
  p++;
  while (p < cfg->end && *p != '"' && *p != '\r' && *p != '\n') {
    if (*p == '\\') {
      /* Handle escaped char */
      p++;
      switch (*p) {
        default   : *q = *p;    break;
        case 'r'  : *q = '\r';  break;
        case 'n'  : *q = '\n';  break;
        case 't'  : *q = '\t';  break;
        case '\r' :
        case '\n' :
        case '\0' : goto end;
      }
    } else {
      /* Handle normal char */
      *q = *p;
    }
    q++, p++;
  }
end:
  return q;
}


/* Splits data in place into strings containing section-headers, keys and
 * values using one or more '\0' as a delimiter. Unescapes quoted values */
static void
split_data (cfg_t *cfg)
{
  char *value_start, *line_start;
  char *p = cfg->data;

  while (p < cfg->end) {
    switch (*p) {
      case '\r':
      case '\n':
      case '\t':
      case ' ':
        *p = '\0';
        /* Fall through */

      case '\0':
        p++;
        break;

      case '[':
        p += strcspn(p, "]\n");
        *p = '\0';
        break;

      case ';':
        p = discard_line(cfg, p);
        break;

      default:
        line_start = p;
        p += strcspn(p, "=\n");

        /* Is line missing a '='? */
        if (*p != '=') {
          p = discard_line(cfg, line_start);
          break;
        }
        trim_back(cfg, p - 1);

        /* Replace '=' and whitespace after it with '\0' */
        do {
          *p++ = '\0';
        } while (*p == ' ' || *p == '\r' || *p == '\t');

        /* Is a value after '=' missing? */
        if (*p == '\n' || *p == '\0') {
          p = discard_line(cfg, line_start);
          break;
        }

        if (*p == '"') {
          /* Handle quoted string value */
          value_start = p;
          p = unescape_quoted_value(cfg, p);

          /* Was the string empty? */
          if (p == value_start) {
            p = discard_line(cfg, line_start);
            break;
          }

          /* Discard the rest of the line after the string value */
          p = discard_line(cfg, p);

        } else {
          /* Handle normal value */
          p += strcspn(p, "\n");
          trim_back(cfg, p - 1);
        }
        break;
    }
  }
}



/* Public functions. */

cfg_t *
cfg_load (const char *filename)
{
  cfg_t *cfg = NULL;
  FILE *fp = NULL;
  int n, sz;

  /* Init ini struct */
  cfg = malloc(sizeof(*cfg));
  if (!cfg) goto fail;
  memset(cfg, 0, sizeof(*cfg));

  /* Open file */
  fp = fopen(filename, "rb");
  if (!fp) goto fail;

  /* Get file size */
  fseek(fp, 0, SEEK_END);
  sz = ftell(fp);
  rewind(fp);

  /* Load file content into memory, null terminate, init end var */
  cfg->data = malloc(sz + 1);
  cfg->data[sz] = '\0';
  cfg->end = cfg->data  + sz;
  n = fread(cfg->data, 1, sz, fp);
  if (n != sz) goto fail;

  /* Prepare data */
  split_data(cfg);

  /* Clean up and return */
  fclose(fp);
  return cfg;

fail:
  if (fp) fclose(fp);
  if (cfg) cfg_free(cfg);
  return NULL;
}

void
cfg_free (cfg_t *cfg)
{
  if (cfg) {
    free(cfg->data);
    free(cfg);
  }
}

const char *
cfg_get (cfg_t *cfg,
         const char *section,
         const char *key)
{
  char *current_section = "";
  char *val;
  char *p = cfg->data;

  if (*p == '\0') p = next(cfg, p);

  while (p < cfg->end) {
    if (*p == '[') {
      /* Handle section */
      current_section = p + 1;

    } else {
      /* Handle key */
      val = next(cfg, p);
      if (!section || !strcmpci(section, current_section)) {
        if (!strcmpci(p, key)) return val;
      }
      p = val;
    }

    p = next(cfg, p);
  }

  return NULL;
}

int
cfg_sget (cfg_t *cfg,
          const char *section,
          const char *key,
          const char *scanfmt,
          void *dst)
{
  const char *val = cfg_get(cfg, section, key);
  if (!val) return 0;
  if (scanfmt)
    sscanf(val, scanfmt, dst);
  else
    *((const char**) dst) = val;
  return 1;
}
