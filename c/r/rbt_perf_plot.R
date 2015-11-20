# -not a script- !/usr/bin/Rscript
#
# Plots brt performance tests.
#
#
# File: rbt_perf_plot.R
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
#
# Author: Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright (c) 2015 Roberto Corradini. All rights reserved.
#
#
# License
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
# or visit the site <http://www.gnu.org/licenses/>.
#

##
## In order to print everything run:
##
##    source(file="./rbt_perf_plot.R")
##    dummy_val <- lapply(names(rbt_op_types), function(x) {rbt_print_perf_data(x, "../build/out/rbt_performance_a_log.csv", "../build/out")})
##



library(ggplot2)



rbt_op_types <- list(fexael = c("find_existing_a_elm",     "OP_INITIAL_COUNT", "CPUTIME_NANO_SEC", "Find existing alpha element"),
                     fexzel = c("find_existing_z_elm",     "OP_INITIAL_COUNT", "CPUTIME_NANO_SEC", "Find existing omega element"),
                     fmisel = c("find_missing_elm",        "OP_INITIAL_COUNT", "CPUTIME_NANO_SEC", "Find missing element"),
                     rineel = c("rnd_insert_new_elm",      "OP_INITIAL_COUNT", "CPUTIME_NANO_SEC", "Insert new element in random order"),
                     rndpop = c("rnd_populate",            "OP_SIZE",          "CPUTIME_NANO_SEC", "Populate empty table with random order elements"),
                     rrexel = c("rnd_remove_existing_elm", "OP_INITIAL_COUNT", "CPUTIME_NANO_SEC", "Remove a random existing element"),
                     tratbl = c("traverse_table",          "OP_SIZE",          "CPUTIME_NANO_SEC", "Traverse the table"))



rbt_print_perf_data <- function(label = "", csv.data.fname, out.dir = ".") {
  
  if (!file.exists(out.dir)) {
    cat(sprintf("Error: output directory \"%s\" does not exists! Aborting.\n", out.dir))
    stop()
  }
  
  plt <- rbt_plot_perf_data(label, csv.data.fname)
  fname <- paste0(out.dir, "/", label,".pdf")
  pdf(fname, paper="a4r", width=297, height=210, pointsize=10);
  print(plt);
  dev.off();
  cat(sprintf("file %s printed succesfully.\n", fname))
}



rbt_plot_perf_data <- function(label = "", csv.data.fname) {
  
  rbt_op_names <- names(sapply(rbt_op_types, names))
  
  if (!label %in% rbt_op_names) {
    cat(sprintf("Label must be in:\n"))
    cat(sprintf("   %s\n", rbt_op_names))
    cat(sprintf("You typed \"%s\"! Aborting.\n", label))
    stop()
  }
  
  op <- rbt_op_types[[label]]

  rbt_perf_data = read.csv(file=csv.data.fname, header = TRUE, sep=";")
  
  nano_seconds_per_second <- 1000000000
  rbt_perf_data$LTIME <- NULL
  rbt_perf_data$CPUTIME_NANO_SEC <- with(rbt_perf_data, as.integer(CPUTIME_SEC * nano_seconds_per_second + CPUTIME_NSEC))
  rbt_perf_data$CPUTIME_SEC <- NULL
  rbt_perf_data$CPUTIME_NSEC <- NULL
  
  ggplot(subset(rbt_perf_data, OP_TYPE==op[1]), aes_string(x = op[2], y = op[3])) +
    theme_bw() +
    layer(geom = "point") +
    ggtitle(op[4])
  
}



rbt_read_perf_data <- function(csv.data.fname)
{
  ## Checks parameters.
  if (!file.exists(csv.data.fname)) {
    cat(sprintf("Error: csv data file \"%s\" does not exists! Aborting.\n", csv.data.fname))
    stop()
  }
  
  ## Reads data file.
  rbt_perf_data = read.csv(file=csv.data.fname, header=TRUE, sep=";")
  
  ## Checks column names.
  required_columns <- c("LTIME", "CPUTIME_SEC", "CPUTIME_NSEC")
  for (i in 1:length(required_columns)) {
    column <- required_columns[i]
    if (!column %in% names(rbt_perf_data)) {
      cat(sprintf("Error: data frame loaded from file \"%s\" does not contains column \"%s\"! Aborting.\n",
                  csv.data.fname, column))
      stop()
    }
  }
  
  ## Transforms columns.
  rbt_perf_data$LTIME <- NULL
  rbt_perf_data$CPUTIME_NANO_SEC <- with(rbt_perf_data, as.integer(CPUTIME_SEC * 1000000000 + CPUTIME_NSEC))
  rbt_perf_data$CPUTIME_SEC <- NULL
  rbt_perf_data$CPUTIME_NSEC <- NULL
  
  rbt_perf_data
}
