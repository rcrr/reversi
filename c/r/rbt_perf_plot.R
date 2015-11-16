#!/usr/bin/Rscript
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

library(ggplot2)

op_types <- list(find_existing_a_elm     = c("OP_INITIAL_COUNT", "CPUTIME_NANO_SEC"),
                 find_existing_z_elm     = c("OP_INITIAL_COUNT", "CPUTIME_NANO_SEC"),
                 find_missing_elm        = c("OP_INITIAL_COUNT", "CPUTIME_NANO_SEC"),
                 rnd_insert_new_elm      = c("OP_INITIAL_COUNT", "CPUTIME_NANO_SEC"),
                 rnd_populate            = c("OP_SIZE",          "CPUTIME_NANO_SEC"),
                 rnd_remove_existing_elm = c("OP_INITIAL_COUNT", "CPUTIME_NANO_SEC"),
                 traverse_table          = c("OP_SIZE",          "CPUTIME_NANO_SEC"))

rbt_perf_data = read.csv(file="rbt_performance_a_log.csv", header = TRUE, sep=";")
#str(rbt_perf_data)
rbt_perf_data$LTIME <- NULL
rbt_perf_data$CPUTIME_NANO_SEC <- with(rbt_perf_data, as.integer(CPUTIME_SEC * 1000000000 + CPUTIME_NSEC))
rbt_perf_data$CPUTIME_SEC <- NULL
rbt_perf_data$CPUTIME_NSEC <- NULL
#str(rbt_perf_data)
#levels(rbt_perf_data$OP_TYPE)

# index goes from 1 to 7 ....
index <- 1
opt <- op_types[[index]]
opt_to_s = names(op_types)[index]
plt <- ggplot(data = subset(rbt_perf_data, OP_TYPE==opt_to_s),
              aes_string(x = opt[1],
                         y = opt[2])) +
theme_bw() +
layer(geom = "point") +
ggtitle(opt_to_s)

pdf("rbt_perf_plot.pdf")
print(plt)
dev.off()


