#!/bin/bash
#
# rglm_batch.sh
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
#
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
# @copyright 2021 Roberto Corradini. All rights reserved.
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
#
# This bash script generates the RGLM Model Weight files, as well as the ancillary data, for
# the construnction of the, next level, endgame solver equipped with the emerging new evaluation function.
#
# Each row has the format:
#
#   ./script/rglm.sh 20 MOBILITY,INTERCEPT EDGE A000
#
#
# Available batch_id:
#
#  batch_id | prng_seed | ngames  | npositions 
# ----------+-----------+---------+------------
#         1 |     97531 |       1 |         61
#         3 |     13579 | 1000000 |   61412190
#         4 |         0 |    1000 |      61356
#         5 |      5577 |   10000 |     614283
#         6 |       881 |  100000 |    6142003
#         7 |       277 |      10 |        614
#         8 |       607 |     100 |       6140
#         9 |     77357 | 2000000 |  122829157
#
# npositions span [60..0] empty count, and doesn't take into account the offspring positions.
# Selected positions should be {CMR,CMS}.
# Positions to be "usable" have to be: generated -> solved -> forked by the offspring "action" -> classified.
# After this sequence the positions can be the data subject to the extraction and the computation executed by this script.
#

do_work () {
    
    local SCRIPT="./script/rglm.sh"
    eval ${SCRIPT} $1 $2 $3 $4 $5 $6 $7 $8
    STATUS=$?
    if [ $STATUS -ne 0 ]
    then
        echo -e "Script ${SCRIPT} ended abnormally, exiting ..."
        exit 1
    fi
    return 0
}

#do_work 20 INTERCEPT --     A2000 3 6 production check_sentinel
#do_work 20 MOBILITY  --     A2001 3 6 production check_sentinel
#do_work 20 MOBILITY2 --     A2002 3 6 production check_sentinel
#do_work 20 MOBILITY3 --     A2003 3 6 production check_sentinel
#do_work 20 --        EDGE   A2004 3 6 production check_sentinel
#do_work 20 --        R2     A2005 3 6 production check_sentinel
#do_work 20 --        R3     A2006 3 6 production check_sentinel
#do_work 20 --        R4     A2007 3 6 production check_sentinel
#do_work 20 --        DIAG8  A2008 3 6 production check_sentinel
#do_work 20 --        DIAG7  A2009 3 6 production check_sentinel
#do_work 20 --        DIAG6  A2010 3 6 production check_sentinel
#do_work 20 --        DIAG5  A2011 3 6 production check_sentinel
#do_work 20 --        DIAG4  A2012 3 6 production check_sentinel
#do_work 20 --        DIAG3  A2013 3 6 production check_sentinel
#do_work 20 --        CORNER A2014 3 6 production check_sentinel
#do_work 20 --        XEDGE  A2015 3 6 production check_sentinel
#do_work 20 --        2X5COR A2016 3 6 production check_sentinel

#do_work 20 INTERCEPT,MOBILITY  -- A2020 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY2 -- A2021 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 -- A2022 3 6 production check_sentinel

#do_work 20 -- EDGE,XEDGE A2025 3 6 production check_sentinel

#do_work 20 INTERCEPT,MOBILITY3 EDGE   A2030 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 R2     A2031 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 R3     A2032 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 R4     A2033 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 DIAG8  A2034 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE  A2035 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 CORNER A2036 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 2X5COR A2037 3 6 production check_sentinel

#do_work 20 INTERCEPT,MOBILITY3 EDGE,R2,R3,R4                                     A2040 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 EDGE,R2,R3,R4,DIAG3,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8 A2041 3 6 production check_sentinel

#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A2050 3 6 production check_sentinel
#do_work 20 --                  XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A2051 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR       A2052 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR        A2053 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR    A2054 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR    A2055 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR    A2056 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR       A2057 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG6,DIAG7,DIAG8,2X5COR       A2058 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG7,DIAG8,2X5COR       A2059 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG8,2X5COR       A2060 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,2X5COR       A2061 3 6 production check_sentinel
#do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8        A2062 3 6 production check_sentinel

#do_work 22 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A2250 3 6 production check_sentinel
#do_work 21 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A2150 3 6 production check_sentinel

#do_work 19 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1950 3 6 production check_sentinel
#do_work 18 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1850 3 6 production check_sentinel
#do_work 17 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1750 3 6 production check_sentinel
#do_work 16 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1650 3 6 production check_sentinel
#do_work 15 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1550 3 6 production check_sentinel
#do_work 14 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1450 3 6 production check_sentinel

#do_work 13 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1350 3 6 test check_sentinel
#do_work 12 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1250 3 6 test check_sentinel
#do_work 11 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1150 3 6 test check_sentinel
#do_work 10 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1050 3 6 test check_sentinel
#do_work  9 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0950 3 6 test check_sentinel
#do_work  8 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0850 3 6 test check_sentinel
#do_work  7 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0750 3 6 test check_sentinel
#do_work  6 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0650 3 6 test check_sentinel
#do_work  5 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0550 3 6 test check_sentinel
#do_work  4 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0450 3 6 test check_sentinel
#do_work  3 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0350 3 6 test check_sentinel
#do_work  2 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0250 3 6 test check_sentinel
#do_work  1 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0150 3 6 production check_sentinel
#do_work  0 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A0050 3 6 production check_sentinel

#do_work 18 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR B1850 9 6 production check_sentinel
#do_work 18 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR C1850 9,3 6 production check_sentinel
do_work 18 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR D1850 12 6 production check_sentinel
do_work 18 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR E1850 3,12 6 production check_sentinel
do_work 18 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR F1850 9,12 6 production check_sentinel
do_work 18 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR G1850 3,9,12 6 production check_sentinel



#do_work 18 INTERCEPT,MOBILITY3 EDGE,CORNER T1850 3 6 production check_sentinel


exit 0
