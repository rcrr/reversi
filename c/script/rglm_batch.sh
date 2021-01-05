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
# This bash script generates ....
#
#   ./script/rglm.sh 20 MOBILITY,INTERCEPT EDGE A000

do_work () {
    
    local SCRIPT="./script/rglm.sh"
    eval ${SCRIPT} $1 $2 $3 $4 $5 
    STATUS=$?
    if [ $STATUS -ne 0 ]
    then
        echo -e "Script ${SCRIPT} ended abnormally, exiting $0 ..."
        exit 1
    fi
    return 0
}

do_work 20 INTERCEPT --     A2000 check_sentinel
do_work 20 MOBILITY  --     A2001 check_sentinel
do_work 20 MOBILITY2 --     A2002 check_sentinel
do_work 20 MOBILITY3 --     A2003 check_sentinel
do_work 20 --        EDGE   A2004 check_sentinel
do_work 20 --        R2     A2005 check_sentinel
do_work 20 --        R3     A2006 check_sentinel
do_work 20 --        R4     A2007 check_sentinel
do_work 20 --        DIAG8  A2008 check_sentinel
do_work 20 --        DIAG7  A2009 check_sentinel
do_work 20 --        DIAG6  A2010 check_sentinel
do_work 20 --        DIAG5  A2011 check_sentinel
do_work 20 --        DIAG4  A2012 check_sentinel
do_work 20 --        DIAG3  A2013 check_sentinel
do_work 20 --        CORNER A2014 check_sentinel
do_work 20 --        XEDGE  A2015 check_sentinel
do_work 20 --        2X5COR A2016 check_sentinel

do_work 20 INTERCEPT,MOBILITY  -- A2020 check_sentinel
do_work 20 INTERCEPT,MOBILITY2 -- A2021 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 -- A2022 check_sentinel

do_work 20 -- EDGE,XEDGE A2025 check_sentinel

do_work 20 INTERCEPT,MOBILITY3 EDGE   A2030 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 R2     A2031 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 R3     A2032 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 R4     A2033 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 DIAG8  A2034 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE  A2035 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 CORNER A2036 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 2X5COR A2037 check_sentinel

do_work 20 INTERCEPT,MOBILITY3 EDGE,R2,R3,R4                                     A2040 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 EDGE,R2,R3,R4,DIAG3,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8 A2041 check_sentinel

do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A2050 check_sentinel
do_work 20 --                  XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A2051 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR       A2052 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR        A2053 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR    A2054 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR    A2055 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR    A2056 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR       A2057 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG6,DIAG7,DIAG8,2X5COR       A2058 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG7,DIAG8,2X5COR       A2059 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG8,2X5COR       A2060 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,2X5COR       A2061 check_sentinel
do_work 20 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8        A2062 check_sentinel

do_work 22 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A2250 check_sentinel
do_work 21 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A2150 check_sentinel

do_work 19 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1950 check_sentinel
do_work 18 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1850 check_sentinel
do_work 17 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1750 check_sentinel
do_work 16 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1650 check_sentinel
do_work 15 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1550 check_sentinel
do_work 14 INTERCEPT,MOBILITY3 XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR A1450 check_sentinel


exit 0
