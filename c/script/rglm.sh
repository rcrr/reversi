#!/bin/bash
#
# rglm.sh
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
# This bash script runs a sequence of REGAB, RGLM, RGLMW programs.
#
# As input it requires four mandatory arguments:
#
# - $1 : empty count        - Board empty square count used for the selection of the game positions 
# - $2 : features           - The list of selected features, separated by commas, without spaces (e.g. MOBILITY,INTERCEPT) 
# - $3 : patterns           - The list of selected features, separated by commas, without spaces (e.g. EDGE,DIAG8)
# - $4 : run-code           - A string used to build the names of the output files. (e.g. A000)
# - $5 : train batches      - It is the REGAB batch_id list (e.g. 3,9) used for the extraction and then for the trainining of the model
# - $6 : validation batches - It is the REGAB batch_id list (e.g. 5,6) used for the extraction and then for the validation of the model
# - $7 : REGAB env          - It is the selected section in the REGAB configuration file (e.g test, production, ...)
# - $8 : check sentinel     - If present and equal to check_sentinel the program checks the existence of the sentinel file
#                             and skips the procedure if found
#
# There are further parameters that oversight the script behaviour:
#
# - OUT_DIR=./rglmdata      - It is the target diregtory for the generated files
# - BINARY_DIR=./build/bin  - It is the directory where are located the REGAB, RGLM, and RGLMW binaries
# - REGAB_CFG=cfg/regab.cfg - it is the REGAB configuration file used to access the database
# - REGAB_PSTAT=CMS,CMR     - It is the list of game position status used for the selection into the REGAB database
#
# The default values are meant to support an usage scenario where the script is located in the ./script directory.
# The script is run from the "REVERSI C base directory", and the output is directed into the ./rglmdata.
# Here an example for a run:
#
#   ./script/rglm.sh 20 MOBILITY,INTERCEPT EDGE A000 3 6 test check_sentinel
#
# The script executes the following steps:
#
# -1- Extracts from the REGAB database the GENERAL DATA file and save it as ${OUT_DIR}/${RUNCODE}_00.dat
#     Using our conventions and the example arguments we will get a new ./rglmdata/A000_00.dat data file,
#     together with its hash file (./rglmdata/A000_00.dat.SHA3-256) and a log file (./rglmdata/A000_00.log).
#
# -2- Extracts from the REGAB database the POSITIONS DATA file and save it as ${OUT_DIR}/${RUNCODE}_positions_check.dat
#     As well the files generated are ./rglmdata/A000_positions_check.dat data file,
#     together with its hash file (./rglmdata/A000_positions_check.dat.SHA3-256) and a log file (./rglmdata/A000_positions_check.log).
#
# -3- Solves the RGLM problem, saving the result as "$OUT_DIR/${RUNCODE}_01.dat". The checksum and log files are created as well.
#
# -4- From the solved data file, the A (extract-ps-table), B (extract-efs-table), and P (extract-gp-table) CVS files are extracted as
#     as well as the binary w (model-weights-output-file) file with the appropriate checksum and a cumulative log file.
#
# -5- The model weight file (${OUT_DIR}/${RUNCODE}_01.w.dat) is used in three different sequential runs of the RGLMW program that
#     generates the weights CSV file, the positions CSV file, and the model check CSV file.
#
# -6- Writes the sentinel file ${OUT_DIR}/${RUNCODE}.sentinel
#
# In summary the target directory is populated with the following files ($ ls -1 ./rglmdata):
# 
# rglmdata/A000_00.dat                        RGLM GENERAL DATA binary file extracted by REGAB.                                      (step 1)
# rglmdata/A000_00.dat.SHA3-256               SHA3-256 fingerprint of the rglmdata/A000_00.dat file.                                 (step 1)
# rglmdata/A000_00.log                        Log file generated by the REGAB extraction.                                            (step 1)
# rglmdata/A000_01.ABPw.log                   Cumulated log file generated by the RGLM extractions.                                  (step 4)
# rglmdata/A000_01.A.csv                      The position summary table, extracted from the solved model, in CSV format.            (step 4)
# rglmdata/A000_01.B.csv                      The entity frequency  summary table, extracted from the solved model, in CSV format.   (step 4)
# rglmdata/A000_01.dat                        RGLM GENERAL DATA binary file solved by RGLM.                                          (step 3)
# rglmdata/A000_01.dat.SHA3-256               SHA3-256 fingerprint of the rglmdata/A000_01.dat file.                                 (step 3)
# rglmdata/A000_01.log                        Log file generated by the REGAB extraction.                                            (step 3)
# rglmdata/A000_01.P.csv                      The game position table, extracted from the solved model, in CSV format.               (step 4)
# rglmdata/A000_01.w.dat                      The model weights binary file extracted from the solved model.                         (step 4)
# rglmdata/A000_01.w.dat.SHA3-256             SHA3-256 fingerprint of the rglmdata/A000_01.w.dat file.                               (step 4)
# rglmdata/A000_01.w.P_check.csv              The residual as a model validation, computed by the RGLMW program, in CSV format.      (step 5)
# rglmdata/A000_01.w.P.csv                    The residual of the game position table, computed by the RGLMW program, in CSV format. (step 5)
# rglmdata/A000_01.w.W.csv                    The model weights table, extracted from the binary file, in CSV format.                (step 5)
# rglmdata/A000_01.w.WPP_check.log            Cumulated log file generated by the RGLMW extractions.                                 (step 5)
# rglmdata/A000_positions_check.dat           RGLM POSITIONS binary file extracted by REGAB.                                         (step 2)
# rglmdata/A000_positions_check.dat.SHA3-256  SHA3-256 fingerprint of the rglmdata/A000_positions_check.dat file.                    (step 2)
# rglmdata/A000_positions_check.log           Log file generated by the REGAB extraction.                                            (step 2)
# rglmdata/A000.sentinel                      The sentinel file.                                                                     (step 6)
#
#
#

BINARY_DIR=./build/bin
OUT_DIR=./rglmdata
REGAB_CFG=cfg/regab.cfg
REGAB_PSTAT=CMS,CMR

CHECK_SENTINEL=false

set -o pipefail

if [ "$1" = "" ]
then
    echo "Positional parameter 1 is empty, exiting script."
    exit 1
fi
EMPTY_COUNT=$1

if [ "$2" = "" ]
then
    echo "Positional parameter 2 (FEATURES) is empty, exiting script."
    exit 1
else
    if [ "$2" = "--" ]
    then
        WITH_FEATURES=false
    else
        WITH_FEATURES=true
    fi    
fi
FEATURES=$2

if [ "$3" = "" ]
then
    echo "Positional parameter 3 (PATTERNS) is empty, exiting script."
    exit 1
else
    if [ "$3" = "--" ]
    then
        WITH_PATTERNS=false
    else
        WITH_PATTERNS=true
    fi
fi
PATTERNS=$3

if [ "$4" = "" ]
then
    echo "Positional parameter 4 (RUNCODE) is empty, exiting script."
    exit 1
fi
RUNCODE=$4

if [ "$5" = "" ]
then
    echo "Positional parameter 5 (TBATCH) is empty, exiting script."
    exit 1
fi
TBATCH=$5

if [ "$6" = "" ]
then
    echo "Positional parameter 6 (VBATCH) is empty, exiting script."
    exit 1
fi
VBATCH=$6

if [ "$7" = "" ]
then
    echo "Positional parameter 7 (REGAB_ENV) is empty, exiting script."
    exit 1
fi
REGAB_ENV=$7

if [ "$8" = "check_sentinel" ]
then
    CHECK_SENTINEL=true
fi
SENTINEL=$OUT_DIR/${RUNCODE}.sentinel
if [ $CHECK_SENTINEL = true ] && [ -f $SENTINEL ]
then
    exit 0
fi


RGLM_DATA_TRAIN_BATCH=${TBATCH}
RGLM_DATA_VALID_BATCH=${VBATCH}

#
# REGAB extraction.
#

REGAB="${BINARY_DIR}/regab"
REGAB_BATCH=$RGLM_DATA_TRAIN_BATCH
REGAB_EC=$EMPTY_COUNT
REGAB_F=$FEATURES
REGAB_P=$PATTERNS
REGAB_OUT="${OUT_DIR}/${RUNCODE}_00.dat"
REGAB_LOG="${OUT_DIR}/${RUNCODE}_00.log"

REGAB_CMD="$REGAB -v --action extract"
REGAB_CMD+=" --config-file $REGAB_CFG"
REGAB_CMD+=" --env $REGAB_ENV"
REGAB_CMD+=" --batch-id $REGAB_BATCH"
REGAB_CMD+=" --position-status $REGAB_PSTAT"
REGAB_CMD+=" --empty-count $REGAB_EC"
if [ "$WITH_FEATURES" = true ]
then
    REGAB_CMD+=" --feature $REGAB_F"
fi
if [ "$WITH_PATTERNS" = true ]
then
    REGAB_CMD+=" --pattern $REGAB_P"
fi
REGAB_CMD+=" --out-file $REGAB_OUT"
REGAB_CMD+=" 2>&1 | tee $REGAB_LOG"

echo -e "The REGAB command is: \"${REGAB_CMD}\""
eval ${REGAB_CMD}
STATUS=$?
if [ $STATUS -ne 0 ]
then
    echo -e "Program REGAB ended abnormally, exiting script."
    exit 1
fi

#
# REGAB extraction of positions used to check the fitness of the model.
#

REGAB_POSITIONS_OUT="${OUT_DIR}/${RUNCODE}_positions_check.dat"
REGAB_POSITIONS_LOG="${OUT_DIR}/${RUNCODE}_positions_check.log"

REGAB_CMD="$REGAB -v --action extract"
REGAB_CMD+=" --config-file $REGAB_CFG"
REGAB_CMD+=" --env $REGAB_ENV"
REGAB_CMD+=" --batch-id $RGLM_DATA_VALID_BATCH"
REGAB_CMD+=" --position-status $REGAB_PSTAT"
REGAB_CMD+=" --empty-count $REGAB_EC"
REGAB_CMD+=" --game-positions"
REGAB_CMD+=" --out-file $REGAB_POSITIONS_OUT"
REGAB_CMD+=" 2>&1 | tee $REGAB_POSITIONS_LOG"

echo -e "The REGAB command is: \"$REGAB_CMD\""

eval $REGAB_CMD
STATUS=$?
if [ $STATUS -ne 0 ]
then
    echo -e "Program REGAB ended abnormally, exiting script."
    exit 1
fi

#
# RGLM solution.
#

RGLM="${BINARY_DIR}/rglm"
RGLM_OUT="$OUT_DIR/${RUNCODE}_01.dat"
RGLM_LOG="$OUT_DIR/${RUNCODE}_01.log"

RGLM_CMD="$RGLM --verbose --solve"
RGLM_CMD+=" --input-file $REGAB_OUT"
RGLM_CMD+=" --output-file $RGLM_OUT"
RGLM_CMD+=" 2>&1 | tee $RGLM_LOG"

echo -e "The RGLM command is: \"$RGLM_CMD\""

eval $RGLM_CMD
STATUS=$?
if [ $STATUS -ne 0 ]
then
    echo -e "Program RGLM ended abnormally, exiting script."
    exit 1
fi

#
# File extraction from the RGLM solution.
#

RGLM_EXT_LOG=$OUT_DIR/${RUNCODE}_01.ABPw.log
RGLM_EXT_A=$OUT_DIR/${RUNCODE}_01.A.csv
RGLM_EXT_B=$OUT_DIR/${RUNCODE}_01.B.csv
RGLM_EXT_P=$OUT_DIR/${RUNCODE}_01.P.csv
RGLM_EXT_w=$OUT_DIR/${RUNCODE}_01.w.dat

RGLM_EXT_CMD="$RGLM --verbose --input-file $RGLM_OUT"
RGLM_EXT_CMD+=" -A $RGLM_EXT_A"
RGLM_EXT_CMD+=" -B $RGLM_EXT_B"
RGLM_EXT_CMD+=" -P $RGLM_EXT_P"
RGLM_EXT_CMD+=" -w $RGLM_EXT_w"
RGLM_EXT_CMD+=" 2>&1 | tee $RGLM_EXT_LOG"

echo -e "The RGLM extraction command is: \"$RGLM_EXT_CMD\""

eval $RGLM_EXT_CMD
STATUS=$?
if [ $STATUS -ne 0 ]
then
    echo -e "Program RGLM ended abnormally while extracting data from the solution, exiting script."
    exit 1
fi

#
# RGLMW : extracting weights and checking the fitness of the model.
#

RGLMW="${BINARY_DIR}/rglmw"
RGLMW_EXT_W=$OUT_DIR/${RUNCODE}_01.w.W.csv
RGLMW_EXT_P=$OUT_DIR/${RUNCODE}_01.w.P.csv
RGLMW_EXT_C=$OUT_DIR/${RUNCODE}_01.w.P_check.csv
RGLMW_EXT_LOG=$OUT_DIR/${RUNCODE}_01.w.WPP_check.log

RGLMW_W_CMD="$RGLMW --verbose --weights-file $RGLM_EXT_w"
RGLMW_W_CMD+=" --extract-weights $RGLMW_EXT_W"
RGLMW_W_CMD+=" 2>&1 | tee $RGLMW_EXT_LOG"

RGLMW_P_CMD="$RGLMW --verbose --weights-file $RGLM_EXT_w"
RGLMW_P_CMD+=" --positions-file $RGLM_OUT"
RGLMW_P_CMD+=" --extract-positions $RGLMW_EXT_P"
RGLMW_P_CMD+=" 2>&1 | tee -a $RGLMW_EXT_LOG"

RGLMW_C_CMD="$RGLMW --verbose --weights-file $RGLM_EXT_w"
RGLMW_C_CMD+=" --positions-file $REGAB_POSITIONS_OUT"
RGLMW_C_CMD+=" --extract-positions $RGLMW_EXT_C"
RGLMW_C_CMD+=" 2>&1 | tee -a $RGLMW_EXT_LOG"

echo -e "The RGLMW weights extraction command is: \"$RGLMW_W_CMD\""
eval $RGLMW_W_CMD
STATUS=$?
if [ $STATUS -ne 0 ]
then
    echo -e "Program RGLMW ended abnormally while extracting the weights table, exiting script."
    exit 1
fi
echo -e "--- *** --- *** --- *** --- *** ---" >> $RGLMW_EXT_LOG

echo -e "The RGLMW positions extraction command is: \"$RGLMW_P_CMD\""
eval $RGLMW_P_CMD
STATUS=$?
if [ $STATUS -ne 0 ]
then
    echo -e "Program RGLMW ended abnormally while extracting the game position table, exiting script."
    exit 1
fi
echo -e "--- *** --- *** --- *** --- *** ---" >> $RGLMW_EXT_LOG

echo -e "The RGLMW residual check command is: \"$RGLMW_C_CMD\""
eval $RGLMW_C_CMD
STATUS=$?
if [ $STATUS -ne 0 ]
then
    echo -e "Program RGLMW ended abnormally while extracting the game position table for model checking, exiting script."
    exit 1
fi

echo -e "Touching file: \"$SENTINEL\""
touch $SENTINEL

set +o pipefail

exit 0
