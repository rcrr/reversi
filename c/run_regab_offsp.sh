#!/bin/bash

# Before running this script:
# prd_regab=> UPDATE regab_prng_gp SET status = 'CMQ', cst_time = now() WHERE status = 'CMP' AND batch_id = 10;
#
# After running the script:
# prd_regab=> SELECT * FROM regab_gp_populate_pattern_class_table(10, 0, 6, '{CMR,CMS}', FALSE, FALSE);
#
# To have a final check:
# SELECT a.empty_count AS ec, count(a.seq) AS cnt, count(b.gp_id) AS classified FROM regab_prng_gp AS a LEFT JOIN regab_prng_gp_pattern_class AS b ON a.seq = b.gp_id
#   WHERE a.batch_id = 10 AND a.status IN ('CMS', 'CMR') GROUP BY a.empty_count ORDER BY a.empty_count;
#

# Batch ID
BID=11

# Empty count range
FIRST=23
LAST=23

# Max number of positions
MAXNP=10000000

for i in $(seq ${FIRST} ${LAST});
do
    ii=$(printf "%02d\n" $i);
    nohup ./build/bin/regab -v -a offspring -c cfg/regab.cfg -e production -b ${BID} -y ${i} -n ${MAXNP} > ./build/out/regab_offspa.${ii}.log 2>&1 &
done
