

#
# Combines file frequencies together, by summing up the COUNT field.
#

library(bit64)
library(data.table)

#DT0 <- fread('data_pattern_index_frequencies_2X5COR_112_10000000000.csv', sep = ';', header = TRUE)
#DT1 <- fread('data_pattern_index_frequencies_2X5COR_116_10000000000.csv', sep = ';', header = TRUE)
#DT2 <- fread('data_pattern_index_frequencies_2X5COR_292_10000000000.csv', sep = ';', header = TRUE)
#DT3 <- fread('data_pattern_index_frequencies_2X5COR_298_10000000000.csv', sep = ';', header = TRUE)
#DT4 <- fread('data_pattern_index_frequencies_2X5COR_372_10000000000.csv', sep = ';', header = TRUE)
#DT5 <- fread('data_pattern_index_frequencies_2X5COR_378_10000000000.csv', sep = ';', header = TRUE)
#DT6 <- fread('data_pattern_index_frequencies_2X5COR_973_10000000000.csv', sep = ';', header = TRUE)
#DT7 <- fread('data_pattern_index_frequencies_2X5COR_977_10000000000.csv', sep = ';', header = TRUE)

#DT <- rbindlist(list(DT0, DT1, DT2, DT3, DT4, DT5, DT6, DT7))[, lapply(.SD, sum, na.rm = TRUE), by = .(EMPTY_COUNT, PATTERN_INDEX)]
#setkey(DT, EMPTY_COUNT, PATTERN_INDEX)

#fwrite(DT, file = 'data_pattern_index_frequencies_2X5COR_112_116_292_298_372_378_973_977_80000000000.csv', sep = ';', col.names = TRUE, append = FALSE)

####################################

#DT0 <- fread('data_pattern_index_frequencies_CORNER_112_4000000000.csv', sep = ';', header = TRUE)
#DT1 <- fread('data_pattern_index_frequencies_CORNER_116_4000000000.csv', sep = ';', header = TRUE)
#DT2 <- fread('data_pattern_index_frequencies_CORNER_292_4000000000.csv', sep = ';', header = TRUE)
#DT3 <- fread('data_pattern_index_frequencies_CORNER_298_4000000000.csv', sep = ';', header = TRUE)
#DT4 <- fread('data_pattern_index_frequencies_CORNER_372_4000000000.csv', sep = ';', header = TRUE)
#DT5 <- fread('data_pattern_index_frequencies_CORNER_378_4000000000.csv', sep = ';', header = TRUE)
#DT6 <- fread('data_pattern_index_frequencies_CORNER_973_4000000000.csv', sep = ';', header = TRUE)
#DT7 <- fread('data_pattern_index_frequencies_CORNER_977_4000000000.csv', sep = ';', header = TRUE)

#DT <- rbindlist(list(DT0, DT1, DT2, DT3, DT4, DT5, DT6, DT7))[, lapply(.SD, sum, na.rm = TRUE), by = .(EMPTY_COUNT, PATTERN_INDEX)]
#setkey(DT, EMPTY_COUNT, PATTERN_INDEX)

#fwrite(DT, file = 'data_pattern_index_frequencies_CORNER_112_116_292_298_372_378_973_977_32000000000.csv', sep = ';', col.names = TRUE, append = FALSE)

####################################

DT0 <- fread('data_pattern_index_frequencies_XEDGE_112_10000000000.csv', sep = ';', header = TRUE)
DT1 <- fread('data_pattern_index_frequencies_XEDGE_116_10000000000.csv', sep = ';', header = TRUE)
DT2 <- fread('data_pattern_index_frequencies_XEDGE_292_10000000000.csv', sep = ';', header = TRUE)
DT3 <- fread('data_pattern_index_frequencies_XEDGE_298_10000000000.csv', sep = ';', header = TRUE)
DT4 <- fread('data_pattern_index_frequencies_XEDGE_372_10000000000.csv', sep = ';', header = TRUE)
DT5 <- fread('data_pattern_index_frequencies_XEDGE_378_10000000000.csv', sep = ';', header = TRUE)
DT6 <- fread('data_pattern_index_frequencies_XEDGE_973_10000000000.csv', sep = ';', header = TRUE)
DT7 <- fread('data_pattern_index_frequencies_XEDGE_977_10000000000.csv', sep = ';', header = TRUE)

DT <- rbindlist(list(DT0, DT1, DT2, DT3, DT4, DT5, DT6, DT7))[, lapply(.SD, sum, na.rm = TRUE), by = .(EMPTY_COUNT, PATTERN_INDEX)]
setkey(DT, EMPTY_COUNT, PATTERN_INDEX)

fwrite(DT, file = 'data_pattern_index_frequencies_XEDGE_112_116_292_298_372_378_973_977_80000000000.csv', sep = ';', col.names = TRUE, append = FALSE)

