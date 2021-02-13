# install.packages("RPostgreSQL")
require(RPostgreSQL)
require(data.table)

source('~/base/prj/reversi/c/r/rglmw.R')

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "tst_regab",
                 host = "localhost", port = 5432,
                 user = "tst_regab", password = "tst_regab")

batches <- dbGetQuery(con, 'SELECT * FROM reversi.regab_prng_gp_h;')

lmc <-data.table(dbGetQuery(con, "SELECT seq, legal_move_count, legal_move_count_adjusted FROM reversi.regab_prng_gp WHERE batch_id = 6 AND empty_count = 20 AND status IN('CMS', 'CMR');"))
setnames(lmc, "seq", "GP_ID")
setnames(lmc, "legal_move_count", "LMC")
setnames(lmc, "legal_move_count_adjusted", "LMCA")
setkey(lmc, "GP_ID")


# close the connection
dbDisconnect(con)

lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
dbUnloadDriver(drv)
rm(drv, con)

dtv_a2050 <- load_dtv_model_weights("/lake/rcrr/rglmdata", "A2050")
setkey(dtv_a2050, "GP_ID")

res <- merge(dtv_a2050, lmc, all.x=TRUE)