
library(RPostgreSQL)
library(data.table)

pg <- dbDriver("PostgreSQL")

con <- dbConnect(pg,
                 host = "localhost",
                 port = 5432,
                 dbname = "tst_regab",
                 user = "tst_regab",
                 password = "tst_regab")

dbGetQuery(con, "SET search_path TO reversi;")

patterns <- dbGetQuery(con, "SELECT seq, pattern_name, ninstances FROM regab_prng_patterns;")
setDT(patterns)
setkey(patterns, seq)

classes <- dbGetQuery(con, "SELECT seq, pattern_id, batch_id, empty_count FROM regab_prng_gp_classification_h;")
setDT(classes)
setkey(classes, seq)

batches <- dbGetQuery(con, "SELECT seq, prng_seed, ngames, npositions FROM regab_prng_gp_h;")
setDT(batches)
setkey(batches, seq)

selected_batch_id <-  4 # 1000 games
selected_min_ec   <-  4
selected_max_ec   <- 20
positions_query <- paste("SELECT",
                         "seq, batch_id, game_id, pos_id, empty_count, legal_move_count_adjusted, game_value, best_move",
                         "FROM regab_prng_gp",
                         sprintf("WHERE batch_id = %d AND", selected_batch_id),
                         sprintf("empty_count <= %d AND empty_count >= %d;", selected_max_ec, selected_min_ec),
                         sep = " ")
positions <- dbGetQuery(con, positions_query)
setDT(positions)
setkey(positions, seq)

selected_pattern_id <- 1 # EDGE
classifications_query <- paste("SELECT",
                               "seq, class_id, gp_id, pattern_instance, index_value",
                               "FROM regab_prng_gp_classification",
                               "WHERE class_id IN (SELECT seq FROM regab_prng_gp_classification_h WHERE",
                               sprintf("batch_id = %d AND pattern_id = %d AND empty_count <= %d AND empty_count >= %d);",
                                       selected_batch_id, selected_pattern_id, selected_max_ec, selected_min_ec),
                               sep = " ")
classifications <- dbGetQuery(con, classifications_query)
setDT(classifications)
setkey(classifications, seq)

p <- positions[, .(seq, ec = empty_count, gv = game_value)]
c <- classifications[, .(gp_id, pi = pattern_instance, iv = index_value)]
setkey(c, gp_id, pi)
c <- dcast(c, gp_id ~ pi, value.var = "iv")
setnames(c, "0", "EDGE_0")
setnames(c, "1", "EDGE_1")
setnames(c, "2", "EDGE_2")
setnames(c, "3", "EDGE_3")
p <- p[c]
rm(c)
setnames(p, "seq", "gp_id")
p[, EDGE_0 := as.factor(EDGE_0)]
p[, EDGE_1 := as.factor(EDGE_1)]
p[, EDGE_2 := as.factor(EDGE_2)]
p[, EDGE_3 := as.factor(EDGE_3)]

p[, gp_id := NULL]
#p[, gv := (gv + 64) / 128]
p[, gv := ifelse(gv > 0, 1, 0)]

dbDisconnect(con)
rm(con, pg, classifications_query, positions_query, selected_batch_id, selected_max_ec, selected_min_ec, selected_pattern_id)

# p_logit <- glm(formula = "gv ~ ec + EDGE_0 + EDGE_1 + EDGE_2 + EDGE_3", family = binomial, data = p)
