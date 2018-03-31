

library(RPostgreSQL)
library(data.table)

#
# pattern: a value from the pattern_name field in regab_prng_patterns
# batch:   a value from the seq field in regab_prng_gp_h
# ec:      empty_count must be in [0..60]
#
#
# Returns a data table having one row for each [GAME_POSITION, PATTERN_INSTANCE]
#
# gp_id             game position id
# pattern_instance  pattern instance, from 0 to the number of patterns - 1, [0..3] for EDGE
# index_value       the index id
# game_value        the game value
#
regab_extract_pattern_batch_ec_data <- function(pattern, batch, ec) {

  dt <- NULL
  con <- NULL

  pg <- dbDriver("PostgreSQL")

  out <- tryCatch (
    {
      con <- dbConnect(pg,
                       host = "localhost",
                       port = 5432,
                       dbname = "tst_regab",
                       user = "tst_regab",
                       password = "tst_regab")

      if (!isPostgresqlIdCurrent(con)) {
        stop("Database connection is not valid!")
      }

      dbGetQuery(con, "SET search_path TO reversi;")

      # PATTERN
      patterns <- dbGetQuery(con, "SELECT * FROM regab_prng_patterns;")
      setDT(patterns)
      setkey(patterns, seq)
      patterns[, pattern_name := trimws(pattern_name)]

      pattern_id <- unname(patterns[pattern_name == pattern, seq])

      if (length(pattern_id) != 1) {
        cat(sprintf("Parameter pattern = %s has not been found in table regab_prng_patterns.\n", pattern),
            sep = '', file = stderr())
        cat(print(patterns))
        stop("Wrong pattern parameter.")
      }

      # BATCH
      batches <- dbGetQuery(con, "SELECT * FROM regab_prng_gp_h;")
      setDT(batches)
      setkey(batches, seq)

      selected_batch <- batches[seq == batch,]
      if (nrow(selected_batch) != 1) {
        cat(sprintf("Parameter batch = %s has not been found in table regab_prng_gp_h.\n", batch),
            sep = '', file = stderr())
        cat(print(batches))
        stop("Wrong batch parameter.")
      }

      # EC (EMPTY COUNT)
      if (ec < 0 || ec > 60) {
        cat(sprintf("Parameter ec = %s must be in [0..60] range.\n", ec),
            sep = '', file = stderr())
        stop("Wrong ec parameter.")
      }

      # CLASS
      class_query <- paste("SELECT * FROM regab_prng_gp_classification_h WHERE",
                           sprintf("pattern_id = %d AND", pattern_id),
                           sprintf("empty_count = %d AND", ec),
                           sprintf("batch_id = %d", batch),
                           sep = " ")
      classes <- dbGetQuery(con, class_query)

      if (nrow(classes) == 0) {
        cat(sprintf("Selection returned zero classification groups.\n"), sep = '', file = stderr())
        stop("Empty selection.")
      } else if (nrow(classes) > 1) {
        cat(sprintf("Selection returned multiple classification groups.\n"), sep = '', file = stderr())
        cat(print(classes))
        stop("Wrong selection.")
      }
      setDT(classes)
      setkey(classes, seq)
      class_id <- classes[, seq]

      # COUNTING DATA
      count_query <- paste("SELECT status, count(1)",
                           "FROM regab_prng_gp_classification WHERE",
                           sprintf("class_id = %d", class_id),
                           "GROUP BY status ORDER BY status;",
                           sep = " ")
      count_by_status <- dbGetQuery(con, count_query)
      setDT(count_by_status)
      setkey(count_by_status, status)
      cnt <- count_by_status[status == "CMP", count]

      # EXTRACTION OF: GAME POSITION - PATTERN INSTANCE - PATTERN CLASSIFICATION
      # SELECT cl.pattern_instance, cl.index_value, cl.gp_id, gp.game_value FROM regab_prng_gp_classification AS cl LEFT JOIN regab_prng_gp AS gp ON cl.gp_id = gp.seq WHERE cl.class_id = 21 AND cl.status = 'CMP';
      extract_query <- paste("SELECT",
                             "cl.gp_id, cl.pattern_instance, cl.index_value, gp.game_value",
                             "FROM regab_prng_gp_classification AS cl LEFT JOIN regab_prng_gp AS gp ON cl.gp_id = gp.seq WHERE",
                             sprintf("cl.class_id = %d AND cl.status = 'CMP';", class_id),
                             sep = " ")
      extraction <- dbGetQuery(con, extract_query)
      setDT(extraction)
      setkey(extraction, gp_id, pattern_instance)

      dt <- extraction

    },
    error = function(msg) { cat(sprintf("ERROR: %s\n", msg), sep = '', file = stderr()) },
    warning = function(msg) { cat(sprintf("WARNING: %s\n", msg), sep = '', file = stderr()) },
    message = function(msg) { cat(sprintf("MESSAGE: %s\n", msg), sep = '', file = stderr()) },
    finally = {
      dbDisconnect(con)
    }
  ) # end of tryCatch

  return(dt)
}

regab_compute_freqs <- function(t) {
  dt <- t[, .(cnt = .N), by = .(index_value, game_value)]
  setkey(dt, index_value, game_value)
  return(dt)
}

#
# Parameter f is a data table that must have the following columns:
# index_value: the pattern index
# game_value:  the game value
# cnt:         count of game positions having the key [index_value, game_value]
#
# Probabilities:
# P(V):   Probability to obtain a game value equal or better than V.
# P(E):   Probability to have a game position with a given edge pattern index equal to E.
# P(V|E): Probability to obtain a game value equal or better than V, given the edge pattern index E.
# P(E|V): Probability to have a game position with a given edge pattern index equal to E,
#         given the subset of the game position population with a game value equal or better than V.
#
# Bayes rule:
# P(V|E) = ( P(E|V) / P(E) ) * P(V)
#
# cnt_s:  scalar, is the sample size, computed as the overall sum of frequencies. cnt is computed as f[,sum(cnt)]
# cnt_v:  a data.table having key [game_value] and columns [game_value, cnt], cnt is the count of game positions
#         in the sample having value equal or greater than game_value.
# cnt_e:  a data.table having key [index_value] and columns [index_value, cnt], cnt is the count of game positions
#         in the sample having pattern index equal to index_value.
# cnt_ev: a data.table having key [index_value, game_value] and columns [index_value, game_value, cnt], cnt is the
#         count of game positions in the sample having the key pattern index, and a game value greater or equal to
#         teh key game_value.
#
# f <- regab_compute_freqs(regab_extract_pattern_batch_ec_data("EDGE", 3, 16))
# ret <- regab_compute_coefficients(f)
# write.csv(format(ret$p_v, scientific = FALSE), file = "data/regab_1m_16e_p_v.csv", quote = FALSE, row.names = FALSE)
# write.csv(format(ret$p_e, scientific = FALSE), file = "data/regab_1m_16e_p_e.csv", quote = FALSE, row.names = FALSE)
# write.csv(format(ret$p_ev, scientific = FALSE), file = "data/regab_1m_16e_p_ev.csv", quote = FALSE, row.names = FALSE)
# write.csv(format(ret$p_ev_on_p_e[,.(index_value, game_value, probability)], scientific = FALSE), file = "data/regab_1m_16e_p_ev_on_p_e.csv", quote = FALSE, row.names = FALSE)
#
regab_compute_coefficients <- function(f) {
  # SHOULD BE TAKEN FROM THE PATTERN TABLE
  pattern_square_count <- 8
  indexes <- seq(0, 3^pattern_square_count - 1)
  values <- as.integer(seq(-64, 64, by = 2))
  dt <- merge(x = indexes, y = values, by = NULL)
  setDT(dt)
  setnames(dt, c("index_value", "game_value"))
  setkey(dt, index_value, game_value)
  dt <- f[dt][, cnt := as.integer(ifelse(is.na(cnt), 0, cnt))]
  setkey(dt, index_value, game_value)

  # Total population count
  cnt_s <- dt[,sum(cnt)]
  # Count of game position having a value equal or better than a given game value.
  cnt_v <- data.table(game_value = values, cnt = sapply(values, function(v) dt[game_value >= v, sum(cnt)]))
  setkey(cnt_v, game_value)
  # Count of game position having a given pattern index value.
  cnt_e <- dt[, .(cnt = sum(cnt)), by = index_value]
  setkey(cnt_e, index_value)
  # Count of game positions having the pattern index equal to index_value, and a game value equal or greater than game_value.
  cnt_ev <- copy(dt)
  lv <- length(values)
  li <- length(indexes)
  for (ii in 1:li) {
    for (iv in 1:lv) {
      gv <- -64 + 2 * (iv - 1)
      idx <- ii - 1
      set(cnt_ev, as.integer(iv + (ii - 1) * lv), as.integer(3), dt[index_value == idx,][game_value >= gv, sum(cnt)])
    }
  }

  # P(V)
  p_v <- copy(cnt_v)
  p_v <- p_v[, probability := cnt/cnt_s]
  p_v <- p_v[, .(game_value, probability)]
  setkey(p_v, game_value)
  # P(E)
  p_e <- copy(cnt_e)
  p_e <- p_e[, probability := cnt/cnt_s]
  p_e <- p_e[, .(index_value, probability)]
  setkey(p_e, index_value)
  # P(E|V)
  p_ev <- copy(cnt_ev)
  p_ev[, probability := cnt/rep(cnt_v$cnt, li)]
  p_ev <- p_ev[, .(index_value, game_value, probability)]
  setkey(p_ev, index_value, game_value)

  # P(E|V) / P(E)
  p_ev_on_p_e <- copy(p_ev)
  setnames(p_ev_on_p_e, c("index_value", "game_value", "p_ev"))
  p_ev_on_p_e[, p_e := c(sapply(p_e$probability, function(x) rep(x, lv)))]
  p_ev_on_p_e[, probability := p_ev/p_e]
  setkey(p_ev_on_p_e, index_value, game_value)

  ret <- list(cnt_s = cnt_s,
              cnt_v = cnt_v,
              cnt_e = cnt_e,
              cnt_ev = cnt_ev,
              p_v = p_v,
              p_e = p_e,
              p_ev = p_ev,
              p_ev_on_p_e = p_ev_on_p_e)

  return(ret)
}
