
library(data.table)
library(moments)
library(ggplot2)

dt_edge_inndex_stats <- function(dt, ec, i) {

  dtec <- dt[empty_count == ec, .(gv = game_value, iv = index_value, cnt)]
  setkey(dtec, gv, iv)

  dteca <- dtec[, .(cnta = sum(cnt)), by = gv]
  setkey(dteca, gv)
  n <- dteca[, sum(cnta)]

  dteci <- dtec[iv == i, .(gv, cnti = cnt)]
  setkey(dteci, gv)

  dtecx <- merge(dteca, dteci, all= TRUE)[is.na(cnti), cnti := 0][, .(gv, cnta, cnti, cntd = cnta - cnti, pa = cnta / n, pi = cnti / n, pd = (cnta - cnti) / n)]
  setkey(dtecx, gv)

  dtecy <- funion(dtecx[, .(gv, t = 'i', p = pi)], dtecx[, .(gv, t = 'd', p = pd)])
  setkey(dtecy, gv, t)

  return(dtecy)
}

plot_edge_inndex_stats <- function(dt, ec, i) {

  d <- dt_edge_inndex_stats(dt, ec, i)

  r <- d[t == 'i', sum(p)]

  g <- ggplot(d) +
    geom_col(aes(x = gv, y = p, fill = t)) +
    scale_fill_manual("x", values = c("d" = "orange", "i" = "black")) +
    geom_line(data = d[t == 'i', .(gv, p = p * (1 / r))], aes(x = gv, y = p))

  return(g)
}

hist_edge_avarage_value <- function(dt, ec) {
  m <- dt[, .(mean = sum(game_value * cnt) / sum(cnt)), by = empty_count]
  d <- dt[, .(cnt = sum(cnt), avg = sum(game_value * cnt) / sum(cnt)), by = .(empty_count, index_value)]
  h <- hist(d[empty_count == ec, avg - m[empty_count == ec, mean]])
  return(h)
}

#scores <- seq(from = -64, to = +64, by = 2)
#edges <- seq(from = 0, to = 3^8, by = 1)
