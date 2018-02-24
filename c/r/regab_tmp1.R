library(data.table)
library(moments)
library(ggplot2)

DT <- fread('./data/out_ec_pid_value_cnt.csv', header = TRUE, sep = ';')
setkey(DT, empty_count, index_value, game_value)

dt0 <- DT[, .(cnt = sum(cnt)), by = .(empty_count, game_value)]
setkey(dt0, empty_count, game_value)

dt1 <- dt0[, .(cnt = sum(cnt), max_cnt = max(cnt)), by = empty_count]
setkey(dt1, empty_count)

dt0_summary <- data.table(empty_count = integer(),
                          mean = numeric(),
                          median = integer(),
                          var = numeric(),
                          skewness = numeric(),
                          kurtosis = numeric())
for (ec in dt1[,empty_count]) {
  d <- rep(dt0[empty_count == ec, game_value], dt0[empty_count == ec, cnt])
  dt0_summary <- rbind(dt0_summary, list(empty_count = ec,
                                         mean = mean(d),
                                         median = median(d),
                                         var = var(d),
                                         skewness = skewness(d),
                                         kurtosis = kurtosis(d)))
}
setkey(dt0_summary, empty_count)
dt1 <- dt0[dt1][cnt == max_cnt, mult = "first", .(empty_count, mode = game_value, cnt = i.cnt, max_cnt)]
setkey(dt1, empty_count)
dt0_summary <- dt1[dt0_summary]
rm(d, ec, dt1)

dt0 <- dt0[dt0_summary][, .(empty_count, game_value, cnt, freq = cnt / i.cnt)]

g_dt0_summary_0 <- ggplot(dt0_summary[empty_count > 1]) +
  geom_col(aes(x = empty_count, y = mean), fill = "#99CCCC", colour = "#9966FF") +
  labs(x = "Board empty count", y = "Game value", title = "Mean of game value")

g_dt0_0 <- ggplot(dt0[empty_count == 2, .(game_value, freq)]) +
  geom_col(aes(x = game_value, y = freq), fill = "#99CCCC", colour = "#9966FF") +
  labs(x = "Game value", y = "Probability", title = "Game value probability distribution")

g_dt_0 <- ggplot(DT[empty_count == 20][index_value == 3280, .(game_value, cnt)]) +
  geom_col(aes(x = game_value, y = cnt), fill = "#FF3300", colour = "#FF6600") +
  labs(x = "Game value", y = "Count", title = "Game value frequency")
