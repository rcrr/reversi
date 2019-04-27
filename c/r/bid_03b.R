
library(ggplot2)
library(data.table)

setwd('/home/rcrr/base/prj/reversi/c/r')

#dt <- fread('../tmp/eval_bid_06_by_bid_03_p_RDS_01_weights.csv')
#dt <- fread('../tmp/eval_bid_06_by_bid_03_p_CORNER_01_weights.csv')
dt <- fread('../tmp/eval_ec_20_bid_05_by_bid_03_p_EDGE_s_CMS-CMR_01_weights.csv')

step <- 0.01
limits <- seq(from = -1.0, to  = 1.0, by = step)
values <- (limits[1:length(limits) - 1] + limits[2:length(limits)] ) * 0.5

hist <- as.data.frame(table(cut(dt$RESIDUAL, breaks = limits)), responseName = "frequency")
hist_dummy <- as.data.frame(table(cut(jitter(dt$GAME_VALUE_TRANSFORMED - 0.5, factor=0.01, amount=0.02), breaks = limits)), responseName = "dummy")

hist[, "residual"] <- values
hist[, "dummy"] <- hist_dummy[, "dummy"]
colnames(hist)[which(names(hist) == "Var1")] <- "interval"

g <- ggplot(hist) +
  geom_line(aes(x=residual, y=frequency, color='red')) +
  geom_line(aes(x=residual, y=dummy, color='black'))
