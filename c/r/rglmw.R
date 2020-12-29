library(ggplot2)
library(bit64)
library(data.table)

setwd('/home/rcrr/base/prj/reversi/c/r')

dt_edge  <- fread('../tmp/bid_03_f_INT_MO2_p_EDGE_01.w.P.csv')
dt_xedge <- fread('../tmp/bid_03_f_INT_MO2_p_XEDGE_01.w.P.csv')

gvt_mean <- mean(dt_edge$GAME_VALUE_TRANSFORMED)

residual_edge_mean <- mean(dt_edge$RESIDUAL)
residual_edge_var <- var(dt_edge$RESIDUAL)
residual_edge_sd <- sd(dt_edge$RESIDUAL)

residual_xedge_mean <- mean(dt_xedge$RESIDUAL)
residual_xedge_var <- var(dt_xedge$RESIDUAL)
residual_xedge_sd <- sd(dt_xedge$RESIDUAL)

p1 <- ggplot() +
  geom_density(aes_string(x = dt_edge$GAME_VALUE_TRANSFORMED - gvt_mean), color = "black") +
  geom_density(aes_string(x = dt_edge$RESIDUAL), color = "red") +
  geom_density(aes_string(x = dt_xedge$RESIDUAL), color = "blue")
  