library(MASS)
library(ggplot2)
library(data.table)
library(moments)

edge_index_to_seq <- function(index) {
  tmp <- index
  result <- c()
  for (i in 1:8) {
    result[i] <- tmp %% 3
    tmp <- tmp %/% 3
  }
  return(result)
}

edge_seq_to_string <- function(edge) {
  s <- c()
  for (i in 1:8) {
    if (edge[i] == 0) {
      s[i] <- "."
    } else if (edge[i] == 1) {
      s[i] <- "@"
    } else if (edge[i] == 2) {
      s[i] <- "O"
    } else {
      s[i] <- "X"
    }
  }
  return(paste(s, collapse = ''))
}

edge_index_to_string <- function(index) {
  return(edge_seq_to_string(edge_index_to_seq(index)))
}

DT <- fread('./data/out_ec_pid_value_cnt.csv', header = TRUE, sep = ';')
setkeyv(DT, c("empty_count", "index_value", "game_value"))

ec <- 20   # empty count
iv <- 5468 # index value
iv_to_s <- edge_index_to_string(iv)

eps <- 0.0001

dt <- DT[which(DT$empty_count == ec & DT$index_value == iv), .(cnt = sum(cnt)), by = game_value][, freq := cnt/sum(cnt)]

x <- rep(dt$game_value, dt$cnt)
y <- (x + 64 + eps) / (128 + 2 * eps)

beta <- fitdistr(y, densfun = dbeta, start = list(shape1 = 3., shape2 = 3.), lower = c(0.0001, 0.0001))

a <- unname(beta$estimate[1])
b <- unname(beta$estimate[2])

me_beta <- a / (a + b)
va_beta <- (a * b) / ((a + b)^2 * (a + b + 1))
sk_beta <- (2 * (b - a) * sqrt(a + b + 1)) / ((a + b + 2) * sqrt(a * b))
ku_beta <- 6 * ((a - b)^2 * (a + b + 1) - (a * b * (a + b + 2))) / (a * b * (a + b + 2) * (a + b + 3))

me_y <- mean(y)
va_y <- var(y)
sd_y <- sd(y)
sk_y <- skewness(y)
ku_y <- kurtosis(y)

h <- hist(y, breaks = 16)
xhist <- c(min(h$breaks), h$breaks)
yhist <- c(0, h$density,0)
xfit <- seq(min(y), max(y), length = 100)
yfit <- dnorm(xfit, mean = mean(y), sd = sd(y))

par(family="arial")
plot(xhist, yhist, type = "s", ylim = c(0, max(4., yhist, yfit)), xlim = c(0, 1), main = "Normal (red), beta (blue), and histogram")
lines(xfit, yfit, col = "red")
lines(xfit, dbeta(xfit, a , b), col = "blue")
par(family="mono")
t_adj = c(0., 0.5); t_vd = 0.2; t_vp = max(4., yhist, yfit);
text(0, t_vp - 0 * t_vd, sprintf("Empty %02d - Edge %04d [%s]", ec, iv, iv_to_s), adj = t_adj)
text(0, t_vp - 1 * t_vd, sprintf("nv = %d", length(y)), adj = t_adj)
text(0, t_vp - 2 * t_vd, sprintf("me = %+0.3f", me_y), adj = t_adj)
text(0, t_vp - 3 * t_vd, sprintf("va = %+0.3f", va_y), adj = t_adj)
text(0, t_vp - 4 * t_vd, sprintf("sk = %+0.3f", sk_y), adj = t_adj)
text(0, t_vp - 5 * t_vd, sprintf("ku = %+0.3f", ku_y), adj = t_adj)

ks_beta <- ks.test(jitter(y, amount = eps), "pbeta", a, b)
ks_norm <- ks.test(jitter(y, amount = eps), "pnorm", mean(y), sd(y))
