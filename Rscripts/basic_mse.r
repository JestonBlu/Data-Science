library(ggplot2)
library(gridExtra)

start = 10
end = 5000
by = 20
steps = end/by

dt = data.frame(N = seq(start, end, by),
                mu = rep(0, steps),
                var = rep(0, steps),
                mse = rep(0, steps))

for (i in 1:nrow(dt)) {
  x = rnorm(dt$N[i])
  dt$mu[i] = mean(x)
  dt$var[i] = var(x)
  dt$mse[i] = dt$var[i]/sqrt(dt$N[i])
}

grid.arrange(nrow = 1, main = "Norm(0,1)",
  qplot(x = N, y = mu, data = dt, geom = 'line'),
  qplot(x = N, y = var, data = dt, geom = 'line'),
  qplot(x = N, y = mse, data = dt, geom = 'line'))