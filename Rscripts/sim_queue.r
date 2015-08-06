rm(list = ls())


arrival = 10      # arrival per hour
service = 5           # minutes to serve
t.end = 60       # minutes per hour
t.step = 1    # time interval, 1 minute

queue = rep(0, t.end)

for (i in 2:length(queue)) {

  if (runif(1) * arrival < service * runif(1)) {
    queue[i] = queue[i - 1] + 1                  # customer arrives
  } else if (runif(1) < arrival * t.step) {
    queue[i] = max(0, queue[i - 1] - 1)          # customer leaves early
      } else {
    queue[i] = queue[i - 1]                      # nothing happens
  }
}

plot(queue, type = "l")












