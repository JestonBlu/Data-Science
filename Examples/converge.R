rm(list = ls())

dta = stackloss
colnames(dta) = c("x1", "x2", "x3", "y")

## y
y = dta$y

## model matrix
dta = data.frame(
  x0 = rep(1, 21),
  x1 = dta$x1
)

## Hypothesis Model
h.x = function(theta.0, theta.1, data) {
  y.hat = rep(0, 21)
  for (i in 1:21) {
    y.hat[i] = with(data, theta.0*x0[i] + theta.1*x1[i]) 
  }
  return(y.hat)
}

## Derivative of cost function is the update function
j.theta = function(alpha, coef, data) {
  final = data.frame()
  theta = coef
  for (j in 1:21) {
    tht = theta
    for (i in 1:2) {
      theta[i] = tht[i] + (alpha * (y[j] - h.x(tht[1], tht[2], data)[j]) * data[j,i])
    }
    out = data.frame(
      x0 = data[j,1],
      x1 = data[j,2],
      theta.0 = round(tht[1], 5),
      theta.1 = round(tht[2], 5),
      mv.0 = round(alpha * (y[j] - h.x(tht[1], tht[2], data)[j]) * data[j,1], 5),
      mv.1 = round(alpha * (y[j] - h.x(tht[1], tht[2], data)[j]) * data[j,2], 5),
      y = y[j],
      h.x = round(h.x(tht[1], tht[2], data)[j], 5)
    )
    final = rbind(final, out)
  }
  return(final)
}

## Single iteration
j.theta(.0001, coef = c(0, 0), data = dta)



## Derivative of cost function is the update function
j.theta = function(alpha, coef, data) {
  final = data.frame()
  theta = coef
  for (j in 1:21) {
    tht = theta
    for (i in 1:2) {
      theta[i] = tht[i] + (alpha * (y[j] - h.x(tht[1], tht[2], data)[j]) * data[j,i])
    }
  }
  out = data.frame(
    theta.0 = round(tht[1], 5),
    theta.1 = round(tht[2], 5)
  )
  final = rbind(final, out)
  return(final)
}




## Multiple iterations
theta = c(-43, 3)

final = data.frame()

for (i in 1:10) {
  itr = j.theta(.00001, coef = theta, data = dta)
  theta = c(itr$theta.0[21], itr$theta.1[21])
  final = rbind(final, itr)
}

par(mfrow = c(1, 2))
plot(final$theta.0, type = "l")
plot(final$theta.1, type = "l")

