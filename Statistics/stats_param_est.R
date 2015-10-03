set.seed(100)
x = rexp(n = 25, rate = .1)

## The easy way
library(MASS)
(fit = fitdistr(x, densfun = "exponential"))


## The manual way
## Estimating MLE
mle = function(beta, n, sum.x) {
  mle = 1/beta^n * exp(-(sum.x/beta))
  return(mle)
}

B = seq(1, 25, .25)
mle.est = mle(beta = B, n = 25, sum.x = sum(x))

## Compare the estimated MLE to the fitted distribution MLE
plot(B, mle.est, type = "l")
abline(v = 1/fit$estimate, lty = 2)
title("Maximum Liklihood for the Exponential Distribution")

## Estimating graphically
x = sort(x)
u = (1:length(x) - .5)/length(x)
t = -log(1 - u)

plot(x, t)
abline(lm(t ~ x))
text(10,3, "Equation of the line")
text(10,2.5, "y = -.057 + .128x")
title("Fitting mle parameters graphically")

## The slope of the estimated line is the estimated MLE
## Fit: .123 vs LM: .128

