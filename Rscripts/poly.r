## function for generating data
f1 = function(x) 1 + x^2 - x^3

## random variable from normal distribution
yran = rnorm(500, 1, .025)

## create data by fitting function f to x points times the random variable
dt = data.frame(x  = seq(.01, 1, .01) * yran, 
                y  = f1(seq(.01, 1, .01))*yran)

## sort data frame by x
dt = dt[order(dt$x, decreasing = FALSE), ]

## create a polynomial model with raw polynomials, not orthogonal
fit3 = lm(y ~ poly(x, 3, raw = TRUE), dt)


## function for extracting the model
ext.mdl = function(lm) {
  
  int = paste(lm$coefficients[[1]][[1]])
  coef = paste(lm$coefficients[2:length(lm[[1]])])
  
  out = as.character()
  
  for (i in 1:length(coef)) {
    out = paste(out, coef[i], "*x^", i, " + ", sep = "")
    
  }
  
  out = gsub('.{3}$', '', out)
  out = paste(int, '+', out)
  
  return(out)
}

## uses the function above to extract the model formula as a 
## string and revaluate it in another function.  This was a pain to
## figure out
target = optimize(function(x) eval(parse(text=ext.mdl(fit3))), 
                  interval = c(0,1), maximum = TRUE)

## plot the generated data
plot(dt, ylim = c(.9, 1.24), col = "grey",
     main = "Polynomial fit through randomized data")

## plot the fitted model over the data
lines(x = dt$x, predict(fit3, data.frame(x = dt$x)), col = "red", lwd = 2)

## fit the original equation that created the data
lines(x = dt$x, f1(dt$x), lwd = 2, lty = 2)

## highlight maximum point on the curve
segments(x0 = -.1, x1 = target$maximum, y0 = target$objective, 
         lty = 2, col = "blue")
segments(y0 = 0, y1 = target$objective, x0 = target$maximum, 
         lty = 2, col = "blue")
points(x = target$maximum, y = target$objective, pch = 16, col = "blue")

## add a legend
legend(0, 1.25, c("Estimate", "Original", "Maximum"), 
       lty = c(1, 2, 2), lwd = c(2, 2, 1), col = c("red", "black", "blue"))

## add some annotation
text(x = .55, y = 1, cex = 1.2,
     labels = paste("Maximum Point \n",
                    "x =", round(target$maximum, 3), 
                    ", y =", round(target$objective, 3)))