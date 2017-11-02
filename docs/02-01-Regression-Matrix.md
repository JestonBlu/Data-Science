# Regression Methods {-}

## Matrix Regression {-}



```r
library(mvtnorm)

## Covariance matrix for random data
A = matrix(c(3, 1.5, 1.5, 3), nrow = 2)

## number of observations to generate
n = 10

## Generate data
set.seed(1123)
(dta = rmvnorm(n = n, mean = c(10, 20), sigma = A))
```

```
           [,1]     [,2]
 [1,] 11.675350 22.16815
 [2,] 11.164364 18.89734
 [3,] 12.400876 20.94524
 [4,] 10.357397 21.56407
 [5,] 10.109067 18.25317
 [6,] 10.444637 19.84917
 [7,] 10.451464 21.36056
 [8,]  8.763568 19.40245
 [9,]  8.923162 22.68341
[10,]  7.600346 18.09957
```

```r
## Create Y vector from random data
(Y = dta[, 1])
```

```
 [1] 11.675350 11.164364 12.400876 10.357397 10.109067 10.444637 10.451464
 [8]  8.763568  8.923162  7.600346
```

```r
## Create design matrix
(X = as.matrix(data.frame("b" = rep(1, n), "x" = dta[, 2])))
```

```
      b        x
 [1,] 1 22.16815
 [2,] 1 18.89734
 [3,] 1 20.94524
 [4,] 1 21.56407
 [5,] 1 18.25317
 [6,] 1 19.84917
 [7,] 1 21.36056
 [8,] 1 19.40245
 [9,] 1 22.68341
[10,] 1 18.09957
```

```r
## Beta
B = solve(t(X) %*% X) %*% t(X)

## Solve coefficients
(B %*% Y)
```

```
       [,1]
b 4.0586986
x 0.3016549
```

```r
## Verify results
(mdl = lm(dta[,1] ~ dta[,2]))
```

```

Call:
lm(formula = dta[, 1] ~ dta[, 2])

Coefficients:
(Intercept)     dta[, 2]  
     4.0587       0.3017  
```

```r
## Hat Matrix
## Projected onto Y will give you Y-hat
## Diagonals of Hat Matrix are leverage
H = X %*% B
diag(H)
```

```
 [1] 0.2401398 0.1835194 0.1159607 0.1634227 0.2760978 0.1092077 0.1443377
 [8] 0.1348034 0.3292979 0.3032130
```

```r
## Sum of squares X
SXX = sum((dta[, 2] - colMeans(dta)[2])^2)

## calculate leverage manually
## values greater than 4/n are considered high leverage
## for multiple regression Hii > 2 + (p + 1)/n are considered high leverage
1/n + (dta[, 2] - colMeans(dta)[2])^2 / SXX
```

```
 [1] 0.2401398 0.1835194 0.1159607 0.1634227 0.2760978 0.1092077 0.1443377
 [8] 0.1348034 0.3292979 0.3032130
```

```r
## verify results
hatvalues(mdl)
```

```
        1         2         3         4         5         6         7 
0.2401398 0.1835194 0.1159607 0.1634227 0.2760978 0.1092077 0.1443377 
        8         9        10 
0.1348034 0.3292979 0.3032130 
```

```r
## Project Hat Matrix on to Y to get Y-hat
H %*% Y
```

```
           [,1]
 [1,] 10.745830
 [2,]  9.759172
 [3,] 10.376933
 [4,] 10.563605
 [5,]  9.564856
 [6,] 10.046298
 [7,] 10.502215
 [8,]  9.911541
 [9,] 10.901259
[10,]  9.518521
```

```r
## Verify results
predict(mdl, data.frame(dta))
```

```
        1         2         3         4         5         6         7 
10.745830  9.759172 10.376933 10.563605  9.564856 10.046298 10.502215 
        8         9        10 
 9.911541 10.901259  9.518521 
```

```r
## MSE of estimate
(mse = sqrt(diag(anova(mdl)[[3]][2] * solve(t(X) %*% X))))
```

```
      b       x 
5.90855 0.28989 
```

```r
## 95% confidence interval for X
mdl$coefficients[1] + c(-1, 1) * mse[1] * qt(1 - .05/2, df = n - 2)
```

```
[1] -9.566442 17.683839
```

```r
mdl$coefficients[2] + c(-1, 1) * mse[2] * qt(1 - .05/2, df = n - 2)
```

```
[1] -0.3668327  0.9701424
```

```r
## Check confidence interval
confint(mdl)
```

```
                 2.5 %     97.5 %
(Intercept) -9.5664418 17.6838390
dta[, 2]    -0.3668327  0.9701424
```

```r
## Calculate R^2
## SST is also SYY
SST = sum((dta[, 1] - colMeans(dta)[1])^2)
SSReg = sum((predict(mdl, data.frame(dta)) - colMeans(dta)[1])^2)

## R^2
SSReg / SST
```

```
[1] 0.1192158
```

```r
## Adjusted R^2
1 - (sum(mdl$residuals^2)/(n - 2))/(SST/(n - 1))
```

```
[1] 0.009117763
```

```r
summary(mdl)
```

```

Call:
lm(formula = dta[, 1] ~ dta[, 2])

Residuals:
    Min      1Q  Median      3Q     Max 
-1.9781 -0.9125  0.1738  0.8332  2.0239 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   4.0587     5.9085   0.687    0.512
dta[, 2]      0.3017     0.2899   1.041    0.328

Residual standard error: 1.429 on 8 degrees of freedom
Multiple R-squared:  0.1192,	Adjusted R-squared:  0.009118 
F-statistic: 1.083 on 1 and 8 DF,  p-value: 0.3285
```
