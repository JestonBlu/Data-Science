## Sample Size and Power {-}

### Proportions {-}


```r
library(pwr)

## Univariate Proportion
pwr.p.test(h = .25, sig.level = .05, power = .8, alternative = "greater")
```

```

     proportion power calculation for binomial distribution (arcsine transformation) 

              h = 0.25
              n = 98.92092
      sig.level = 0.05
          power = 0.8
    alternative = greater
```

```r
## Two Proportions (equal n)
pwr.2p.test(h = .25, sig.level = .05, power = .8, alternative = "greater")
```

```

     Difference of proportion power calculation for binomial distribution (arcsine transformation) 

              h = 0.25
              n = 197.8418
      sig.level = 0.05
          power = 0.8
    alternative = greater

NOTE: same sample sizes
```

```r
## Two Proportions (different n)
pwr.2p2n.test(h = .25, n1 = 100, n2 = 120, sig.level = .05, alternative = "greater")
```

```

     difference of proportion power calculation for binomial distribution (arcsine transformation) 

              h = 0.25
             n1 = 100
             n2 = 120
      sig.level = 0.05
          power = 0.5798535
    alternative = greater

NOTE: different sample sizes
```

### T-test {-}


```r
## Equal n
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "one.sample")
```

```

     One-sample t test power calculation 

              n = 33.36713
              d = 0.5
      sig.level = 0.05
          power = 0.8
    alternative = two.sided
```

```r
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "two.sample")
```

```

     Two-sample t test power calculation 

              n = 63.76561
              d = 0.5
      sig.level = 0.05
          power = 0.8
    alternative = two.sided

NOTE: n is number in *each* group
```

```r
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "paired")
```

```

     Paired t test power calculation 

              n = 33.36713
              d = 0.5
      sig.level = 0.05
          power = 0.8
    alternative = two.sided

NOTE: n is number of *pairs*
```

```r
## Different n
pwr.t2n.test(n1 = 10, n2 = 15, d = 1, sig.level = .05)
```

```

     t test power calculation 

             n1 = 10
             n2 = 15
              d = 1
      sig.level = 0.05
          power = 0.6503918
    alternative = two.sided
```

### Chi-square {-}


```r
pwr.chisq.test(w = .25, df = 4, sig.level = .05, power = .8)
```

```

     Chi squared power calculation 

              w = 0.25
              N = 190.9646
             df = 4
      sig.level = 0.05
          power = 0.8

NOTE: N is the number of observations
```

### ANOVA {-}


```r
pwr.anova.test(k = 5, n = 10, f = .5, sig.level = .05)
```

```

     Balanced one-way analysis of variance power calculation 

              k = 5
              n = 10
              f = 0.5
      sig.level = 0.05
          power = 0.7730915

NOTE: n is number in each group
```
