## Contingency Tables {-}


Table: (\#tab:a20)Data for applicant entrance for 6 departments

 Department   Male.Yes   Male.No   Female.Yes   Female.No
-----------  ---------  --------  -----------  ----------
          1        512       313           89          19
          2        353       207           17           8
          3        120       205          202         391
          4        138       279          131         244
          5         53       138           94         299
          6         22       351           24         317


```r
dta$OR = with(dta, (Male.Yes * Female.No) / (Male.No * Female.Yes))
dta$se = with(dta, (sqrt(1/Male.Yes + 1/Female.No + 1/Male.No + 1/Female.Yes)))
dta$Conf.lwr = with(dta, OR - (1.96 * se))
dta$Conf.Upr = with(dta, OR + (1.96 * se))

kable(dta, split.tables = Inf)
```



 Department   Male.Yes   Male.No   Female.Yes   Female.No          OR          se     Conf.lwr    Conf.Upr
-----------  ---------  --------  -----------  ----------  ----------  ----------  -----------  ----------
          1        512       313           89          19   0.3492120   0.2627081   -0.1656958   0.8641199
          2        353       207           17           8   0.8025007   0.4375926   -0.0551808   1.6601823
          3        120       205          202         391   1.1330596   0.1439424    0.8509325   1.4151868
          4        138       279          131         244   0.9212838   0.1502084    0.6268753   1.2156922
          5         53       138           94         299   1.2216312   0.2002426    0.8291558   1.6141066
          6         22       351           24         317   0.8278727   0.3051635    0.2297522   1.4259933


```r
marginal = colSums(dta[, 2:5])
OR = (marginal[1] * marginal[4]) / (marginal[2] * marginal[3])
se = sqrt(1/marginal[1] + 1/marginal[4] + 1/marginal[2] + 1/marginal[3])

## Confidence Interval for the Marginal OR
OR + c(-1, 1) * 1.96 * se
```

```
[1] 1.71585 1.96631
```


```r
library(DescTools)
library(lawstat)

dta = xtabs(freq ~ .,
            cbind(expand.grid(Gender = c("Male", "Female"),
                              Entrace = c("Yes", "No"),
                              Department = c("1", "2", "3", "4", "5", "6")),
                  freq =  c(512, 89, 313, 19, 353, 17, 207, 8, 120, 202, 205, 391,
                            138, 131, 279, 244, 53, 94, 138, 299, 22, 24, 351, 317)
                  )
            )

## Ho: OR = 1, Ha: OR > 1
BreslowDayTest(dta, OR = 1)
```

```

	Breslow-Day test on Homogeneity of Odds Ratios

data:  dta
X-squared = 19.938, df = 5, p-value = 0.001283
```

```r
## Ho: OR_1 = OR_2 = OR_3 = OR_4 = OR_5 = OR_6, Ha: At least one set of OR are not equal
cmh.test(dta)
```

```

	Cochran-Mantel-Haenszel Chi-square Test

data:  dta
CMH statistic = 1.52460, df = 1.00000, p-value = 0.21692, MH
Estimate = 0.90470, Pooled Odd Ratio = 1.84110, Odd Ratio of level
1 = 0.34921, Odd Ratio of level 2 = 0.80250, Odd Ratio of level 3
= 1.13310, Odd Ratio of level 4 = 0.92128, Odd Ratio of level 5 =
1.22160, Odd Ratio of level 6 = 0.82787
```

Based on the Breslow Day test we reject the null hypothesis that the odds ratios are equal to 1. The CMH test fails to reject that gender and entrance are independent.
