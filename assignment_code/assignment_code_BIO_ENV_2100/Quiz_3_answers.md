Quiz 3
================
jsg
10/21/2020

-----

# ZOMBIES\!

You wake up one morning and find that zombies have taken over your
neighborhood (bummer).

Evidence also suggests that temperature may influence propensity for
zombie attacks, with zombies preferring warmer bodies. Long-term studies
suggest that average human temperature is 36.57 C.  
Unfortunately, measuring temperatures before/after attacks is hard, so
data on 5 survivors of attacks have been recorded. They were: 36.12,
36.37, 35.7, 36.01, 36.9.

1.  Is there any evidence that temperature influences zombie attacks?

<!-- end list -->

``` r
temp <- c(36.12, 36.37, 35.7, 36.01, 36.9)
t.test(temp, mu = 36.57, alternative = "greater")
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  temp
    ## t = -1.7398, df = 4, p-value = 0.9216
    ## alternative hypothesis: true mean is greater than 36.57
    ## 95 percent confidence interval:
    ##  35.79113      Inf
    ## sample estimates:
    ## mean of x 
    ##     36.22

``` r
#using non-sided test ok too if justified
t.test(temp, mu = 36.57)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  temp
    ## t = -1.7398, df = 4, p-value = 0.1569
    ## alternative hypothesis: true mean is not equal to 36.57
    ## 95 percent confidence interval:
    ##  35.66146 36.77854
    ## sample estimates:
    ## mean of x 
    ##     36.22

*Although this is a small sample size, we know temperature typically
follows a normal distribution. For this reason, and given that
bootstrapping was not an option, I used a t-test to test the null
hypothesis that the temperature of zombie attack victims is less than or
equal to 36.57. I used a sided-test because we wanted to know if zombies
prefer warmer bodies. The alternative hypothesis, therefore, is that the
temperature of zombie attack victims is greater than 36.57. Using this
approach I found t<sub>4</sub>=-1.74, leading to a p-value of .9216.
Since this is greater than the alpha level we typically use (.05), I
fail reject the null hypothesis.*

2.  How would knowing the standard deviation of human temperature is
    0.42 change your methods and results?

<!-- end list -->

``` r
library(BSDA)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'BSDA'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     Orange

``` r
z.test(temp, mu = 36.57, sigma.x = .42, alternative = "greater")
```

    ## 
    ##  One-sample z-Test
    ## 
    ## data:  temp
    ## z = -1.8634, p-value = 0.9688
    ## alternative hypothesis: true mean is greater than 36.57
    ## 95 percent confidence interval:
    ##  35.91105       NA
    ## sample estimates:
    ## mean of x 
    ##     36.22

``` r
#using non-sided test ok too if justified
z.test(temp, mu = 36.57, sigma.x = .42)
```

    ## 
    ##  One-sample z-Test
    ## 
    ## data:  temp
    ## z = -1.8634, p-value = 0.06241
    ## alternative hypothesis: true mean is not equal to 36.57
    ## 95 percent confidence interval:
    ##  35.85186 36.58814
    ## sample estimates:
    ## mean of x 
    ##     36.22

*If I knew the standard deviation of human temperature I could use a
z-test instead to test the same hypothesis. I would still fail to reject
the null hypothesis, which makes sense.*

Make sure your answers include

  - null hypothesis
  - alternative hypothesis
  - explanation for test you will use
  - results from statistical test
  - clear explanation of how results relate to your stated hypotheses
  - confidence interval for your estimate
