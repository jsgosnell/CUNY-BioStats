Binomial
================
jsg
10/21/2020

-----

# ZOMBIES\!

You wake up one morning and find that zombies have taken over your
neighborhood (bummer). One idea is to use biocontrol to reduce zombie
attacks. A zombie parasite is identified. Test plots (neighborhoods\!)
are subjected to one or two releases of the parasite at low, medium, or
high densities. Past evidence suggests that the parasite should infect
70% of the zombies, but some citizens are concerned it won’t work in New
York due to lower temperatures. In a random sample, 7 of 12 zombies are
infected with the parasite.

  - What is the observed rate of infection in the sample?

<!-- end list -->

``` r
7/12
```

    ## [1] 0.5833333

*The observed rate of infection is .58 (58%).*

  - Is there evidence the parasite infects zombies at a different rate
    in New York? \*\* If so, why \*\* If not, how do you explain any
    discrepancy between the observed rate of infection and your results?

Make sure your answer includes

  - null hypothesis
  - alternative hypothesis
  - explanation for test you will use
  - results from statistical test
  - clear explanation of how results relate to your stated hypotheses
  - confidence interval for your estimate

<!-- end list -->

``` r
binom.test(7,12, .7)
```

    ## 
    ##  Exact binomial test
    ## 
    ## data:  7 and 12
    ## number of successes = 7, number of trials = 12, p-value = 0.3614
    ## alternative hypothesis: true probability of success is not equal to 0.7
    ## 95 percent confidence interval:
    ##  0.2766697 0.8483478
    ## sample estimates:
    ## probability of success 
    ##              0.5833333

``` r
library(binom)
binom.confint(7,12,.7)
```

    ##           method x  n      mean     lower     upper
    ## 1  agresti-coull 7 12 0.5833333 0.4348543 0.7181188
    ## 2     asymptotic 7 12 0.5833333 0.4358294 0.7308372
    ## 3          bayes 7 12 0.5769231 0.4446214 0.7270358
    ## 4        cloglog 7 12 0.5833333 0.4224651 0.7137914
    ## 5          exact 7 12 0.5833333 0.3952747 0.7537490
    ## 6          logit 7 12 0.5833333 0.4328086 0.7197749
    ## 7         probit 7 12 0.5833333 0.4334543 0.7218826
    ## 8        profile 7 12 0.5833333 0.4341973 0.7227810
    ## 9            lrt 7 12 0.5833333 0.4341873 0.7227969
    ## 10     prop.test 7 12 0.5833333 0.2859928 0.8350075
    ## 11        wilson 7 12 0.5833333 0.4350063 0.7179668

*I analyzed the data using a binomial test looking for any deviations
from 70% infection rates. Using this method I found a p-value of .3614,
so I fail to reject the null hypothesis that the infection rate is 70%.
The alternative hypothesis is that the infection rate is not equal to
70%. Although the rate I observed was 58%, the small sample size means
this isn’t that unlikely to occur by chance if the true infection rate
is 70%. The small sample size means the 95% confidence interval for our
estimate of infection rate is .43-.72 (using Agresti-Coul estimates). We
need more zombies to truly assess the population for any differences in
infection rate.*
