Quiz 4
================
jsg
11/16/2020

# Vaccine effectiveness\!

A drug company wants to make sure the vaccine they are developing has
similar effectiveness for multiple groups. Out of 10,000 patients, they
find the following effectivess among groups (labelled with letters to
prevent bias\!)

| *Outcome* | Success | Failure |
| --------- | ------- | ------- |
| A         | 4600    | 400     |
| B         | 2750    | 250     |
| C         | 800     | 200     |
| D         | 495     | 505     |

1.  What is the overall effectiveness of the vaccine?

<!-- end list -->

``` r
(4600+2750+800+495)/10000
```

    ## [1] 0.8645

*The overall effectiveness of the vaccine (among all users) could be
measured as 86.45%. However, we could also consider it as 0.7829167
given the unequal sample sizes.*

2.  Is there any evidence for differences among groups?

<!-- end list -->

``` r
effectiveness <-matrix(c(4600, 400, 2750, 250, 800, 200, 495, 505), ncol = 2, byrow = T)
colnames(effectiveness) <- c("Success", "Failure")
rownames(effectiveness) <- c("A","B", "C", "D")
#check it
effectiveness
```

    ##   Success Failure
    ## A    4600     400
    ## B    2750     250
    ## C     800     200
    ## D     495     505

``` r
chisq.test(effectiveness)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  effectiveness
    ## X-squared = 1402.2, df = 3, p-value < 2.2e-16

``` r
library(rcompanion)
```

    ## Warning: package 'rcompanion' was built under R version 4.0.3

``` r
pairwiseNominalIndependence(effectiveness, compare = "row", method = "holm")
```

    ##   Comparison  p.Fisher p.adj.Fisher p.Gtest p.adj.Gtest   p.Chisq p.adj.Chisq
    ## 1      A : B  6.12e-01     6.12e-01   0.598       0.598  6.27e-01    6.27e-01
    ## 2      A : C  2.83e-26     8.49e-26   0.000       0.000  1.49e-30    4.47e-30
    ## 3      A : D 5.10e-201    3.06e-200   0.000       0.000 7.92e-257   4.75e-256
    ## 4      B : C  8.82e-22     1.76e-21   0.000       0.000  8.84e-24    1.77e-23
    ## 5      B : D 7.58e-169    3.79e-168   0.000       0.000 8.33e-191   4.16e-190
    ## 6      C : D  4.84e-47     1.94e-46   0.000       0.000  6.10e-46    2.44e-45

*I tested for a difference among groups using a chi^2 test. This led to
a test statistics of chi^2<sub>3</sub> = 1402.2, which corresponds to a
p-value of \<.001. I thus reject the null hypothesis of no difference in
effectiveness among groups (the alternative is that there is a
difference in effectiveness among groups). Given this result, I used a
post-hoc test to compare groups. Data (using the p.adj.Chisq column)
indicate that groups A and B do not differ from each other, but all
others do.*

``` r
effectiveness_t <-matrix(c(4600, 400, 2750, 250, 800, 200, 495, 505), nrow = 2, byrow = F)
rownames(effectiveness_t) <- c("Success", "Failure")
colnames(effectiveness_t) <- c("A","B", "C", "D")
#check it
effectiveness_t
```

    ##            A    B   C   D
    ## Success 4600 2750 800 495
    ## Failure  400  250 200 505

``` r
chisq.test(effectiveness_t)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  effectiveness_t
    ## X-squared = 1402.2, df = 3, p-value < 2.2e-16

``` r
library(rcompanion)
pairwiseNominalIndependence(effectiveness_t, compare = "col", method = "holm")
```

    ##   Comparison  p.Fisher p.adj.Fisher p.Gtest p.adj.Gtest   p.Chisq p.adj.Chisq
    ## 1      A : B  6.12e-01     6.12e-01   0.598       0.598  6.27e-01    6.27e-01
    ## 2      A : C  2.83e-26     8.49e-26   0.000       0.000  1.49e-30    4.47e-30
    ## 3      A : D 5.10e-201    3.06e-200   0.000       0.000 7.92e-257   4.75e-256
    ## 4      B : C  8.82e-22     1.76e-21   0.000       0.000  8.84e-24    1.77e-23
    ## 5      B : D 7.58e-169    3.79e-168   0.000       0.000 8.33e-191   4.16e-190
    ## 6      C : D  4.84e-47     1.94e-46   0.000       0.000  6.10e-46    2.44e-45

3.  How would you related your findings from question 2 to your answer
    to question 1? Why does it matter (what you would you recommend or
    do to follow-up)?

*Even though the overall vaccine was 86% effective, this was heavily
influenced by data from groups a and b. Further analysis may be needed
to consider effects on other groups.*

Make sure your answers include

  - null hypothesis
  - alternative hypothesis
  - explanation for test you will use
  - results from statistical test (including post-hoc tests if needed\!)
  - clear explanation of how results relate to your stated hypotheses
