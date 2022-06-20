---
title: "Quiz 2 Spring 2022 answers"
author: "jsg"
date: "Last compiled on 21 March, 2022 10:26"
output:
  html_document:
    toc: true
    toc_float: true
    keep_md: true
    self_contained: true
---



# Medical applications of oxygen

A study was done to determine if oxygen was being appropriately supplied to patients
in a medical unit (Barrett et al 2020). They surveyed 636 patients in mutiple wards and
found 66 were using oxygen.  Thirty of the patients receiving oxygen were female
and 36 were male.

* What is the observed rate of oxygen use for females in the sample?


```r
30/(30+36)
```

```
## [1] 0.4545455
```

The observed rate of oxygen use for females was 0.4545455. **Note code shows
you how to do an inline code chunk!**

* Address the research question:  Is oxygen use impacted by gender? Make sure your 
answer includes

  * null hypothesis
    * H~O~ : The proportion of patients receiving oxygen that are female is equal to .5.
  * alternative hypothesis
     * H~A~ : The proportion of patients receiving oxygen that are female is not equal .5.
  * explanation for test you will use
    * I will use a binomial test because the patient identified as 2 categories 
      and the data is independent.
  * results from statistical test

```r
binom.test(30, (30+36))
```

```
## 
## 	Exact binomial test
## 
## data:  30 and (30 + 36)
## number of successes = 30, number of trials = 66, p-value = 0.5386
## alternative hypothesis: true probability of success is not equal to 0.5
## 95 percent confidence interval:
##  0.3314394 0.5818632
## sample estimates:
## probability of success 
##              0.4545455
```
  * clear explanation of how results relate to your stated hypotheses
    * Since the binomial test returned a p-value of 0.53, I fail to reject the null
    hypothesis. The proportion of patients recieving oxygen that are female is not
    significantly different than .05.
  
  * confidence interval for your estimate
    * I will the Agresti-Coul method to construct confidence intervals.
    * My 95% confidence interval is .340 - .574.  


```r
library(binom)
binom.confint(30,(30+36))
```

```
##           method  x  n      mean     lower     upper
## 1  agresti-coull 30 66 0.4545455 0.3402160 0.5738752
## 2     asymptotic 30 66 0.4545455 0.3344175 0.5746734
## 3          bayes 30 66 0.4552239 0.3376325 0.5735537
## 4        cloglog 30 66 0.4545455 0.3320653 0.5689814
## 5          exact 30 66 0.4545455 0.3314394 0.5818632
## 6          logit 30 66 0.4545455 0.3392053 0.5749789
## 7         probit 30 66 0.4545455 0.3382402 0.5749145
## 8        profile 30 66 0.4545455 0.3378755 0.5746837
## 9            lrt 30 66 0.4545455 0.3378713 0.5746860
## 10     prop.test 30 66 0.4545455 0.3332483 0.5811493
## 11        wilson 30 66 0.4545455 0.3402413 0.5738499
```
   
    

