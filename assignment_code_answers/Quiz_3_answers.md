---
title: "Quiz 3 Spring 2022 answers"
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
found 66 were using oxygen.  Oxygen saturation levels were measured for each patient
using oxygen. Is there evidence patients receiving oxygen have saturation levels
that differ from the standard for a healthy person (95%)?  

Data can be loaded using 


```r
oxygen_use <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSC8ft2LReouozG9fDK0Pa-QXESZ82qzADTCCBr_LoQeVRlRDLS4KOhMNYjGnGZFTrq5hkuYcOk6cKm/pub?gid=1295231330&single=true&output=csv",
                       stringsAsFactors = T)
```

Make sure your answers include

* null hypothesis
  * H~O~: Oxygen saturation levels of sampled patients is equal 95%.
* alternative hypothesis
  * H~A~: Oxygen saturation levels of sampled patients is not equal to 95%.
* explanation for test you will use
  * Although the data is technically bounded and proportion based, many people 
  were measured, oxygen saturation levels are likely an average of multiple factors,
  and the value can technically be non-discrete (though device may impact your 
  measurement resolution). 
  For these reasons I used a t-test.  Other possible options include bootstrapping 
  (good for making no assumptions!)
  or sign test (if I don't trust normality given bounded nature of data). 
  **I show all here but you only need to pick one**!
* results from statistical test


```r
t.test(oxygen_use$Oxygen_saturation_level, mu = .95)
```

```
## 
## 	One Sample t-test
## 
## data:  oxygen_use$Oxygen_saturation_level
## t = 255.46, df = 65, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0.95
## 95 percent confidence interval:
##  93.25752 94.71218
## sample estimates:
## mean of x 
##  93.98485
```

```r
library(BSDA)
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'BSDA'
```

```
## The following object is masked from 'package:datasets':
## 
##     Orange
```

```r
SIGN.test(oxygen_use$Oxygen_saturation_level, md = .95)
```

```
## 
## 	One-sample Sign-Test
## 
## data:  oxygen_use$Oxygen_saturation_level
## s = 66, p-value = 2.22e-16
## alternative hypothesis: true median is not equal to 0.95
## 95 percent confidence interval:
##  94 96
## sample estimates:
## median of x 
##          95 
## 
## Achieved and Interpolated Confidence Intervals: 
## 
##                   Conf.Level L.E.pt U.E.pt
## Lower Achieved CI     0.9360     94     96
## Interpolated CI       0.9500     94     96
## Upper Achieved CI     0.9644     94     96
```

```r
source("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/code_examples/bootstrapjsg.R")
bootstrapjsg(data1=oxygen_use$Oxygen_saturation_level, null = .95)
```

```
## 
## Attaching package: 'boot'
```

```
## The following object is masked from 'package:lattice':
## 
##     melanoma
```

```
## Simple Bootstrap Routines (1.1-7)
```

```
## Warning in boot.ci(a, conf): bootstrap variances needed for studentized
## intervals
```

```
##                                                                         
##                  "0.95" "% Confidence Interval"      "93.2575757575758" 
##                                                                         
##      "94.6818181818182"               "p-value"                     "0"
```

* clear explanation of how results relate to your stated hypotheses

  * Results from a t-test (t~65~=255.46, p<.001) indicate we can reject the null
  hypotheses.
  * Results from a sign-test (S=66, p<.001) indicate we can not reject the null
  hypotheses.
  * Results from bootstrapping the sample 10,000 times indicate we can reject 
  the null hypothesis (p=0).
* confidence interval for your estimate
  * t-test confidence intervals:  93.3- 94.7
  * bootstrap confidence intervals: 93.3- 94.7
  * sign-test confidence intervals:  94 - 96. **Note this includes the median
  value under the null because  the sign test removes all equal ties with the 
  proposed median before computing a p-value.**

