---
title: "Quiz 4 Spring 2022 answers"
author: "jsg"
date: "Last compiled on 23 March, 2022 10:26"
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
found 66 were using oxygen.  Is there evidence that the ward the patient is located 
in impacts the probability they will receive oxygen?

 *Ward*| Patients prescribed oxygen | Patients not prescribed oxygen
  :-:|  :-:  |  :-:
 Acute Medical Unit (AMU) |  8   | 81 
 Coronary Care Unit (CCU) |  2   | 14
 Medical (General) |  32  | 348
 Medical (Respiratory) |  24  | 67
 
 Make sure your answers include

* null hypothesis
  * H~O~: Proportion of patients receiving oxygen is independent of ward.
* alternative hypothesis
  * H~A~: Proportion of patients receiving oxygen is not independent of ward.
* explanation for test you will use (**Note: make sure you do post-hoc tests if needed!
I will not remind of this on exam!**)
  * I will use $\chi^2$ test since we are determining if proportions differ among 2+ 
  groups (contingency analysis)
* results from statistical test

```r
ward_impacts <-matrix(c(8, 81, 2, 14, 32, 348, 24, 67), nrow = 4, byrow = T)
rownames(ward_impacts) <- c("AMU", "CCU","Gen", "Resp")
colnames(ward_impacts) <- c("oxygen", "no oxygen")
chisq.test(ward_impacts)
```

```
## Warning in chisq.test(ward_impacts): Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  ward_impacts
## X-squared = 23.962, df = 3, p-value = 2.544e-05
```
Results are significant but assumptions may not be met.


```r
chisq.test(ward_impacts)$expected
```

```
## Warning in chisq.test(ward_impacts): Chi-squared approximation may be incorrect
```

```
##         oxygen no oxygen
## AMU  10.197917  78.80208
## CCU   1.833333  14.16667
## Gen  43.541667 336.45833
## Resp 10.427083  80.57292
```
Appears to be ok (only 0.125 less than 5, which is < 20%, and none less than 1)

* clear explanation of how results relate to your stated hypotheses

$\chi^2$ = 23.9, p <. 001, so I reject the null hypothesis. Oxygen use does appear to differ 
by ward. Given this result, I also carried out post-hoc test using the Holm's (
sequential Bonferroni) method to control family-wise error rate.


```r
library(rcompanion)
pairwiseNominalIndependence(ward_impacts, compare = "row", method = "holm")
```

```
## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be incorrect
```

```
##   Comparison p.Fisher p.adj.Fisher  p.Gtest p.adj.Gtest  p.Chisq p.adj.Chisq
## 1  AMU : CCU 6.47e-01     1.00e+00 6.70e-01    1.00e+00 1.00e+00    1.00e+00
## 2  AMU : Gen 8.34e-01     1.00e+00 8.64e-01    1.00e+00 1.00e+00    1.00e+00
## 3 AMU : Resp 3.08e-03     1.54e-02 1.87e-03    9.35e-03 4.30e-03    2.15e-02
## 4  CCU : Gen 6.38e-01     1.00e+00 5.90e-01    1.00e+00 9.08e-01    1.00e+00
## 5 CCU : Resp 3.47e-01     1.00e+00 2.05e-01    8.20e-01 3.80e-01    1.00e+00
## 6 Gen : Resp 1.48e-05     8.88e-05 1.32e-05    7.92e-05 4.82e-06    2.89e-05
```
Results indicate the only significant difference is between AMU and General 
respiratory wards and between General and Respiratory medical wards.

