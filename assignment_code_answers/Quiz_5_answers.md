---
title: "Quiz 5 Spring 2022 answers"
author: "jsg"
date: "Last compiled on 23 March, 2022 10:26"
output:
  html_document:
    toc: true
    toc_float: true
    keep_md: true
    self_contained: true
---

COVID-19 caused respiratory issues for many individuals.The same scientists from
quizzes 1-4 wanted to know if respirators worked as well for patients ill with 
COVID-19 as they did during earlier illnesses.  Data on oxygen saturation levels
is available using


```r
covid_respirator <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS5y8NL2ZR_tp_YUJoAUlPmRe3CZ8v94wyraannmeNZWpYNOgil_kvzJVntOve8jBPxskM17-t35NB8/pub?gid=1295231330&single=true&output=csv",
                             stringsAsFactors = T)
```
Investigate the question. Make sure you include

* null hypothesis
  * H~O~: Oxygen saturation level using respirator is same before and during Covid.  
* alternative hypothesis
  * H~A~: Oxygen saturation level using respirator isnot the same before and during Covid.  
* explanation for test you will use (**Note: make sure you do post-hoc tests if needed!
I will not remind of this on exam!**)
  * I will use a paired t-test since the same individuals were tested and data is 
  can be considered continuous and independent among samples.  
* results from statistical test

```r
t.test(covid_respirator$on_respirator_b4_covid, covid_respirator$on_respirator_after_covid, paired = T)
```

```
## 
## 	Paired t-test
## 
## data:  covid_respirator$on_respirator_b4_covid and covid_respirator$on_respirator_after_covid
## t = 8.287, df = 65, p-value = 9.03e-12
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.6210022 1.0153614
## sample estimates:
## mean of the differences 
##               0.8181818
```
  
* clear explanation of how results relate to your stated hypotheses
  * t~65~ = 8.287, p<.01,so I reject the null hypothesis.  Estimate suggests 
  saturation levels were .81% higher during previous use of respirators.

