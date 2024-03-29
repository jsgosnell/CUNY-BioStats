---
title: "2022 Fall Final (23 points total)"
subtitle:  "Thanks for a good semester"
author: "jsg"
date: "Last compiled on 01 November, 2023 10:13"
output:
  html_document:
    toc: true
    toc_float: true
    keep_md: true
    self_contained: true
    toc_depth: 6
---

# Instructions! Read this first.

The exam is open note, open course website, open class-related code repositories 
(mine and those you produced in class). However, you may not get external help (
from other students, directed internet searches, etc.).  Please update the statement
below to acknowledge these instructions (and that you can use git).

I, INSERT YOUR NAME HERE, promise to not seek external help on the exam. I 
understand any academic issues will result in a D or F on the exam or in the class
and be reported to the Dean of Students.  Infractions will also result in me 
being unable to obtain a letter of recommendation from the department for 
professional school applications.

**I highly recommend reading entire exam and doing what you can without R before
jumping into the code**. You can earn a C or higher without opening R, just answer
questions and make assumptions (that you state!) as needed.  **Good luck!**

# Sage Grouse

In the following datasets, scientists investigated the 
impact of a large fires in Nevada on sage grouses (*Centrocercus urophasianus*),
a ground-nesting bird found in sagebrush (a plant) habitat in the interior of North 
America.  Sage grouses are
considered threatened due to habitat loss.  A related issue is that
wildfires may lead to native sagebrush plants (*Artemisia* spp.), upon which 
the grouses depend, being replaced by invasive plants.  

![](https://upload.wikimedia.org/wikipedia/commons/3/38/Sage-grouse_%2819790797410%29.jpg)  


Data was collected before and after a large fire.  

## 1

1. Data from before the fires suggested that grouse nests succeeded (hatched at
least one egg) 63% of the time.  After the fire, 118 nests were tagged and followed
for outcomes.  53 of those produced at least one hatchling, while the rest 
failed to do so. Is there any evidence the fires impacted nest success?  

Make sure you include (6 pts)

* null hypothesis
* alternative hypothesis
* explanation for test you will use 
* results from statistical test
* clear explanation of how results relate to your stated hypotheses

## 2


2. Another potential impact of fires is the change they produce in available food levels.
Sage grouses eat sagebrush in the winter, and invasive plants could offer less 
food. To consider this, scientists weighed male and female grouse from recently burned plots,
those burned 5 years ago, and those not burned in the past 20 years.  Data can 
be imported using


```r
grouse_weight <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/grouse_weight.csv", stringsAsFactors = T)
```

Is there any evidence the fires impacted grouse weight?  

Make sure you include (6 pts)

* null hypothesis
* alternative hypothesis
* explanation for test you will use 
* results from statistical test
* clear explanation of how results relate to your stated hypotheses

## 3

3. Sometimes capturing grouses is hard. One student working on the project proposed
using nest diamter to estimate grouse weight  To consider this, 50 birds were weighed (in grams),
and the diameter of their nest (in cm) was measured. Is there evidence nest diameter is a good
predictor of grouse mass?  Data can be imported using

Make sure you include (6 pts)


```r
nest_approach <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/nest_approach.csv", stringsAsFactors = T)
```

* null hypothesis
* alternative hypothesis
* explanation for test you will use 
* results from statistical test
* clear explanation of how results relate to your stated hypotheses



## 4 

4. Plot the data from question 3. (5 pts)

