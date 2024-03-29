---
title: "Meta-analysis, mega-fun"
author: "jsg"
date: "Last compiled on 01 November, 2023 11:15"
output:
  slidy_presentation:
    keep_md: yes
    theme: journal
subtitle: "Meta-analysis steps"
---



## Goals

:::::::::::::: {.columns}
::: {.column}
* Be able to define meta-analysis and compare to vote counting methods
* List steps needed to complete meta-analysis
* Compute basic summary statistics used in meta-analysis

:::
::: {.column}
![XKCD take](https://imgs.xkcd.com/comics/meta-analysis.png "https://xkcd.com/1447")
:::
::::::::::::::

::: notes
This is a speaker note.

- Use basic Markdown
- like this list
- *and inline formatting*
:::

## Warnings

* These tend to be long, iterative processes!

## Why we need meta-analysis

* Questions may be asked by multiple research groups/papers
* We need to understand general trends and outcomes, but all studies may not say 
the same thing?
  * 20 studies support my hypothesis
  * 10 studies refute my hypothesis
  
---

  * Why might this happen?
      * Sample size
      * Random chance
      * Difference in techniques, quality, species, conditions…
      

## Why not just compare p-values (aka vote counting) (from Harrison 2010)

* P-values are not effect sizes!
  + highly dependent on sample size!
      
## We need a quantitative framework for combining results

* Meta-analyses can address
  * How strong is the effect, on average?
  * Does the collection of studies reject some null hypothesis?
  * How variable is the effect?
  * What factors might explain this variability?
  * Is there bias in the studies that have been published (is there a gap in the published studies that would change the direction of our inference?)
  
## Potential outcomes?

* Useful way to use literature review 
  * separate paper
  * example of gap
  * great introduction
  
## Steps (expanded from Harrison et al 2010)

1. Perform thorough literature search
2. Decide which studies should be included in the dataset
3. Decide on appropriate metric of effect size
4. Extract the data 
5. Calculate effect sizes 
6. Analyze average effect size
7. Analyze impact of other factors
8. Check for robustness of results

## Decide on what to search and include


* What is your question?
* Make clear criteria for inclusion/exclusion
* [Example from Zhu et al paper](https://docs.google.com/document/d/1pwaqYRGBx9GOzqoJU51PMQ17m4zVmtheZ6sLeH8-pm4/edit?usp=sharing)

## Decide on what to search and include

* **Warning: Easy way to lose reviewers**
* Grey literature?
* English-only?
  * biased!
* Year range

## Literature search

:::::::::::::: {.columns}
::: {.column}
* Can use a number of engines
* Make clear criteria for inclusion/exclusion
* [Example from Zhu et al paper](https://docs.google.com/document/d/1pwaqYRGBx9GOzqoJU51PMQ17m4zVmtheZ6sLeH8-pm4/edit?usp=sharing){target="_blank"}
:::
::: {.column}
![Options include Web of Knowledge, Google Scholar, and directed searches](https://logowik.com/content/uploads/images/google-scholar4372.jpg "Google Scholar logo")
:::
::::::::::::::

::: notes
This is a speaker note.

- Use basic Markdown
- like this list
- *and inline formatting*
:::

## Filtering papers

* You may end up with a lot!
  * Zhu et al initial search ended with over 3000
* Recommend two-step process with double reviewers if possible
  * Initial phase is determining if paper meets criteria
    * This is double-checked before data extractino
* [Example from Zhu et al paper](https://docs.google.com/spreadsheets/d/1E8tu8RWHVsGX0wQAoZfXhSIS5qEtIfruhNS70kwPjPY/edit?usp=sharing){target="_blank"}
* Note ![Publish or Perish](https://harzing.com/resources/publish-or-perish) software can download full results

## Data extraction

* After papers are double-checked, move info for those that meet criteria to new
sheet
* add columns for various data types, but in general for each treatment
you need to extract
  * basic info
  * potential explanatory variables
  * data type (proportion, numeric)
  * mean
  * variance measure
    * record how this is coded (SE, SEM, SD)
* [Web plot digitizer](https://automeris.io/ WebPlotDigitizer){target="_blank"} 
is useful for extracting graph data 
* [Example from Zhu et al paper](https://docs.google.com/spreadsheets/d/1iKvPHzvUJaHzLdHwuK5UNVWdjvQD2Jcu5_aYzBWUsGE/edit?usp=sharing){target="_blank"}  
    
  
## Why all the numbers?

* For each study, the goal is to quantify
  * size of study
  * strength of effect
  * magnitude of uncertainty 
* Need to standardize measurement of impact among studies
  + **effect sizes**!
  + these are the data you eventually analyze!
* Should incorporate sample sizes...
  + we trust larger studies
* and variance
  + we trust more precise studies!
  
## Other issues

* Need to standardize direction
  * eg, mortality vs survival
  
## Types of effect sizes

* standard mean difference
  + for studies comparing continuous response variable between control and
  experimental groups
    + Cohen's d or Hedge's g
  
$$
\text{Hedge's g = SMD = }\frac{\bar{X}_1 -\bar{X}_2}{SD^*_{pooled}}
$$ 
      * Cohen's d does not weight based on sample size in each group, but both assume
  homogeneity of variances 
  
## Types of effect sizes

  * response ratio
     + generally log-transformed

$$
RR = ln\frac{\bar{X}_1}{\bar{X}_2}
$$
         
## Types of effect sizes
       
* correlation coefficients
  + for studies analyzing relationship between two continuous variables
  + automatically incorporates variance, so a little easier
* odds ratio or risk ratio (not the same thing!)
  * for relationships among categories
  * count data
  
## Types of effect sizes
       
* correlation coefficients
  + for studies analyzing relationship between two continuous variables
  + automatically incorporates variance, so a little easier
* odds ratio or risk ratio (not the same thing!)
  * for relationships among categories
  * count data

## Finding effect sizes

* Can be found in many ways, but less inference is better
* Best is to have raw data
* Second (typical) is group means and variances (for numeric data)
  + may be estimated/derived from graphs with confidence intervals!
* or the counts from table-type data
* Can also be found from p-values, F statistics, etc

## Numeric data require standard deviation measures

* For numeric data, we'll need to find the standard deviation of each group
* Can be calculated from raw data or provided in text
* If given as standard error, remember 

$$
SE = \frac{SD}{\sqrt{n}}
$$ 
    
* so $SE \cdot \sqrt{n} = SD$

---

* if given as 95% confidence interval, remember

$$
95\%~CI = mean \pm \approx2 \cdot SE 
$$

* where $\approx2$ is typically determined by a t-distribution and dependent on 
sample size (it approaches 1.96 (normality) as sample size increases), so we can 
say 


$$
95\%~CI - mean = \approx2 \cdot \frac{SD}{\sqrt{n}}\text{, so}
$$
$$
\frac{(95\%~CI-mean) \cdot \sqrt{n}}{\approx 2}  = SD
$$

## Count data

* Can be calculated using the metafor package
* Make sure treatment/control line up as with numerical data setup if merging

## From statistical results

* Can be calculated using the esc package
* [See this site](https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/es-calc.html)

## Combining different forms

* Sometimes different types of data can be combined
* Odds ratios can be calculated and transformed to smd using the formula (Chinn 
2000, Murad et al 2019)


$$
OR = \frac{\pi}{\sqrt{3}}
$$

---

* Percentages can be transformed to odds ratios using (NCSS Statistical software chapter 134)

$$
OR = \frac{p_1 + (1 - p_1 )}{p_2 + (1 - p_2 )}
$$

## Interpreting effect sizes

* Small effect (cannot be discerned by the naked eye) ~ 0.2
* Medium Effect ~ 0.5
* Large Effect (can be seen by the naked eye) ~ 0.8

## Analysis overview!

* Look for mean effect size
  + remember this coming from a sample of a population, so we need to see if it 
  really differs from 0!
  + we tend to use a random effects model here to account for between study 
  variance
* Test for heterogeneity in effect sizes  
  + *do this even if mean effect size is 0* (Harrison 2010)
* Consider what drives heterogeneity with **meta-regression**
  
## Considering mean effect size and heterogeneity 

* Calculate
* Consider using barplot to see study-level contribution to mean
* Determine if mean effect size differs from 0
  + calculate using random effects model to consider heterogeneity among
  studies (next step)
  
## Are results the same among studies?

* Heterogeneity is important as it may indicate other factors are driving effect
size
* Also may impact mean effect size, so worth testing even if mean effect size is 
0 (Harrison 2010)
  
## Test for heterogeneity 

+ Q statistic 
  - larger is more heterogeneity
  - can test with chisquared test
  - built into rma object, but low power if studies or samples are small
+ Higgin's I^2^ (levels from Kovalchik 2013)
  * % of unexplained variance
    + < 30%  is low
    + 30-60% is moderate
    + 50 - 90% is substantial
    + 75 - 100% is considerable
  * built into rma object
  
## Test for heterogeneity 

* H^2^
  * \> 1 suggests unexplained heterogeneity
  
## Visualizing heterogeneity and study level contribution

* Forest plot shows effect size for each study
  * most common way of reporting meta-analysis outcomes
  * can be useful in visualizing patterns
    * color/shape code factors you eventually investigate with 
    meta-regression
* Use leave-one-out method to make sure you don't have major outliers
* visualize with funnel plots or orchard  plots

## Meta-regression to explain heterogeneity

* Use mods argument in metafor
* Can also consider decrease in heterogeneity

## Issues (and fixes)

* Publication bias
  + funnel plot
  + failsafe sample size/file drawer number
* study contribution
  * barplot of percentage contribution (Kovalchik 2013)
  * include random effects in models
* Advanced issues noted at end of Kovalchic (2013)

## Useful examples

 * Preisser et al 2005
 * Buchanen et al 2016
 * Tzetzlaff et al 2019
 
# Doing meta-analysis in R

## Package options (see [Kovalchik 2013](https://www.r-project.org/conferences/useR-2013/Tutorials/Kovalchik/kovalchik_meta_tutorial.pdf){target="_blank"}  tutorial for overview)

* metafor (focused on in above notes) 
  + key is rma object
  + escalc calculates effect sizes, but can be wrapped by rma
      * works as group 1 - group 2
  + if data is in long format, you can use a formula to find effect sizes
  + use random effect (RE) model to consider heterogeneity among studies

## Package options (see [Kovalchik 2013](https://www.r-project.org/conferences/useR-2013/Tutorials/Kovalchik/kovalchik_meta_tutorial.pdf){target="_blank"}  tutorial for overview)

* metafor (focused on in above notes) 
* meta
* rmeta
* dmetar
  * [has full book example](https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/){target="_blank"}

## Tutorials

* [metafor example (just to effect sizes, forest, and funnel plots)](https://rstudio-pubs-static.s3.amazonaws.com/10913_5858762ec84b458d89b0f4a4e6dd5e81.html){target="_blank"}
* [Kovalchik 2013 (long and and detailed)](https://www.r-project.org/conferences/useR-2013/Tutorials/Kovalchik/kovalchik_meta_tutorial.pdf){target="_blank"}
  * You can also check out the [help page](http://www.metafor-project.org/doku.php/metafor){target="_blank"}
  * [This book](https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/){target="_blank"}, though 
focused on the meta package, is also useful.


## This is example of 2 columns

:::::::::::::: {.columns}
::: {.column}
* hello
:::
::: {.column}
![This is the caption](https://upload.wikimedia.org/wikipedia/commons/9/94/Puffin_Mrkoww.jpg "this is alt text")
:::
::::::::::::::

::: notes
This is a speaker note.

- Use basic Markdown
- like this list
- *and inline formatting*
:::




