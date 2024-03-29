---
title: "2022 Spring Midterm (30 points total)"
author: "jsg"
date: "Last compiled on 26 May, 2022 09:00"
output:
  html_document:
    toc: true
    toc_float: true
    keep_md: true
    self_contained: true
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

Good luck!

COVID-19 vaccines are a key part of the country's response to the pandemic, but 
they only work if people take them.  Real data from:  

* https://covid.cdc.gov/covid-data-tracker/#vaccinations_vacc-people-onedose-pop-5yr
* https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
* https://www.census.gov/library/stories/2021/04/how-do-covid-19-vaccination-and-vaccine-hesitancy-rates-vary-over-time.html

Note some questions are based on pretend surveys, etc, for teaching purposes.

# #1

1. The first national survey of vaccine hesitancy carried out in January 2021 
by the US Census Bureau showed 21.6% of the population was hesitant about getting 
the vaccine.  A random sample of 2500 people in March 2022 shows that 548 are still hesitant
about the vaccine. Is there evidence that vaccine hesitancy has changed 
over time? (6 pts)

Investigate the question. Make sure you include

* null hypothesis
* alternative hypothesis
* explanation for test you will use 
* results from statistical test
* clear explanation of how results relate to your stated hypotheses

# #2



2.  What is the confidence interval for proportion of people who are vaccine
hesitant in the sample described in questons 1? 
Calculate it and state what a confidence interval means. (3 pts)

# #3

3. Note the signal in the sample from question 1 was 0.2192. This is  not equal to .216. Explain
why this simple fact is or isn't enough to use for a comparision. Make sure you
use the term sampling error. (3 pts)

# #4


4. Nationally 443 adults per 1000 are vaccinated against the flu. Data on the 
number
of adults per 1000 vaccinated against COVID (as of 3/2022) in each state is available @


```r
covid <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQvZOcUlQy3dV3tqS65mwA2gUAI7a-bPW_AIdZX2I0vWWIjVwasoJ9QjtfR53MkxmSoyae3UVL9IFHS/pub?gid=1461643787&single=true&output=csv",
                             stringsAsFactors = T)
```


in the **Adults_vaccinated_per_1000** column.  Is
there evidence the number of adults vaccinated against COVID differs from that 
of those vaccinated against flu? (6 pts)

Investigate the question. Make sure you include

* null hypothesis
* alternative hypothesis
* explanation for test you will use 
* results from statistical test
* clear explanation of how results relate to your stated hypotheses

# #5


5. Graph the data used in question 3 in a way that shows variability in the 
number of adults vaccinated for COVID among states (several options here!) (5 pts)

# #6


6. A new booster is developed to aid against future strains. In testing, 25 
individuals who get the new booster develop COVID out of a sample of 1000. This 
is compared to a group who received the current boosters, where 42 individuals out 
of 1000 developed COVID.  Is there evidence the boosters differ in effectiveness? (6 pts)

Investigate the question. Make sure you include

* null hypothesis
* alternative hypothesis
* explanation for test you will use 
* results from statistical test
* clear explanation of how results relate to your stated hypotheses

# #7


7. Explain what the p-value you provided in question 6 means (what is a p-value)?
Given what you found, what would you recommend regarding the booster and/or future work? (4 pts)


