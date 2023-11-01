Quiz 5
================
jsg
11/18/2020

# Biocontrol effectiveness!

One way to reduce herbivory by invasive species is biocontrol - the
intentional release or supplementation of species that prey on a pest.
To consider if biocontrol is effective at reducing aphid density across
generations, scientists consider the number of aphids in square meter
plots at 20 fields prior to the release of biocontrol agents. A year
later they visit the same plots and sample for aphids again. Data is
available using

``` r
aphid_density <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR7IAJjpKonpJLjENw9GQ7tiZm63YZZq5ed7es0OFceWvEqvPSiqpYrEnnKiCsNGIspUhn3YDry_ChF/pub?gid=552866496&single=true&output=csv", stringsAsFactors = T)
```

How would you evaluate the data?

Make sure your answers include

- null hypothesis
- alternative hypothesis
- explanation for test you will use
- results from statistical test (including post-hoc tests if needed!)
- clear explanation of how results relate to your stated hypotheses
- a graph that clearly displays the data

In case it helps (it may not, but I want you to know how to melt data
into long format)

``` r
library(reshape2)
aphid_density_long <- melt(aphid_density, id.vars = "field", variable.name = "before_after",
                           value.name = "aphid_density")
```
