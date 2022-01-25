Plotting quiz
================
jsg
10/16/2020

------------------------------------------------------------------------

# ZOMBIES!

You wake up one morning and find that zombies have taken over your
neighborhood (bummer). One idea is to use biocontrol to reduce zombie
attacks. A zombie parasite is identified. Test plots (neighborhoods!)
are subjected to one or two releases of the parasite at low, medium, or
high densities. Use the data (in R chunk below!) to construct a graph
(You should use a bar graph with confidence intervals!) that properly
summarizes how the number of releases impacts the number of zombie
attacks.

``` r
biocontrol <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/biocontrol.csv",
                       stringsAsFactors = T)
```
