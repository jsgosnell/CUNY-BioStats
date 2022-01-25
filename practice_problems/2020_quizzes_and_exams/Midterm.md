2020 Bio 2100 Midterm: Halloween Redux (26 pts total)
================
jsg
10/27/2020

# Instructions! Read this first.

The exam is open note, open course website, open class-related code
repositories (mine and those you produced in class). However, you may
not get external help ( from other students, directed internet searches,
etc.). Please update the statement below to acknowledge these
instructions (and that you can use git).

I, INSERT YOUR NAME HERE, promise to not seek external help on the exam.
I understand any academic issues will result in a D or F on the exam or
in the class and be reported to the Dean of Students.

Good luck!

# Mind reading

## *Questions 1 - 3*

Researchers want to determine if mind-reading is possible. To test this,
they place a purported mind-reader in a soundproof room. Directly
outside the room is a chair. For each data point, a volunteer sits in
the chair or doesn’t. All the mind-reader has to do is “detect” the
presence of the volunteer (this should be the first step in reading
minds!). Even though the mind-reader can’t see or hear if the volunteer
is present, he correctly determines if a person is present or not 12
times out of 20.

1.  How would you assess this evidence to see if it offers evidence
    supporting the existence of mind-reading (6 pts)?

Make sure your answers include (if applicable)

-   null hypothesis
-   alternative hypothesis
-   explanation for test you will use
-   results from statistical test
-   clear explanation of how results relate to your stated hypotheses

2.  Calculate a confidence interval for the mind-reader data. Share it
    and clearly explain what it means (3 pts).

3.  If the mind-reader wanted to redo the experiment, what could they do
    to reduce the width of the confidence interval **ASSUMING** the
    noted signal in the data remains the same. Make sure you explain and
    justify your answer (3 pts).

# Ghosts

## *Questions 4 - 6*

People who hunt for ghosts typically note their “presence” is associated
with lower temperatures. To note this, they use a “ghost detecting
machine” to document the presence or absence of ghosts in a room. The
machine also documents the room temperature. Data is available using

``` r
ghosts <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/ghosts.csv", stringsAsFactors = T)
```

4.  Provide a graph of the data that clearly displays patterns in the
    central tendency and variance of the data (many options here!) (5
    pts).

5.  One issue with the data is that ghosts mainly occur in old (and
    poorly insulated) homes. Data shows the average temperature of homes
    where ghosts are present is on average 56 degrees when ghosts are
    not detected. Using this information, how would you assess the
    evidence to see if it offers evidence supporting the existence of
    ghosts (6 pts)?

As a hint, remember you can subset the data using
`ghosts[ghosts$Present == "Present", "Temperature"]`.

Make sure your answers include (if applicable)

-   null hypothesis
-   alternative hypothesis
-   explanation for test you will use
-   results from statistical test
-   clear explanation of how results relate to your stated hypotheses
-   confidence interval for your estimate

6.  The investigator is confused. He found an average temerature of
    55.8, which is less than 56. He thinks that should confirm the
    existence of ghosts. Explain any issues you see with his rationale
    in plain English. Also explain to him what a p-value means as part
    of your answer (3 pts).
