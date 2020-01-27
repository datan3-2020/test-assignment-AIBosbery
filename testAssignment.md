Test statistical assignment
================
Andrew Bosbery
25 January 2020

## Introduction

Please change the author and date fields above as appropriate. Do not
change the output format. Once you have completed the assignment you
want to knit your document into a markdown document in the
“github\_document” format and then commit both the .Rmd and .md files
(and all the associated files with graphs) to your private assignment
repository on Github.

## Reading data (40 points)

First, we need to read the data into R. For this assignment, I ask you
to use data from the youth self-completion questionnaire (completed by
children between 10 and 15 years old) from Wave 9 of the Understanding
Society. It is one of the files you have downloaded as part of SN6614
from the UK Data Service. To help you find and understand this file you
will need the following documents:

1)  The Understanding Society Waves 1-9 User Guide:
    <https://www.understandingsociety.ac.uk/sites/default/files/downloads/documentation/mainstage/user-guides/mainstage-user-guide.pdf>
2)  The youth self-completion questionnaire from Wave 9:
    <https://www.understandingsociety.ac.uk/sites/default/files/downloads/documentation/mainstage/questionnaire/wave-9/w9-gb-youth-self-completion-questionnaire.pdf>
3)  The codebook for the file:
    <https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/youth/wave/9>

<!-- end list -->

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages --------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

    ## -- Conflicts ------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# This attaches the tidyverse package. If you get an error here you need to install the package first. 
Data <- read_tsv("C:/Users/Andrew/Documents/University/Year 2/Term 2/Data Analysis 3/Project/data/UKDA-6614-tab/tab/ukhls_w9/i_youth.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
# You need to add between the quotation marks a full path to the required file on your computer.
```

## Tabulate variables (10 points)

In the survey children were asked the following question: “Do you have a
social media profile or account on any sites or apps?”. In this
assignment we want to explore how the probability of having an account
on social media depends on children’s age and gender.

Tabulate three variables: children’s gender, age (please use derived
variables) and having an account on social media.

``` r
# add your code here
#Table for age
table(Data$i_age_dv)
```

    ## 
    ##   9  10  11  12  13  14  15  16 
    ##   1 460 496 467 463 491 434   9

``` r
#Table for sex
table(Data$i_sex_dv)
```

    ## 
    ##    0    1    2 
    ##    2 1411 1408

``` r
#Table for social media platform
table(Data$i_ypsocweb)
```

    ## 
    ##   -9    1    2 
    ##   14 2277  530

## Recode variables (10 points)

We want to create a new binary variable for having an account on social
media so that 1 means “yes”, 0 means “no”, and all missing values are
coded as NA. We also want to recode gender into a new variable with the
values “male” and “female” (this can be a character vector or a factor).

``` r
# add your code here

#Recoding the social media variable
#1 = Yes, 0 = No,
Data$Have_Social_Media <- NA
Data$Have_Social_Media <- ifelse(Data$i_ypsocweb == 1,
                                 1,
                                 0)


#Recoding sex
Data$Gender <- NA
Data$Gender <- as.factor(ifelse(Data$i_sex_dv == 1,
                                "Male",
                                "Female"))
```

## Calculate means (10 points)

Produce code that calculates probabilities of having an account on
social media (i.e. the mean of your new binary variable produced in the
previous problem) by age and gender.

``` r
# add your code here
#Probability of someone having social media by age
tapply(Data$Have_Social_Media, Data$i_age_dv, mean, na.rm =TRUE)
```

    ##         9        10        11        12        13        14        15        16 
    ## 0.0000000 0.4847826 0.6975806 0.8543897 0.9092873 0.9429735 0.9585253 1.0000000

``` r
#Probability of someone having social media by gender
tapply(Data$Have_Social_Media, Data$Gender, mean, na.rm = TRUE)
```

    ##    Female      Male 
    ## 0.8361702 0.7781715

``` r
#Probability based on age and gender
tapply(Data$Have_Social_Media, list(Data$Gender, Data$i_age_dv), mean, na.rm = TRUE)
```

    ##         9        10        11        12        13        14        15 16
    ## Female  0 0.5327511 0.7587549 0.8681818 0.9240000 0.9576271 0.9859813  1
    ## Male   NA 0.4372294 0.6317992 0.8421053 0.8920188 0.9294118 0.9318182  1

## Write short interpretation (10 points)

\<\<\<\<\<\<\< HEAD Age and probability of using social media are
positively correlated. This means that an older child/teenager is more
likely to use social media than a younger child/teenager. Females are
roughly 6% more likely to use social media than males over the whole
sample and at every age females have a higher probability of using
social media than males

> > > > > > > 100850e7479bded70a156d5d97a72ce8d5482eef

## Visualise results (20 points)

Create a statistical graph (only one, but it can be faceted)
illustrating your results (i.e. showing how the probability of having an
account on social media changes with age and gender). Which type of
statistical graph would be most appropriate for this?

``` r
# add your code here

barplot(tapply(Data$Have_Social_Media, list(Data$Gender, Data$i_age_dv), mean, na.rm = TRUE),
        main = "Proportion of Young People with Social Media",
        xlab = "Age (Years)",
        ylab = "Proportion of Social Media users",
        legend = Data$Gender[1:2], 
        beside = TRUE)
```

![](testAssignment_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Conclusion

This is a test formative assignment and the mark will not count towards
your final mark. If you cannot answer any of the questions above this is
fine – we are just starting this module\! However, please do submit this
assignment in any case to make sure that you understand the procedure,
that it works correctly and you do not have any problems with summative
assignments later.
