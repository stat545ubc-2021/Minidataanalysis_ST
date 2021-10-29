Mini Data-Analysis Deliverable 3
================
Sasha Tuttle

# Welcome to your last milestone in your mini data analysis project!

In Milestone 1, you explored your data and came up with research
questions. In Milestone 2, you obtained some results by making summary
tables and graphs.

In this (3rd) milestone, you’ll be sharpening some of the results you
obtained from your previous milestone by:

-   Manipulating special data types in R: factors and/or dates and
    times.
-   Fitting a model object to your data, and extract a result.
-   Reading and writing data as separate files.

**NOTE**: The main purpose of the mini data analysis is to integrate
what you learn in class in an analysis. Although each milestone provides
a framework for you to conduct your analysis, it’s possible that you
might find the instructions too rigid for your data set. If this is the
case, you may deviate from the instructions – just make sure you’re
demonstrating a wide range of tools and techniques taught in this class.

## Instructions

**To complete this milestone**, edit [this very `.Rmd`
file](https://raw.githubusercontent.com/UBC-STAT/stat545.stat.ubc.ca/master/content/mini-project/mini-project-3.Rmd)
directly. Fill in the sections that are tagged with
`<!--- start your work here--->`.

**To submit this milestone**, make sure to knit this `.Rmd` file to an
`.md` file by changing the YAML output settings from
`output: html_document` to `output: github_document`. Commit and push
all of your work to your mini-analysis GitHub repository, and tag a
release on GitHub. Then, submit a link to your tagged release on canvas.

**Points**: This milestone is worth 40 points (compared to the usual 30
points): 30 for your analysis, and 10 for your entire mini-analysis
GitHub repository. Details follow.

**Research Questions**: In Milestone 2, you chose two research questions
to focus on. Wherever realistic, your work in this milestone should
relate to these research questions whenever we ask for justification
behind your work. In the case that some tasks in this milestone don’t
align well with one of your research questions, feel free to discuss
your results in the context of a different research question.

# Setup

Begin by loading your data and the tidyverse package below:

``` r
Pilot<-read.csv("C:/Users/stutt/OneDrive/Desktop/Greenhouse_study/Pilot_study.csv", 
  header=T)

suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(rstatix))
suppressPackageStartupMessages(library(datateachr))
suppressPackageStartupMessages(library(dplyr))
```

From Milestone 2, you chose two research questions. What were they? Put
them here.

<!-------------------------- Start your work below ---------------------------->

1.  *Do blueberry flowers of the Bluecrop variety experience a greater
    fruit set when they receive pollen from another blueberry variety?*

2.  *Does the Bluecrop variety experience greater benefits from
    receiving pollen from another variety relative to the Duke variety?*
    <!----------------------------------------------------------------------------->

# Exercise 1: Special Data Types (10)

For this exercise, you’ll be choosing two of the three tasks below –
both tasks that you choose are worth 5 points each.

But first, tasks 1 and 2 below ask you to modify a plot you made in a
previous milestone. The plot you choose should involve plotting across
at least three groups (whether by facetting, or using an aesthetic like
colour). Place this plot below (you’re allowed to modify the plot if
you’d like). If you don’t have such a plot, you’ll need to make one.
Place the code for your plot below.

<!-------------------------- Start your work below ---------------------------->

The first plot I created in Milestone 2 was a boxplot of the different
fruit sets in the Bluecrop variety (given the treatments). I need to
re-create one of the tibbles I used beforehand to re-create this
boxplot.

``` r
#Creating a tibble to focus on the Bluecrop variety 
Blue<-as_tibble(Pilot) %>%
  filter(Variety == "Bluecrop") %>%
  rename(TSS = Average_TSS)

#Re-creating the boxplot from before 
ggplot(Blue, aes(Pollen_donor, fruit_set)) + geom_boxplot() + 
    geom_jitter(width=0.1, alpha=0.5) + xlab("pollen donor") + ylab("fruit set (%)") +
    ggtitle("Fruit Set vs Pollen Donor in the Bluecrop variety")
```

![](Milestone3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#If I wanted to edit this boxplot to include the Duke variety
ggplot(Pilot, aes(Pollen_donor, fruit_set)) + facet_wrap(~Variety) + geom_boxplot() + 
    geom_jitter(width=0.1, alpha=0.5) + xlab("pollen donor") + 
    ylab("fruit set (%)") +
    ggtitle("Fruit Set vs Pollen Donor in Both Varieties")
```

![](Milestone3_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

<!----------------------------------------------------------------------------->

Now, choose two of the following tasks.

1.  Produce a new plot that reorders a factor in your original plot,
    using the `forcats` package (3 points). Then, in a sentence or two,
    briefly explain why you chose this ordering (1 point here for
    demonstrating understanding of the reordering, and 1 point for
    demonstrating some justification for the reordering, which could be
    subtle or speculative.)

2.  Produce a new plot that groups some factor levels together into an
    “other” category (or something similar), using the `forcats` package
    (3 points). Then, in a sentence or two, briefly explain why you
    chose this grouping (1 point here for demonstrating understanding of
    the grouping, and 1 point for demonstrating some justification for
    the grouping, which could be subtle or speculative.)

3.  If your data has some sort of time-based column like a date (but
    something more granular than just a year):

    1.  Make a new column that uses a function from the `lubridate` or
        `tsibble` package to modify your original time-based column. (3
        points)
        -   Note that you might first have to *make* a time-based column
            using a function like `ymd()`, but this doesn’t count.
        -   Examples of something you might do here: extract the day of
            the year from a date, or extract the weekday, or let 24
            hours elapse on your dates.
    2.  Then, in a sentence or two, explain how your new column might be
        useful in exploring a research question. (1 point for
        demonstrating understanding of the function you used, and 1
        point for your justification, which could be subtle or
        speculative).
        -   For example, you could say something like “Investigating the
            day of the week might be insightful because penguins don’t
            work on weekends, and so may respond differently”.

<!-------------------------- Start your work below ---------------------------->

**Task Number**: 2 (without use of forcats)

The boxplot I created before is an improvement from before since I can
now compare the fruit set between treatments for both varieties (to see
if one variety experiences a greater benefit from cross-pollination
relative to the other). However, the control for the Bluecrop treatment
is the Bluecrop pollen donor whereas the control for the Duke variety is
the Duke pollen donor. I need to make the control treatments stick out
in some way.

``` r
alldata<- Pilot %>%
  mutate(Cross = ifelse(Variety == Pollen_donor, "self-cross", "out-cross")) %>%
  select(Variety, Pollen_donor, fruit_set, berry_weight, Average_TSS:Cross) %>%
  arrange(Variety, Pollen_donor, Cross) %>%
  rename("TSS" = "Average_TSS")

ggplot(alldata, aes(Pollen_donor, fruit_set, fill = Cross)) + facet_wrap(~Variety) + geom_boxplot() + 
    geom_jitter(width=0.1, alpha=0.5) + xlab("pollen donor") + 
    ylab("fruit set (%)") +
    ggtitle("Fruit Set vs Pollen Donor in Both Varieties")
```

![](Milestone3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Now that I’ve grouped the treatments (self-cross if the flower received
pollen from the same variety and out-cross if the flower received pollen
form a different variety), I can more easily compare the control groups
against the others. I can see that median fruit set is greater across
all treatments in the Bluecrop variety relative to the Duke variety. I
can also see that the control treatment had the lowest fruit set in the
Bluecrop variety but this wasn’t the case in the Duke variety!

**Task Number**: (without use of forcats)

It may also be helpful to combine data from both varieties, increasing
the chances of finding significant differences but perhaps unfairly so.
Blueberry variety can have a significant effect on blueberry weight,
TSS, pH, and TA, so removing this factor from the model could hurt it
more than help. Regardless, I will explore what a boxplot would look
like if I combined this data.

``` r
#Note: the code used to remove the x-axis label was obtained from https://www.datanovia.com/en/blog/ggplot-axis-labels/#:~:text=Remove%20the%20x%20and%20y,x%20%3D%20element_blank())%20.

#Examining TSS differences among treatments in Bluecrop
ggplot(Blue, aes(Pollen_donor, TSS)) + geom_boxplot() + 
    geom_jitter(width=0.1, alpha=0.5) + xlab("pollen donor") + 
    ylab("total soluble solids (%)") +
    ggtitle("TSS vs Pollen Donor in the Bluecrop variety")
```

![](Milestone3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#Creating a tibble to focus on Duke 
Du <-as_tibble(Pilot) %>%
  filter(Variety == "Duke") %>%
  rename(TSS = Average_TSS)

#Examining TSS differences among treatments in Duke 
ggplot(Du, aes(Pollen_donor, TSS)) + geom_boxplot() + 
    geom_jitter(width=0.1, alpha=0.5) + xlab("pollen donor") + 
    ylab("total soluble solids (%)") +
    ggtitle("TSS vs Pollen Donor in the Duke variety")
```

![](Milestone3_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
#Examining TSS differences in combined treatments 
ggplot(alldata, aes(Cross, TSS)) + geom_boxplot() + 
    geom_jitter(width=0.1, alpha=0.5) + 
    ylab("fruit set (%)") +  theme(axis.title.x = element_blank()) +
    ggtitle("Fruit Set vs Pollen Donor")
```

![](Milestone3_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

Surprisingly, combining the data and ignoring the different varieties
actually made the differences between my boxplots look smaller! I would
still compare the models for both the combined data and the original but
I feel the original would probably be better for exploring this
potential relationship.

<!----------------------------------------------------------------------------->
<!-------------------------- Start your work below ---------------------------->

**Task Number**: #1 reorder factor in original plot using forcats

Unfortunately, because my original data set only has two categorical
variables it is rather difficult/meaningless to re-arrange my factors.
Both the columns “Variety” and “pollen_donor” have repeating levels and
no unique IDs. As you saw before, any meaningful way to sort my data was
done without the need of the forcats package. So, I will have to ignore
my research questions for this task and use one of the alternative data
sets I looked at in Milestone 1. Below, I will be reviewing and working
with the parking meter dataset to demonstrate my ability to re-arrange
factors using ‘forcats’.

``` r
#Reviewing the parking meter data set from Milestone 1
#View(twin)

#New research question: Do parking meters collect less after 6 PM during the business week?
#Note, when I say less, I mean relative to the money collected from 9 - 6 PM.
#Because there are different types of meters, I'll also narrow it down just to the "Twin" type. 
twin <- parking_meters %>% 
filter(meter_head %in% c("Twin"))

business <- twin %>% 
  mutate(r_mf_9a_6p = fct_rev(fct_infreq(r_mf_9a_6p)))
ggplot(business, aes(r_mf_9a_6p)) + geom_bar()
```

![](Milestone3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
after_business <- twin %>% 
  mutate(r_mf_6p_10 = fct_rev(fct_infreq(r_mf_6p_10)))
#ggplot(after_business, aes(r_mf_6p_10)) + geom_col()

#May be more helpful to just use the count function
  twin %>%
    count(r_mf_9a_6p) 
```

    ## # A tibble: 9 x 2
    ##   r_mf_9a_6p     n
    ##   <chr>      <int>
    ## 1 $1.00       3062
    ## 2 $2.00       1742
    ## 3 $3.00       1232
    ## 4 $4.00        776
    ## 5 $5.00        812
    ## 6 $6.00        446
    ## 7 $7.00        320
    ## 8 $8.00        162
    ## 9 $9.00         62

``` r
  twin %>%
    count(r_mf_6p_10)
```

    ## # A tibble: 8 x 2
    ##   r_mf_6p_10     n
    ##   <chr>      <int>
    ## 1 $1.00       5012
    ## 2 $2.00       1114
    ## 3 $3.00        938
    ## 4 $4.00        690
    ## 5 $5.00        388
    ## 6 $6.00        258
    ## 7 $7.00        182
    ## 8 $8.00         32

Comparing these figures side by side would be a little misleading since
the y-axis are different. But upon close examination, one can see there
are many more instances of people only paying a dollar after 6pm
relative to business hours. You can also see that there are incidences
of people paying $9 during business hours but not after 6 pm. This plot
does not prove that the meters collect more after business hours during
the work week (in fact,it suggests that the meter collects less). The
figure, however, is a good first step at determining if this is a
relationship/queston worth pursuing further with statistics. Neither a
regression or an ANOVA would work in this case though because the data
is NOT normally distributed (and the data is NOT continuous).

**Task Number**: #2 group some of the factors into another category

Let’s say that I wanted to regroup the meter rates for a specific column
(in this case weekdays 9 am to 6 pm). There is so many individual rows
and if you’re looking for broader patterns, a difference in a dollar may
not be significant to you.

``` r
#Creating factors for the weekday rates
twin$r_mf_9a_6p<- factor(twin$r_mf_9a_6p)
twin$r_mf_9a_6p <- factor(twin$r_mf_6p_10)

NEW <- twin %>%
      mutate(r_mf_9a_6p = fct_collapse(r_mf_9a_6p,
             "$1" = c("$1.00"),
             "$2-4" = c("$2.00", "$3.00", "$4.00"),
             "$5-7" = c("$5.00", "$6.00", "$7.00" ),
             "$8" = c("$8.00")))
#The old plot
ggplot(data = twin, aes(x=r_mf_9a_6p)) + geom_bar() + xlab("meter rates") +
  ggtitle("Twin meter rates from Monday through Friday 9 am - 6pm")
```

![](Milestone3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#The new plot
ggplot(data = NEW, aes(x=r_mf_9a_6p)) + geom_bar() + xlab("meter rates") +
    ggtitle("Twin meter rates from Monday through Friday 9 am - 6pm")
```

![](Milestone3_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

The second plot created is a little easier on the eyes relative to the
first one, while still showing that the $1 rates far outnumber the other
rates, even after combining some factors!

<!----------------------------------------------------------------------------->

# Exercise 2: Modelling

## 2.0 (no points)

Pick a research question, and pick a variable of interest (we’ll call it
“Y”) that’s relevant to the research question. Indicate these.

<!-------------------------- Start your work below ---------------------------->

**Research Question**: Does the Bluecrop variety experience greater
benefits from receiving pollen from another variety relative to the Duke
variety?

**Variable of interest**: Total soluble solids (TSS) %

<!----------------------------------------------------------------------------->

## 2.1 (5 points)

Fit a model or run a hypothesis test that provides insight on this
variable with respect to the research question. Store the model object
as a variable, and print its output to screen. We’ll omit having to
justify your choice, because we don’t expect you to know about model
specifics in STAT 545.

-   **Note**: It’s OK if you don’t know how these models/tests work.
    Here are some examples of things you can do here, but the sky’s the
    limit.
    -   You could fit a model that makes predictions on Y using another
        variable, by using the `lm()` function.
    -   You could test whether the mean of Y equals 0 using `t.test()`,
        or maybe the mean across two groups are different using
        `t.test()`, or maybe the mean across multiple groups are
        different using `anova()` (you may have to pivot your data for
        the latter two).
    -   You could use `lm()` to test for significance of regression.

<!-------------------------- Start your work below ---------------------------->

In order to see if there is a statistically significant difference
between the TSS of the different treatments in Bluecrop and Duke, I will
have to try running an anova (assuming the data follows a normal
distribution).

``` r
#Creating a model for the Bluecrop variety 
model1 <- aov(TSS ~ Pollen_donor, data = Blue)

#Checking normal distribution and equal variance assumptions
ggqqplot(residuals(model1))
```

![](Milestone3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
plot(model1,1)
```

![](Milestone3_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
shapiro_test(data = Blue, TSS)
```

    ## # A tibble: 1 x 3
    ##   variable statistic     p
    ##   <chr>        <dbl> <dbl>
    ## 1 TSS          0.962 0.646

``` r
#Filtering just for the Duke variety 
Du<-as_tibble(Pilot) %>%
  filter(Variety == "Duke")

#Creating a model just for Duke 
model2 <- aov(Average_TSS ~ Pollen_donor, data = Du)

#Checking normal distribution and equal variance assumptions
ggqqplot(residuals(model2))
```

![](Milestone3_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
plot(model2,1)
```

![](Milestone3_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
shapiro_test(data = Du, Average_TSS)
```

    ## # A tibble: 1 x 3
    ##   variable    statistic     p
    ##   <chr>           <dbl> <dbl>
    ## 1 Average_TSS     0.931 0.227

I’ve fit the data to an anova model and checked the normal distribution
and equal variance of residuals assumptions.

<!----------------------------------------------------------------------------->

## 2.2 (5 points)

Produce something relevant from your fitted model: either predictions on
Y, or a single value like a regression coefficient or a p-value.

-   Be sure to indicate in writing what you chose to produce.
-   Your code should either output a tibble (in which case you should
    indicate the column that contains the thing you’re looking for), or
    the thing you’re looking for itself.
-   Obtain your results using the `broom` package if possible. If your
    model is not compatible with the broom function you’re needing, then
    you can obtain your results by some other means, but first indicate
    which broom function is not compatible.

<!-------------------------- Start your work below ---------------------------->

I will use the summary function to obtain a p-value for both models.

``` r
#Obtaining a p-value for the 1st model 
summary(model1)
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Pollen_donor  2  3.376   1.688   1.027  0.382
    ## Residuals    15 24.650   1.643

``` r
#Obtaining a p-value for the 2nd model 
summary(model2)
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## Pollen_donor  2  13.47   6.736   2.622  0.108
    ## Residuals    14  35.96   2.569               
    ## 1 observation deleted due to missingness

I assessed the difference between treatments using anova(). I compared
the p-values to alpha 0.05 and concluded that I lack evidence to reject
the null hypothesis. I concluded that the difference in treatments is
not statistically significant for either variety.

Note: I was not able to use the broom package to tidy my model results
because the model I created was not a linear model between two
continuous variables. Had I have created a linear model to test for a
correlation between berry weight and TSS (like I was interested in
Milestone 2), I could have used this package.
<!----------------------------------------------------------------------------->

# Exercise 3: Reading and writing data

Get set up for this exercise by making a folder called `output` in the
top level of your project folder / repository. You’ll be saving things
there.

## 3.1 (5 points)

Take a summary table that you made from Milestone 2 (Exercise 1.2), and
write it as a csv file in your `output` folder. Use the `here::here()`
function.

-   **Robustness criteria**: You should be able to move your Mini
    Project repository / project folder to some other location on your
    computer, or move this very Rmd file to another location within your
    project repository / folder, and your code should still work.
-   **Reproducibility criteria**: You should be able to delete the csv
    file, and remake it simply by knitting this Rmd file.

<!-------------------------- Start your work below ---------------------------->

I did not follow the instructions correctly in Milestone 2 (Exercise
1.2) so I will have to create a table here. In Milestone 2, I was
interested in descriptive statistics surrounding the fruit set of the
two varieties.

``` r
#Creating the table 
Table1 <- Blue %>%
  group_by(Pollen_donor) %>% 
  summarise(mean = mean(fruit_set, na.rm = TRUE), 
  median = median(fruit_set, na.rm = TRUE), 
  sd(fruit_set, na.rm = TRUE)) 

Table1
```

    ## # A tibble: 3 x 4
    ##   Pollen_donor  mean median `sd(fruit_set, na.rm = TRUE)`
    ##   <chr>        <dbl>  <dbl>                         <dbl>
    ## 1 Bluecrop     0.533  0.466                         0.138
    ## 2 Duke         0.634  0.640                         0.183
    ## 3 Reka         0.713  0.675                         0.169

``` r
#Writing the table I created above as a csv
write_csv(Table1, here::here("output", "Blue_fruitset.csv"))
```

<!----------------------------------------------------------------------------->

## 3.2 (5 points)

Write your model object from Exercise 2 to an R binary file (an RDS),
and load it again. Be sure to save the binary file in your `output`
folder. Use the functions `saveRDS()` and `readRDS()`.

-   The same robustness and reproducibility criteria as in 3.1 apply
    here.

<!-------------------------- Start your work below ---------------------------->

``` r
saveRDS(model1, here::here("output", "Model1"))
dir(here::here("output"))
```

    ## [1] "Blue_fruitset.csv" "exported_data.csv" "Model1"

``` r
readRDS(here::here("output", "Model1"))
```

    ## Call:
    ##    aov(formula = TSS ~ Pollen_donor, data = Blue)
    ## 
    ## Terms:
    ##                 Pollen_donor Residuals
    ## Sum of Squares      3.376178 24.650317
    ## Deg. of Freedom            2        15
    ## 
    ## Residual standard error: 1.281934
    ## Estimated effects may be unbalanced

<!----------------------------------------------------------------------------->

# Tidy Repository

Now that this is your last milestone, your entire project repository
should be organized. Here are the criteria we’re looking for.

## Main README (3 points)

There should be a file named `README.md` at the top level of your
repository. Its contents should automatically appear when you visit the
repository on GitHub.

Minimum contents of the README file:

-   In a sentence or two, explains what this repository is, so that
    future-you or someone else stumbling on your repository can be
    oriented to the repository.
-   In a sentence or two (or more??), briefly explains how to engage
    with the repository. You can assume the person reading knows the
    material from STAT 545A. Basically, if a visitor to your repository
    wants to explore your project, what should they know?

Once you get in the habit of making README files, and seeing more README
files in other projects, you’ll wonder how you ever got by without them!
They are tremendously helpful.

## File and Folder structure (3 points)

You should have at least four folders in the top level of your
repository: one for each milestone, and one output folder. If there are
any other folders, these are explained in the main README.

Each milestone document is contained in its respective folder, and
nowhere else.

Every level-1 folder (that is, the ones stored in the top level, like
“Milestone1” and “output”) has a `README` file, explaining in a sentence
or two what is in the folder, in plain language (it’s enough to say
something like “This folder contains the source for Milestone 1”).

## Output (2 points)

All output is recent and relevant:

-   All Rmd files have been `knit`ted to their output, and all data
    files saved from Exercise 3 above appear in the `output` folder.
-   All of these output files are up-to-date – that is, they haven’t
    fallen behind after the source (Rmd) files have been updated.
-   There should be no relic output files. For example, if you were
    knitting an Rmd to html, but then changed the output to be only a
    markdown file, then the html file is a relic and should be deleted.

Our recommendation: delete all output files, and re-knit each
milestone’s Rmd file, so that everything is up to date and relevant.

PS: there’s a way where you can run all project code using a single
command, instead of clicking “knit” three times. More on this in STAT
545B!

## Error-free code (1 point)

This Milestone 3 document knits error-free. (We’ve already graded this
aspect for Milestone 1 and 2)

## Tagged release (1 point)

You’ve tagged a release for Milestone 3. (We’ve already graded this
aspect for Milestone 1 and 2)
