Loading and Preprocessing the data
----------------------------------

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
setwd("/Users/mbarhanp/Documents/Coursera coursework/Reproducible Research")
activity <- read.csv("activity.csv")
```

Exploring the data

``` r
dim(activity)
```

    ## [1] 17568     3

``` r
names(activity)
```

    ## [1] "steps"    "date"     "interval"

``` r
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
#total number of missing data
sum(is.na(activity$steps))/dim(activity)[[1]]
```

    ## [1] 0.1311475

Histogram of Total Number of Steps taken per day
------------------------------------------------

**1. Number of steps per day**

``` r
StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
```

**2. Histogram of the total number of steps taken each day**

``` r
# draw the histogram
g <- ggplot(StepsPerDay, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkgreen", fill="lightgreen")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))
```

![](Reproducible_Research_assignment1_files/figure-markdown_github/histogram1-1.png)

Mean and median number of steps taken per day
---------------------------------------------

``` r
# Mean
mean(StepsPerDay$Steps, na.rm=TRUE)
```

    ## [1] 10766.19

``` r
#Median
median(StepsPerDay$Steps, na.rm=TRUE)
```

    ## [1] 10765

Time series plot of the average number of steps taken
-----------------------------------------------------

``` r
# create table with steps per time
StepsPerTime <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
StepsPerTime$time <- StepsPerTime$interval/100
# draw the line plot
h <- ggplot(StepsPerTime, aes(time, steps))
h+geom_line(col="brown")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))
```

![](Reproducible_Research_assignment1_files/figure-markdown_github/timeplot1-1.png)

The 5-minute interval that, on average, contains the maximum number of steps
----------------------------------------------------------------------------

``` r
# table for dplyr
ST <- tbl_df(StepsPerTime)
# find the column
ST %>% select(time, steps) %>% filter(steps==max(ST$steps))
```

    ## # A tibble: 1 x 2
    ##    time steps
    ##   <dbl> <dbl>
    ## 1  8.35  206.

Code to describe and show a strategy for imputing missing data
--------------------------------------------------------------

**1. Total number of missing values in the dataset**

``` r
# table for dplyr
ACT <- tbl_df(activity)
# find the column
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())
```

    ## # A tibble: 1 x 1
    ##   missing_values
    ##            <int>
    ## 1           2304

**2. Replace missing values**  
The rounded values of the average 5-minute interval is used to replace
the NA values.  
*CompleteSteps* is the new column without missing values.

``` r
# values without NA are imputed in a new column
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(StepsPerTime$steps[match(activity$interval, StepsPerTime$interval)],0), activity$steps)
```

**3. New dataset that is equal to the original dataset but with the
missing data filled in**  
The first ten values of the new dataset are shown below.

``` r
# new dataset activityFull
activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)
# see first 10 values of the new dataset
head(activityFull, n=10)
```

    ##    steps interval       date
    ## 1      2        0 2012-10-01
    ## 2      0        5 2012-10-01
    ## 3      0       10 2012-10-01
    ## 4      0       15 2012-10-01
    ## 5      0       20 2012-10-01
    ## 6      2       25 2012-10-01
    ## 7      1       30 2012-10-01
    ## 8      1       35 2012-10-01
    ## 9      0       40 2012-10-01
    ## 10     1       45 2012-10-01

Histogram of the total number of steps taken each day after missing values are imputed
--------------------------------------------------------------------------------------

``` r
# prepare data
StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
# draw the histogram
g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))
```

![](Reproducible_Research_assignment1_files/figure-markdown_github/histogram2-1.png)

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
---------------------------------------------------------------------------------------------------------

**1. Create a new factor variable in the dataset with two levels -
“weekday” and “weekend” indicating whether a given date is a weekday or
weekend day.**  
*DayType* is the new column indicating if the day is a weekday day or a
weekend day: the first ten values of the new table are shown below

``` r
# Create variable with date in correct format
activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
# create a variable with weekdays name
activityFull$weekday <- weekdays(activityFull$RealDate)
# create a new variable indicating weekday or weekend
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(activityFull, n=10)
```

    ##    steps interval       date   RealDate weekday DayType
    ## 1      2        0 2012-10-01 2012-10-01  Monday weekday
    ## 2      0        5 2012-10-01 2012-10-01  Monday weekday
    ## 3      0       10 2012-10-01 2012-10-01  Monday weekday
    ## 4      0       15 2012-10-01 2012-10-01  Monday weekday
    ## 5      0       20 2012-10-01 2012-10-01  Monday weekday
    ## 6      2       25 2012-10-01 2012-10-01  Monday weekday
    ## 7      1       30 2012-10-01 2012-10-01  Monday weekday
    ## 8      1       35 2012-10-01 2012-10-01  Monday weekday
    ## 9      0       40 2012-10-01 2012-10-01  Monday weekday
    ## 10     1       45 2012-10-01 2012-10-01  Monday weekday

**2. Two time series plot of the 5-minute interval (x) and the average
number of steps taken averaged across weekday days or weekend days
(y).**

``` r
# create table with steps per time across weekdaydays or weekend days
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
StepsPerTimeDT$time <- StepsPerTime$interval/100
# draw the line plot
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
```

![](Reproducible_Research_assignment1_files/figure-markdown_github/timeplot2-1.png)
