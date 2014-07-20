# Reproducible Research: Peer Assessment 1
Margarette K.


## Loading and preprocessing the data

At first unzip and read our data.

```r
unzip("activity.zip", exdir = "data")
activity <- read.csv("data/activity.csv")
```
Now we have dataset called "activity". Formatting dates from factors to dates. Take a look at it with summury function.

```r
activity$date <- as.Date(activity$date)
summary(activity)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

Histigram of steps per date.

```r
library(ggplot2)
activity.by.date <- aggregate(steps ~ date, data = activity, FUN = sum)
qplot(date, steps, data=activity.by.date, geom = "histogram", stat = "identity", main = "Total number of steps per date", ylab = "number of steps")
```

![plot of chunk step_hist](./PA1_template_files/figure-html/step_hist.png) 


The mean and median total number of steps taken per day.

```r
steps.mean <- aggregate(steps ~ date, data = activity, FUN = mean)
names(steps.mean) <- c("date", "mean_steps")
steps.median <- aggregate(steps ~ date, data = activity, FUN = median)
names(steps.median) <- c("date", "median_steps")
merge(steps.mean, steps.median)
```

```
##          date mean_steps median_steps
## 1  2012-10-02     0.4375            0
## 2  2012-10-03    39.4167            0
## 3  2012-10-04    42.0694            0
## 4  2012-10-05    46.1597            0
## 5  2012-10-06    53.5417            0
## 6  2012-10-07    38.2465            0
## 7  2012-10-09    44.4826            0
## 8  2012-10-10    34.3750            0
## 9  2012-10-11    35.7778            0
## 10 2012-10-12    60.3542            0
## 11 2012-10-13    43.1458            0
## 12 2012-10-14    52.4236            0
## 13 2012-10-15    35.2049            0
## 14 2012-10-16    52.3750            0
## 15 2012-10-17    46.7083            0
## 16 2012-10-18    34.9167            0
## 17 2012-10-19    41.0729            0
## 18 2012-10-20    36.0938            0
## 19 2012-10-21    30.6285            0
## 20 2012-10-22    46.7361            0
## 21 2012-10-23    30.9653            0
## 22 2012-10-24    29.0104            0
## 23 2012-10-25     8.6528            0
## 24 2012-10-26    23.5347            0
## 25 2012-10-27    35.1354            0
## 26 2012-10-28    39.7847            0
## 27 2012-10-29    17.4236            0
## 28 2012-10-30    34.0938            0
## 29 2012-10-31    53.5208            0
## 30 2012-11-02    36.8056            0
## 31 2012-11-03    36.7049            0
## 32 2012-11-05    36.2465            0
## 33 2012-11-06    28.9375            0
## 34 2012-11-07    44.7326            0
## 35 2012-11-08    11.1771            0
## 36 2012-11-11    43.7778            0
## 37 2012-11-12    37.3785            0
## 38 2012-11-13    25.4722            0
## 39 2012-11-15     0.1424            0
## 40 2012-11-16    18.8924            0
## 41 2012-11-17    49.7882            0
## 42 2012-11-18    52.4653            0
## 43 2012-11-19    30.6979            0
## 44 2012-11-20    15.5278            0
## 45 2012-11-21    44.3993            0
## 46 2012-11-22    70.9271            0
## 47 2012-11-23    73.5903            0
## 48 2012-11-24    50.2708            0
## 49 2012-11-25    41.0903            0
## 50 2012-11-26    38.7569            0
## 51 2012-11-27    47.3819            0
## 52 2012-11-28    35.3576            0
## 53 2012-11-29    24.4688            0
```
## What is the average daily activity pattern?

Time series of 5 minut interval...

```r
activity.by.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
qplot(interval, steps, data=activity.by.interval, geom = "line", main = "Average number of steps per 5-minute interval", xlab = "5-minute interval ", ylab = "number of steps")
```

![plot of chunk interval](./PA1_template_files/figure-html/interval.png) 


The 5-minute interval = 835, on average across all the days in the dataset, contains the maximum number of steps = 206.1698.


```r
library(plyr)
interval.max.desc <- arrange(activity.by.interval, desc(steps))
head(interval.max.desc)
```

```
##   interval steps
## 1      835 206.2
## 2      840 195.9
## 3      850 183.4
## 4      845 179.6
## 5      830 177.3
## 6      820 171.2
```
So, this is the time to go to work 8:35

## Imputing missing values

The total numer of NA in dataset = 2304.

```r
sum(is.na(activity))
```

```
## [1] 2304
```
Fill the NA's with the mean for that 5-minute interval.

```r
activity.filled <- ldply(lapply(split(activity, as.factor(activity$date)), function(ad) {
  for(i in ad$interval) {
    if (is.na(ad[ad$interval == i, ]$steps)) {
      ad[ad$interval == i, ]$steps <- activity.by.interval[activity.by.interval$interval == i, ]$steps
    }
  }
  ad
}), data.frame)
activity.filled <- activity.filled[, 2:ncol(activity.filled)]
```

Histigram of steps per date.

```r
library(ggplot2)
activity.by.date <- aggregate(steps ~ date, data = activity.filled, FUN = sum)
qplot(date, steps, data=activity.by.date, geom = "histogram", stat = "identity", main = "Total number of steps per date", ylab = "number of steps")
```

![plot of chunk step_hist_filled](./PA1_template_files/figure-html/step_hist_filled.png) 


The mean and median total number of steps taken per day.

```r
steps.mean <- aggregate(steps ~ date, data = activity.filled, FUN = mean)
names(steps.mean) <- c("date", "mean_steps")
steps.median <- aggregate(steps ~ date, data = activity.filled, FUN = median)
names(steps.median) <- c("date", "median_steps")
merge(steps.mean, steps.median)
```

```
##          date mean_steps median_steps
## 1  2012-10-01    37.3826        34.11
## 2  2012-10-02     0.4375         0.00
## 3  2012-10-03    39.4167         0.00
## 4  2012-10-04    42.0694         0.00
## 5  2012-10-05    46.1597         0.00
## 6  2012-10-06    53.5417         0.00
## 7  2012-10-07    38.2465         0.00
## 8  2012-10-08    37.3826        34.11
## 9  2012-10-09    44.4826         0.00
## 10 2012-10-10    34.3750         0.00
## 11 2012-10-11    35.7778         0.00
## 12 2012-10-12    60.3542         0.00
## 13 2012-10-13    43.1458         0.00
## 14 2012-10-14    52.4236         0.00
## 15 2012-10-15    35.2049         0.00
## 16 2012-10-16    52.3750         0.00
## 17 2012-10-17    46.7083         0.00
## 18 2012-10-18    34.9167         0.00
## 19 2012-10-19    41.0729         0.00
## 20 2012-10-20    36.0938         0.00
## 21 2012-10-21    30.6285         0.00
## 22 2012-10-22    46.7361         0.00
## 23 2012-10-23    30.9653         0.00
## 24 2012-10-24    29.0104         0.00
## 25 2012-10-25     8.6528         0.00
## 26 2012-10-26    23.5347         0.00
## 27 2012-10-27    35.1354         0.00
## 28 2012-10-28    39.7847         0.00
## 29 2012-10-29    17.4236         0.00
## 30 2012-10-30    34.0938         0.00
## 31 2012-10-31    53.5208         0.00
## 32 2012-11-01    37.3826        34.11
## 33 2012-11-02    36.8056         0.00
## 34 2012-11-03    36.7049         0.00
## 35 2012-11-04    37.3826        34.11
## 36 2012-11-05    36.2465         0.00
## 37 2012-11-06    28.9375         0.00
## 38 2012-11-07    44.7326         0.00
## 39 2012-11-08    11.1771         0.00
## 40 2012-11-09    37.3826        34.11
## 41 2012-11-10    37.3826        34.11
## 42 2012-11-11    43.7778         0.00
## 43 2012-11-12    37.3785         0.00
## 44 2012-11-13    25.4722         0.00
## 45 2012-11-14    37.3826        34.11
## 46 2012-11-15     0.1424         0.00
## 47 2012-11-16    18.8924         0.00
## 48 2012-11-17    49.7882         0.00
## 49 2012-11-18    52.4653         0.00
## 50 2012-11-19    30.6979         0.00
## 51 2012-11-20    15.5278         0.00
## 52 2012-11-21    44.3993         0.00
## 53 2012-11-22    70.9271         0.00
## 54 2012-11-23    73.5903         0.00
## 55 2012-11-24    50.2708         0.00
## 56 2012-11-25    41.0903         0.00
## 57 2012-11-26    38.7569         0.00
## 58 2012-11-27    47.3819         0.00
## 59 2012-11-28    35.3576         0.00
## 60 2012-11-29    24.4688         0.00
## 61 2012-11-30    37.3826        34.11
```
Now we have medians that are not zero.

## Are there differences in activity patterns between weekdays and weekends?

