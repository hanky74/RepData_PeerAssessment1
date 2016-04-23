# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1.Loading libraries required.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

2.Loading dataset from csv file

```r
setwd("C:/Users/GilbertHan/Documents/6.비전 준비하기/6.3 Data Science/Coursera_DataScience/5.Reproducible Research/week 1/Assignment")
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken


## What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day

```r
GroupByDate <- group_by(activity, date)
DailySteps <- summarise(GroupByDate, TotalStep = sum(steps, na.rm = T))
```

2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

```r
hist(DailySteps$TotalStep )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)



3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(DailySteps$TotalStep)
```

```
## [1] 9354.23
```

```r
median(DailySteps$TotalStep)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
GroupByInterval <- group_by(activity, interval)
AvgSteps <- summarise(GroupByInterval, AvgSteps = mean(steps, na.rm = T))

maxInterval <- AvgSteps[ AvgSteps$AvgSteps == max(AvgSteps$AvgSteps),1]

plot(AvgSteps$interval, AvgSteps$AvgSteps, type ="l"
     , main = "Average number of steps of 5-min Interval"
     , xlab = "interval", ylab = 'Average steps')
abline(v = maxInterval, col = "red", lwd = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxInterval
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```


## Imputing missing values
1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
WithNA <- activity[is.na(activity$steps) =='TRUE',] 
WithoutNA <- activity[is.na(activity$steps) =='FALSE',] 

intervals <- AvgSteps[,1]

for(i in intervals) {
    WithNA$steps[WithNA$interval == i] <- as.integer(AvgSteps$AvgSteps[AvgSteps$interval == i])
}

activity2 <- rbind(WithNA, WithoutNA)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
GroupByDate2 <- group_by(activity2, date)
DailySteps2 <- summarise(GroupByDate2, TotalStep = sum(steps, na.rm = T))

hist(DailySteps2$TotalStep, main = "Total Steps after imputing missing data"
     , xlab = "Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)

```r
mean(DailySteps$TotalStep)
```

```
## [1] 9354.23
```

```r
mean(DailySteps2$TotalStep)
```

```
## [1] 10749.77
```

```r
median(DailySteps$TotalStep)
```

```
## [1] 10395
```

```r
median(DailySteps2$TotalStep)
```

```
## [1] 10641
```

## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels – “weekday” and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity2$flag_wkday <- "weekday"
activity2$flag_wkday[weekdays(as.Date(activity2$date)) %in% c("토요일","일요일")] <- "weekend"

activity2$flag_wkday <- as.factor(activity2$flag_wkday)
```
2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
GroupByInterval2 <- group_by(activity2, flag_wkday, interval)
AvgSteps2 <- summarise(GroupByInterval2, AvgSteps = mean(steps, na.rm = T))

qplot(interval,AvgSteps, data = AvgSteps2, facets = flag_wkday ~ .,geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)




