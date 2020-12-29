---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Project 1 of the Reproducible Research Course
=============================================
Load the packages we will need:

```r
#Loading the packages
library(dplyr)
library(lattice)
options(scipen = 1, digits = 2)
```

First we load the data:


```r
monitor_data <- readr::read_csv('activity.zip')
```

## What is mean total number of steps taken per day?

Now we show an histogram of the total number of steps taken each day:


```r
tot_steps <- summarise(group_by(monitor_data, date), total_steps = sum(steps,
                                                               na.rm = TRUE))
```



```r
hist(tot_steps$total_steps, main = 'Total Steps Taken', xlab = 'Steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Now we calculate the mean and the median:


```r
mean_steps <- mean(tot_steps$total_steps)
median_steps <- median(tot_steps$total_steps)
```

The median steps taken per day is **10395**

The mean of steps taken per day is **9354.23**.

## What is the average daily activity pattern?

First we summarise the steps data of each 5-minute interval across all days.


```r
steps_interval <- summarise(group_by(monitor_data, interval),steps_mean = mean(steps, na.rm = TRUE))
```

And create a lineplot with time vs steps:


```r
plot(steps_interval$interval, steps_interval$steps_mean, type = 'l', ylab = 'Steps', xlab = 'time (minutes)')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Finally we find the interval with the maximum number of steps:


```r
max_steps_row <- which.max(steps_interval$steps_mean)
interval <- steps_interval[max_steps_row, 1]
```


The interval with the maximum number of steps on average across all days is the **835**.

## Inputing missing values

The total number of rows with NA's can be calculated like this:


```r
total.na = sum(is.na(monitor_data$steps))
```

There are **2304** rows with NA.

To fill each row from this data, we can use the mean of each 5-minute interval to replace the NAs. 

First we create a function that gives us the mean of an interval.


```r
interval_mean <- function(interval){
  return(steps_interval$steps_mean[
    which(steps_interval$interval == interval)])
}
```

Then we create a copy of the original dataset and use a for loop to replace the NAs in the data.


```r
copydata <- monitor_data
for (i in 1:nrow(copydata)){
  if (is.na(copydata[i,1])){
    interval = copydata[[i,3]]
    copydata[i,1] <- as.list(interval_mean(interval))
    }
}
```

To confirm the operation we print a summary of the corrected data.


```r
summary(copydata)
```

```
##      steps          date               interval   
##  Min.   :  0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0   Median :2012-10-31   Median :1178  
##  Mean   : 37   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 27   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806   Max.   :2012-11-30   Max.   :2355
```

As before, we create a histogram of the total number of steps taken each day.


```r
total_steps_day <- summarise(group_by(copydata, date), total_steps = sum(steps, na.rm = TRUE))

hist(total_steps_day$total_steps, main = 'Total Steps Taken Without Missing Data', xlab = 'Steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
mean_na_steps = mean(total_steps_day$total_steps)
median_na_steps = median(total_steps_day$total_steps)
```



The mean of the data without NAs is **10766.19**, it increased from the original value: **9354.23**.

The median of the data without NAs is **10766.19**, it increased also from **10395**.

Depending of the method used to replace the data, it can change its statistics, in this case the mean became equal to the median of the data and both increased in value.

## Are there differences in activity patterns between weekdays and weekends?

First, we create a column which say if the day is a weekday day or a weekend day.


```r
Sys.setlocale("LC_TIME", "English")
monitor_data$daytype = ifelse(weekdays(monitor_data$date) %in% list('Saturday', 'Sunday'), 'weekend', 'weekday')
```

Now, we calculate the mean of the weekday days and the weekend days or each interval.


```r
weekdays_sum = summarise(group_by(monitor_data, daytype, interval), mean_steps = mean(steps, na.rm = TRUE))
```

Finally, we show a comparison between the activity on weekdays and the activity on the weekend.




```r
weekdays_sum <- transform(weekdays_sum, daytype = factor(daytype))
xyplot(mean_steps ~ interval | daytype, data = weekdays_sum, layout = c(1,2), type = 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

The last plot shows a clear difference between weekdays and weekends, while the beginning of a weekday is clearly more activity than the weekend, during the rest of the day the weekends are more active.
