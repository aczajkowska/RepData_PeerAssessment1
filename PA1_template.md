---
title: "Assessment 1"
author: "aczajkowska"
date: "Sunday, May 17, 2015"
output: html_document
---

# Loading and preprocessing the data


```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}

data <- read.csv("activity.csv")
```

# What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day


```r
daily_steps <- aggregate(steps ~ date, data, sum)
```

2. Make a histogram of the total number of steps taken each day


```r
hist(daily_steps$steps, main = paste("Total number of steps per day"),
     xlab = "Number of steps", ylab = "Frequency", col = "Green",
     breaks = 8)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
my_mean <- mean(daily_steps$steps)
my_median <- median(daily_steps$steps)
my_mean
```

```
## [1] 10766.19
```

```r
my_median
```

```
## [1] 10765
```

# What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
steps_by_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, type="l",
     xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per day by Interval",
     breaks = 8)
```

```
## Warning in plot.window(...): 'breaks' nie jest parametrem graficznym
```

```
## Warning in plot.xy(xy, type, ...): 'breaks' nie jest parametrem graficznym
```

```
## Warning in axis(side = side, at = at, labels = labels, ...): 'breaks' nie
## jest parametrem graficznym
```

```
## Warning in axis(side = side, at = at, labels = labels, ...): 'breaks' nie
## jest parametrem graficznym
```

```
## Warning in box(...): 'breaks' nie jest parametrem graficznym
```

```
## Warning in title(...): 'breaks' nie jest parametrem graficznym
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maximum_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```

# Imputing missing values
1. Calculate and report the total number of missing values in the dataset


```r
missing <- sum(!complete.cases(data))

imputed_data <- transform(data, steps = ifelse(is.na(data$steps),
      steps_by_interval$steps[match(data$interval,
      steps_by_interval$interval)], data$steps))
```
2. Devise a strategy for filling in all of the missing values in the dataset.

Missing values were imputed by inserting the average for each interval. 
Zeroes were imputed for 10-01-2012.


```r
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
daily_steps2 <- aggregate(steps ~ date, imputed_data, sum)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
hist(daily_steps2$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
my_mean2 <- mean(daily_steps2$steps)
my_median2 <- median(daily_steps$steps)
```
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
mean_diff <- my_mean2 - my_mean
med_diff <- my_median2 - my_median
total_diff <- sum(daily_steps2$steps) - sum(daily_steps$steps)
```

# Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend”


```r
weekdays <- c("poniedzia³ek", "wtorek", "œroda", "czwartek", "pi¹tek")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval2 <- aggregate(steps ~ interval + dow, imputed_data, mean)
```

2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days. 

```r
library(lattice)
xyplot(steps_by_interval2$steps ~ steps_by_interval2$interval|steps_by_interval2$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
