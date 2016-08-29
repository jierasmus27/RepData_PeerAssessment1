# Reproducible Research: Peer Assessment 1
#Activity data Summaries

##Week 2 Assignment: Reproducible research

## Loading and preprocessing the data

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
activity_dataset <- read.csv(unzip("activity.zip"))
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
total_steps_per_day <- aggregate(steps ~ date, data = activity_dataset, sum, na.rm = TRUE)
```

2. Histogram of the total number of steps taken each day

```r
hist(total_steps_per_day$steps, main = "Total Number of steps per day", xlab = "Total steps per day", ylab = "Frequency", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
total_mean <- mean(total_steps_per_day$steps)
total_median <- median(total_steps_per_day$steps)
```
The mean is 1.0766189\times 10^{4} and the median is 10765.

## What is the average daily activity pattern?
1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean_steps_per_interval <- aggregate(steps ~ interval, data = activity_dataset, mean, na.rm = TRUE)

plot(mean_steps_per_interval$interval, mean_steps_per_interval$steps, type = "l", main = "Average number of steps taken", xlab = "Interval", ylab = "Average Steps", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval_index <- which.max(mean_steps_per_interval$steps)
max_interval <- mean_steps_per_interval[max_interval_index, ]
```
The maximum interval is at index 835, with 206.1698113 steps

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset

```r
missing_values_total = nrow(activity_dataset[is.na(activity_dataset$steps), ])
```
The amount of missing step values is: 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We previously computed the 5 minute interval means from the original dataset, so we will use that data to impute the missing values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputed_dataset <- activity_dataset
for (i in 1:nrow(activity_dataset)) {
  if (is.na(activity_dataset[i, ]$steps)) {
    imputed_dataset[i, ]$steps <- mean_steps_per_interval[mean_steps_per_interval$interval == activity_dataset$interval[i], ]$steps
  }
}
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_steps_per_day <- aggregate(steps ~ date, data = imputed_dataset, sum, na.rm = TRUE)
hist(total_steps_per_day$steps, main = "Imputed steps per day", xlab = "Total steps per day", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
imputed_mean <- mean(total_steps_per_day$steps)
imputed_median <- median(total_steps_per_day$steps)
```
The imputed mean is 1.0766189\times 10^{4} versus the original mean 1.0766189\times 10^{4}
The imputed median is 1.0766189\times 10^{4} versus the original median 10765

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
imputed_weekdays <- weekdays(as.Date(imputed_dataset$date))
for (i in 1:length(imputed_weekdays)) {
    if (imputed_weekdays[i] %in% c("Saturday", "Sunday")) {
      imputed_weekdays[i] <- "Weekend"
    } else { 
      imputed_weekdays[i] <- "Weekday"
    }
}
imputed_dataset$weekday <- as.factor(imputed_weekdays)
```
2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(lattice)

weekend_dataset <- imputed_dataset[imputed_dataset$weekday == "Weekend", ]
weekday_dataset <- imputed_dataset[imputed_dataset$weekday == "Weekday", ]

total_steps_per_weekend <- aggregate(steps ~ interval, data = weekend_dataset, mean, na.rm = TRUE)
total_steps_per_weekday <- aggregate(steps ~ interval, data = weekday_dataset, mean, na.rm = TRUE)

total_steps_per_weekend$weekday <- "Weekend"
total_steps_per_weekday$weekday <- "Weekday"

totals_combined <- rbind(total_steps_per_weekday, total_steps_per_weekend)

xyplot(steps ~ interval | weekday, totals_combined, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Steps Amount")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->




