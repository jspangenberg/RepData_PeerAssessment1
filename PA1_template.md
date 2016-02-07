# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
- Load data

```r
activityData <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
- Calculate total number of steps taken per day

```r
totalSteps <- aggregate(steps ~ date, data = activityData, sum, na.rm = TRUE)
```
- Make a histogram of the total number of steps taken each day

```r
hist(totalSteps$steps, main = "Histogram of the total number of steps taken each day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\
- Calculate and report the mean and median of the total number of steps taken per day

```r
meanTotalNrOfStepsPerDay <- mean(totalSteps$steps)
medianTotalNrOfStepsPerDay <- median(totalSteps$steps)
```
- The mean total number of steps taken per day: 1.0766189\times 10^{4}
- The median total number of steps taken per day: 10765

## What is the average daily activity pattern?
- Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsInterval <- aggregate(steps ~ interval, data = activityData, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)\
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxInterval <- stepsInterval[which.max(stepsInterval$steps), ]$interval
```
The 835th interval is the interval with the maximum number of steps.

## Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totNrWithNAs <- sum(is.na(activityData$steps))
```
Total number of rows with NAs: 2304

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
getMeanStepForInterval <- function(interval) {
    stepsInterval[stepsInterval$interval == interval, ]$steps 
}
```
- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityComplete <- activityData  # New dataset with original activity data
count = 0
for (i in 1:nrow(activityComplete)) {
    if (is.na(activityComplete[i, ]$steps)) {
        activityComplete[i, ]$steps <- getMeanStepForInterval(activityComplete[i, ]$interval)
        count = count + 1
    }
}
```
- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
totalStepsComplete <- aggregate(steps ~ date, data = activityComplete, sum)
hist(totalStepsComplete$steps, main = "Histogram of the total number of steps taken each day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)\


```r
meanTotalNrOfStepsPerDayComplete <- mean(totalStepsComplete$steps)
medianTotalNrOfStepsPerDayComplete <- median(totalStepsComplete$steps)
```
- Mean total number of steps taken per day: 1.0766189\times 10^{4}
- Median total number of steps taken per day: 1.0766189\times 10^{4}
- Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? Only median values differ slightly.

## Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityComplete$day = ifelse(as.POSIXlt(as.Date(activityComplete$date))$wday%%6 == 0, "weekend", "weekday")
activityComplete$day = factor(activityComplete$day, levels = c("weekend", "weekday"))
```
- Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
stepsIntervalComplete = aggregate(steps ~ interval + day, activityComplete, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = stepsIntervalComplete, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)\
