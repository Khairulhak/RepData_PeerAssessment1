## Loading and preprocessing the data

- Load the data and unzip the file (i.e. read.csv())
- Process/transform the data (if necessary) into a format suitable for your analysis


```r
filename <- unzip("activity.zip")
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))

library(lattice)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day

```r
TotalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(TotalSteps$steps, main = "Total steps", xlab = "Day", col = "grey")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(TotalSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
TimeSeries <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(row.names(TimeSeries), TimeSeries, type = "l", xlab = "Interval", 
    ylab = "Average", main = "Average number of steps taken", 
    col = "grey")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MaximumInterval <- which.max(TimeSeries)
names(MaximumInterval)
```

```
## [1] "835"
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missingValue <- sum(is.na(activity))
missingValue
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
averageSteps <- aggregate(steps ~ interval, data = activity, FUN = mean)
NA_fill <- numeric()
for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(averageSteps, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    NA_fill <- c(NA_fill, steps)
}
```

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityNew <- activity
activityNew$steps <- NA_fill
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
TotalStepsNew <- aggregate(steps ~ date, data = activityNew, sum, na.rm = TRUE)
hist(TotalStepsNew$steps, main = "Total steps", xlab = "Day", col = "grey")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(TotalStepsNew$steps)
```

```
## [1] 10766.19
```

```r
median(TotalStepsNew$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
```


- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
