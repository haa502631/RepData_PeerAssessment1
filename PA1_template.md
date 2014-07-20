# Reproducible Research: Peer Assessment 1
This is the markdown file for Peer Assessment1 of Reproducible Research in Data Science Specialization course.

## Loading and preprocessing the data
At first, we load the data.

```r
# load data
rawData <- read.csv("activity.csv", stringsAsFactors=FALSE)
# check the structure of raw data
str(rawData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Next, we clean the data by removing the NA.

```r
# clean data
cleanData <- rawData[complete.cases(rawData), ]
# check 'cleanData'
head(cleanData)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## What is mean total number of steps taken per day?
At first, we aggregate the total number of steps taken per day.

```r
# calculate the total number of steps per day
stepsPerDay <- setNames(aggregate(cleanData$steps, 
                                  by=list(date=cleanData$date),
                                  FUN=sum),
                        c("date", "totalsteps"))
# check 'stepsPerDay'
head(stepsPerDay)
```

```
##         date totalsteps
## 1 2012-10-02        126
## 2 2012-10-03      11352
## 3 2012-10-04      12116
## 4 2012-10-05      13294
## 5 2012-10-06      15420
## 6 2012-10-07      11015
```

Next, we make a histogram of the total number of steps taken per day.

```r
library(ggplot2)

# plot a histogram
ggplot(stepsPerDay, aes(x=stepsPerDay$totalsteps)) + 
    geom_histogram(binwidth=1000, colour="white", fill="blue", ) + 
    xlab("total number of steps per day") + 
    ylab("frequency") + 
    ggtitle("histogram of total number of steps per day")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

Let us calculate the mean and median total number of steps taken per day.

```r
mean(stepsPerDay$totalsteps)
```

```
## [1] 10766
```

```r
median(stepsPerDay$totalsteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Let us build a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.


```r
# calculate the average number of steps per interval
meanStepsPerInterval <- setNames(aggregate(cleanData$steps, 
                                           by=list(cleanData$interval), 
                                           FUN=mean), 
                                 c("interval", "meansteps"))

# Plot the mean number of steps taken per interval
ggplot(meanStepsPerInterval, aes(interval, meansteps)) + 
    geom_line() + 
    xlab("time Interval") + 
    ylab("average number of steps taken") + 
    ggtitle("average number of steps taken per interval")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

Let us check the 5-minute interval that, on average across all the days in the dataset, contains the maximum number of steps.

```r
meanStepsPerInterval[which.max(meanStepsPerInterval$meansteps), ]$interval
```

```
## [1] 835
```

## Imputing missing values
Let us calculate the total number of missing values in the dataset.

```r
sum(!complete.cases(rawData))
```

```
## [1] 2304
```

Let us replace each NA with the mean of the available values for that interval in the other days.

```r
mergeData <- merge(rawData, meanStepsPerInterval, by.x="interval", by.y="interval")
imputedData0 <- data.frame(steps=ifelse(is.na(mergeData$steps), 
                                        mergeData$meansteps, 
                                        mergeData$steps), 
                           date=mergeData$date, 
                           interval=mergeData$interval)
sortList <- order(imputedData0$date)
imputedData0 <- imputedData0[sortList, ]
imputedData <- data.frame(steps=imputedData0$steps, 
                          date=imputedData0$date, 
                          interval=imputedData0$interval)
```

Let us make a histogram of the total number of steps taken per day for imputed data. 

```r
# calculate the total number of steps per day for imputed data
imputedStepsPerDay <- setNames(aggregate(imputedData$steps, 
                                         by=list(date=imputedData$date), 
                                         FUN=sum), 
                               c("date", "totalsteps"))

# plot a histogram
ggplot(imputedStepsPerDay, aes(x=imputedStepsPerDay$totalsteps)) + 
    geom_histogram(binwidth=1000, colour="white", fill="blue", ) + 
    xlab("total number of steps per day") + 
    ylab("frequency") + 
    ggtitle("histogram of total number of steps per day for imputed data")
```

![plot of chunk unnamed-chunk-10](./PA1_template_files/figure-html/unnamed-chunk-10.png) 

Let us calculate the mean and median total number of steps taken per day for imputed data.

```r
mean(imputedStepsPerDay$totalsteps)
```

```
## [1] 10766
```

```r
median(imputedStepsPerDay$totalsteps)
```

```
## [1] 10766
```

Therefore we find that the impact of imputing missing data on the estimates of the total daily number of steps is very small. 

## Are there differences in activity patterns between weekdays and weekends?
Let us create a new factor variable in the dataset with two levels "weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
imputedData$daytype <- as.factor(ifelse(weekdays(as.Date(imputedData$date)) %in% c('Saturday','Sunday'), 
                                        "weekend", 
                                        "weekday"))
```

Next, we calculate the average number of steps per interval for "weekday" and "weekend".

```r
meanStepsPerIntervalDaytype <- setNames(aggregate(imputedData$steps, 
                                                  by=list(imputedData$interval, imputedData$daytype), 
                                                  FUN=mean), 
                                        c("interval", "daytype", "meansteps"))
```

Let us make a panel plot.

```r
library(lattice)

xyplot(meansteps ~ interval | daytype, 
       data=meanStepsPerIntervalDaytype, 
       type=c("l", "g"), 
       layout=c(1, 2), 
       xlab="interval", 
       ylab="number of steps", 
       main="activity patterns between weekdays and weekends")
```

![plot of chunk unnamed-chunk-14](./PA1_template_files/figure-html/unnamed-chunk-14.png) 
