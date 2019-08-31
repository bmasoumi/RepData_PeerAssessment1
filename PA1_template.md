---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
    variant: markdown_github
---


## Loading and preprocessing the data

Here is the link to the dataset <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

```r
activity <- read.csv("activity.csv")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

From the summary: dataset has 17,568 rows and 3 columns 
And 61 different dates(levels), but for some of those days the number of steps is NA

Let see for how many days the value for steps is NA

```r
noDays<- nrow(aggregate(steps ~ date, activity, sum, na.rm = TRUE))
paste("Number of days that we have NA value for steps is:", 61-noDays, "days")
```

```
## [1] "Number of days that we have NA value for steps is: 8 days"
```

Converting $date from Factor to Date

```r
activity$date = as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Histogram of the total number of steps taken each day

```r
stepsPerDay <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
plot(stepsPerDay$date, stepsPerDay$steps, type = "h", main = "Number of steps per day", xlab = "Date", ylab = "Steps", col = "blue", lwd ="5")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## What is mean total number of steps taken per day?

```r
meanOfStepsPerDay <- mean(stepsPerDay$steps, na.rm = TRUE)
paste("Mean of the total number of steps per day is ", meanOfStepsPerDay)
```

```
## [1] "Mean of the total number of steps per day is  10766.1886792453"
```

```r
medianOfStepsPerDay <- median(stepsPerDay$steps, na.rm = TRUE)
paste("Median of the total number of steps per day is ", medianOfStepsPerDay)
```

```
## [1] "Median of the total number of steps per day is  10765"
```

## What is the average daily activity pattern?

```r
avgStepsPerInterval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)
plot(avgStepsPerInterval$interval, avgStepsPerInterval$steps, type = "l", col = "dark green", lwd = "2", main = "Average daily activity pattern", xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Looking at the graph you can tell that the maximum number of steps across all days/intervals is a bit more than 200 steps and it happens somewhere between the interval 750 and 1000. Let's find out which 5-minute interval contains the max number of steps.

```r
maxStepsAcrossInetervals <- avgStepsPerInterval[which.max(avgStepsPerInterval$steps),]
paste("The max number of steps across all days/intervals is", as.integer(maxStepsAcrossInetervals$steps), "steps, at the interval ", maxStepsAcrossInetervals$interval )
```

```
## [1] "The max number of steps across all days/intervals is 206 steps, at the interval  835"
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
totalNA = nrow(activity[is.na(activity$steps),])
paste("Total number of rows with NA is ", totalNA)
```

```
## [1] "Total number of rows with NA is  2304"
```

Alternatively, we can use mice libaray function md.pattern for displaying missing data pattern

```r
library(mice)
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'mice'
```

```
## The following objects are masked from 'package:base':
## 
##     cbind, rbind
```

```r
md.pattern(activity)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```
##       date interval steps     
## 15264    1        1     1    0
## 2304     1        1     0    1
##          0        0  2304 2304
```

Replacing NA values with mean of steps per interval (not the best startegy for imputing NA values)

```r
# reading in the data again as a good practice
activity <- read.csv("activity.csv")
# create a temporary dataset that contains both NAs and values for imputing NAs
tempActivity <- merge.data.frame(x = activity, y = avgStepsPerInterval, by = "interval")
tempActivity$steps.x[is.na(tempActivity$steps.x)] = tempActivity$steps.y[is.na(tempActivity$steps.x)]
# now create a new dataset that contains 3 columns of steps, date, interval and NAs are being replaced
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
activityNoNA <- select(tempActivity, steps.x, date, interval)
colnames(activityNoNA)[colnames(activityNoNA) == "steps.x"] <- "steps"
head(activityNoNA)
```

```
##      steps       date interval
## 1 1.716981 2012-10-01        0
## 2 0.000000 2012-11-23        0
## 3 0.000000 2012-10-28        0
## 4 0.000000 2012-11-06        0
## 5 0.000000 2012-11-24        0
## 6 0.000000 2012-11-15        0
```

Check if all NA values have been replaced

```r
totalNA = nrow(activityNoNA[is.na(activityNoNA$steps),])
paste("Total number of rows with NA value is ", totalNA)
```

```
## [1] "Total number of rows with NA value is  0"
```


```r
# and check it visually
md.pattern(activityNoNA)
```

```
##  /\     /\
## {  `---'  }
## {  O   O  }
## ==>  V <==  No need for mice. This data set is completely observed.
##  \  \|/  /
##   `-----'
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```
##       steps date interval  
## 17568     1    1        1 0
##           0    0        0 0
```

## Histogram of the total number of steps taken each day after missing values are imputed

```r
stepsPerDayNoNA <- aggregate(steps ~ date, activityNoNA, sum, na.rm = TRUE)
plot(as.numeric(stepsPerDayNoNA$date), as.numeric(stepsPerDayNoNA$steps), type = "h", main = "Number of steps per day after imputing NA values", xlab = "Date", ylab = "Steps", col = "blue", lwd =5)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

## Mean and median number of steps taken each day AFTER imputing NA values
Do these values differ from the estimates from the first part (the original dataset with NA values)?

```r
meanOfStepsPerDayNoNA = mean(stepsPerDayNoNA$steps, na.rm = TRUE)
paste("Mean of the total number of steps per day AFTER imputation is", meanOfStepsPerDayNoNA)
```

```
## [1] "Mean of the total number of steps per day AFTER imputation is 10766.1886792453"
```

```r
medianOfStepsPerDayNoNA = mean(stepsPerDayNoNA$steps, na.rm = TRUE)
paste("Median of the total number of steps per day AFTER imputation is", medianOfStepsPerDayNoNA)
```

```
## [1] "Median of the total number of steps per day AFTER imputation is 10766.1886792453"
```

```r
meanDiff = meanOfStepsPerDayNoNA - meanOfStepsPerDay
paste("The difference between the mean of the total number of steps per day AFTER and BEFORE imputation is", meanDiff)
```

```
## [1] "The difference between the mean of the total number of steps per day AFTER and BEFORE imputation is 0"
```

Why are the two mean values the same?
Because for the original dataset we calculated mean based on 53 rows (number of rows without NA) and for calculating the mean of the steps after imputation while the number of steps were increased because of imputation, however to calculate the mean we used 61 rows (instead of 53). Mathematical consequence of this would be getting the same output when calculating mean.
In mathematical notation:

- mean before imputation = total number of steps / total number of rows without NA value

- mean before imputation = 570608/53 = 10766.19

- mean after imputation = total number of steps / total number of rows without NA value

- mean after imputation = 656737/61 = 10766.18

### As expected, the mean and median haven't changed, becasue the NAs were replaced with the mean

What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
paste("Total daily number of steps before imputation is:", sum(stepsPerDay$steps))
```

```
## [1] "Total daily number of steps before imputation is: 570608"
```


```r
paste("Total daily number of steps after imputation is:", as.integer(sum(stepsPerDayNoNA$steps)))
```

```
## [1] "Total daily number of steps after imputation is: 656737"
```

As expected total daily number of steps has increased after imputing NA values 

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable with 2 levels: weekday and weekend

```r
library(chron)
for (i in 1:nrow(activityNoNA)) {
        if (is.weekend(activityNoNA$date[i])) {
               activityNoNA$dayOfWeek[i] = "weekend"
        }
        else 
                activityNoNA$dayOfWeek[i] = "weekday"
}
# create two seperate datasets: weekdays and weekends
weekdays <- subset.data.frame(activityNoNA, activityNoNA$dayOfWeek == "weekday")
weekends <- subset.data.frame(activityNoNA, activityNoNA$dayOfWeek == "weekend")

stepsPerIntervalWday <- aggregate(steps ~ interval, weekdays, sum, na.rm = TRUE)
stepsPerIntervalWend <- aggregate(steps ~ interval, weekends, sum, na.rm = TRUE)

par(mfrow=c(1,2))

plot(stepsPerIntervalWday$interval, stepsPerIntervalWday$steps, type = "l", col = "orange", lwd = "2", main = "Pattern for weekdays", xlab = "Interval", ylab = "Steps")
plot(stepsPerIntervalWend$interval, stepsPerIntervalWend$steps, type = "l", col = "red", lwd = "2", main = "Pattern for weekends", xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
