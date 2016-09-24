

```r
---
title: "PA1_template.Rmd"
author: "Emilio Blanco"
date: "September 20, 2016"
output: html_document
---
```

```
## Error: <text>:8:0: unexpected end of input
## 6: ---
## 7: 
##   ^
```


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(dplyr)
library(ggplot2)
##Create directory if it doesnt exists
if(!file.exists("./datafolder")){
    dir.create("./datafolder")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

## add if working on Mac OS (,method="curl")
download.file(fileUrl,destfile="./datafolder/AM.zip") 

##unzip the file downloaded
unzip(zipfile="./datafolder/AM.zip",exdir="./datafolder")
DF <- read.csv("./datafolder/activity.csv", na.strings = "NA", stringsAsFactors=FALSE)
DF$date <- as.Date(DF$date, format = "%Y-%m-%d")
str(DF)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(DF)
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

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
DFperDay = na.omit(summarize(group_by(DF, date), sum(steps)))
colnames(DFperDay) <- c("date","steps")
```

2. Make a histogram of the total number of steps taken each day

```r
hist(DFperDay$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(DFperDay$steps)
```

```
## [1] 10766.19
```

```r
median(DFperDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
actperDay = summarize(group_by(DF, interval), mean(steps, na.rm = TRUE))
colnames(actperDay) <- c("interval","steps")
with( actperDay, plot(interval,steps, type="l", col="red",main="Average Daily Activity Pattern", 
                    xlab="5 min - interval",
                    ylab="Average Steps"))
```

![plot of chunk dailyAct](figure/dailyAct-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
with(actperDay,interval[which.max(steps)])
```

```
## [1] 835
```
## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    No missing values on dates and intervals, 2304 rows with NA values
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
    

```r
nomiss <- DF
nomiss$steps[is.na(nomiss$steps)] <- mean(nomiss$steps, na.rm = TRUE)

nomissagg <- summarize(group_by(nomiss, date), sum(steps))
colnames(nomissagg) <- c("date","steps")
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(nomissagg$steps, col="red", main="Histogram of Total Steps per Day",
     xlab="Total Steps per Day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Mean and Median


```r
mean(nomissagg$steps)
```

```
## [1] 10766.19
```

```r
median(nomissagg$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
wk <- nomiss
wk$weekday <- weekdays(wk$date)
wk$weekday <- as.factor(ifelse(wk$weekday %in% c("Saturday","Sunday"),"weekend","weekday"))

wkagg <-summarize(group_by(wk, weekday, interval), mean(steps))
colnames(wkagg) <- c("weekday","interval","steps")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
with (wkagg, xyplot(steps ~ interval|weekday, type="l", 
             ylab="Number of steps",layout=c(1,2)))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
```

