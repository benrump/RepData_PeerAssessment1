# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1) Import raw datafile.


```r
setwd("~/Desktop/R/Coursera/ReproducibleResearch/RepData_PeerAssessment1")
ds = read.csv("activity.csv")
head(ds)
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

2)  (No general transformation necessary.)

## What is mean total number of steps taken per day?

1)  Make a histogram of the total number of steps taken each day


```r
ds.agg = aggregate(steps ~ date, data=ds, FUN=sum)
hist(ds.agg$steps, breaks=10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

2)  Calculate and report the mean and median total number of steps taken per day


```r
mean(ds.agg$steps)
```

```
## [1] 10766
```

```r
median(ds.agg$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1)  Make a time series plot of the average number of steps taken by interval.


```r
ds.int = aggregate(steps ~ interval, data=ds, FUN=mean)
plot(ds.int$steps, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

2)  Interval 835 is the 5-minute interval contains the maximum number of average steps.


```r
int.max = max(ds.int$steps)
ds.int[which(ds.int$steps==int.max),]
```

```
##     interval steps
## 104      835 206.2
```

## Imputing missing values

1) Calculate the number of missing values


```r
sum(is.na(ds$steps))
```

```
## [1] 2304
```

2-3)  Replace missing values for a date/interval with the mean for that interval.


```r
ds.imp = merge(ds,ds.int,by="interval",suffixes=c("",".int"),all.x=T)
ii = is.na(ds.imp$steps)
ds.imp$steps[ii] = ds.imp$steps.int[ii]
ds.imp = ds.imp[order(ds.imp$date,ds.imp$interval),]
head(ds.imp)
```

```
##     interval   steps       date steps.int
## 1          0 1.71698 2012-10-01   1.71698
## 63         5 0.33962 2012-10-01   0.33962
## 128       10 0.13208 2012-10-01   0.13208
## 205       15 0.15094 2012-10-01   0.15094
## 264       20 0.07547 2012-10-01   0.07547
## 327       25 2.09434 2012-10-01   2.09434
```

4)  Make a histogram of the total number of steps taken each day and calculate the mean/median per day.  Note that the mean is unchanged after the imputation and the median is similar.


```r
ds.imp.agg = aggregate(steps ~ date, data=ds.imp, FUN=sum)
hist(ds.imp.agg$steps)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(ds.imp.agg$steps)
```

```
## [1] 10766
```

```r
median(ds.imp.agg$steps)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

1)  Create a factor variable indicating whether the day is a weekday or weekend.


```r
ds.imp$weekday = weekdays(as.Date(ds.imp$date))
ii = ds.imp$weekday %in% c("Saturday","Sunday")
ds.imp$day.type[ii] = "weekend"
ds.imp$day.type[!ii] = "weekday"
ds.imp$day.type = factor(ds.imp$day.type)
str(ds.imp)
```

```
## 'data.frame':	17568 obs. of  6 variables:
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps    : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date     : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ steps.int: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ weekday  : chr  "Monday" "Monday" "Monday" "Monday" ...
##  $ day.type : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```


2) Plot time series of the average number of steps taken each interval by weekday/weekend.


```r
ds.weekday = aggregate(steps ~ interval + day.type, data=ds.imp, FUN=mean)
library(lattice)
xyplot(steps ~ interval | day.type, data=ds.weekday, type="l", layout=c(1,2))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

