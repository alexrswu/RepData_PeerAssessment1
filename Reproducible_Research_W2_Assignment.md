# Reproducible Research Week 2 Assignment
Alexander Wu  
7/4/2017  
####Reproducible Research Week 2 Project
#####Load the required packages

```r
library(knitr)
library(ggplot2)
library(plyr)
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```
#####Loading and preprocessing the data
#####Show any code that is needed to load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())

```r
#make sure the data file is under the correct working directory 
dat <- read.csv("activity.csv")
```
#####Process/transform the data (if necessary) into a format suitable for your analysis

```r
#reformat the variables
dat$date <- as.Date(dat$date, format = "%Y-%m-%d")
```
#####What is mean total number of steps taken per day?
#####For this part of the assignment, you can ignore the missing values in the dataset.
#####Calculate the total number of steps taken per day

```r
sumstep <- tapply(dat$steps, dat$date, sum, na.rm = T)
```
#####If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
qplot(sumstep, binwidth = 1000, xlab = "Total Number of Steps Taken Each Day")
```

![](Reproducible_Research_W2_Assignment_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#####Calculate and report the mean and median of the total number of steps taken per day

```r
mean(sumstep, na.rm = T)
```

```
## [1] 9354.23
```

```r
median(sumstep, na.rm = T)
```

```
## [1] 10395
```
#####What is the average daily activity pattern?
#####Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgdaily <- aggregate(steps~interval, dat, mean, na.rm=TRUE)
plot(avgdaily, type = "l", xlab = "5-minute interval", ylab = "avergae number of steps taken")
```

![](Reproducible_Research_W2_Assignment_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxstep <- which.max(avgdaily$steps)
avgdaily[maxstep, 'interval']
```

```
## [1] 835
```
#####Imputing missing values
#####Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.
#####Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
misstotal <- length(which(is.na(dat)))
```
#####Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#####Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
datimputed <- dat
for (i in 1:nrow(datimputed)) {
    if (is.na(datimputed$steps[i])) {
        interval_value <- datimputed$interval[i]
        steps_value <- avgdaily[
            avgdaily$interval == interval_value,]
        datimputed$steps[i] <- steps_value$steps
    }
}
```
#####Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. #Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
dtimputedavgdaily <- aggregate(steps~date, datimputed, sum, na.rm=TRUE)
hist(dtimputedavgdaily$steps, main = "Total Number of Steps Per Day (Post-Imputation)",xlab = "Total Number of Steps Taken Each Day")
```

![](Reproducible_Research_W2_Assignment_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(dtimputedavgdaily$steps)
```

```
## [1] 10766.19
```

```r
median(dtimputedavgdaily$steps)
```

```
## [1] 10766.19
```
#####Are there differences in activity patterns between weekdays and weekends?
#####For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

```r
datimputed['day.of.week'] <- weekdays(datimputed$date)
datimputed$daytype[datimputed$day.of.week %in% c("Saturday","Sunday")] <- "weekend"
datimputed$daytype[datimputed$day.of.week != c("Saturday","Sunday")] <- "weekday"
```
#####Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
datimputed$daytype <- as.factor(datimputed$daytype)
```
#####Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
avgweeklyimputed <- aggregate(steps ~ interval + daytype, datimputed, mean)
ggplot(avgweeklyimputed, aes(interval, steps, group = 1)) + 
    geom_line() + 
    facet_wrap(~daytype, ncol = 1) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](Reproducible_Research_W2_Assignment_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
