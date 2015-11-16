#Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

#Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

#Loading and preprocessing the data

Show any code that is needed to

Adding libraries


```r
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(markdown)
```

1. Load the data (i.e. read.csv())




```r
setwd("C:/Users/Gayathri Kulathumani/Desktop/1Gayathri/Data Science/Course5")
if(!file.exists("assign1")) dir.create("assign1")
rm(list=ls())
activity <- read.csv("./assign1/activity.csv",colClasses = c("numeric", "character","integer"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
dim(activity)
```

```
## [1] 17568     3
```

```r
head(activity)
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

```r
tail(activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```




```r
totalStepsPD <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
activity$date <- ymd(activity$date)
```

#What is mean total number of steps taken per day?


```r
mean(totalStepsPD)
```

```
## [1] 9354.23
```

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
totalsteps <- sum(activity$steps, na.rm = TRUE)
totalsteps
```

```
## [1] 570608
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
totalstep <- aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)

hist(totalstep$steps)
```

![plot of chunk histogramplot](figure/histogramplot-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(totalStepsPD)
```

```
## [1] 9354.23
```

```r
median(totalStepsPD)
```

```
## [1] 10395
```

#What is the average daily activity pattern?


1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
fiveMinAvg <- aggregate(steps~interval, data=activity, FUN=mean , na.rm=TRUE)
plot(x = fiveMinAvg$interval, y = fiveMinAvg$steps, type = "l") 
```

![plot of chunk 5minAvg](figure/5minAvg-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- max(fiveMinAvg$steps)
     for (i in 1:288){
         if (fiveMinAvg$steps[i] == maxSteps)
             fiveMinsMaxSteps <- fiveMinAvg$interval[i]
     }
print(fiveMinsMaxSteps)
```

```
## [1] 835
```

#Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missingNas <- sum(is.na(activity))
print(missingNas)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Will used fiveMinAvg to fill the missing NAs

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
editedActivity <- activity 
for (i in 1:nrow(editedActivity)) {
    if (is.na(editedActivity$steps[i])) {
        editedActivity$steps[i] <- fiveMinAvg[which(editedActivity$interval[i] == fiveMinAvg$interval), ]$steps
    }
}

head(editedActivity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```


```r
missingNas <- sum(is.na(editedActivity))
print(missingNas)
```

```
## [1] 0
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
editedVersion <- aggregate(steps~date, data=editedActivity, FUN=sum, na.rm=TRUE)

hist(editedVersion$steps)
```

![plot of chunk plotting](figure/plotting-1.png) 

```r
editedtotalStepsPD <- tapply(editedActivity$steps, editedActivity$date, FUN = sum, na.rm = TRUE)
editedActivity$date <- ymd(editedActivity$date)

head(editedtotalStepsPD)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00
```

```r
mean(editedtotalStepsPD)
```

```
## [1] 10766.19
```

```r
median(editedtotalStepsPD)
```

```
## [1] 10766.19
```
The mean and median changed because we filled NAs with the 5 min average. They are much closer now because the missing values are all the same and its helping in analyzing the data.

#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
givenDay <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
editedActivity$typeOfDay <- as.factor(sapply(editedActivity$date, givenDay))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = editedActivity, subset = editedActivity$typeOfDay == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

![plot of chunk daytype](figure/daytype-1.png) 


