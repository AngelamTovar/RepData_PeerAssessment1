---
title: "PA1_template"
author: "Angela Tovar"
date: "4/27/2021"
output: html_document
---

# Reproducible Research: Peer Assessment 1

```{r, echo=FALSE, results='hide', warning=FALSE, message=FALSE}
library(ggplot2)
library(scales)
library(Hmisc)
```

## Loading the data
##### 1. Load the data (i.e. read.csv())
```{r, results='markup', warning=TRUE, message=TRUE}
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
actData <- read.csv('activity.csv')
```
##### 2. Process and transform the data (if necessary) into a format suitable for your analysis
```{r}
#actData$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", actData$interval), format='%H:%M')
```

-----

## What is mean total number of steps taken per day?
```{r}
stepsByDay <- tapply(actData$steps, actData$date, sum, na.rm=TRUE)
```

##### 1. Make a histogram of the total number of steps taken each day
```{r}
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 1000', binwidth=1000)
```

##### 2. Calculate and report the mean and median total number of steps taken per day
```{r}
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```
* Mean: `r stepsByDayMean`
* Median:  `r stepsByDayMedian`

-----

## What is the average daily activity pattern?
```{r}
avgStepsPerTimeBlock <- aggregate(x=list(meanSteps=actData$steps), by=list(interval=actData$interval), FUN=mean, na.rm=TRUE)
```

##### 1. Make a time series plot
```{r}
ggplot(data=avgStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
mostSteps <- which.max(avgStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgStepsPerTimeBlock[mostSteps,'interval'])
```


----

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset 
```{r}
numMissingValues <- length(which(is.na(actData$steps)))
```

* Number of missing values: `r numMissingValues`

##### 2. Devise a strategy for filling in all of the missing values in the dataset.
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
actDataImputed <- actData
actDataImputed$steps <- impute(actData$steps, fun=mean)
```


##### 4. Make a histogram of the total number of steps taken each day 
```{r}
stepsByDayImputed <- tapply(actDataImputed$steps, actDataImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 1000', binwidth=1000)
```

##### ... and Calculate and report the mean and median total number of steps taken per day. 
```{r}
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
```
* Mean (Imputed): `r stepsByDayMeanImputed`
* Median (Imputed):  `r stepsByDayMedianImputed`


----

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
actDataImputed$dateType <-  ifelse(as.POSIXlt(actDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

##### 2. Make a panel plot containing a time series plot

```{r}
averagedActDataImputed <- aggregate(steps ~ interval + dateType, data=actDataImputed, mean)
ggplot(averagedActDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("average number of steps")
```

