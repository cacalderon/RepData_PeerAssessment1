Project \# 1
------------

Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
"quantified self" movement - a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

The variables included in this dataset are:

        steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

        date: The date on which the measurement was taken in YYYY-MM-DD format

        interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset. Below a summary of
the original data.

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

    # Load libraries 

    library(RCurl)

    ## Loading required package: bitops

    library(lubridate)

    ## Warning: package 'lubridate' was built under R version 3.2.5

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    library(plyr)

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     here

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.2.4

    library(lattice)

    # Section #1: Loading and Processing the Data

    #Get Data from Website and unzip file in your current working directory

    data <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
    download.file(data, 'repdata-data-activity.zip')
    unzip('repdata-data-activity.zip')
    activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

    #Change date field and clean the data
    activity$day <- weekdays(as.Date(activity$date))
    activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
    clean <- activity[!is.na(activity$steps),]

    # summary of total steps per date
    sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum)
    colnames(sumTable)<- c("Date", "Steps")

    ## Section # 2. What is mean total number of steps taken per day?

    # Creating the historgram of total steps per day
    hist(sumTable$Steps, breaks=5, xlab="Steps", col = "red", main = "Total Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # Calculate the mean and the median of the steps per day
    as.integer(mean(sumTable$Steps))

    ## [1] 10766

    as.integer(median(sumTable$Steps))

    ## [1] 10765

    ## Section # 3. What is the average daily activity pattern?

    # create average number of steps per interval
    intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))

    # create max steps by interval
    maxSteps <- max(intervalTable$Avg)

    # Create line plot of average number of steps per interval
    p <- ggplot(intervalTable, aes(x=interval, y=Avg),xlab = "Interval", ylab="Average Number of Steps")
    p + geom_line(colour = 'red')+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-2.png)

    # Which interval contains the maximum average number of steps
    intervalTable[intervalTable$Avg==maxSteps,1]

    ## [1] 835

    # Section # 4. Input Missing Values

    # Process and calculate the number of NAs in original data set
    nrow(activity[is.na(activity$steps),])

    ## [1] 2304

    # Create table with the average number of steps per weekday and interval
    avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))

    # Create a new dataset with all NAs for substitution
    NAdata<- activity[is.na(activity$steps),]

    # Merge NA data with average weekday interval for substitution
    newdataset<-merge(NAdata, avgTable, by=c("interval", "day"))

    # Reorder the new substituded data in the same format as clean data set
    finalnewdata<- newdataset[,c(6,4,1,2,5)]
    colnames(finalnewdata)<- c("steps", "date", "interval", "day", "DateTime")

    # Merge the NA averages and non NA data together
    mergedData <- rbind(clean, finalnewdata)

    # Create sum of steps per date to compare with step 1
    sumTable2 <- aggregate(mergedData$steps ~ mergedData$date, FUN=sum)
    colnames(sumTable2)<- c("Date", "Steps")

    ## Creating the histogram of total steps per day, categorized by data set to show impact
    hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NA and Non NA", col="Black")
    hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NA and Non NA", col="Red", add=T)
    legend("topright", c("NA Data", "Non-NA Data"), fill=c("black", "red") )

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-3.png)

    # Calculate the mean and the median with new data set
    as.integer(mean(sumTable2$Steps))

    ## [1] 10821

    as.integer(median(sumTable2$Steps))

    ## [1] 11015

    # Section # 5. Are there differences in activity patterns between weekdays and weekends?

    ## Create new category based on the days of the week
    mergedData$DayCategory <- ifelse(mergedData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

    ## Summarize data by interval and type of day
    intervalTable2 <- ddply(mergedData, .(interval, DayCategory), summarize, Avg = mean(steps))

    ##Plot data in a panel plot
    xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
           main="Average Steps per Interval Based on Type of Day", 
           ylab="Average Number of Steps", xlab="Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-4.png)
