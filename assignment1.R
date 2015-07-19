##Loading the activity file as well as all necessary packages

library(plyr)
library(ggplot2)
library(timeDate)
activity <- read.csv("activity.csv")

## WHAT IS THE MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY

total_steps_per_day <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
total_steps_per_day

#Draw the histogram
hist(total_steps_per_day)

#Average total number of steps taken per day
mean1 <- mean(total_steps_per_day, na.rm=TRUE)
mean1

#Median total number of steps taken per day
median1 <- median(total_steps_per_day,na.rm=TRUE)
median1

## WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN

# Create a new activity datafram (act2) and modify the class of the interval
act2 <- activity
act2$interval <- factor(act2$interval)
# Calculate mean steps per interval
mean_steps_per_interval <- tapply(act2$steps, act2$interval, mean, na.rm=TRUE)

# Create a new variable "intervals" which is converted to POSIXct
intervals <- strptime(sprintf("%04d", as.numeric(names(mean_steps_per_interval))), format="%H%M")

#Create a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(intervals,mean_steps_per_interval,type="l")

# I create a new data fram with mean_steps_per_interval and intervals, so that I can extract the requested value easily
d <- data.frame(mean_steps_per_interval,intervals)
#Interval during which the largest number of steps was monitored
maxSteps <- d[which(d$mean_steps_per_interval == max(d$mean_steps_per_interval)), ]
maxSteps

## IMPUTING MISSING VALUES

#Count the number of rows that have missing values
sum(!complete.cases(activity))


#Imputing mean values:
#Define a function impute.mean
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

#Create activity 2,where missing values are replaced with the average for that time interval
activity2 <- ddply(activity, "interval", transform, steps = impute.mean(steps))

# Summarize new data with the NA values replaced
daily_activity2 <- ddply(activity2, "date", summarize, daily.steps = sum(steps))

#Plot Historgam, this time with ggplot2
qplot(daily.steps, data=daily_activity2, geom="histogram", binwidth=1000)

#Calculating new mean
new_mean <- mean(daily_activity2$daily.steps, na.rm=TRUE)
new_mean

#Calculating new median
new_median <- median(daily_activity2$daily.steps, na.rm=TRUE)
new_median

#Mean difference
mean_diff <- new_mean - mean1
mean_diff

#Median difference
median_diff <- new_median - median1
median_diff


# Is this a weekday or not?
activity2$weekday <- factor(isWeekday(activity2$date,wday=1:5),
                            labels=c('Weekend', 'Weekday'))
average.weekday <- ddply(activity2, c("interval","weekday"), summarize, steps = mean(steps))

# Average steps: Weekday vs Weekend
aggregate(steps~weekday, data=average.weekday, FUN='sum')

#Plot for weekdays and weekends
ggplot(average.weekday, aes(interval,steps)) + geom_line(color="blue") + xlab("Interval") +ylab("Average Steps") + facet_wrap(~weekday, nrow=2)

