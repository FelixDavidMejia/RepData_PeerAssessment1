## Loading and preprocessing the data  

# Setting the right folder
setwd("~/GitHub/RepData_PeerAssessment1")
#Reading data
activity_data <- read.csv(unz("activity.zip", "activity.csv"))
#Converting Date from strings to date format
activity_data$date <- as.Date(activity_data$date,"%Y-%m-%d")

## What is mean total number of steps taken per day?
    library(dplyr, warn.conflicts = FALSE, quietly=TRUE)
    library(ggplot2, warn.conflicts = FALSE, quietly=TRUE)

    #Grouping intervals by date
        dates.g <- group_by(activity_data, date)
    # Summarizing steps by day
        steps_per_day.df <- summarize(dates.g, steps_per_day = sum(steps, na.rm = TRUE))
    
    # Creating Graph
        ## Setup ggplot with data frame
        g <- ggplot(steps_per_day.df, aes(x = date, y = steps_per_day))
        g + geom_bar(alpha = 7/10, stat = "identity") +
            labs(x = "Day") +
            labs(y = "Steps per Day") +
            labs(title = expression(atop(bold("Total steps taken per Day"), atop(italic("For Subject XXX"), ""))))
    
    # Calculate and report the mean and median total number of steps taken per day
    # Mean steps per day
        avg_steps_day <- mean(steps_per_day.df$steps_per_day)
    # Median steps per day
        median_steps_per_day <- median(steps_per_day.df$steps_per_day)

## What is the average daily activity pattern?
        
        #Grouping by interval
        intervals.g <- group_by(activity_data, interval)
        # Summarizing steps by day
        steps_per_interval.df <- summarize(intervals.g, steps_per_interval = mean(steps, na.rm = TRUE))
        
        # Creating Graph
        ## Setup ggplot with data frame
        g <- ggplot(steps_per_interval.df, aes(x = interval, y = steps_per_interval))
        g + geom_line(alpha = 7/10, color = "black", stat = "identity") +
            labs(x = "Interval of Day (5 mins)") +
            labs(y = "Average of steps taken per 5m interval") +
            labs(title = expression(atop(bold("Total steps taken per 5 min Interval of Day"), atop(italic("For Subject XXX"), ""))))
        
        # Interval with most steps
        most_steps_interval_idx <- which.max(steps_per_interval.df$steps_per_interval)
        most_steps_interval_lbl <- steps_per_interval.df$interval[most_steps_interval_idx]


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
