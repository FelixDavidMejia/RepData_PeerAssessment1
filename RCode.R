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
    
        # Calculating intervals with no data (NA)
        na_intervals <- sum(is.na(activity_data$steps))
        
        # Devise a strategy for filling in all of the missing values in the
        # dataset. The strategy does not need to be sophisticated.
        # For example, you could use the mean/median for that day, or the mean
        # for that 5-minute interval, etc.
        
        # I will replace missing values in original data (steps which are NA)
        # with the average step value computed with the existing data for that
        # interval, already stored in steps_per_interval.df data frame.
        
        # Create a new dataset that is equal to the original dataset but with
        # the missing data filled in.
        
        # Making a copy of the original file
        # Copy of df to be populated with imputed values
        activity_data.imp <- left_join(activity_data, steps_per_interval.df) 
        #activity_data.imp$steps2 <- activity_data.imp$steps
        activity_data.imp$steps[is.na(activity_data.imp$steps)] <- activity_data.imp$steps_per_interval
        activity_data.imp <- select(activity_data.imp, 1:3)
        
        # Make a histogram of the total number of steps taken each day and
        # Calculate and report the mean and median total number of steps
        # taken per day. Do these values differ from the estimates from the
        # first part of the assignment?

        #Grouping intervals by date
        dates.imp.g <- group_by(activity_data.imp, date)
        # Summarizing steps by day
        steps_per_day.imp.df <- summarize(dates.imp.g, steps_per_day = sum(steps, na.rm = TRUE))
        
        # Creating Graph
        ## Setup ggplot with data frame
        g <- ggplot(steps_per_day.imp.df, aes(x = date, y = steps_per_day))
        g + geom_bar(alpha = 7/10, stat = "identity") +
            labs(x = "Day") +
            labs(y = "Steps per Day") +
            labs(title = expression(atop(bold("Total steps taken per Day"), +
            atop(italic("For Subject XXX (imputing NA values with interval average)"), ""))))
        
        # Calculate and report the mean and median total number of steps taken per day
        # Mean steps per day
        avg_steps_day.imp <- mean(steps_per_day.imp.df$steps_per_day)
        # Median steps per day
        median_steps_day.imp <- median(steps_per_day.imp.df$steps_per_day)
        
        # What is the impact of imputing missing data on the estimates of
        # the total daily number of steps?
        
        # New table with day totals for steps, non imputed and imputed
        comparedata <- steps_per_day.df
        comparedata$imputed <- FALSE
        comparedata <- bind_rows(comparedata, steps_per_day.imp.df)
        comparedata$imputed[is.na(comparedata$imputed)] <- TRUE
        
        # Drawing Histogram
        g <- ggplot(comparedata, aes(x = date, y = steps_per_day))
        g + geom_bar(alpha = 7/10, stat = "identity") +
            facet_grid(. ~ imputed) +
            labs(x = "Date") +
            labs(y = expression("Total Steps per Day")) +
            labs(title = expression(atop(bold("Total Steps per Day"), +
            atop(italic("Non-Imputed vs Imputed missing values"), ""))))
        
## Are there differences in activity patterns between weekdays and weekends?

        # For this part the weekdays() function may be of some help here.
        # Use the dataset with the filled-in missing values for this part.
        
        # Create a new factor variable in the dataset with two
        # levels – “weekday” and “weekend” indicating whether a given date is
        # a weekday or weekend day.
        
        # Weekdays will be flagged FALSE (0) and weekends will be TRUE (1)
        activity_data.imp$weekpart <- !is.element(weekdays(activity_data.imp$date), c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        activity_data.imp$weekpart <- factor(activity_data.imp$weekpart, labels = c("weekdays", "weekend"))
        
        #Grouping by interval
        intervals.w.g <- group_by(activity_data.imp, weekpart, interval)
        # Summarizing steps by day
        steps_per_interval.w.df <- summarize(intervals.w.g, steps_per_interval = mean(steps, na.rm = TRUE))
        
        # Creating Graph
        ## Setup ggplot with data frame
        g <- ggplot(steps_per_interval.w.df, aes(x = interval, y = steps_per_interval))
        g + geom_line(alpha = 7/10, color = "black", stat = "identity") +
            facet_grid(weekpart ~ .) +
            labs(x = "Interval of Day (5 mins)") +
            labs(y = "Average of steps taken per 5m interval") +
            labs(title = expression(atop(bold("Total steps taken per 5 min Interval of Day"), atop(italic("For Subject XXX by part of week"), ""))))
        

        
        
        
