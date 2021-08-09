## Loading and preprocessing the data

if(!file.exists("./dataset")){dir.create("./dataset")}
downloadUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(downloadUrl, destfile = "./dataset/download.zip") 

unzip(zipfile = "./dataset/download.zip", exdir = "./dataset")

movement_data <- read.table("./dataset/activity.csv", sep = ",", header = T, na.strings = "NA")
movement_data$date <- as.Date(movement_data$date, format = "%Y-%m-%d")

library(ggplot2)
library(dplyr)


## What is mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day
total_steps_per_date <- aggregate(steps ~ date,
                                  data = movement_data,
                                  na.rm = TRUE,
                                  FUN = sum)

# 2. Make a histogram of the total number of steps taken each day
ggplot(total_steps_per_date, aes(steps)) +
        geom_histogram(binwidth = 1000, fill = "skyblue", col = "black") +
        ggtitle("Total Number of Steps taken per Day") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        xlab("Total Steps per Day") +
        ylab("Frequency") + 
        scale_x_continuous(breaks = seq(0, 25000, 2500))

# 3. Calculate and report the mean and median of the total number of steps taken per day
movement_data %>% group_by(date) %>% summarise(Mean=mean(steps), Median=median(steps))

# Mean and median of total steps
mean(total_steps_per_date$steps, na.rm = TRUE)

median(total_steps_per_date$steps, na.rm = TRUE)


## What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
average_steps_per_interval <- aggregate(steps ~ interval,
                                        data = movement_data,
                                        na.rm = TRUE,
                                        FUN = mean)

ggplot(average_steps_per_interval, aes(x = interval, y = steps)) +
        geom_line(col = "blue") + 
        ggtitle("Average Steps per Interval") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        xlab("5 Minutes Interval") +
        ylab("Average Steps")


# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_steps_per_interval[which.max(average_steps_per_interval$steps),]


## Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
dim(movement_data[movement_data$steps == "NA",])

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

unique(movement_data[which(movement_data$date == "2012-10-01"),]$steps)

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
movement_data$average_interval <- ifelse(is.na(movement_data$steps),
                                         yes = average_steps_per_interval$steps[match(movement_data$interval, average_steps_per_interval$interval)],
                                         no = movement_data$steps)

average_steps_per_date_NAimputed <- aggregate(average_interval ~ date,
                                              data = movement_data,
                                              FUN = sum)

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

ggplot(average_steps_per_date_NAimputed, aes(average_interval)) +
        geom_histogram(binwidth = 1000, fill = "skyblue", col = "black") +
        ggtitle("Total number of steps taken per day (NAs imputed with average steps per day)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        xlab("Total Steps per Day") +
        ylab("Frequency") + 
        scale_x_continuous(breaks = seq(0, 25000, 2500))


## Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
movement_data$day_type <- ifelse(weekdays(movement_data$date) == "Saturday" | weekdays(movement_data$date) == "Sunday",
                                 yes = "Weekend",
                                 no = "Weekday")

average_steps_per_date_type <- aggregate(average_interval ~ interval + day_type,
                                         data = movement_data,
                                         FUN = mean)

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
ggplot(average_steps_per_date_type, aes(interval, average_interval, col = day_type)) +
        geom_line() + 
        facet_grid(day_type ~ .) +
        ggtitle("Average Steps per Interval between Weekdays and Weekends") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
        xlab("5 Minutes Interval") +
        ylab("Average Steps")

