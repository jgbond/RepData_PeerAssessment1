############################
# Load and Preprocess Data #
############################

# Load Tidyverse
library(tidyverse)

# Unzip and load data into a table

if (!file.exists("activity.csv")) { unzip("activity.zip", exdir = ".") }
data <- read.csv("activity.csv")
data <- tbl_df(data)

# Convert date variable to date class
data$date <- as.Date(data$date, "%Y-%m-%d")

# Convert daily interval variable to factor class
data$interval <- as.factor(data$interval)

#####################################################
# What is mean total number of steps taken per day? #
#####################################################

# Group data by date
data2 <- group_by(data, date)

# Calculate total number of steps taken per day
data2 <- summarize(data2, daily_steps = sum(steps, na.rm=T))

# Make a histogram of total number of steps taken each day
hist(data2$daily_steps, breaks=20)

# Calculate mean and median of total number of steps taken per day
mean(data2$daily_steps, na.rm=T)
median(data2$daily_steps, na.rm=T)


###############################################
# What is the average daily activity pattern? #
###############################################

# Group data by daily interval
data3 <- group_by(data, interval)

# Calculate mean steps by daily interval across all dates
data3 <- summarize(data3, avg_steps = mean(steps, na.rm=T))

# Plot mean steps (y axis) by daily interval (x axis) across all dates
plot(data3$interval, data3$avg_steps)
lines(data3$interval, data3$avg_steps)

# Find 5-minute interval with max average number of daily steps
filter(data3, avg_steps == max(data3$avg_steps))


###########################
# Imputing missing values #
###########################

# Calculate total number of rows with NAs
sum(is.na(data$steps))

# Devise a strategy for filling in all of the missing values in the dataset
# If interval is NA, then use daily average of the interval

# Fill in missing data

# Join datasets so interval average is observed alongside interval observation
data4 <- left_join(data3, data)

# Create new variable that uses interval average where steps is NA
data4 <- mutate(data4, steps2 = ifelse(is.na(steps), avg_steps, steps))

# Convert to numeric
data4$steps <- as.numeric(data4$steps2)

# Remove extraneous columns
data4 <- select(data4, steps, date, interval)

# Group data by day and calculate total steps per day
data4_grouped <- group_by(data4, date)
data4_grouped <- summarize(data4_grouped, daily_steps = sum(steps))

# Make a histogram of the total number of steps taken each day 
hist(data4_grouped$daily_steps, breaks=20)

# Calculate mean and median of total number of steps taken per day
mean(data4_grouped$daily_steps)
median(data4_grouped$daily_steps)


#################################################################
# Difference in activity patterns between weekdays and weekends #
#################################################################

# Create a new factor variable with two levels – “weekday” and “weekend”

data5 <- mutate(data4, weekday = weekdays(date))

data5 <- mutate(data5, weekday = ifelse(
  weekday %in% c("Saturday","Sunday"),
  "weekend",
  "weekday"))

data5$weekday <- as.factor(data5$weekday)

data5_grouped <- group_by(data5, weekday, interval)
data5_grouped <- summarize(data5_grouped, daily_steps = sum(steps))

# Make a panel plot comparing 5-minute interval averages by weekday vs. weekend

ggplot(data5_grouped, aes(interval, daily_steps)) +
  geom_point() + 
  facet_wrap(~weekday)