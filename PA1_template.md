---
title: "Responsible Research Course Project 1"
author: "C. Becher"
date: "2025-03-07"
output: 
  html_document:
    keep_md: true
---
## Introduction 

The goal of this assignment is to write a report that answer the questions below using one single R markdown document that can be processed by knitr and transformed into a HTML file. 


# Conditions
1. Use a code that is used to generate the output 
2. When writing the code chunks always use echo=TRUE
3. The submission of the assignment will consists of the URL to my GitHub repository and the SHA-1 commit ID for my repository state 

## Assignment

1. Load the data 


``` r
data <- read.csv("Z:/2. Personal/Clarissa/Courses/Responsible Research/activity.csv")
```

2. What is the mean total number of steps taken per day? 
Missing values N/A can be ignored 


``` r
#1. Calculate the total number of steps per day 
daily_steps <- tapply(data$steps, data$date, sum, na.rm = TRUE)

# 2. Create a histogram of the total number of steps taken per day
hist(daily_steps, 
     main = "Histogram of Total Steps per Day",
     xlab = "Total Steps per Day",
     col = "blue",
     border = "black")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
#. Calculate the mean and media of the total steps per day 
mean_steps <- mean(daily_steps, na.rm = TRUE)
median_steps <- median(daily_steps, na.rm = TRUE)

# Print results
cat("Mean of total steps per day:", mean_steps, "\n")
```

```
## Mean of total steps per day: 9354.23
```

``` r
cat("Median of total steps per day:", median_steps, "\n")
```

```
## Median of total steps per day: 10395
```
3. What is the average daily activity pattern? 


``` r
# Make a time series plot of a 5-minute interval and the average number of steps taken 
average_steps_interval <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)
plot(average_steps_interval$interval, average_steps_interval$steps, 
     type = "l", 
     main = "Average Daily Activity Pattern",
     xlab = "5-Minute Interval",
     ylab = "Average Steps",
     col = "blue",
     lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
max_interval <- average_steps_interval[which.max(average_steps_interval$steps), ]
cat("5-minute interval with maximum average steps:", max_interval$interval, 
    "with an average of", max_interval$steps, "steps.\n")
```

```
## 5-minute interval with maximum average steps: 835 with an average of 206.1698 steps.
```
4. Inputting missing values 


``` r
# 1. Calculate the total number of missing values
missing_values <- sum(is.na(data$steps))
cat("Total number of missing values in the dataset:", missing_values, "\n")
```

```
## Total number of missing values in the dataset: 2304
```

``` r
# 2. Strategy for filling missing values is to replace NA value with the mean for that 5-minute interval 
interval_means <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)
# Merge mean interval steps into original dataset
filled_data <- merge(data, interval_means, by = "interval", suffixes = c("", "_mean"))

# Replace NA values in 'steps' with the corresponding interval mean
filled_data$steps[is.na(filled_data$steps)] <- filled_data$steps_mean
```

```
## Warning in filled_data$steps[is.na(filled_data$steps)] <-
## filled_data$steps_mean: number of items to replace is not a multiple of
## replacement length
```

``` r
# Remove the extra column used for merging
filled_data <- filled_data[, c("steps", "date", "interval")]
```

``` r
# 3. Create a histogram of total steps per day with imputed data
filled_daily_steps <- tapply(filled_data$steps, filled_data$date, sum)

hist(filled_daily_steps, 
     main = "Histogram of Total Steps per Day (With Imputed Data)",
     xlab = "Total Steps per Day",
     col = "green",
     border = "black")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

``` r
# 4. Calculate the mean and median total steps per day after imputation
filled_mean_steps <- mean(filled_daily_steps)
filled_median_steps <- median(filled_daily_steps)

# Print the results
cat("Mean of total steps per day (with imputed data):", filled_mean_steps, "\n")
```

```
## Mean of total steps per day (with imputed data): 9371.437
```

``` r
cat("Median of total steps per day (with imputed data):", filled_median_steps, "\n")
```

```
## Median of total steps per day (with imputed data): 10395
```

``` r
# Compare with original dataset (from previous calculation)
cat("Difference in mean:", filled_mean_steps - mean_steps, "\n")
```

```
## Difference in mean: 17.20755
```

``` r
cat("Difference in median:", filled_median_steps - median_steps, "\n")
```

```
## Difference in median: 0
```
## Interpretation of the results

Yes, the mean total number of steps per day increased slightly after imputing missing values, from 9354.23 to 9371.437. However, the median total number of steps per day remained unchanged at 10,395.

Imputing missing values had a small effect on the mean, increasing it slightly because missing values were replaced with the mean of their respective 5-minute intervals.
The median remained unchanged, indicating that the overall distribution of daily steps was not significantly altered.
The histogram of total steps per day remained visually the same, suggesting that the imputation did not dramatically change the shape of the data distribution.
Overall, imputing missing values helps maintain data completeness and reduces bias in summary statistics without drastically changing the dataset’s distribution.

5. Are there differences in activity patterns between weekdays and weekends? 

Doing everything at once did not result in the desired graph, so i am debugging and checking what was the problem going step by step.


``` r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.3.3
```

``` r
# Set locale to English (only for this session)
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

``` r
# Convert the 'date' column to Date format
filled_data$date <- as.Date(filled_data$date)

# Create a new variable indicating whether it's a weekday or weekend
filled_data$day_type <- ifelse(weekdays(filled_data$date) %in% c("Saturday", "Sunday"), 
                               "weekend", "weekday")

# Convert to factor
filled_data$day_type <- factor(filled_data$day_type, levels = c("weekday", "weekend"))

# Check if the classification worked
table(filled_data$day_type)
```

```
## 
## weekday weekend 
##   12960    4608
```

``` r
head(weekdays(filled_data$date))
```

```
## [1] "Monday"   "Friday"   "Sunday"   "Tuesday"  "Saturday" "Thursday"
```

``` r
# Calculate the average number of steps per 5-minute interval, grouped by weekday/weekend
average_steps_day_type <- aggregate(steps ~ interval + day_type, 
                                    data = filled_data, mean)
```


``` r
# Create the faceted time series plot
ggplot(average_steps_day_type, aes(x = interval, y = steps)) +
  geom_line(color = "blue") +
  facet_grid(day_type ~ .) +  # Facet by weekday/weekend
  labs(title = "Activity Patterns: Weekdays vs. Weekends",
       x = "5-Minute Interval",
       y = "Average Number of Steps") +
  theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



