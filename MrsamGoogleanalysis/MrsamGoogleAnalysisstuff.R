# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(scales)
library(ggpubr)
library(knitr)
library(kableExtra)

# Set theme for better visualizations
theme_set(theme_minimal())

# Read the data
activity_data <- read.csv("dailyActivity_merged.csv")

# Data Exploration and Cleaning
cat("=== DATASET OVERVIEW ===\n")
cat("Number of records:", nrow(activity_data), "\n")
cat("Number of unique users:", n_distinct(activity_data$Id), "\n")
cat("Date range:", min(activity_data$ActivityDate), "to", max(activity_data$ActivityDate), "\n")

# Convert ActivityDate to proper date format
activity_data$ActivityDate <- as.Date(activity_data$ActivityDate, format = "%m/%d/%Y")

# Check for missing values
cat("\nMissing values in each column:\n")
print(colSums(is.na(activity_data)))

# Create additional useful columns
activity_data <- activity_data %>%
  mutate(
    Weekday = weekdays(ActivityDate),
    WeekNumber = week(ActivityDate),
    TotalActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes,
    TotalActiveHours = TotalActiveMinutes / 60,
    SedentaryHours = SedentaryMinutes / 60,
    ActivityRatio = TotalActiveMinutes / (TotalActiveMinutes + SedentaryMinutes),
    IsWeekend = Weekday %in% c("Saturday", "Sunday")
  )

# Remove days where device likely wasn't worn (0 steps and 1440 sedentary minutes)
valid_data <- activity_data %>%
  filter(!(TotalSteps == 0 & SedentaryMinutes == 1440))

cat("\nRecords after removing non-wear days:", nrow(valid_data), "\n")
cat("Percentage of valid records:", round(nrow(valid_data)/nrow(activity_data)*100, 1), "%\n")

# 1. USER ENGAGEMENT ANALYSIS
cat("\n=== USER ENGAGEMENT ANALYSIS ===\n")

user_summary <- valid_data %>%
  group_by(Id) %>%
  summarise(
    RecordDays = n(),
    AvgSteps = mean(TotalSteps),
    AvgCalories = mean(Calories),
    AvgSedentaryHours = mean(SedentaryHours),
    AvgActiveHours = mean(TotalActiveHours),
    MaxSteps = max(TotalSteps),
    Consistency = sd(TotalSteps) / mean(TotalSteps)  # Lower = more consistent
  ) %>%
  mutate(
    ActivityLevel = case_when(
      AvgSteps >= 10000 ~ "Highly Active",
      AvgSteps >= 7500 ~ "Moderately Active", 
      AvgSteps >= 5000 ~ "Lightly Active",
      TRUE ~ "Sedentary"
    )
  )

cat("\nUser Activity Level Distribution:\n")
print(table(user_summary$ActivityLevel))

# 2. DESCRIPTIVE STATISTICS
cat("\n=== DESCRIPTIVE STATISTICS ===\n")

overall_stats <- valid_data %>%
  summarise(
    AvgSteps = mean(TotalSteps),
    MedianSteps = median(TotalSteps),
    AvgCalories = mean(Calories),
    AvgVeryActiveMinutes = mean(VeryActiveMinutes),
    AvgFairlyActiveMinutes = mean(FairlyActiveMinutes),
    AvgLightlyActiveMinutes = mean(LightlyActiveMinutes),
    AvgSedentaryMinutes = mean(SedentaryMinutes),
    AvgSedentaryHours = mean(SedentaryHours)
  )

print(overall_stats)

# 3. VISUALIZATIONS

# 3.1 Distribution of Daily Steps
p1 <- ggplot(valid_data, aes(x = TotalSteps)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 10000, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Daily Steps",
       subtitle = "Red line shows 10,000 steps benchmark",
       x = "Total Steps", y = "Frequency") +
  scale_x_continuous(labels = comma)

# 3.2 Steps vs Calories
p2 <- ggplot(valid_data, aes(x = TotalSteps, y = Calories)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Steps vs Calories Burned",
       x = "Total Steps", y = "Calories") +
  scale_x_continuous(labels = comma)

# 3.3 Activity Intensity Composition
activity_minutes <- valid_data %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  summarise_all(mean) %>%
  pivot_longer(cols = everything(), names_to = "ActivityType", values_to = "Minutes") %>%
  mutate(
    ActivityType = factor(ActivityType, 
                          levels = c("SedentaryMinutes", "LightlyActiveMinutes", 
                                     "FairlyActiveMinutes", "VeryActiveMinutes")),
    Hours = Minutes / 60,
    Percentage = Minutes / (24 * 60) * 100
  )

p3 <- ggplot(activity_minutes, aes(x = "", y = Hours, fill = ActivityType)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Average Daily Time Distribution",
       fill = "Activity Type") +
  scale_fill_brewer(palette = "Set2",
                    labels = c("Sedentary", "Lightly Active", 
                               "Fairly Active", "Very Active")) +
  theme_void()

# 3.4 Weekly Patterns
weekly_patterns <- valid_data %>%
  group_by(Weekday) %>%
  summarise(
    AvgSteps = mean(TotalSteps),
    AvgCalories = mean(Calories),
    AvgSedentaryHours = mean(SedentaryHours)
  ) %>%
  mutate(Weekday = factor(Weekday, 
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                     "Friday", "Saturday", "Sunday")))

p4 <- ggplot(weekly_patterns, aes(x = Weekday, y = AvgSteps, fill = Weekday)) +
  geom_col() +
  labs(title = "Average Steps by Day of Week",
       x = "", y = "Average Steps") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3.5 Top 10 Most Active Users
top_users <- user_summary %>%
  arrange(desc(AvgSteps)) %>%
  head(10)

p5 <- ggplot(top_users, aes(x = reorder(as.character(Id), AvgSteps), y = AvgSteps)) +
  geom_col(fill = "coral", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 10 Most Active Users",
       x = "User ID", y = "Average Daily Steps") +
  scale_y_continuous(labels = comma)

# Arrange plots
grid_plots <- ggarrange(p1, p2, p3, p4, p5, ncol = 2, nrow = 3)
print(grid_plots)

# 4. CORRELATION ANALYSIS
cat("\n=== CORRELATION ANALYSIS ===\n")

correlation_data <- valid_data %>%
  select(TotalSteps, TotalDistance, VeryActiveMinutes, FairlyActiveMinutes,
         LightlyActiveMinutes, SedentaryMinutes, Calories)

cor_matrix <- cor(correlation_data)

# Correlation plot
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black",
         title = "Correlation Matrix of Activity Metrics",
         mar = c(0,0,1,0))

# 5. TRENDS OVER TIME
cat("\n=== TIME TREND ANALYSIS ===\n")

# Daily averages over time
daily_trends <- valid_data %>%
  group_by(ActivityDate) %>%
  summarise(
    AvgSteps = mean(TotalSteps),
    AvgCalories = mean(Calories),
    AvgSedentaryHours = mean(SedentaryHours),
    UserCount = n()
  )

p6 <- ggplot(daily_trends, aes(x = ActivityDate)) +
  geom_line(aes(y = AvgSteps, color = "Average Steps"), size = 1) +
  geom_line(aes(y = AvgCalories/50, color = "Average Calories (/50)"), size = 1) +
  scale_y_continuous(
    name = "Average Steps",
    sec.axis = sec_axis(~ . * 50, name = "Average Calories")
  ) +
  labs(title = "Daily Activity Trends Over Time",
       x = "Date", color = "Metric") +
  scale_color_manual(values = c("Average Steps" = "blue", 
                                "Average Calories (/50)" = "red")) +
  theme(legend.position = "bottom")

print(p6)

# 6. USER SEGMENTATION ANALYSIS
cat("\n=== USER SEGMENTATION ===\n")

# Create user segments based on activity patterns
user_segments <- user_summary %>%
  mutate(
    Segment = case_when(
      AvgSteps >= 10000 & AvgSedentaryHours <= 12 ~ "Active & Balanced",
      AvgSteps >= 10000 & AvgSedentaryHours > 12 ~ "Active but Sedentary",
      AvgSteps >= 5000 & AvgSteps < 10000 ~ "Moderately Active",
      AvgSteps < 5000 ~ "Less Active",
      TRUE ~ "Other"
    )
  )

cat("User Segmentation:\n")
segment_summary <- user_segments %>%
  group_by(Segment) %>%
  summarise(Count = n(), Percentage = round(n()/nrow(user_segments)*100, 1))
print(segment_summary)

# 7. INTENSITY ANALYSIS
cat("\n=== ACTIVITY INTENSITY ANALYSIS ===\n")

intensity_analysis <- valid_data %>%
  summarise(
    TotalVeryActivePct = sum(VeryActiveMinutes) / sum(TotalActiveMinutes) * 100,
    TotalFairlyActivePct = sum(FairlyActiveMinutes) / sum(TotalActiveMinutes) * 100,
    TotalLightlyActivePct = sum(LightlyActiveMinutes) / sum(TotalActiveMinutes) * 100
  )

cat("Activity Intensity Distribution (% of total active time):\n")
print(intensity_analysis)

# 8. WEEKDAY VS WEEKEND COMPARISON
cat("\n=== WEEKDAY VS WEEKEND ANALYSIS ===\n")

weekend_comparison <- valid_data %>%
  group_by(IsWeekend) %>%
  summarise(
    AvgSteps = mean(TotalSteps),
    AvgCalories = mean(Calories),
    AvgSedentaryHours = mean(SedentaryHours),
    AvgVeryActiveMinutes = mean(VeryActiveMinutes),
    n = n()
  ) %>%
  mutate(DayType = ifelse(IsWeekend, "Weekend", "Weekday"))

print(weekend_comparison)

# Statistical test for difference
weekday_data <- valid_data %>% filter(!IsWeekend) %>% pull(TotalSteps)
weekend_data <- valid_data %>% filter(IsWeekend) %>% pull(TotalSteps)

t_test_result <- t.test(weekday_data, weekend_data)
cat("\nT-test for steps (Weekday vs Weekend):\n")
cat("p-value:", t_test_result$p.value, "\n")

# 9. DETAILED USER PROFILES
cat("\n=== SAMPLE USER PROFILES ===\n")

# Get most active user
most_active_user <- user_summary %>%
  arrange(desc(AvgSteps)) %>%
  head(1)

most_active_data <- valid_data %>%
  filter(Id == most_active_user$Id)

cat("Most Active User Profile (ID:", most_active_user$Id, "):\n")
cat("Average Steps:", round(most_active_user$AvgSteps), "\n")
cat("Average Calories:", round(most_active_user$AvgCalories), "\n")
cat("Activity Level:", most_active_user$ActivityLevel, "\n")

# Get least active user (among those with reasonable data)
least_active_user <- user_summary %>%
  filter(RecordDays >= 10) %>%  # At least 10 days of data
  arrange(AvgSteps) %>%
  head(1)

cat("\nLeast Active User Profile (ID:", least_active_user$Id, "):\n")
cat("Average Steps:", round(least_active_user$AvgSteps), "\n")
cat("Average Calories:", round(least_active_user$AvgCalories), "\n")
cat("Activity Level:", least_active_user$ActivityLevel, "\n")

# 10. EXPORT SUMMARY STATISTICS
cat("\n=== SUMMARY REPORT ===\n")

summary_report <- list(
  Dataset = list(
    TotalRecords = nrow(activity_data),
    ValidRecords = nrow(valid_data),
    UniqueUsers = n_distinct(activity_data$Id),
    DateRange = paste(min(activity_data$ActivityDate), "to", max(activity_data$ActivityDate))
  ),
  ActivityLevels = as.list(table(user_summary$ActivityLevel)),
  OverallAverages = as.list(overall_stats),
  UserSegments = as.list(setNames(segment_summary$Count, segment_summary$Segment))
)

print(summary_report)

# Save detailed user summary
write.csv(user_summary, "user_activity_summary.csv", row.names = FALSE)
cat("\nDetailed user summary saved to 'user_activity_summary.csv'\n")

# Print final insights
cat("\n=== KEY INSIGHTS ===\n")
cat("1. Average daily steps across all users:", round(mean(valid_data$TotalSteps)), "\n")
cat("2. Percentage of users meeting 10,000 steps goal:", 
    round(mean(user_summary$AvgSteps >= 10000) * 100, 1), "%\n")
cat("3. Average sedentary time:", round(mean(valid_data$SedentaryHours), 1), "hours per day\n")
cat("4. Strongest correlation: Steps vs Calories =", 
    round(cor(valid_data$TotalSteps, valid_data$Calories), 3), "\n")
cat("5. Data quality: ", round(nrow(valid_data)/nrow(activity_data)*100, 1), 
    "% of records show device was likely worn\n")

# Additional visualization: Activity patterns for top 5 users
top5_users <- user_summary %>%
  arrange(desc(AvgSteps)) %>%
  head(5) %>%
  pull(Id)

top_users_data <- valid_data %>%
  filter(Id %in% top5_users) %>%
  group_by(Id, ActivityDate) %>%
  summarise(DailySteps = mean(TotalSteps))

p7 <- ggplot(top_users_data, aes(x = ActivityDate, y = DailySteps, color = as.character(Id))) +
  geom_line(size = 0.8) +
  labs(title = "Daily Steps Trend for Top 5 Most Active Users",
       x = "Date", y = "Daily Steps", color = "User ID") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom")

print(p7)
