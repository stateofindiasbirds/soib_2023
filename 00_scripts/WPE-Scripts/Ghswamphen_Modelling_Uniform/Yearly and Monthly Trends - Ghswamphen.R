# Monthly and Daily Trends
# Month-wise Total Bird Count
library(dplyr)
install.packages("lubridate")
library(lubridate)
library(ggplot2)

################################# Year and month-wise frequency plots ##########################
#ghswamphen_aggcounts - for max data
#ghswamphen_clean - for raw, filtered data

plot_histogram <- function(data, m, y) {
  
  ggplot(data, aes(x = OBSERVATION.COUNT)) +
    
    geom_histogram(
      bins = 20,
      fill = "#2C7FB8",
      color = "white",
      alpha = 0.9,
    ) +
    geom_vline(aes(xintercept = mean(OBSERVATION.COUNT, na.rm = TRUE)),
               linetype = "dashed",
               color = "red",
               linewidth = 1) +
    
    labs(
      title = paste("Count Distribution"),
      subtitle = paste("Month:", m, "| Year:", y),
      x = "Count",
      y = "Frequency"
    ) +
    
    theme_minimal(base_size = 14) +
    
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

library(dplyr)

unique_combinations <- ghswamphen_aggcounts %>%
  distinct(YEAR, MONTH)

for (i in 1:nrow(unique_combinations)) {
  
  y <- unique_combinations$YEAR[i]
  m <- unique_combinations$MONTH[i]
  
  subset_data <- ghswamphen_aggcounts %>%
    filter(YEAR == y, MONTH == m)
  
  p <- plot_histogram(subset_data, m, y)
  
  ggsave(
    filename = paste0("hist_count_", y, "_", m, ".png"),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}
##########################################################################
################################# Year-wise mean monthly counts #########################
# ================================
# Load libraries
# ================================
library(dplyr)
library(ggplot2)

# ================================
# Step 1: Summarise mean count per month per year
# ================================
monthly_means <- ghswamphen_clean %>%
  group_by(YEAR, MONTH) %>%
  summarise(
    mean_count = mean(OBSERVATION.COUNT, na.rm = TRUE),
    .groups = "drop"
  )

# ================================
# Step 2: Ensure months are ordered properly
# ================================
monthly_means$MONTH <- factor(
  monthly_means$MONTH,
  levels = month.abb
)
# Ensure all 12 months appear (even if missing)

# ================================
# Step 3: Plotting function
# ================================
plot_monthly_means <- function(data, y) {
  
  ggplot(data, aes(x = MONTH, y = mean_count, group = 1)) +
    
    geom_point(
      size = 3,
      color = "#2C7FB8"
    ) +
  geom_line(
      linewidth = 1.2,
      color = "#2C7FB8"
    ) +
    
    labs(
      title = paste("Mean Monthly Counts"),
      subtitle = paste("Year:", y),
      x = "Month",
      y = "Mean Count"
    ) +
    
    theme_minimal(base_size = 14) +
    
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# ================================
# Step 4: Loop through each year and save plots
# ================================
unique_years <- unique(monthly_means$YEAR)

for (y in unique_years) {
  
  subset_data <- monthly_means %>%
    filter(YEAR == y)
  
  p <- plot_monthly_means(subset_data, y)
  
  ggsave(
    filename = paste0("mean_monthly_counts_", y, ".png"),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

###################################### Count vs Duration ###########################################
# ================================
# LOAD LIBRARIES
# ================================
library(dplyr)
library(ggplot2)
library(mgcv)

# ================================
# STEP 1: Clean data
# ================================
# Replace DURATION.MINUTES with your actual column name
df <- ghswamphen_clean %>%
  filter(
      !is.na(DURATION.MINUTES),
    !is.na(OBSERVATION.COUNT),
    DURATION.MINUTES > 0
  )

jan_df <- df %>%
  filter(YEAR == 2026, MONTH == "Jan")

# ================================
# STEP 2: Fit GAM
# ================================
gam_model <- gam(
  OBSERVATION.COUNT ~ s(EFFORT.DISTANCE.KM),
  data = df,
  family = nb()   # negative binomial (better than poisson)
)
plot(gam_model)

# ================================
# STEP 3: Plot
# ================================
ggplot(jan_df, aes(x = log(DURATION.MINUTES), y = log(OBSERVATION.COUNT))) +
        geom_point(alpha = 0.1) +
        stat_smooth(
        method = "gam",
        formula = y ~ s(x),
        color = "steelblue") +
  theme_minimal(base_size = 14) +
  labs(
    x = "log(Duration in minutes)",
    y = "log(Observation Count)",
    title = "Effect of Duration on Counts (Jan 2026)"
  )
ggsave("Effort vs Duration log Jan26.tiff", width=7,height=5, dpi=300, units = "in")

################################### Old Trends ##################################3
# Convert date to months
ghswamphen_aggcounts <- ghswamphen_aggcounts %>%
  mutate(
    OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
    month = month(OBSERVATION.DATE, label = TRUE),
    year = year(OBSERVATION.DATE)
  )

# Summarize total bird count per month
monthly_counts <- ghswamphen_aggcounts %>% 
  group_by(year, month) %>%
  summarise(total_birds = sum(as.numeric(OBSERVATION.COUNT), na.rm = TRUE)) %>%
  ungroup()

library(ggplot2)
# Visualize monthly bird count trend
ggplot(monthly_counts, aes(month, total_birds, group=1)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ year) +
  labs(
    x = "Month",
    y = "Total Bird Count",
    title = "Month-wise Total Bird Counts (Nov 2024 – Oct 2025)"
  )

# Month-wise TOTAL NUMBER OF CHECKLISTS
checklists <- ebd_unique %>%
  mutate(
    date = as.Date(observation_date),
    month = floor_date(date, "month")
  )
# Count checklists per month
monthly_checklists <- ghswamphen_clean %>% filter(YEAR==2024) %>%
  group_by(MONTH) %>%
  summarise(n_checklists = n_distinct(CHECKLIST.ID))

# Visualize monthly checklist trend
ggplot(monthly_checklists, aes(MONTH, n_checklists, group=1)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Month",
    y = "Number of Checklists",
    title = "Month-wise Checklist Counts for 2024"
  )
ggsave("Monthly Checklists 2024.tiff", width=7,height=5, dpi=300, units = "in")

# Combined Plot
library(tidyr)
combined <- full_join(monthly_counts, monthly_checklists, by = "month") %>%
  pivot_longer(cols = c(total_birds, n_checklists),
               names_to = "metric",
               values_to = "value")
ggplot(combined, aes(month, value, color = metric)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Month",
    y = "Value",
    title = "Bird Counts and Checklist Counts Over Time"
  )

# How detections vary across time of day (start time).
jan <- checklists %>%
  mutate(
    time = hms(time_observations_started),   # convert "HH:MM" to duration
    hour = hour(time)                       # extract hour as numeric 0–23
  )

# Summarize detections by hour
detections_by_hour <- jan %>%
  filter(observation_count != "X") %>%
  mutate(count = as.numeric(observation_count)) %>%
  group_by(hour) %>%
  summarise(total_detections = sum(count, na.rm = TRUE))

# Plot detection trend by hour
library(ggplot2)
ggplot(detections_by_hour, aes(hour, total_detections)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Hour of Day",
    y = "Total Bird Detections",
    title = "Bird Detection Trend by Time of Day (Jan 2025)"
  )

# Number of Checklists by Time of Day
checklists_by_hour <- jan %>%
  group_by(hour) %>%
  summarise(n_checklists = n_distinct(checklist_id))
# Plot
ggplot(checklists_by_hour, aes(hour, n_checklists)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Hour of Day",
    y = "Number of Checklists",
    title = "Checklist Effort by Time of Day (Jan 2025)"
  )

# Normalize by Effort
hour_effort <- detections_by_hour %>%
  left_join(checklists_by_hour, by = "hour") %>%
  mutate(detections_per_checklist = total_detections / n_checklists)
# Plot
ggplot(hour_effort, aes(hour, detections_per_checklist)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Hour of Day",
    y = "Detections per Checklist",
    title = "Detection Rate per Checklist (Jan 2025)"
  )

ggplot(data = ebd_unique, aes(x=number_observers,y=observation_count)) +
  geom_point(color="red", size=1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_smooth(method = 'loess', se = TRUE, color = 'steelblue',fill='steelblue') +
  labs(x = "Number of observers", y = "Number of detections")
