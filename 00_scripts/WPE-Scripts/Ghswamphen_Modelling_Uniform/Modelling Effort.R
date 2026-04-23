
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

state_df <- df %>%
  filter(YEAR == 2026, MONTH == "Jan", STATE.CODE== "IN-WB")
#IN-WB, IN-UP, IN-MH, IN-GJ, IN-OR, IN-RJ, IN-KL

# ================================
# STEP 2: Fit GAM
# ================================
gam_model <- gam(
  OBSERVATION.COUNT ~ s(EFFORT.DISTANCE.KM),
  data = state_df,
  family = nb()   # negative binomial (better than poisson)
)
summary(gam_model)
plot(gam_model)
# DURATION.MINUTES
# EFFORT.DISTANCE.KM

newdata <- data.frame(
  EFFORT.DISTANCE.KM = seq(min(df$EFFORT.DISTANCE.KM),
                           max(df$EFFORT.DISTANCE.KM),
                           length.out = 100)
)

pred <- predict(gam_model, newdata, type = "response")

ggplot(newdata, aes(x = EFFORT.DISTANCE.KM, y = pred)) +
  geom_line()
# ================================
# STEP 3: Plot
# ================================
ggplot(state_df, aes(x = EFFORT.DISTANCE.KM, y = OBSERVATION.COUNT)) +
  geom_point(alpha = 0.1) +
  stat_smooth(
    method = "gam",
    formula = y ~ s(x),
    method.args = list(family = nb()),
    color = "steelblue") +
  theme_minimal(base_size = 14)
 # labs(
 #   x = "log(Duration in minutes)",
 #   y = "log(Observation Count)",
 #   title = "Effect of Duration on Counts (Jan 2026)"
 #  )
ggsave("Effort vs Duration log Jan26.tiff", width=7,height=5, dpi=300, units = "in")

#======================================================================================

############################ Fit GAM Model
# Time of day, location, day of year,  as co-variate and observer.id as random effect will improve the model
# + s(OBSERVER.ID, bs = "re")
# + s(LATITUDE, LONGITUDE)
# + s(DAY.OF.YEAR)
# s(TIME.OF.DAY)
library(mgcv)
library(dplyr)

# Optional: log-transform count if overdispersed
# Or use negative binomial

gam_model <- gam(OBSERVATION.COUNT ~ 
                   s(DURATION.MINUTES) + 
                   s(EFFORT.DISTANCE.KM),
                 data = state_df,
                 family = nb(),   # better than poisson for eBird
                 method = "REML")
summary(gam_model)
plot(gam_model, pages = 1, shade = TRUE)

ghswamphen_clean <- ghswamphen_clean %>%
  mutate(
    time_parsed = hm(TIME.OBSERVATIONS.STARTED),
    TIME_NUM = hour(time_parsed) + minute(time_parsed)/60
  )

gam_model2 <- gam(OBSERVATION.COUNT ~ 
                    s(DURATION.MINUTES) +
                    s(EFFORT.DISTANCE.KM) +
                    s(DAY_OF_YEAR, bs = "cc") +
                    s(TIME_NUM, bs = "cc") +
                    s(LATITUDE, LONGITUDE),
                  #s(OBSERVER.ID, bs = "re"),
                  data = ghswamphen_clean,
                  family = nb(),
                  method = "REML",
                  knots = list(DAY_OF_YEAR = c(0.5, 366.5), TIME_NUM = c(0, 24)))
summary(gam_model2)
plot(gam_model2, pages = 1, shade = TRUE)

# Extract plateau numerically
library(gratia)
# Derivative for duration
deriv_dur <- derivatives(gam_model, term = "s(DURATION.MINUTES)")

# Find where slope is ~0
plateau_dur <- deriv_dur %>%
  filter(abs(derivative) < 0.01) %>%
  slice(1)
plateau_dur

# Repeat for distance
deriv_dist <- derivatives(gam_model, term = "s(EFFORT.DISTANCE.KM)")
plateau_dist <- deriv_dist %>%
  filter(abs(derivative) < 0.01) %>%
  slice(1)
plateau_dist

# Cleaner prediction curve - Cleaner prediction curve
newdata <- data.frame(
  DURATION.MINUTES = seq(0, 400, by = 5),
  EFFORT.DISTANCE.KM = median(ebird_data$EFFORT.DISTANCE.KM, na.rm = TRUE)
)

pred <- predict(gam_model, newdata, type = "response")

plot(newdata$DURATION.MINUTES, pred, type = "l",
     xlab = "Duration", ylab = "Predicted Count")