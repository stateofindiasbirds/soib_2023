# ==========================================================
# Compares population trend outputs between two runs
# Requires:
# - results/trends.csv" files. However, full resolving is not necessary as we need only the few columns 
# ==========================================================

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# -----------------------------
# 1. Read and prepare the data
# -----------------------------
cols <- c(1, 2, 6, 7, 8, 9, 10, 11, 12, 13, 14)
colnames <- c("timegroups", "COMMON.NAME", "lci", "mean", "rci",
              "lci_std", "mean_std", "rci_std",
              "lci_std_recent", "mean_std_recent", "rci_std_recent")

old <- read.csv("01_analyses_full/results/trends_Dec18_old_CI_no_filter_fix.csv")[,colnames]
new <- read.csv("01_analyses_full/results/trends_Dec18_new_CI_no_filter_fix.csv")[,colnames]
write_path <- "01_analyses_full/results/figs/no_filter_fix_old_vs_new_CI/species_trends_batch_"

old <- read.csv("01_analyses_full/results/trends_Dec18_new_CI_no_filter_fix.csv")[,colnames]
new <- read.csv("01_analyses_full/results/trends.csv")[,colnames]
write_path <- "01_analyses_full/results/figs/new_CI_filter_fix_vs_no_fix/species_trends_batch_"

old <- read.csv("01_analyses_full/results/trends_2023.csv")[,colnames]
new <- read.csv("01_analyses_full/results/trends.csv")[,colnames]
write_path <- "01_analyses_full/results/figs/filter_fix_new_CI_vs_2023_report/species_trends_batch_"

old <- read.csv("01_analyses_full/results/trends_old_CI.csv")[,colnames]
new <- read.csv("01_analyses_full/results/trends.csv")[,colnames]
write_path <- "01_analyses_full/results/figs/filter_fix_old_CI_vs_new_CI/species_trends_batch_"

old <- read.csv("01_analyses_full/results/trends.csv")[,colnames]
new <- read.csv("01_analyses_full/results/trends_subsamp_test.csv")[,colnames]
write_path <- "01_analyses_full/results/figs/subsamp_test/species_trends_batch_"



names(old) <- colnames
names(new) <- colnames

# -----------------------------
# 2. Combine datasets
# -----------------------------
combined <- old %>%
  rename(lci_old = lci, mean_old = mean, rci_old = rci,
         lci_std_old = lci_std, mean_std_old = mean_std, rci_std_old = rci_std,
         lci_std_recent_old = lci_std_recent, mean_std_recent_old = mean_std_recent, rci_std_recent_old = rci_std_recent) %>%
  inner_join(new %>%
               rename(lci_new = lci, mean_new = mean, rci_new = rci,
                      lci_std_new = lci_std, mean_std_new = mean_std, rci_std_new = rci_std,
                      lci_std_recent_new = lci_std_recent, mean_std_recent_new = mean_std_recent, rci_std_recent_new = rci_std_recent),
             by = c("COMMON.NAME", "timegroups")) %>%
  filter(!is.na(mean_old) & !is.na(mean_new))

# -----------------------------
# 3. Compute summary statistics (baseline-aware MD)
# -----------------------------
trend_summary <- combined %>%
  group_by(COMMON.NAME) %>%
  group_modify(~{
    df <- .x
    use_recent <- !any(df$timegroups == 1992)
    
    if (use_recent) {
      df <- df %>%
        mutate(
          mean_old_used = mean_std_recent_old,
          mean_new_used = mean_std_recent_new,
          lci_old_used  = lci_std_recent_old,
          lci_new_used  = lci_std_recent_new,
          rci_old_used  = rci_std_recent_old,
          rci_new_used  = rci_std_recent_new
        )
    } else {
      df <- df %>%
        mutate(
          mean_old_used = mean_std_old,
          mean_new_used = mean_std_new,
          lci_old_used  = lci_std_old,
          lci_new_used  = lci_std_new,
          rci_old_used  = rci_std_old,
          rci_new_used  = rci_std_new
        )
    }
    
    baseline <- 100
    dev_old <- abs(df$mean_old_used - baseline)
    dev_new <- abs(df$mean_new_used - baseline)
    mean_dev <- mean(dev_old + dev_new, na.rm = TRUE)
    
    tibble(
      n_points = nrow(df),
      mean_diff = mean_dev,
      mean_pct_diff = mean((df$mean_new_used - df$mean_old_used) / df$mean_old_used, na.rm = TRUE) * 100,
      corr_means = if (sum(!is.na(df$mean_old_used) & !is.na(df$mean_new_used)) > 1)
        cor(df$mean_old_used, df$mean_new_used, use = "complete.obs") else NA_real_,
      lci_range_diff = mean(df$lci_new_used - df$lci_old_used, na.rm = TRUE),
      rci_range_diff = mean(df$rci_new_used - df$rci_old_used, na.rm = TRUE),
      mean_width_old = mean(df$rci_old_used - df$lci_old_used, na.rm = TRUE),
      mean_width_new = mean(df$rci_new_used - df$lci_new_used, na.rm = TRUE),
      width_ratio = mean_width_new / mean_width_old
    )
  }) %>%
  ungroup() %>%
  mutate(width_ratio = ifelse(is.infinite(width_ratio) | is.na(width_ratio), NA, width_ratio))


write.csv(trend_summary, "trend_summary.csv")


# -----------------------------
# 4. Compute yearly differences (baseline-aware MD)
# -----------------------------
yearly_diff <- combined %>%
  mutate(
    mean_old_used = ifelse(!any(timegroups == 1992), mean_std_recent_old, mean_std_old),
    mean_new_used = ifelse(!any(timegroups == 1992), mean_std_recent_new, mean_std_new)
  ) %>%
  group_by(timegroups) %>%
  summarise(
    mean_abs_diff = mean(abs(mean_old_used - 100) + abs(mean_new_used - 100), na.rm = TRUE),
    
    # safe percentage difference: only compute where mean_old_used != 0
    mean_pct_diff = mean(
      ifelse(!is.na(mean_old_used) & mean_old_used != 0,
             (mean_new_used - mean_old_used) / mean_old_used,
             NA_real_), 
      na.rm = TRUE
    ) * 100,
    
    sd_pct_diff = sd(
      ifelse(!is.na(mean_old_used) & mean_old_used != 0,
             (mean_new_used - mean_old_used) / mean_old_used,
             NA_real_), 
      na.rm = TRUE
    ) * 100,
    
    n_species = sum(!is.na(mean_old_used) & !is.na(mean_new_used)),
    .groups = "drop"
  ) %>%
  mutate(
    se_pct_diff = ifelse(n_species > 0, sd_pct_diff / sqrt(n_species), NA)
  )

# Plot yearly mean % differences
p_yearly <- ggplot(yearly_diff, aes(x = timegroups, y = mean_pct_diff)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_errorbar(aes(ymin = mean_pct_diff - se_pct_diff,
                    ymax = mean_pct_diff + se_pct_diff),
                width = 0.3, color = "black") +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(mean_pct_diff, 1)),
            vjust = -0.4, size = 3.2, color = "black") +
  labs(
    title = "Average % Difference between Old and New Standardized Trends by Year",
    subtitle = "Dashed line marks 2014",
    x = "Year",
    y = "Mean % difference (± SE)"
  ) +
  theme_minimal(base_size = 13)

ggsave("Yearly_Mean_Difference.jpg", plot = p_yearly, width = 12, height = 8, dpi = 300, bg = "white")

# -----------------------------
# 5. Other visualisations
# -----------------------------
# 5a. Correlation histogram
p_corr <- ggplot(trend_summary, aes(x = corr_means)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Correlation of trend means (Old vs New)",
       x = "Correlation coefficient", y = "Number of species") +
  theme_minimal(base_size = 13)
ggsave("Correlation_of_Means.jpg", plot = p_corr, width = 12, height = 8, dpi = 300, bg = "white")

# 5b. CI width scatter
p_ci <- ggplot(trend_summary, aes(x = mean_width_old, y = mean_width_new)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Comparison of Confidence Interval widths",
       x = "Old CI width", y = "New CI width") +
  theme_minimal(base_size = 13)
ggsave("CI_Width_Scatter.jpg", plot = p_ci, width = 12, height = 8, dpi = 300, bg = "white")

# 5c. Distribution of % mean change
p_pct <- ggplot(trend_summary, aes(x = mean_pct_diff)) +
  geom_histogram(bins = 40, fill = "darkorange", color = "white") +
  labs(title = "Distribution of % change in mean trend (New vs Old)",
       x = "Percentage change", y = "Species count") +
  theme_minimal(base_size = 13)
ggsave("Distribution_Percent_Mean_Change.jpg", plot = p_pct, width = 12, height = 8, dpi = 300, bg = "white")

# -----------------------------
# 6. Top species with biggest changes
# -----------------------------
top_changes <- trend_summary %>%
  arrange(desc(abs(mean_pct_diff))) %>%
  slice_head(n = 10)
write.csv(top_changes, "top_changes.csv")

# -----------------------------
# 7. Bottom species with least changes
# -----------------------------
bottom_changes <- trend_summary %>%
  arrange(desc(abs(mean_pct_diff))) %>%
  slice_tail(n = 10)

# -----------------------------
# 8. Prepare batches for 5x5 plots
# -----------------------------
trend_summary <- trend_summary %>%
  arrange(desc(abs(mean_pct_diff)))

species_batches <- split(trend_summary$COMMON.NAME,
                         ceiling(seq_along(trend_summary$COMMON.NAME) / 25))



for (i in seq_along(species_batches)) {
  #i <- 1
  species_set <- species_batches[[i]]
  
  label_data <- trend_summary %>%
    filter(COMMON.NAME %in% species_set) %>%
    dplyr::select(COMMON.NAME, mean_pct_diff, width_ratio)
  
  plot_data_list <- lapply(species_set, function(sp) {
    sp_data <- combined %>% filter(COMMON.NAME == sp)
    
    # --- choose baseline variant
    use_recent <- !any(sp_data$timegroups == 1992)
    if (use_recent) {
      sp_data <- sp_data %>%
        mutate(
          mean_std_old = mean_std_recent_old,
          mean_std_new = mean_std_recent_new,
          lci_std_old  = lci_std_recent_old,
          rci_std_old  = rci_std_recent_old,
          lci_std_new  = lci_std_recent_new,
          rci_std_new  = rci_std_recent_new,
          baseline_year = 2015,
          baseline_label = "2015 baseline"
        )
    } else {
      sp_data <- sp_data %>%
        mutate(
          baseline_year = 1992,
          baseline_label = "Pre-2000 baseline"
        )
    }
    
    # --- baseline value = mean trend at baseline year
    base_val <- sp_data %>%
      filter(timegroups == baseline_year) %>%
      summarise(val = mean(mean_std_new, na.rm = TRUE)) %>%
      pull(val)
    if (length(base_val) == 0 || is.na(base_val)) base_val <- 0
    
    # --- normalize relative to baseline
    sp_data <- sp_data %>%
      mutate(
        baseline_value = base_val,
        mean_std_old = mean_std_old - base_val,
        mean_std_new = mean_std_new - base_val,
        lci_std_old  = lci_std_old  - base_val,
        rci_std_old  = rci_std_old  - base_val,
        lci_std_new  = lci_std_new  - base_val,
        rci_std_new  = rci_std_new  - base_val
      )
    
    # --- compute y-axis range from widest CI
    y_min <- min(c(sp_data$lci_std_old, sp_data$lci_std_new), na.rm = TRUE)
    y_max <- max(c(sp_data$rci_std_old, sp_data$rci_std_new), na.rm = TRUE)
    span  <- y_max - y_min
    if (!is.finite(span) || span == 0) span <- 1
    pad <- span * 0.1
    sp_data <- sp_data %>%
      mutate(y_min = y_min - pad, y_max = y_max + pad)
    
    sp_data
  })
  
  plot_data <- bind_rows(plot_data_list) %>%
    left_join(label_data, by = "COMMON.NAME") %>%
    mutate(label = paste0(COMMON.NAME,
                          "\nMD=", round(mean_pct_diff, 1),
                          "  WR=", round(width_ratio, 1))) %>%
    group_by(COMMON.NAME) %>%
    filter(timegroups > unique(baseline_year)) %>%
    ungroup() %>%
    dplyr::select(COMMON.NAME, timegroups, baseline_value, baseline_label,
           label, mean_std_old, mean_std_new,
           lci_std_old, lci_std_new, rci_std_old, rci_std_new)
  
  # --- plotting
  p <- ggplot(plot_data, aes(x = timegroups)) +
    geom_ribbon(aes(ymin = lci_std_new, ymax = rci_std_new, fill = "2025 Update - Subsamp change"), alpha = 0.25) +
    geom_ribbon(aes(ymin = lci_std_old, ymax = rci_std_old, fill = "2025 Update"), alpha = 0.4) +
    geom_line(aes(y = mean_std_new, color = "2025 Update - Subsamp change"), size = 1.1) +
    geom_line(aes(y = mean_std_old, color = "2025 Update"), size = 1.1) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.8) +
    geom_text(aes(x = min(timegroups, na.rm = TRUE), y = 0, label = baseline_label),
              hjust = 0, vjust = -0.5, size = 3.2, color = "black") +
    facet_wrap(~ label, scales = "free_y", ncol = 5) +
    labs(
      title = paste("Standardized species trends (batch", i, ")"),
      x = "Year",
      y = "Change relative to baseline (%)"
    ) +
    scale_color_manual(
      name = "Trend Type",
      values = c("2025 Update" = "#1f77b4", "2025 Update - Subsamp change" = "#d62728")
    ) +
    scale_fill_manual(
      name = "Trend Type (CI)",
      values = c("2025 Update" = "#1f77b4", "2025 Update - Subsamp change" = "#d62728")
    ) +
    # --- Y-axis signed labels (+/-)
    scale_y_continuous(
      labels = function(x) ifelse(x > 0, paste0("+", round(x, 0)), round(x, 0))
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    filename = paste0(write_path, i, ".jpg"),
    plot = p, width = 16, height = 16, dpi = 300, bg = "white"
  )
  
  message("Saved: species_trends_batch_", i, ".jpg")
}

target <- "Pied Falconet"

has_target <- sapply(species_batches, function(v) target %in% v)
which(has_target)
