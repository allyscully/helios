library(dplyr)
library(ggplot2)

# 1. Prepare the first dataset (Direct Riskiness)
riskiness_data <- data.frame(
  setting = c(
    rep("Workplace", length(params_with_riskiness$workplace_specific_riskiness)),
    rep("School", length(params_with_riskiness$school_specific_riskiness)),
    rep("Leisure", length(params_with_riskiness$leisure_specific_riskiness)),
    rep("Household", length(params_with_riskiness$household_specific_riskiness))
  ),
  riskiness = c(
    params_with_riskiness$workplace_specific_riskiness,
    params_with_riskiness$school_specific_riskiness,
    params_with_riskiness$leisure_specific_riskiness,
    params_with_riskiness$household_specific_riskiness
  ),
  Source = "Direct Riskiness" # Add identifier
)

# 2. Prepare the second dataset (ACH Riskiness)
# Assuming ACH_riskiness_250k has columns 'setting' and 'riskiness'
# If it is structured like your first list, repeat the data.frame creation above for it.
ACH_riskiness_250k$Source <- "ACH"

# 3. Combine both datasets
combined_data <- bind_rows(riskiness_data, ACH_riskiness_250k)

# 4. Plot both datasets together
p2 <- combined_data %>%
  ggplot(aes(x = riskiness, fill = Source)) + # Fill by Source to compare them
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") + # 'identity' allows overlap
  facet_wrap(~setting, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Direct Riskiness" = "steelblue",
      "ACH"              = "firebrick"
    )
  ) +
  labs(
    title = "Comparison: Direct Riskiness vs. ACH by Setting",
    x     = "Relative Riskiness",
    y     = "Count",
    fill  = "Dataset Source"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold") # Makes facet labels bold
  )

print(p2)

# 5. Updated Summary statistics comparing both sources
riskiness_summary <- combined_data %>%
  group_by(setting, Source) %>%
  summarise(
    n_locations = n(),
    mean_val = mean(riskiness, na.rm = TRUE),
    median_val = median(riskiness, na.rm = TRUE),
    sd_val = sd(riskiness, na.rm = TRUE),
    .groups = "drop"
  )

print(riskiness_summary)
