library(dplyr)
library(ggplot2)
library(tidyr)

devtools::load_all()

base_params <- get_parameters(
  overrides = list(
    human_population = 10000*25,
    number_initial_S = 9990*25,
    number_initial_E = 10*25,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time  = 150,
    seed             = 42
  ),
  archetype = "sars_cov_2"
)

# Set riskiness parameters
params_1a <- base_params %>%
  set_setting_specific_riskiness("workplace", mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("school",    mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("leisure",   mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("household", mean = 0, sd = 0.37, min = 0.4472, max = 2.236)

# Create variables to get riskiness values
variables_output <- create_variables(params_1a)
params_with_riskiness <- variables_output$parameters_list

# Run simulation
output_1a <- run_simulation(params_1a)

# Extract result data frame
output_df <- as.data.frame(output_1a$result)

# Print metrics
cat("\n=== KEY METRICS ===\n")
cat("Peak infections:", max(output_df$I_count),
    "at timestep", which.max(output_df$I_count), "\n")
cat("Attack rate:",
    round(max(output_df$R_count) / base_params$human_population * 100, 1), "%\n")
cat("Final susceptible:", tail(output_df$S_count, 1), "\n\n")

# Plot 1: Epidemic curve
p1 <- output_df %>%
  select(timestep, S_count, E_count, I_count, R_count) %>%
  pivot_longer(
    cols      = ends_with("_count"),
    names_to  = "compartment",
    values_to = "count"
  ) %>%
  mutate(
    compartment = factor(
      compartment,
      levels = c("S_count", "E_count", "I_count", "R_count"),
      labels = c("Susceptible", "Exposed", "Infectious", "Recovered")
    )
  ) %>%
  ggplot(aes(x = timestep, y = count, color = compartment)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Susceptible" = "steelblue",
      "Exposed"     = "orange",
      "Infectious"  = "red",
      "Recovered"   = "darkgreen"
    )
  ) +
  labs(
    title = "Epidemic | Direct Riskiness | no-UVC",
    x     = "Timestep",
    y     = "Number of individuals",
    color = "Compartment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# Plot 2: Riskiness distributions by setting
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
  )
)

p2 <- riskiness_data %>%
  ggplot(aes(x = riskiness, fill = setting)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~setting, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Workplace" = "steelblue",
      "School"    = "orange",
      "Leisure"   = "purple",
      "Household" = "darkgreen"
    )
  ) +
  labs(
    title = "Riskiness Distributions by Setting (Direct Riskiness Input)",
    x     = "Relative Riskiness",
    y     = "Count",
    fill  = "Setting"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# Summary statistics for riskiness
riskiness_summary <- riskiness_data %>%
  group_by(setting) %>%
  summarise(
    n_locations = n(),
    mean_riskiness = mean(riskiness),
    median_riskiness = median(riskiness),
    sd_riskiness = sd(riskiness),
    min_riskiness = min(riskiness),
    max_riskiness = max(riskiness)
  )

print(riskiness_summary)
