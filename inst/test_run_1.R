library(dplyr)
library(ggplot2)

devtools::load_all()

base_params <- get_parameters(
  overrides = list(
    human_population = 5000 * 2,
    number_initial_S = 4995 * 2,
    number_initial_E = 5 * 2,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time  = 150,
    seed             = 42
  ),
  archetype = "sars_cov_2"
)

output_1a <- base_params %>%
  set_setting_specific_riskiness("workplace", mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("school",    mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("leisure",   mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  set_setting_specific_riskiness("household", mean = 0, sd = 0.37, min = 0.4472, max = 2.236) %>%
  run_simulation()

# Extract result data frame
output_df <- as.data.frame(output_1a$result)


cat("Peak infections:", max(output_df$I_count),
    "at timestep", which.max(output_df$I_count), "\n")
cat("Attack rate:",
    round(max(output_df$R_count) / base_params$human_population * 100, 1), "%\n")
cat("Final susceptible:", tail(output_df$S_count, 1), "\n\n")

# Visualize epidemic curve
output_df %>%
  select(timestep, S_count, E_count, I_count, R_count) %>%
  tidyr::pivot_longer(
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

