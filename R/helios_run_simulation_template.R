#installing and call packages
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("individual", quietly = TRUE)) {
  remotes::install_github("mrc-ide/individual@feat/logi_size", upgrade = "never")
}

for (pkg in c("ggplot2", "dplyr", "tidyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

if (!requireNamespace("helios", quietly = TRUE)) {
  remotes::install_github("mrc-ide/helios", upgrade = "never")
}

## only need to run the section above once

library(helios)
library(ggplot2)
library(dplyr)
library(tidyr)


# Build the parameter list
# Each helios run starts from a parameter list created by get_parameters().
# If you call get_parameters() with no arguments it returns the full set of sensible defaults.
# To change a parameter, pass a named list to `overrides`.

parameters_baseline <- get_parameters(
  overrides = list(
    seed = 1,
    human_population = 20000,
    number_initial_S = 19995,
    number_initial_E = 5,
    number_initial_I = 0,
    number_initial_R = 0,
    endemic_or_epidemic = "epidemic",
    simulation_time = 150
  )
)

parameters_intv <- set_uvc(
  parameters_list = parameters_baseline, #use the baseline parameters for population + intervention added
  setting = "workplace", #options are "workplace", "school", "household", "leisure", or "joint" (covers work )
  coverage = 0.6, # proportion of {sqft or people} covered, range 0 to 1
  coverage_target = "square_footage", #can also be set to "individual". determines if coverage % is # of people or sq ft
  coverage_type = "random", #options are "random" or "targeted" (selects riskiest locations to target)
  efficacy = 0.8, #efficacy of intervention, range 0 to 1
  timestep = 0 # when the intervention is turned on. helpful for endemic sims where you want to establish a baseline first and then turn it on
  )


# Running the simulation
## run_simulation() runs the simulation returns a list with two parts:
##   $result -> a data frame, one row per time-step, with counts in each
##              disease state (S/E/I/R), hospitalisations, deaths, etc.

output_baseline <- run_simulation(parameters_list = parameters_baseline) #running simulation for baseline
output_intv <- run_simulation(parameters_list = parameters_intv) #run simulation with intervention

results_baseline <- output_baseline$result
results_intv      <- output_intv$result

# Helper to reshape a results data frame into long SEIR form
make_plot_data <- function(results, dt) {
  results |>
    mutate(
      day = timestep * dt,
      Infectious = I_mild_count + I_hosp_count
    ) |>
    select(
      day,
      Susceptible = S_count,
      Exposed     = E_count,
      Infectious,
      Recovered   = R_count
    ) |>
    pivot_longer(
      cols = -day,
      names_to = "compartment",
      values_to = "count"
    ) |>
    mutate(
      compartment = factor(
        compartment,
        levels = c("Susceptible", "Exposed", "Infectious", "Recovered")
      )
    )
}

plot_data_baseline <- make_plot_data(results_baseline, parameters_baseline$dt)
plot_data_intv      <- make_plot_data(results_intv, parameters_intv$dt)

# Plot 1: baseline SEIR curves
plot_baseline <- ggplot(plot_data_baseline, aes(x = day, y = count, colour = compartment)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Baseline simulation",
    x = "Day",
    y = "Number of individuals",
    colour = "Disease state"
  ) +
  theme_minimal()

print(plot_baseline)

# Plot 2: Air Quality Intervention SEIR curves
plot_intv <- ggplot(plot_data_intv, aes(x = day, y = count, colour = compartment)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Simulation with workplace AQI",
    x = "Day",
    y = "Number of individuals",
    colour = "Disease state"
  ) +
  theme_minimal()

print(plot_intv)

# Plot 3: comparison of infectious curves between scenarios
comparison_data <- bind_rows(
  results_baseline |> mutate(day = timestep * parameters_baseline$dt,
                             Infectious = I_mild_count + I_hosp_count,
                             scenario = "Baseline"),
  results_intv |> mutate(day = timestep * parameters_intv$dt,
                        Infectious = I_mild_count + I_hosp_count,
                        scenario = "AQI (workplace)")
) |>
  select(day, Infectious, scenario)

plot_comparison <- ggplot(comparison_data, aes(x = day, y = Infectious, colour = scenario)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Infectious individuals: baseline vs intervention",
    x = "Day",
    y = "Number infectious (mild + hospitalised)",
    colour = "Scenario"
  ) +
  theme_minimal()

print(plot_comparison)
