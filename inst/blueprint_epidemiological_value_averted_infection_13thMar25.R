library(helios)
library(individual)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(purrr)

theme_set(theme_minimal())
blueprint_colours <- colorRampPalette(c("#00AFFF", "#03113E"))(4)
blueprint_colours_min <- c(blueprint_colours[1], blueprint_colours[4])

endemic_summary_outputs <- readRDS(
  "inst/blueprint_output_3_Sep9/Report_3_Endemic/Endemic_Simulation_Batch_3/endemic_summary_outputs.rds"
)

targetable_transmission <- 0.6
df_comparison <- endemic_summary_outputs %>%
  filter(coverage_type == "random") %>%
  rowwise() %>%
  mutate(
    hypothetical_reduction = targetable_transmission * coverage * efficacy
  ) %>%
  group_by(archetype, coverage, efficacy, coverage_type) %>%
  summarise(
    incidence_percentage_reduction_mean = mean(reduction_incidence),
    incidence_percentage_reduction_lower = min(reduction_incidence),
    incidence_percentage_reduction_upper = max(reduction_incidence),
    hypothetical_reduction = mean(hypothetical_reduction)
  ) %>%
  mutate(
    Reff = ifelse(
      archetype == "flu",
      1.5 * (1 - hypothetical_reduction),
      2.5 * (1 - hypothetical_reduction)
    )
  )


ggplot(
  data = df_comparison,
  aes(
    x = 100 * hypothetical_reduction,
    y = 100 * incidence_percentage_reduction_mean,
    col = coverage
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = 100 * incidence_percentage_reduction_lower,
      ymax = 100 * incidence_percentage_reduction_upper,
      col = coverage
    ),
    linewidth = 1
  ) +
  geom_abline(slope = 1, linetype = "dashed") +
  facet_grid(factor(efficacy) ~ archetype) +
  labs(
    x = "Reduction in Annual Incidence\n Predicted By Simple Model",
    y = "Reduction in Annual Incidence\nPredicted by helios (%)"
  ) +
  theme_bw()

b <- ggplot(
  data = df_comparison,
  aes(
    x = Reff,
    y = 100 *
      incidence_percentage_reduction_mean /
      (100 * hypothetical_reduction),
    col = hypothetical_reduction
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = 100 *
        incidence_percentage_reduction_lower /
        (100 * hypothetical_reduction),
      ymax = 100 *
        incidence_percentage_reduction_upper /
        (100 * hypothetical_reduction),
      col = hypothetical_reduction
    ),
    linewidth = 1
  ) +
  facet_grid(. ~ archetype, scales = "free") +
  coord_cartesian(xlim = c(1, NA)) +
  labs(
    x = "(very) approximate reproduction number",
    y = "Epidemiological Value of Averted Infection",
    col = "Prop reduction\nin incidence\npredicted by\nsimple model"
  ) +
  theme_bw()

a <- ggplot(
  data = df_comparison,
  aes(
    x = 1 - hypothetical_reduction,
    y = 100 *
      incidence_percentage_reduction_mean /
      (100 * hypothetical_reduction),
    col = hypothetical_reduction
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = 100 *
        incidence_percentage_reduction_lower /
        (100 * hypothetical_reduction),
      ymax = 100 *
        incidence_percentage_reduction_upper /
        (100 * hypothetical_reduction),
      col = hypothetical_reduction
    ),
    linewidth = 1
  ) +
  facet_grid(. ~ archetype, scales = "free_y") +
  coord_cartesian(xlim = c(0.6, 1)) +
  labs(
    x = "Proportion of Transmission Remaining Predicted By Simple Model\n(1 - Prop Reduction Predicted by Simple Model)",
    y = "Epidemiological Value of Averted Infection",
    col = "Prop reduction\nin incidence\npredicted by\nsimple model"
  ) +
  theme_bw()

cowplot::plot_grid(a, b, nrow = 2)
# flu is 1.5, sc2 is 2.5
