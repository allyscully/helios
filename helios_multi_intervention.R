# helios_multi_intervention.R
#
# Extends the helios individual-based model to support two additional airborne
# interventions — HEPA filtration and glycol vapor deposition — alongside the
# existing far UV-C intervention.  All three interventions may be deployed
# simultaneously; their efficacies combine multiplicatively (each independently
# reduces airborne pathogen concentration).
#
# Usage pattern:
#   library(helios)
#   source("helios_multi_intervention.R")
#
#   params <- get_parameters_extended()
#   params <- set_uvc(params, setting = "workplace", coverage = 0.5,
#                     coverage_target = "individuals", coverage_type = "random",
#                     efficacy = 0.8, timestep = 10)
#   params <- set_hepa(params, setting = "workplace", coverage = 0.5,
#                      coverage_target = "individuals", coverage_type = "random",
#                      efficacy = 0.7, timestep = 10)
#   params <- set_glycol(params, setting = "school", coverage = 0.3,
#                        coverage_target = "individuals", coverage_type = "random",
#                        efficacy = 0.5, timestep = 0)
#   output <- run_simulation_extended(params)
#
# Functions defined here that shadow or extend helios internals:
#   get_parameters_extended()     - wraps get_parameters(); adds hepa_* and glycol_* defaults
#   set_hepa()                    - user API: configure HEPA for one setting
#   set_glycol()                  - user API: configure glycol vapor for one setting
#   generate_hepa_switches()      - assign per-location HEPA installation indicators
#   generate_joint_hepa_switches()
#   generate_setting_hepa_switches()
#   generate_glycol_switches()    - assign per-location glycol installation indicators
#   generate_joint_glycol_switches()
#   generate_setting_glycol_switches()
#   create_variables_extended()   - wraps create_variables(); generates new intervention switches
#   create_SE_process_extended()  - rewritten FOI loop with multiplicative multi-intervention support
#   create_processes_extended()   - wraps create_processes(); substitutes the extended SE process
#   run_simulation_extended()     - drop-in replacement for run_simulation()


# =============================================================================
# 1.  Extended parameter defaults
# =============================================================================

#' Establish model parameters including HEPA and glycol vapor interventions
#'
#' Wraps [helios::get_parameters()] and appends default values for all
#' HEPA filtration and glycol vapor deposition parameters.  All original
#' helios parameters remain available via \code{overrides}.
#'
#' @inheritParams helios::get_parameters
#'
#' HEPA Filtration Parameters (same structure as far UVC):
#' \describe{
#'   \item{hepa_joint}{boolean; TRUE if HEPA parameterised jointly; default FALSE}
#'   \item{hepa_joint_coverage}{coverage proportion across all settings [0,1]}
#'   \item{hepa_joint_coverage_target}{"individuals" or "square_footage"}
#'   \item{hepa_joint_coverage_type}{"random" or "targeted_riskiness"}
#'   \item{hepa_joint_efficacy}{efficacy of HEPA [0,1]}
#'   \item{hepa_joint_timestep}{deployment timestep}
#'   \item{hepa_workplace / hepa_school / hepa_leisure / hepa_household}{per-setting booleans}
#'   \item{hepa_<setting>_coverage / _coverage_target / _coverage_type / _efficacy / _timestep}{per-setting params}
#' }
#'
#' Glycol Vapor Deposition Parameters (same structure as far UVC):
#' \describe{
#'   \item{glycol_joint}{boolean; TRUE if glycol parameterised jointly; default FALSE}
#'   \item{glycol_joint_coverage / _coverage_target / _coverage_type / _efficacy / _timestep}{joint params}
#'   \item{glycol_workplace / glycol_school / glycol_leisure / glycol_household}{per-setting booleans}
#'   \item{glycol_<setting>_coverage / _coverage_target / _coverage_type / _efficacy / _timestep}{per-setting params}
#' }
#'
#' @export
get_parameters_extended <- function(overrides = list(), archetype = "none") {
  new_defaults <- list(
    # HEPA: Joint
    hepa_joint                    = FALSE,
    hepa_joint_coverage           = NULL,
    hepa_joint_coverage_target    = NULL,
    hepa_joint_coverage_type      = NULL,
    hepa_joint_efficacy           = NULL,
    hepa_joint_timestep           = NULL,
    # HEPA: Workplace
    hepa_workplace                = FALSE,
    hepa_workplace_coverage       = NULL,
    hepa_workplace_coverage_target = NULL,
    hepa_workplace_coverage_type  = NULL,
    hepa_workplace_efficacy       = NULL,
    hepa_workplace_timestep       = NULL,
    # HEPA: School
    hepa_school                   = FALSE,
    hepa_school_coverage          = NULL,
    hepa_school_coverage_target   = NULL,
    hepa_school_coverage_type     = NULL,
    hepa_school_efficacy          = NULL,
    hepa_school_timestep          = NULL,
    # HEPA: Leisure
    hepa_leisure                  = FALSE,
    hepa_leisure_coverage         = NULL,
    hepa_leisure_coverage_target  = NULL,
    hepa_leisure_coverage_type    = NULL,
    hepa_leisure_efficacy         = NULL,
    hepa_leisure_timestep         = NULL,
    # HEPA: Household
    hepa_household                = FALSE,
    hepa_household_coverage       = NULL,
    hepa_household_coverage_target = NULL,
    hepa_household_coverage_type  = NULL,
    hepa_household_efficacy       = NULL,
    hepa_household_timestep       = NULL,

    # Glycol Vapor: Joint
    glycol_joint                    = FALSE,
    glycol_joint_coverage           = NULL,
    glycol_joint_coverage_target    = NULL,
    glycol_joint_coverage_type      = NULL,
    glycol_joint_efficacy           = NULL,
    glycol_joint_timestep           = NULL,
    # Glycol Vapor: Workplace
    glycol_workplace                = FALSE,
    glycol_workplace_coverage       = NULL,
    glycol_workplace_coverage_target = NULL,
    glycol_workplace_coverage_type  = NULL,
    glycol_workplace_efficacy       = NULL,
    glycol_workplace_timestep       = NULL,
    # Glycol Vapor: School
    glycol_school                   = FALSE,
    glycol_school_coverage          = NULL,
    glycol_school_coverage_target   = NULL,
    glycol_school_coverage_type     = NULL,
    glycol_school_efficacy          = NULL,
    glycol_school_timestep          = NULL,
    # Glycol Vapor: Leisure
    glycol_leisure                  = FALSE,
    glycol_leisure_coverage         = NULL,
    glycol_leisure_coverage_target  = NULL,
    glycol_leisure_coverage_type    = NULL,
    glycol_leisure_efficacy         = NULL,
    glycol_leisure_timestep         = NULL,
    # Glycol Vapor: Household
    glycol_household                = FALSE,
    glycol_household_coverage       = NULL,
    glycol_household_coverage_target = NULL,
    glycol_household_coverage_type  = NULL,
    glycol_household_efficacy       = NULL,
    glycol_household_timestep       = NULL
  )

  # Split overrides: base helios params vs new extension params
  base_param_names <- names(helios::get_parameters())
  new_param_names  <- names(new_defaults)

  base_overrides <- overrides[names(overrides) %in% base_param_names]
  ext_overrides  <- overrides[names(overrides) %in% new_param_names]

  unknown_overrides <- names(overrides)[
    !(names(overrides) %in% c(base_param_names, new_param_names))
  ]
  if (length(unknown_overrides) > 0) {
    stop(paste("unknown parameter(s):", paste(unknown_overrides, collapse = ", ")))
  }

  params <- helios::get_parameters(overrides = base_overrides, archetype = archetype)

  for (nm in names(new_defaults)) {
    params[[nm]] <- new_defaults[[nm]]
  }
  for (nm in names(ext_overrides)) {
    params[[nm]] <- ext_overrides[[nm]]
  }

  params
}


# =============================================================================
# 2.  User API: set_hepa() and set_glycol()
# =============================================================================

#' Configure HEPA filtration for a single setting type
#'
#' Mirrors [helios::set_uvc()] exactly, using a \code{hepa_} prefix.
#'
#' @param parameters_list A parameter list from [get_parameters_extended()]
#' @param setting One of "workplace", "school", "leisure", "household", or "joint"
#' @param coverage Fraction of the setting covered [0, 1]
#' @param coverage_target "individuals" or "square_footage"
#' @param coverage_type "random" or "targeted_riskiness"
#' @param efficacy Fractional reduction in FOI [0, 1]
#' @param timestep Deployment timestep (numeric >= 0)
#'
#' @export
set_hepa <- function(
  parameters_list,
  setting,
  coverage,
  coverage_target,
  coverage_type,
  efficacy,
  timestep
) {
  .validate_intervention_args("HEPA", setting, coverage, coverage_target,
                               coverage_type, efficacy)

  parameters_list[[paste0("hepa_", setting)]]                  <- TRUE
  parameters_list[[paste0("hepa_", setting, "_coverage")]]     <- coverage
  parameters_list[[paste0("hepa_", setting, "_coverage_target")]] <- coverage_target
  parameters_list[[paste0("hepa_", setting, "_coverage_type")]]   <- coverage_type
  parameters_list[[paste0("hepa_", setting, "_efficacy")]]     <- efficacy
  parameters_list[[paste0("hepa_", setting, "_timestep")]]     <- timestep

  parameters_list
}


#' Configure glycol vapor deposition for a single setting type
#'
#' Mirrors [helios::set_uvc()] exactly, using a \code{glycol_} prefix.
#'
#' @inheritParams set_hepa
#'
#' @export
set_glycol <- function(
  parameters_list,
  setting,
  coverage,
  coverage_target,
  coverage_type,
  efficacy,
  timestep
) {
  .validate_intervention_args("glycol vapor", setting, coverage, coverage_target,
                               coverage_type, efficacy)

  parameters_list[[paste0("glycol_", setting)]]                  <- TRUE
  parameters_list[[paste0("glycol_", setting, "_coverage")]]     <- coverage
  parameters_list[[paste0("glycol_", setting, "_coverage_target")]] <- coverage_target
  parameters_list[[paste0("glycol_", setting, "_coverage_type")]]   <- coverage_type
  parameters_list[[paste0("glycol_", setting, "_efficacy")]]     <- efficacy
  parameters_list[[paste0("glycol_", setting, "_timestep")]]     <- timestep

  parameters_list
}


# Shared input validation for set_hepa / set_glycol
.validate_intervention_args <- function(
  label, setting, coverage, coverage_target, coverage_type, efficacy
) {
  valid_settings <- c("workplace", "school", "leisure", "household", "joint")
  if (length(setting) != 1 || !(setting %in% valid_settings)) {
    stop(sprintf(
      "Error: setting must be exactly one of: %s",
      paste(valid_settings, collapse = ", ")
    ))
  }
  if (coverage < 0 || coverage > 1) {
    stop(sprintf("Error: %s coverage must be in [0, 1]", label))
  }
  if (length(coverage_target) != 1 ||
      !(coverage_target %in% c("individuals", "square_footage"))) {
    stop(sprintf(
      "Error: %s coverage_target must be 'individuals' or 'square_footage'", label
    ))
  }
  if (length(coverage_type) != 1 ||
      !(coverage_type %in% c("random", "targeted_riskiness"))) {
    stop(sprintf(
      "Error: %s coverage_type must be 'random' or 'targeted_riskiness'", label
    ))
  }
  if (efficacy < 0 || efficacy > 1) {
    stop(sprintf("Error: %s efficacy must be in [0, 1]", label))
  }
}


# =============================================================================
# 3.  Switch generation: HEPA
# =============================================================================

#' Generate per-location HEPA installation indicators
#'
#' Mirrors [helios::generate_far_uvc_switches()].
#'
#' @param parameters_list A parameter list (from [get_parameters_extended()] with
#'   HEPA settings configured via [set_hepa()])
#' @param variables_list A variables list from [create_variables_extended()]
#'
#' @export
generate_hepa_switches <- function(parameters_list, variables_list) {
  setting_types <- c("workplace", "school", "leisure", "household")

  if (
    parameters_list$hepa_joint &&
    any(unlist(parameters_list[paste0("hepa_", setting_types)]))
  ) {
    stop("If hepa_joint is TRUE, setting-type specific hepa switches must be FALSE")
  }

  if (parameters_list$hepa_joint) {
    parameters_list <- generate_joint_hepa_switches(parameters_list, variables_list)
  } else {
    for (setting in setting_types) {
      if (isTRUE(parameters_list[[paste0("hepa_", setting)]])) {
        parameters_list <- generate_setting_hepa_switches(
          parameters_list, variables_list, setting = setting
        )
      }
    }
  }
  parameters_list
}


#' Generate joint HEPA switches across all setting types
#'
#' @inheritParams generate_hepa_switches
#'
#' @export
generate_joint_hepa_switches <- function(parameters_list, variables_list) {
  .generate_joint_intervention_switches(
    parameters_list = parameters_list,
    variables_list  = variables_list,
    prefix          = "hepa"
  )
}


#' Generate HEPA switches for a single setting type
#'
#' @inheritParams generate_hepa_switches
#' @param setting One of "workplace", "school", "leisure", "household"
#'
#' @export
generate_setting_hepa_switches <- function(parameters_list, variables_list, setting) {
  .generate_setting_intervention_switches(
    parameters_list = parameters_list,
    variables_list  = variables_list,
    prefix          = "hepa",
    setting         = setting
  )
}


# =============================================================================
# 4.  Switch generation: Glycol vapor
# =============================================================================

#' Generate per-location glycol vapor installation indicators
#'
#' Mirrors [helios::generate_far_uvc_switches()].
#'
#' @inheritParams generate_hepa_switches
#'
#' @export
generate_glycol_switches <- function(parameters_list, variables_list) {
  setting_types <- c("workplace", "school", "leisure", "household")

  if (
    parameters_list$glycol_joint &&
    any(unlist(parameters_list[paste0("glycol_", setting_types)]))
  ) {
    stop("If glycol_joint is TRUE, setting-type specific glycol switches must be FALSE")
  }

  if (parameters_list$glycol_joint) {
    parameters_list <- generate_joint_glycol_switches(parameters_list, variables_list)
  } else {
    for (setting in setting_types) {
      if (isTRUE(parameters_list[[paste0("glycol_", setting)]])) {
        parameters_list <- generate_setting_glycol_switches(
          parameters_list, variables_list, setting = setting
        )
      }
    }
  }
  parameters_list
}


#' Generate joint glycol vapor switches across all setting types
#'
#' @inheritParams generate_hepa_switches
#'
#' @export
generate_joint_glycol_switches <- function(parameters_list, variables_list) {
  .generate_joint_intervention_switches(
    parameters_list = parameters_list,
    variables_list  = variables_list,
    prefix          = "glycol"
  )
}


#' Generate glycol vapor switches for a single setting type
#'
#' @inheritParams generate_hepa_switches
#' @param setting One of "workplace", "school", "leisure", "household"
#'
#' @export
generate_setting_glycol_switches <- function(parameters_list, variables_list, setting) {
  .generate_setting_intervention_switches(
    parameters_list = parameters_list,
    variables_list  = variables_list,
    prefix          = "glycol",
    setting         = setting
  )
}


# =============================================================================
# 5.  Shared switch-generation helpers (internal)
# =============================================================================

# Builds per-location 0/1 switches for one setting, for any intervention prefix.
# The switch vector is stored as parameters_list[[paste0(prefix, "_switches_", setting)]].
.generate_setting_intervention_switches <- function(
  parameters_list, variables_list, prefix, setting
) {
  cov_target_key <- paste0(prefix, "_", setting, "_coverage_target")
  cov_key        <- paste0(prefix, "_", setting, "_coverage")
  cov_type_key   <- paste0(prefix, "_", setting, "_coverage_type")
  switches_key   <- paste0(prefix, "_switches_", setting)

  if (parameters_list[[cov_target_key]] == "individuals") {
    if (setting == "leisure") {
      setting_size <- parameters_list$setting_sizes$leisure
    } else {
      setting_size <- helios::get_setting_size(variables_list, setting = setting)
    }
  } else if (parameters_list[[cov_target_key]] == "square_footage") {
    size_per_ind <- parameters_list[[paste0("size_per_individual_", setting)]]
    if (setting == "leisure") {
      setting_size <- parameters_list$setting_sizes$leisure * size_per_ind
    } else {
      setting_size <- helios::get_setting_size(variables_list, setting = setting) * size_per_ind
    }
  } else {
    stop(paste(prefix, "coverage_target must be 'individuals' or 'square_footage'"))
  }

  total          <- sum(setting_size)
  switches       <- rep(0L, length(setting_size))
  total_covered  <- floor(parameters_list[[cov_key]] * total)

  if (parameters_list[[cov_type_key]] == "random") {
    running_sum    <- 0
    indices        <- c()
    location_indices <- seq_along(setting_size)
    while (running_sum < total_covered) {
      i              <- sample(location_indices, 1)
      running_sum    <- running_sum + setting_size[i]
      indices        <- c(indices, i)
      location_indices <- setdiff(location_indices, i)
      if (length(location_indices) == 0 && running_sum < total_covered) {
        stop(paste("Insufficient capacity to meet", prefix, "coverage for", setting))
      }
    }
    switches[indices] <- 1L

  } else if (parameters_list[[cov_type_key]] == "targeted_riskiness") {
    riskiness        <- parameters_list[[paste0(setting, "_specific_riskiness")]]
    riskiness_sorted <- sort(riskiness, decreasing = TRUE, index.return = TRUE)
    final_index      <- min(which(
      cumsum(setting_size[riskiness_sorted$ix]) >= total_covered
    ))
    indices          <- riskiness_sorted$ix[seq_len(final_index)]
    switches[indices] <- 1L

  } else {
    stop(paste(prefix, "coverage_type must be 'random' or 'targeted_riskiness'"))
  }

  parameters_list[[switches_key]] <- switches
  parameters_list
}


# Builds per-location 0/1 switches jointly across workplace, school, and leisure
# for any intervention prefix (mirrors generate_joint_far_uvc_switches).
.generate_joint_intervention_switches <- function(
  parameters_list, variables_list, prefix
) {
  cov_target_key <- paste0(prefix, "_joint_coverage_target")
  cov_key        <- paste0(prefix, "_joint_coverage")
  cov_type_key   <- paste0(prefix, "_joint_coverage_type")

  if (parameters_list[[cov_target_key]] == "individuals") {
    setting_size_list <- list(
      workplace = helios::get_setting_size(variables_list, "workplace"),
      school    = helios::get_setting_size(variables_list, "school"),
      leisure   = parameters_list$setting_sizes$leisure
    )
  } else if (parameters_list[[cov_target_key]] == "square_footage") {
    setting_size_list <- list(
      workplace = helios::get_setting_size(variables_list, "workplace") *
        parameters_list$size_per_individual_workplace,
      school    = helios::get_setting_size(variables_list, "school") *
        parameters_list$size_per_individual_school,
      leisure   = parameters_list$setting_sizes$leisure *
        parameters_list$size_per_individual_leisure
    )
  } else {
    stop(paste(prefix, "_joint_coverage_target must be 'individuals' or 'square_footage'"))
  }

  size_flat   <- unlist(setting_size_list, use.names = FALSE)
  total_size  <- sum(size_flat)
  n_locations <- length(size_flat)
  switches    <- rep(0L, n_locations)
  target_size <- total_size * parameters_list[[cov_key]]

  if (parameters_list[[cov_type_key]] == "random") {
    running_sum    <- 0
    indices        <- c()
    location_indices <- seq_len(n_locations)
    while (running_sum < target_size) {
      i            <- sample(location_indices, 1)
      running_sum  <- running_sum + size_flat[i]
      indices      <- c(indices, i)
      location_indices <- setdiff(location_indices, i)
      if (length(location_indices) == 0 && running_sum < target_size) {
        stop(paste("Insufficient capacity to meet joint", prefix, "coverage"))
      }
    }
  } else if (parameters_list[[cov_type_key]] == "targeted_riskiness") {
    riskiness_list <- list(
      workplace = parameters_list$workplace_specific_riskiness,
      school    = parameters_list$school_specific_riskiness,
      leisure   = parameters_list$leisure_specific_riskiness
    )
    riskiness_flat   <- unlist(riskiness_list, use.names = FALSE)
    riskiness_sorted <- sort(riskiness_flat, decreasing = TRUE, index.return = TRUE)
    final_index      <- min(which(
      cumsum(size_flat[riskiness_sorted$ix]) >= target_size
    ))
    indices <- riskiness_sorted$ix[seq_len(final_index)]
  } else {
    stop(paste(prefix, "_joint_coverage_type must be 'random' or 'targeted_riskiness'"))
  }
  switches[indices] <- 1L

  setting_name_index <- rep(names(setting_size_list), lengths(setting_size_list))
  for (s in names(setting_size_list)) {
    parameters_list[[paste0(prefix, "_switches_", s)]] <-
      switches[setting_name_index == s]
  }
  parameters_list
}


# =============================================================================
# 6.  Extended create_variables()
# =============================================================================

#' Create model variables and generate all intervention switches
#'
#' Wraps [helios::create_variables()] and additionally generates per-location
#' installation indicator vectors for HEPA filtration and glycol vapor deposition.
#' If either intervention is configured with \code{joint = TRUE}, the joint
#' efficacy and timestep are propagated to each setting exactly as helios does for
#' far UV-C.
#'
#' @param parameters_list A parameter list from [get_parameters_extended()]
#'
#' @export
create_variables_extended <- function(parameters_list) {
  result           <- helios::create_variables(parameters_list)
  variables_list   <- result$variables_list
  parameters_list  <- result$parameters_list

  setting_types <- c("workplace", "school", "leisure", "household")

  # HEPA switches
  if (any(
    parameters_list$hepa_joint,
    parameters_list$hepa_workplace,
    parameters_list$hepa_school,
    parameters_list$hepa_leisure,
    parameters_list$hepa_household
  )) {
    parameters_list <- generate_hepa_switches(parameters_list, variables_list)

    if (isTRUE(parameters_list$hepa_joint)) {
      covered_settings <- c("workplace", "school", "leisure")
      parameters_list[paste0("hepa_", covered_settings)] <- TRUE
      parameters_list[paste0("hepa_", covered_settings, "_efficacy")] <-
        parameters_list$hepa_joint_efficacy
      parameters_list[paste0("hepa_", covered_settings, "_timestep")] <-
        parameters_list$hepa_joint_timestep
    }
  }

  # Glycol vapor switches
  if (any(
    parameters_list$glycol_joint,
    parameters_list$glycol_workplace,
    parameters_list$glycol_school,
    parameters_list$glycol_leisure,
    parameters_list$glycol_household
  )) {
    parameters_list <- generate_glycol_switches(parameters_list, variables_list)

    if (isTRUE(parameters_list$glycol_joint)) {
      covered_settings <- c("workplace", "school", "leisure")
      parameters_list[paste0("glycol_", covered_settings)] <- TRUE
      parameters_list[paste0("glycol_", covered_settings, "_efficacy")] <-
        parameters_list$glycol_joint_efficacy
      parameters_list[paste0("glycol_", covered_settings, "_timestep")] <-
        parameters_list$glycol_joint_timestep
    }
  }

  list(variables_list = variables_list, parameters_list = parameters_list)
}


# =============================================================================
# 7.  Extended SE process with multiplicative multi-intervention FOI
# =============================================================================

#' Create the S→E process with combined far UV-C, HEPA, and glycol efficacies
#'
#' Replaces [helios::create_SE_process()].  For each location in each setting,
#' a combined FOI reduction factor is computed as the product of
#' \code{(1 - efficacy_i)} across all active interventions installed in that
#' location.  If no interventions are active the FOI is unchanged.
#'
#' @inheritParams helios::create_processes
#'
#' @export
create_SE_process_extended <- function(
  variables_list,
  events_list,
  parameters_list,
  renderer
) {
  # Pre-calculate structural quantities (identical to helios::create_SE_process)

  num_households <- max(as.numeric(variables_list$household$get_categories()))
  household_bitset_list <- vector("list", num_households)
  household_index_list  <- vector("list", num_households)
  household_size_list   <- vector("list", num_households)
  for (i in seq(num_households)) {
    household_bitset_list[[i]] <-
      variables_list$household$get_index_of(as.character(i))
    household_index_list[[i]]  <- household_bitset_list[[i]]$to_vector()
    household_size_list[[i]]   <- length(household_index_list[[i]])
  }

  num_workplaces <- max(as.numeric(variables_list$workplace$get_categories()))
  workplace_bitset_list <- vector("list", num_workplaces)
  workplace_index_list  <- vector("list", num_workplaces)
  workplace_size_list   <- vector("list", num_workplaces)
  for (i in seq(num_workplaces)) {
    workplace_bitset_list[[i]] <-
      variables_list$workplace$get_index_of(as.character(i))
    workplace_index_list[[i]]  <- workplace_bitset_list[[i]]$to_vector()
    workplace_size_list[[i]]   <- length(workplace_index_list[[i]])
  }

  num_schools <- max(as.numeric(variables_list$school$get_categories()))
  school_bitset_list <- vector("list", num_schools)
  school_index_list  <- vector("list", num_schools)
  school_size_list   <- vector("list", num_schools)
  for (i in seq(num_schools)) {
    school_bitset_list[[i]] <-
      variables_list$school$get_index_of(as.character(i))
    school_index_list[[i]]  <- school_bitset_list[[i]]$to_vector()
    school_size_list[[i]]   <- length(school_index_list[[i]])
  }

  num_leisure <- length(parameters_list$setting_sizes$leisure)
  leisure_individual_possible_visits_list <- vector(
    "list", parameters_list$human_population
  )
  for (i in seq(parameters_list$human_population)) {
    leisure_individual_possible_visits_list[[i]] <-
      unlist(variables_list$leisure$get_values(i))
  }

  # The process closure
  function(t) {
    I <- variables_list$disease_state$get_index_of("I_mild")

    #=== Household FOI ===#
    household_FOI <- vector("numeric", parameters_list$human_population)
    for (i in seq(num_households)) {
      if (household_size_list[[i]] > 1) {
        spec_I <- individual:::bitset_count_and(I, household_bitset_list[[i]])
        reduction <- .combined_reduction(parameters_list, "household", i, t)
        spec_household_FOI <-
          parameters_list$household_specific_riskiness[i] *
          reduction *
          parameters_list$beta_household *
          spec_I / household_size_list[[i]]
        household_FOI[household_index_list[[i]]] <- spec_household_FOI
      }
    }

    #=== Workplace FOI ===#
    workplace_FOI <- vector("numeric", parameters_list$human_population)
    for (i in seq(num_workplaces)) {
      spec_I <- individual:::bitset_count_and(I, workplace_bitset_list[[i]])
      reduction <- .combined_reduction(parameters_list, "workplace", i, t)
      spec_workplace_FOI <-
        parameters_list$workplace_specific_riskiness[i] *
        reduction *
        parameters_list$beta_workplace *
        spec_I / workplace_size_list[[i]]
      workplace_FOI[workplace_index_list[[i]]] <- spec_workplace_FOI
    }

    #=== School FOI ===#
    school_FOI <- vector("numeric", parameters_list$human_population)
    for (i in seq(num_schools)) {
      spec_I <- individual:::bitset_count_and(I, school_bitset_list[[i]])
      reduction <- .combined_reduction(parameters_list, "school", i, t)
      spec_school_FOI <-
        parameters_list$school_specific_riskiness[i] *
        reduction *
        parameters_list$beta_school *
        spec_I / school_size_list[[i]]
      school_FOI[school_index_list[[i]]] <- spec_school_FOI
    }

    #=== Leisure FOI ===#
    if ((t * parameters_list$dt) == floor(t * parameters_list$dt)) {
      leisure_visit <- vector("numeric", parameters_list$human_population)
      for (i in seq(parameters_list$human_population)) {
        leisure_visit[i] <-
          leisure_individual_possible_visits_list[[i]][
            dqrng::dqsample.int(n = 7, size = 1)
          ]
      }
      variables_list$specific_leisure$initialize(
        categories    = as.character(parameters_list$leisure_indices),
        initial_values = as.character(leisure_visit)
      )
    }

    leisure_FOI <- vector("numeric", parameters_list$human_population)
    leisure_locations <- variables_list$specific_leisure$get_categories()
    leisure_locations <- leisure_locations[leisure_locations != "0"]
    for (i in seq_along(leisure_locations)) {
      spec_leisure_location <- as.numeric(leisure_locations[i])
      if (spec_leisure_location != 0) {
        spec_leisure <- variables_list$specific_leisure$get_index_of(
          as.character(spec_leisure_location)
        )
        spec_I     <- individual:::bitset_count_and(I, spec_leisure)
        reduction  <- .combined_reduction(parameters_list, "leisure", i, t)
        spec_leisure_FOI <-
          parameters_list$leisure_specific_riskiness[i] *
          reduction *
          parameters_list$beta_leisure *
          spec_I / spec_leisure$size()
        leisure_FOI[spec_leisure$to_vector()] <- spec_leisure_FOI
      }
    }

    #=== Community FOI ===#
    community_FOI <-
      parameters_list$beta_community *
      variables_list$disease_state$get_size_of("I_mild") /
      parameters_list$human_population

    #=== Total FOI ===#
    total_FOI <- household_FOI + workplace_FOI + school_FOI + leisure_FOI + community_FOI

    if (parameters_list$render_diagnostics) {
      renderer$render("FOI_household",  max(household_FOI),  t)
      renderer$render("FOI_workplace",  max(workplace_FOI),  t)
      renderer$render("FOI_school",     max(school_FOI),     t)
      renderer$render("FOI_leisure",    max(leisure_FOI),    t)
      renderer$render("FOI_community",  max(community_FOI),  t)
      renderer$render("FOI_total",      max(total_FOI),      t)
    }

    p_inf <- 1 - exp(-total_FOI * parameters_list$dt)
    S     <- variables_list$disease_state$get_index_of("S")
    S$sample(rate = p_inf[S$to_vector()])
    renderer$render("E_new", S$size(), t)
    variables_list$disease_state$queue_update(value = "E", index = S)
  }
}


# Compute the combined FOI reduction factor for a given setting, location index,
# and timestep.  Returns a scalar in (0, 1].
#
# The factor is the product of (1 - efficacy_j) over all interventions j whose:
#   (a) master switch for this setting is TRUE,
#   (b) per-location switch for location i is 1, and
#   (c) deployment timestep has been reached.
#
# Intervention switches are stored under:
#   far UV-C:   parameters_list[["uvc_<setting>"]][i]
#   HEPA:       parameters_list[["hepa_switches_<setting>"]][i]
#   glycol:     parameters_list[["glycol_switches_<setting>"]][i]
.combined_reduction <- function(parameters_list, setting, i, t) {
  reduction <- 1.0

  # Far UV-C
  uvc_master <- parameters_list[[paste0("far_uvc_", setting)]]
  if (isTRUE(uvc_master)) {
    uvc_switches <- parameters_list[[paste0("uvc_", setting)]]
    uvc_timestep <- parameters_list[[paste0("far_uvc_", setting, "_timestep")]]
    uvc_efficacy <- parameters_list[[paste0("far_uvc_", setting, "_efficacy")]]
    if (!is.null(uvc_switches) &&
        length(uvc_switches) >= i &&
        uvc_switches[i] == 1 &&
        t > uvc_timestep) {
      reduction <- reduction * (1 - uvc_efficacy)
    }
  }

  # HEPA filtration
  hepa_master <- parameters_list[[paste0("hepa_", setting)]]
  if (isTRUE(hepa_master)) {
    hepa_switches <- parameters_list[[paste0("hepa_switches_", setting)]]
    hepa_timestep <- parameters_list[[paste0("hepa_", setting, "_timestep")]]
    hepa_efficacy <- parameters_list[[paste0("hepa_", setting, "_efficacy")]]
    if (!is.null(hepa_switches) &&
        length(hepa_switches) >= i &&
        hepa_switches[i] == 1 &&
        t > hepa_timestep) {
      reduction <- reduction * (1 - hepa_efficacy)
    }
  }

  # Glycol vapor deposition
  glycol_master <- parameters_list[[paste0("glycol_", setting)]]
  if (isTRUE(glycol_master)) {
    glycol_switches <- parameters_list[[paste0("glycol_switches_", setting)]]
    glycol_timestep <- parameters_list[[paste0("glycol_", setting, "_timestep")]]
    glycol_efficacy <- parameters_list[[paste0("glycol_", setting, "_efficacy")]]
    if (!is.null(glycol_switches) &&
        length(glycol_switches) >= i &&
        glycol_switches[i] == 1 &&
        t > glycol_timestep) {
      reduction <- reduction * (1 - glycol_efficacy)
    }
  }

  reduction
}


# =============================================================================
# 8.  Extended create_processes()
# =============================================================================

#' Create all model processes, substituting the extended SE process
#'
#' Identical to [helios::create_processes()] except that it uses
#' [create_SE_process_extended()] for the S→E transition, which supports
#' simultaneous far UV-C, HEPA, and glycol vapor interventions.
#'
#' @inheritParams helios::create_processes
#'
#' @export
create_processes_extended <- function(
  variables_list,
  events_list,
  parameters_list,
  renderer
) {
  processes_list <- list(
    SE_process = create_SE_process_extended(
      variables_list  = variables_list,
      events_list     = events_list,
      parameters_list = parameters_list,
      renderer        = renderer
    ),
    EI_process = helios::create_EI_process(
      variables_list  = variables_list,
      events_list     = events_list,
      parameters_list = parameters_list,
      renderer        = renderer
    ),
    I_mild_R_process = helios::create_I_mild_R_process(
      variables_list  = variables_list,
      events_list     = events_list,
      parameters_list = parameters_list,
      renderer        = renderer
    ),
    I_hosp_exit_process = helios::create_I_hosp_exit_process(
      variables_list  = variables_list,
      events_list     = events_list,
      parameters_list = parameters_list,
      renderer        = renderer
    )
  )

  if (parameters_list$endemic_or_epidemic == "endemic") {
    processes_list <- c(
      processes_list,
      list(RS_process = helios::create_RS_process(
        variables_list  = variables_list,
        events_list     = events_list,
        parameters_list = parameters_list
      )),
      list(external_source_process = helios::create_external_source_process(
        variables_list  = variables_list,
        events_list     = events_list,
        parameters_list = parameters_list,
        renderer        = renderer
      ))
    )
  }

  processes_list <- c(
    processes_list,
    renderer = individual::categorical_count_renderer_process(
      renderer,
      variables_list$disease_state,
      c("S", "E", "I_mild", "I_hosp", "R", "D")
    )
  )

  processes_list
}


# =============================================================================
# 9.  Extended run_simulation()
# =============================================================================

#' Run the helios model with HEPA and glycol vapor intervention support
#'
#' Drop-in replacement for [helios::run_simulation()].  Uses the extended
#' versions of [create_variables_extended()] and [create_processes_extended()].
#'
#' @param parameters_list A parameter list built with [get_parameters_extended()]
#'   and optionally configured with [helios::set_uvc()], [set_hepa()], and/or
#'   [set_glycol()].
#'
#' @return A data frame of rendered outputs (same format as [helios::run_simulation()])
#'
#' @export
run_simulation_extended <- function(parameters_list) {
  if (!is.null(parameters_list$seed)) {
    set.seed(parameters_list$seed)
    dqrng::dqset.seed(parameters_list$seed)
  }

  result          <- create_variables_extended(parameters_list)
  variables_list  <- result$variables_list
  parameters_list <- result$parameters_list

  events_list <- helios::create_events(parameters_list)

  renderer <- individual::Render$new(
    timesteps = parameters_list$simulation_time / parameters_list$dt
  )

  processes_list <- create_processes_extended(
    variables_list  = variables_list,
    events_list     = events_list,
    parameters_list = parameters_list,
    renderer        = renderer
  )

  individual::simulation_loop(
    variables = variables_list,
    events    = events_list,
    processes = processes_list,
    timesteps = parameters_list$simulation_time / parameters_list$dt
  )

  renderer$to_dataframe()
}
