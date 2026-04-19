#' server
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# info logo in name html wrapper
name_with_info <- function(name, description) {
  HTML(
    '<div title="',
    description,
    '">',
    name,
    '<i class="fas fa-info"></i>',
    '</div>')
}

# define . function
if (getRversion() >= "2.15.1") utils::globalVariables(".")


# input checks vec
#' Validate inputs for optim_vec (in Shiny)
#'
#' @noRd
check_vec_inputs <- function(
    N,
    target_mean,
    range,
    tolerance,
    max_iter,
    init_temp,
    cooling_rate,
    max_starts
) {
  if (!is.numeric(N) || length(N) != 1 || N < 10  || N != as.integer(N)) {
    showNotification(
      sprintf("N (%s) must be a single positive integer > 10.", N),
      type = "error"
    )
    return(FALSE)
  }
  bad_idx <- which(target_mean < range[1, ] | target_mean > range[2, ])
  if (length(bad_idx) > 0) {
    bad_vals <- target_mean[bad_idx]
    bad_str <- paste0(head(sort(bad_vals), 5), collapse = ", ")
    min_str <- paste0(range[1, ], collapse = ", ")
    max_str <- paste0(range[2, ], collapse = ", ")
    showNotification(
      sprintf(
        "Each target mean must lie between its min (%s) and max (%s). Offending means: %s",
        min_str, max_str,
        bad_str
      ),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    showNotification(
      sprintf("tolerance (%s) must be a single non-negative numeric value.", tolerance),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    showNotification(
      sprintf("max_iter (%s) must be a single positive number.", max_iter),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0) {
    showNotification(
      sprintf("init_temp (%s) must be a single positive number.", init_temp),
      type = "error"
    )
    return(FALSE)
  }
  if (!(
    (is.numeric(cooling_rate) &&
     length(cooling_rate) == 1 &&
     cooling_rate > 0 &&
     cooling_rate < 1) ||
    is.null(cooling_rate)
  )) {
    showNotification(
      "cooling_rate must be a single number between 0 and 1, or NULL.",
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    showNotification(
      sprintf("max_starts (%s) must be a single positive integer.", max_starts),
      type = "error"
    )
    return(FALSE)
  }
  # all checks passed
  TRUE
}


# input checks aov
#' Validate inputs for optim_aov (in Shiny)
#'
#' @noRd
check_aov_inputs <- function(
    N,
    levels,
    target_group_means,
    range,
    tolerance,
    max_iter,
    init_temp,
    cooling_rate,
    max_starts
) {
  if (!is.numeric(N) || length(N) != 1 || N < 10  || N != as.integer(N)) {
    showNotification(
      sprintf("N (%s) must be a single positive integer > 10.", N),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(levels) || length(levels) < 1 || any(levels != as.integer(levels))) {
    showNotification(
      sprintf("Levels (%s) must be a numeric vector of integers (e.g. 1, 2, 3) with length > 0, specifying the number of levels per factor.", levels),
      type = "error"
    )
    return(FALSE)
  }
  if (any(target_group_means < range[1] | target_group_means > range[2])) {
    showNotification(
      sprintf(
        "All Subgroup means must lie within the outcome range %s and %s. Got means: %s",
        range[1], range[2],
        paste0(head(sort(target_group_means[ target_group_means < range[1] | target_group_means > range[2] ]), 5), collapse = ", ")
      ),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    showNotification(
      sprintf("tolerance (%s) must be a single non-negative numeric value.", tolerance),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    showNotification(
      sprintf("max_iter (%s) must be a single positive number.", max_iter),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0) {
    showNotification(
      sprintf("init_temp (%s) must be a single positive number.", init_temp),
      type = "error"
    )
    return(FALSE)
  }
  if (!(
    (is.numeric(cooling_rate) &&
     length(cooling_rate) == 1 &&
     cooling_rate > 0 &&
     cooling_rate < 1) ||
    is.null(cooling_rate)
  )) {
    showNotification(
      "cooling_rate must be a single number between 0 and 1, or NULL.",
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    showNotification(
      sprintf("max_starts (%s) must be a single positive integer.", max_starts),
      type = "error"
    )
    return(FALSE)
  }
  # all checks passed
  TRUE
}


# input checks lm
#' Validate inputs for optim_mlr (in Shiny)
#'
#' @noRd
check_lm_inputs <- function(
    tolerance,
    max_iter,
    init_temp,
    cooling_rate,
    hill_climbs,
    max_starts
) {
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    showNotification(
      sprintf("tolerance (%s) must be a single non-negative numeric value.", tolerance),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    showNotification(
      sprintf("max_iter (%s) must be a single positive number.", max_iter),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0) {
    showNotification(
      sprintf("init_temp (%s) must be a single positive number.", init_temp),
      type = "error"
    )
    return(FALSE)
  }
  if (!(
    (is.numeric(cooling_rate) &&
     length(cooling_rate) == 1 &&
     cooling_rate > 0 &&
     cooling_rate < 1) ||
    is.null(cooling_rate)
  )) {
    showNotification(
      "cooling_rate must be a single number between 0 and 1, or NULL.",
      type = "error"
    )
    return(FALSE)
  }
  if (!(
    is.null(hill_climbs) ||
    (is.numeric(hill_climbs) && length(hill_climbs) == 1 &&
     hill_climbs >= 0 && hill_climbs == as.integer(hill_climbs))
  )) {
    showNotification(
      sprintf("Total hill climbs (%d) must be a non-negative integer.", hill_climbs),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    showNotification(
      sprintf("max_starts (%s) must be a single positive integer.", max_starts),
      type = "error"
    )
    return(FALSE)
  }
  # all checks passed
  TRUE
}
