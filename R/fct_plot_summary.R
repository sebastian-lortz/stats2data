# --------------------------------------------------------------------------
# stats2data: plot_summary S3 methods
# --------------------------------------------------------------------------

# ---- Internal workhorse --------------------------------------------------

#' Build a target-vs-simulated lollipop chart
#'
#' Shared plotting logic used by all \code{plot_summary.*} methods.
#' Not exported.
#'
#' @param df Data frame with columns \code{Measure}, \code{Variable},
#'   \code{Simulated}, \code{Target}, \code{Centered}, \code{SimulatedType}.
#' @param standardised Logical; whether centered values are standardised.
#' @param x_lab Character; x-axis label.
#' @param title_module Character; module name for the title.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @noRd
.plot_summary_engine <- function(df, standardised, x_lab,
                                 title_module = "") {

  y_label <- if (standardised) {
    "(Simulated \u2013 Target) / Target"
  } else {
    "Simulated \u2013 Target"
  }

  title_prefix <- if (standardised) "Standardised" else "Unstandardised"
  title <- paste(title_prefix, "Difference of Summary Statistics")
  if (nzchar(title_module)) {
    title <- paste0(title, " \u2014 ", title_module)
  }

  if (any(df$SimulatedType == "Unstandardized Diff", na.rm = TRUE)) {
    warning(
      "One or more target values were practically 0; ",
      "the unstandardised difference was computed for these values.",
      call. = FALSE
    )
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data$Variable, y = .data$Centered
  )) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$SimulatedType),
      size = 4, na.rm = TRUE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$Variable, xend = .data$Variable,
        y = .data$Centered, yend = 0
      ),
      color = "gray50", linetype = "dashed", na.rm = TRUE
    ) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dotted", color = "gray50"
    ) +
    ggplot2::facet_wrap(~ .data$Measure, scales = "free_x") +
    ggplot2::scale_color_manual(
      name   = "",
      values = c(
        "Simulated"            = "steelblue",
        "Unstandardized Diff"  = "red"
      )
    ) +
    ggplot2::labs(title = title, y = y_label, x = x_lab) +
    theme_stats2data() +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position   = "bottom",
      strip.background  = ggplot2::element_rect(fill = "gray90", color = "gray50"),
      strip.text        = ggplot2::element_text(face = "bold")
    )

  p
}


# ---- Internal helper: centered difference --------------------------------

#' Compute (optionally standardised) differences
#'
#' @param sim Numeric vector of simulated values.
#' @param target Numeric vector of target values.
#' @param standardised Logical.
#' @param eps Numeric; zero-threshold for standardisation.
#'
#' @return Numeric vector of differences.
#'
#' @noRd
.compute_centered <- function(sim, target, standardised, eps) {
  if (standardised) {
    ifelse(abs(target) < eps, sim - target, (sim - target) / target)
  } else {
    sim - target
  }
}


# ---- Internal helper: tag SimulatedType ----------------------------------

#' Add SimulatedType column to a data frame
#'
#' @param df Data frame with a \code{Target} column.
#' @param standardised Logical.
#' @param eps Numeric.
#'
#' @return The data frame with a \code{SimulatedType} column appended.
#'
#' @noRd
.tag_sim_type <- function(df, standardised, eps) {
  df$SimulatedType <- ifelse(
    standardised & abs(df$Target) < eps,
    "Unstandardized Diff", "Simulated"
  )
  df
}


# ---- S3 methods ----------------------------------------------------------

# -- stats2data_mlr --------------------------------------------------------

#' @describeIn plot_summary Method for MLR results (\code{stats2data_mlr}).
#'
#' @param x An object of class \code{stats2data_mlr}.
#' @param standardised Logical; if \code{TRUE} (default), differences are
#'   divided by target values (except when targets are near zero).
#' @param eps Numeric; threshold below which a target is treated as zero
#'   for standardisation. Default \code{1e-12}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_mlr(...)
#' plot_summary(res)
#' plot_summary(res, standardised = FALSE)
#' }
#'
#' @export
plot_summary.stats2data_mlr <- function(x, standardised = TRUE, eps = 1e-12, ...) {

  if (!is.logical(standardised) || length(standardised) != 1L) {
    stop("`standardised` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(eps) || length(eps) != 1L) {
    stop("`eps` must be a single numeric value.", call. = FALSE)
  }

  inp   <- x$inputs
  stats <- get_stats(x)

  target_reg <- inp$target_reg
  target_cor <- inp$target_cor
  target_se  <- inp$target_se

  reg_dec <- max(count_decimals(target_reg))
  cor_dec <- max(count_decimals(target_cor))

  sim_reg_r <- round(stats$reg, reg_dec)
  sim_cor_r <- round(stats$cor, cor_dec)

  # regression coefficients
  df_reg <- data.frame(
    Measure   = "Regression Coefficient",
    Variable  = names(target_reg),
    Simulated = as.numeric(sim_reg_r),
    Target    = as.numeric(target_reg),
    Centered  = .compute_centered(sim_reg_r, target_reg, standardised, eps),
    stringsAsFactors = FALSE
  )
  df_reg <- .tag_sim_type(df_reg, standardised, eps)

  # correlations
  var_names_cor <- names(target_cor)
  if (is.null(var_names_cor)) {
    var_names_cor <- paste0("Cor", seq_along(target_cor))
  }
  df_cor <- data.frame(
    Measure   = "Correlation",
    Variable  = var_names_cor,
    Simulated = as.numeric(sim_cor_r),
    Target    = as.numeric(target_cor),
    Centered  = .compute_centered(sim_cor_r, target_cor, standardised, eps),
    stringsAsFactors = FALSE
  )
  df_cor <- .tag_sim_type(df_cor, standardised, eps)

  # standard errors (optional)
  df_se <- NULL
  if (!is.null(target_se)) {
    se_dec   <- max(count_decimals(target_se))
    sim_se_r <- round(stats$se, se_dec)
    var_names_se <- if (!is.null(names(target_se))) {
      names(target_se)
    } else {
      names(target_reg)[seq_along(target_se)]
    }
    df_se <- data.frame(
      Measure   = "Standard Error",
      Variable  = var_names_se,
      Simulated = as.numeric(sim_se_r),
      Target    = as.numeric(target_se),
      Centered  = .compute_centered(sim_se_r, target_se, standardised, eps),
      stringsAsFactors = FALSE
    )
    df_se <- .tag_sim_type(df_se, standardised, eps)
  }

  df_all <- dplyr::bind_rows(df_reg, df_cor, df_se) %>%
    dplyr::group_by(.data$Measure) %>%
    dplyr::mutate(
      Variable = factor(.data$Variable, levels = unique(.data$Variable))
    ) %>%
    dplyr::ungroup()

  p <- .plot_summary_engine(df_all, standardised, x_lab = "Parameter",
                            title_module = "MLR Module")
  print(p)
  invisible(p)
}


# -- stats2data_aov --------------------------------------------------------

#' @describeIn plot_summary Method for ANOVA results (\code{stats2data_aov}).
#'
#' @param x An object of class \code{stats2data_aov}.
#' @param standardised Logical; if \code{TRUE} (default), differences are
#'   divided by target values (except when targets are near zero).
#' @param eps Numeric; zero-threshold. Default \code{1e-12}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_aov(...)
#' plot_summary(res)
#' }
#'
#' @export
plot_summary.stats2data_aov <- function(x, standardised = TRUE, eps = 1e-12, ...) {

  if (!is.logical(standardised) || length(standardised) != 1L) {
    stop("`standardised` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(eps) || length(eps) != 1L) {
    stop("`eps` must be a single numeric value.", call. = FALSE)
  }

  inp   <- x$inputs
  stats <- get_stats(x)

  target_F <- inp$target_f_list$F
  F_dec    <- count_decimals(target_F)
  sim_F_r  <- round(stats$F_value, F_dec)

  effect_names <- inp$target_f_list$effect
  if (is.null(effect_names)) {
    effect_names <- paste0("Effect", seq_along(target_F))
  }

  df <- data.frame(
    Measure   = "F Statistic",
    Variable  = effect_names,
    Simulated = as.numeric(sim_F_r),
    Target    = as.numeric(target_F),
    Centered  = .compute_centered(sim_F_r, target_F, standardised, eps),
    stringsAsFactors = FALSE
  )
  df <- .tag_sim_type(df, standardised, eps)

  p <- .plot_summary_engine(df, standardised, x_lab = "Effect",
                            title_module = "ANOVA Module")
  print(p)
  invisible(p)
}


# -- stats2data_vec --------------------------------------------------------

#' @describeIn plot_summary Method for Descriptives results
#'   (\code{stats2data_vec}).
#'
#' @param x An object of class \code{stats2data_vec}.
#' @param standardised Logical; if \code{TRUE} (default), differences are
#'   divided by target values (except when targets are near zero).
#' @param eps Numeric; zero-threshold. Default \code{1e-12}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_vec(...)
#' plot_summary(res)
#' }
#'
#' @export
plot_summary.stats2data_vec <- function(x, standardised = TRUE, eps = 1e-12, ...) {

  if (!is.logical(standardised) || length(standardised) != 1L) {
    stop("`standardised` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(eps) || length(eps) != 1L) {
    stop("`eps` must be a single numeric value.", call. = FALSE)
  }

  inp   <- x$inputs
  stats <- get_stats(x)

  target_mean <- inp$target_mean
  target_sd   <- inp$target_sd
  mean_dec    <- count_decimals(target_mean)
  sd_dec      <- count_decimals(target_sd)

  sim_mean_r <- round(stats$mean, mean_dec)
  sim_sd_r   <- round(stats$sd,   sd_dec)

  vars <- colnames(x$data)
  if (is.null(vars)) vars <- names(target_mean)

  df <- data.frame(
    Variable  = rep(vars, 2L),
    Measure   = rep(c("Mean", "SD"), each = length(vars)),
    Simulated = c(as.numeric(sim_mean_r), as.numeric(sim_sd_r)),
    Target    = c(as.numeric(target_mean), as.numeric(target_sd)),
    Centered  = c(
      .compute_centered(sim_mean_r, target_mean, standardised, eps),
      .compute_centered(sim_sd_r,   target_sd,   standardised, eps)
    ),
    stringsAsFactors = FALSE
  )
  df <- .tag_sim_type(df, standardised, eps)

  df <- df %>%
    dplyr::group_by(.data$Measure) %>%
    dplyr::mutate(
      Variable = factor(.data$Variable, levels = unique(.data$Variable))
    ) %>%
    dplyr::ungroup()

  p <- .plot_summary_engine(df, standardised, x_lab = "Variable",
                            title_module = "Descriptives Module")
  print(p)
  invisible(p)
}
