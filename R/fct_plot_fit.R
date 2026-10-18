# stats2data: plot_fit S3 methods


#' Move near-zero-target parameters into their own panel
#'
#' @param df Data frame with \code{Measure} and \code{SimulatedType} columns.
#'
#' @return \code{df} with \code{Measure} as character, suffixed with
#'   \code{" (raw)"} for fall-back rows.
#'
#' @noRd
.tag_raw_panel <- function(df) {
  is_raw <- !is.na(df$SimulatedType) &
    df$SimulatedType == "Unstandardised Diff"
  df$Measure <- as.character(df$Measure)
  df$Measure[is_raw] <- paste0(df$Measure[is_raw], " (raw)")
  df
}


#' Apply the same panel relabelling to a secondary layer frame
#'
#' @param df Data frame with \code{Measure} and \code{Variable} columns.
#' @param ref The aggregated frame, before \code{.tag_raw_panel} was applied.
#'
#' @return \code{df} with \code{Measure} relabelled to match.
#'
#' @noRd
.match_raw_panel <- function(df, ref) {
  is_raw <- !is.na(ref$SimulatedType) &
    ref$SimulatedType == "Unstandardised Diff"
  if (!any(is_raw)) {
    df$Measure <- as.character(df$Measure)
    return(df)
  }
  keys <- paste(ref$Measure[is_raw], ref$Variable[is_raw], sep = "\r")
  hit  <- paste(df$Measure, df$Variable, sep = "\r") %in% keys
  df$Measure <- as.character(df$Measure)
  df$Measure[hit] <- paste0(df$Measure[hit], " (raw)")
  df
}


#' Fill for the region
#'
#' @noRd
.s2d_band <- "grey95"


#' Build a target-vs-simulated lollipop chart
#'
#'
#' @param df Data frame with columns \code{Measure}, \code{Variable},
#'   \code{Simulated}, \code{Target}, \code{Centered}, \code{SimulatedType}.
#' @param standardised Logical; whether centered values are standardised.
#' @param var_lab Character; label for the variable axis (y).
#' @param order Character; \code{"input"} (default) keeps the order in which
#'   the parameters were supplied, \code{"fit"} sorts them by the absolute
#'   difference, worst fit first.
#' @param thresh Numeric or \code{NULL}; the convergence threshold of the
#'   optimisation run, on the raw difference scale.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @noRd
.plot_fit_engine <- function(df, standardised, var_lab,
                             order = c("input", "fit"),
                             thresh = NULL) {

  order <- match.arg(order)
   half_max   <- NULL
  if (!is.null(thresh)) {
        if (!is.numeric(thresh) || length(thresh) != 1L || !is.finite(thresh) ||
        thresh < 0) {
      stop("`thresh` must be NULL or a single non-negative finite number.",
           call. = FALSE)
    }
  }

  x_label <- if (standardised) {
    "Std. Difference: (Simulated - Target) / Target"
  } else {
    "Difference: Simulated - Target"
  }

   has_fallback <- any(df$SimulatedType == "Unstandardised Diff", na.rm = TRUE)
  if (has_fallback) {
    warning(
      "One or more target values were practically 0; ",
      "the unstandardised difference was computed for these values.",
      call. = FALSE
    )
    df <- .tag_raw_panel(df)
    x_label <- paste0(
       x_label,
      "\nPanels marked \u201craw\u201d: Simulated - Target, target \u2248 0"
    )
  }

  if (order == "fit") {
    vals <- abs(df$Centered)
    vals[!is.finite(vals)] <- NA_real_
     key <- tapply(vals, as.character(df$Variable), function(v) {
      v <- v[!is.na(v)]
      if (length(v) == 0L) -Inf else max(v)
    })
      df$Variable <- factor(
      as.character(df$Variable),
      levels = names(sort(key, decreasing = TRUE))
    )
  }

  band_layer <- NULL
   band_note  <- NULL
  if (!is.null(thresh) && thresh > 0) {
    half <- if (standardised) {
      ifelse(
        !is.na(df$SimulatedType) & df$SimulatedType == "Unstandardised Diff",
        thresh,
          thresh / abs(df$Target)
      )
    } else {
      rep(thresh, nrow(df))
    }
    half[!is.finite(half)] <- NA_real_
     uniq <- unique(half[!is.na(half)])
    if (length(uniq) > 0L) half_max <- max(uniq)

    if (length(uniq) == 1L) {
      band_layer <- ggplot2::annotate(
        "rect",
        xmin = -uniq, xmax = uniq, ymin = -Inf, ymax = Inf,
        fill = .s2d_band
      )
    } else if (length(uniq) > 1L) {
      band_df <- data.frame(
         Measure  = df$Measure,
        Variable = df$Variable,
        Half     = half
      )
      band_layer <- ggplot2::geom_tile(
         data        = band_df,
        mapping     = ggplot2::aes(
          x = 0, y = .data$Variable, width = 2 * .data$Half
        ),
        height      = 0.85,
        fill        = .s2d_band,
         color       = NA,
        inherit.aes = FALSE,
        na.rm       = TRUE
      )
      band_note <- paste0(
        "Shaded: objective threshold, \u00b1", format(thresh, digits = 3),
        " on the raw scale, rescaled per parameter by |Target|."
      )
    }
  }

   span <- abs(df$Centered[is.finite(df$Centered)])
  span <- if (length(span) == 0L) 0 else max(span)
  if (!is.null(half_max)) span <- max(span, 2 * half_max)

    p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data$Centered, y = .data$Variable
  )) +
    band_layer +
    ggplot2::geom_vline(
      xintercept = 0,
       color      = .s2d_ink[["ink"]],
      linetype   = "solid",
      linewidth  = 0.3
    ) +
    ggplot2::geom_segment(
        ggplot2::aes(
        x = .data$Centered, xend = 0,
        y = .data$Variable, yend = .data$Variable
      ),
      color = "gray50", linetype = "dotted", na.rm = TRUE
    ) +
    ggplot2::geom_point(
      color = "steelblue", size = 2.5, na.rm = TRUE
    ) +
     ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = x_label, y = var_lab, caption = band_note) +
    theme_stats2data()

  if (span > 0) {
    p <- p + ggplot2::expand_limits(x = c(-span, span))
  }

    if (length(unique(df$Measure[!is.na(df$Measure)])) > 1L) {
    p <- p + ggplot2::facet_wrap(
      ~ .data$Measure, scales = "free_y", ncol = 1L
    )
  }

  p
}


#' Compute (optionally standardised) differences
#'
#' @param sim Numeric vector of simulated values.
#' @param target Numeric vector of target values.
#' @param standardised Logical.
#' @param eps Numeric; zero-threshold for standardization.
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
    "Unstandardised Diff", "Simulated"
  )
  df
}


# S3 methods

#' @describeIn plot_fit Method for MLR results (\code{stats2data_mlr}).
#'
#' @param x An object of class \code{stats2data_mlr}.
#' @param standardised Logical; if \code{TRUE} (default), differences are
#'   divided by target values (except when targets are near zero).
#' @param eps Numeric; threshold below which a target is treated as zero
#'   for standardization. Default \code{1e-12}.
#' @param order Character; \code{"input"} (default) keeps the supplied
#'   parameter order, \code{"fit"} sorts by absolute difference, worst fit
#'   first.
#' @param thresh Numeric or \code{NULL}; convergence threshold used to shade
#'   the region within \code{+/- thresh} of the target. Defaults to the
#'   threshold of the run, \code{x$inputs$thresh}. Pass \code{NULL} to
#'   suppress the shading.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_mlr(...)
#' plot_fit(res)
#' plot_fit(res, standardised = FALSE)
#' plot_fit(res, order = "fit")
#' }
#'
#' @export
plot_fit.stats2data_mlr <- function(x, standardised = TRUE, eps = 1e-12,
                                    order = c("input", "fit"),
                                    thresh = x$inputs$thresh, ...) {

  if (!is.logical(standardised) || length(standardised) != 1L) {
    stop("`standardised` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(eps) || length(eps) != 1L) {
    stop("`eps` must be a single numeric value.", call. = FALSE)
  }
  order <- match.arg(order)

  inp   <- x$inputs
  stats <- get_stats(x)

  target_reg <- inp$target_reg
   target_cor <- inp$target_cor
  target_se  <- inp$target_se

   sim_reg_r <- stats$reg
  sim_cor_r <- stats$cor

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
    sim_se_r <- stats$se
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

  p <- .plot_fit_engine(df_all, standardised, var_lab = NULL,
                        order = order, thresh = thresh)
  print(p)
  invisible(p)
}


#' @describeIn plot_fit Method for ANOVA results (\code{stats2data_aov}).
#'
#' @param x An object of class \code{stats2data_aov}.
#' @param standardised Logical; if \code{TRUE} (default), differences are
#'   divided by target values (except when targets are near zero).
#' @param eps Numeric; zero-threshold. Default \code{1e-12}.
#' @param order Character; \code{"input"} (default) or \code{"fit"}.
#' @param thresh Numeric or \code{NULL}; convergence threshold used to shade
#'   the region within \code{+/- thresh} of the target. Defaults to
#'   \code{x$inputs$thresh}; \code{NULL} suppresses the shading.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_aov(...)
#' plot_fit(res)
#' }
#'
#' @export
plot_fit.stats2data_aov <- function(x, standardised = TRUE, eps = 1e-12,
                                    order = c("input", "fit"),
                                    thresh = x$inputs$thresh, ...) {

  if (!is.logical(standardised) || length(standardised) != 1L) {
    stop("`standardised` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(eps) || length(eps) != 1L) {
    stop("`eps` must be a single numeric value.", call. = FALSE)
  }
   order <- match.arg(order)

  inp   <- x$inputs
   stats <- get_stats(x)

  target_F <- inp$target_f_list$F_value
  sim_F_r  <- stats$F_value

  effect_names <- inp$target_f_list$effect
  if (length(effect_names) != length(target_F)) {
    stop("`inputs$target_f_list` needs one `effect` name per `F_value`.",
         call. = FALSE)
  }

  df <- data.frame(
    Measure   = "F",
    Variable  = effect_names,
    Simulated = as.numeric(sim_F_r),
    Target    = as.numeric(target_F),
    Centered  = .compute_centered(sim_F_r, target_F, standardised, eps),
    stringsAsFactors = FALSE
  )
  df <- .tag_sim_type(df, standardised, eps)

  p <- .plot_fit_engine(df, standardised, var_lab = NULL,
                        order = order, thresh = thresh)
  print(p)
  invisible(p)
}


#' @describeIn plot_fit Method for Descriptives results
#'   (\code{stats2data_vec}).
#'
#' @param x An object of class \code{stats2data_vec}.
#' @param standardised Logical; if \code{TRUE} (default), differences are
#'   divided by target values (except when targets are near zero).
#' @param eps Numeric; zero-threshold. Default \code{1e-12}.
#' @param order Character; \code{"input"} (default) or \code{"fit"}.
#' @param thresh Numeric or \code{NULL}; convergence threshold used to shade
#'   the region within \code{+/- thresh} of the target. Defaults to
#'   \code{x$inputs$thresh}; \code{NULL} suppresses the shading.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_vec(...)
#' plot_fit(res)
#' }
#'
#' @export
plot_fit.stats2data_vec <- function(x, standardised = TRUE, eps = 1e-12,
                                    order = c("input", "fit"),
                                    thresh = x$inputs$thresh, ...) {

  if (!is.logical(standardised) || length(standardised) != 1L) {
    stop("`standardised` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(eps) || length(eps) != 1L) {
    stop("`eps` must be a single numeric value.", call. = FALSE)
  }
  order <- match.arg(order)

  inp   <- x$inputs
  stats <- get_stats(x)

  target_mean <- inp$target_mean
  target_sd   <- inp$target_sd

   sim_mean_r <- stats$mean
  sim_sd_r   <- stats$sd

  vars <- colnames(x$data)
  if (is.null(vars)) vars <- names(target_mean)

  df <- data.frame(
    Variable  = rep(vars, 2L),
    Measure   = rep(c("Mean", "Standard Deviation"), each = length(vars)),
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

   p <- .plot_fit_engine(df, standardised, var_lab = NULL,
                        order = order, thresh = thresh)
  print(p)
  invisible(p)
}
