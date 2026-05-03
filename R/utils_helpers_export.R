#' Plot Histograms for Each Variable
#'
#' Given a data frame or matrix of numeric variables, this function returns a named list of
#' \code{ggplot2} histogram (or bar) plots—one per column.  For each variable it displays
#' a vertical line at the mean and an annotation of mean and standard deviation (or just mean).
#'
#' @param df A \code{data.frame} or matrix whose columns are the variables to plot.
#' @param tol Numeric; thresh for deciding whether a variable is integer-valued.
#'   Values within \code{tol} of an integer are plotted as counts (bars).
#' @param SD Logical; if \code{TRUE}, annotate each plot with mean and standard deviation;
#'   if \code{FALSE}, annotate with mean only.
#'
#' @return A named list of \code{ggplot2} objects, one per column of \code{df}.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_col geom_vline annotate labs theme_minimal theme element_text
#' @export
plot_histogram <- function(df, tol = 1e-8, SD = TRUE) {
  is_int <- function(x) all(abs(x - round(x)) < tol)

  plots <- lapply(names(df), function(var) {
    x   <- df[[var]]
    m   <- mean(x, na.rm = TRUE)
    s   <- stats::sd(x,   na.rm = TRUE)
    if (SD) {
      lbl <- sprintf("M = %.2f\nSD = %.2f", m, s)
    } else {
      lbl <- paste0("M = ", round(m,1))
    }
    if (is_int(x)) {
      tbl <- as.data.frame(table(x, useNA = "no"))
      names(tbl) <- c("Value", "Count")
      tbl$Value <- as.numeric(as.character(tbl$Value))

      p <- ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Value, y = .data$Count)) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::geom_vline(xintercept = m,
                            linetype   = "dashed",
                            linewidth  = 1,
                            color      = "darkgrey") +
        ggplot2::annotate("text",
                          x      = Inf, y = Inf,
                          hjust  = 1.1, vjust = 1.5,
                          label  = lbl,
                          size   = 4) +
        ggplot2::labs(title = paste("Frequency of", var),
                      x     = var,
                      y     = "Count")
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
        ggplot2::geom_histogram(binwidth = (max(x, na.rm=TRUE)-min(x, na.rm=TRUE))/30,
                                fill     = "steelblue") +
        ggplot2::geom_vline(xintercept = m,
                            linetype   = "dashed",
                            linewidth  = 1,
                            color      = "darkgrey") +
        ggplot2::annotate("text",
                          x      = Inf, y = Inf,
                          hjust  = 1.1, vjust = 1.5,
                          label  = lbl,
                          size   = 4) +
        ggplot2::labs(title = paste("Distribution of", var),
                      x     = var,
                      y     = "Count")
    }
    p +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(hjust = 1),
        legend.position = "none",
        plot.title      = ggplot2::element_text(face = "bold")
      )
  })

  names(plots) <- names(df)
  plots
}


#' Partial Regression Plots for a Linear Model
#'
#' Given a fitted linear model, this function computes partial‐regression plots
#' for each term in the model.  It regress out the other predictors and plots
#' the residuals against each term’s residuals, annotating with the slope (beta)
#' and residual standard deviation.
#'
#' @param model A fitted \code{lm} model object.
#'
#' @return A named list of \code{ggplot2} objects, one per predictor term in the model.
#'
#' @importFrom stats formula lm model.frame resid reformulate sd coef
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth annotate labs theme_minimal theme element_text
#' @export
plot_partial_regression <- function(model) {
  df_orig    <- as.data.frame(model.frame(model))
  fm         <- stats::formula(model)
  resp_name  <- as.character(fm)[2]
  term_labels<- base::attr(stats::terms(model), "term.labels")
  safe_labels <- make.names(term_labels, unique = TRUE)

  df <- df_orig
  for (i in seq_along(term_labels)) {
    if (term_labels[i] != safe_labels[i]) {
      parts <- strsplit(term_labels[i], ":", fixed=TRUE)[[1]]
      df[[ safe_labels[i] ]] <- df[[ parts[1] ]] * df[[ parts[2] ]]
    }
  }

  plots <- lapply(seq_along(safe_labels), function(i) {
    term_safe <- safe_labels[i]
    term_lbl  <- term_labels[i]
    others    <- setdiff(safe_labels, term_safe)

    fY <- stats::reformulate(others, resp_name)
    yres <- stats::resid(stats::lm(fY, data = df))

    fX <- stats::reformulate(others, term_safe)
    xres <- stats::resid(stats::lm(fX, data = df))

    fit   <- stats::lm(yres ~ xres)
    beta  <- stats::coef(fit)[2]
    sd_r  <- stats::sd(stats::resid(fit))

    ggplot2::ggplot(data.frame(xres,yres), ggplot2::aes(x=xres,y=yres)) +
      ggplot2::geom_point(color="steelblue", alpha=0.7) +
      ggplot2::geom_smooth(method="lm", se=FALSE,
                           color="darkgrey", linewidth=1) +
      ggplot2::annotate("text", x=Inf,y=Inf, hjust=1.1,vjust=1.5,
                        label=sprintf("Beta = %.3f\nSD(resid)=%.3f", beta, sd_r),
                        size=4) +
      ggplot2::labs(
        title = paste("Partial Regression for", term_lbl),
        x     = paste(term_lbl, "residuals"),
        y     = paste(resp_name,   "residuals")
      ) +
      ggplot2::theme_minimal(base_size=14) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(angle=45, hjust=1),
        plot.title      = ggplot2::element_text(face="bold"),
        legend.position = "none"
      )
  })

  names(plots) <- term_labels
  plots
}


#' Reshape Data from Long to Wide Format
#'
#' Converts a data.frame in long format (with an \code{ID} column, optional between-subject
#' grouping columns, a \code{time} column, and one or more measurement columns) into
#' wide format.  Column names in the result follow the pattern \code{<measure>_<time>}.
#'
#' @param data A \code{data.frame} in long format.  The first column is taken as the
#'   subject identifier (\code{ID}); the next columns before \code{time} are treated as
#'   between‐subject factors; the \code{time} column must be named \code{"time"}; remaining
#'   columns are the measurements.
#'
#' @return A \code{data.frame} in wide format, with one row per subject (and between‐subject
#'   factor combination) and one column per measure–time combination.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#' @export
long_to_wide <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data.frame.")
  if (ncol(data) < 3) stop("Need: ID + time + at least 1 measure.")

  participant_col <- names(data)[1]
  time_col <- "time"
  if (!time_col %in% names(data)) stop("'time' column not found")

  time_index <- which(names(data) == time_col)
  if (time_index > 2) {
    between_cols <- names(data)[2:(time_index - 1)]
  } else {
    between_cols <- character(0)
  }

  value_cols <- setdiff(names(data), c(participant_col, between_cols, time_col))

  wide_data <- tidyr::pivot_wider(
    data,
    id_cols     = tidyselect::all_of(c(participant_col, between_cols)),
    names_from  = tidyselect::all_of(time_col),
    values_from = tidyselect::all_of(value_cols),
    names_glue  = "{.value}_{time}"
  )

  as.data.frame(wide_data)
}


#' Reshape Data from Wide to Long Format
#'
#' Converts a data.frame or matrix in wide format (with an \code{ID} column and
#' repeated‐measure columns named \code{<variable>_<time>}, e.g. \code{V1_1, V1_2})
#' into long format.  The resulting data has columns \code{ID}, any between‐subject
#' factors, \code{time}, and one column per measure.
#'
#' @param data A \code{data.frame} or \code{matrix} in wide format.  If a matrix is
#'   provided, it will be coerced to a data.frame.  Must have at least two columns
#'   matching the regex \code{"<var>_<time>"}.
#'
#' @return A \code{data.frame} in long format with columns \code{ID}, any between‐subject
#'   factors (if present), \code{time} (integer), and one column per measure.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom tidyselect all_of
#' @export
wide_to_long <- function(data) {
  if (!is.data.frame(data) && !is.matrix(data)) stop("Input must be a data.frame or matrix.")
  if (ncol(data) < 3) stop("Need the data in wide format.")
  if (is.matrix(data)) data <- as.data.frame(data)

  if (!"ID" %in% names(data)) {
    data <- cbind(1:nrow(data),data)
    names(data)[1] <- "ID"
  }

  id_cols <- names(data)[!grepl("_", names(data))]
  if (is.null(id_cols)) stop("The data in wide format have to contain at least two repeated measures with columns named [var]_[time.index]; e.g. V1_1, V2_2")

  vt <- grep("^[^_]+_[0-9]+$", names(data), value = TRUE)
  if (length(vt) < 2) {
    stop("Need at least two repeated-measure columns named like V1_1, V1_2.")
  }
  data %>%
    tidyr::pivot_longer(
      cols       = -tidyselect::all_of(id_cols),
      names_to   = c(".value", "time"),
      names_sep  = "_"
    ) %>%
    dplyr::mutate(time = as.integer(.data$time)) %>%
    as.data.frame()
}
