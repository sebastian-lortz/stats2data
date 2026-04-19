# --------------------------------------------------------------------------
# stats2data: shared ggplot2 theme for all plot functions
# --------------------------------------------------------------------------

#' Internal APA-style theme for stats2data plots
#'
#' Provides a consistent, minimal ggplot2 theme across all plotting functions.
#' Not exported; used internally by \code{plot_error}, \code{plot_summary},
#' \code{plot_cooling}, \code{plot_error_ratio}, and \code{plot_rmse}.
#'
#' @param base_size Numeric; base font size passed to
#'   \code{\link[ggplot2]{theme_minimal}}. Default \code{12}.
#'
#' @return A \code{\link[ggplot2]{theme}} object.
#'
#' @noRd
theme_stats2data <- function(base_size = 12) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting. ",
      "Install it with install.packages(\"ggplot2\").",
      call. = FALSE
    )
  }
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(
        face = "bold", size = base_size + 2, hjust = 0
      ),
      axis.title = ggplot2::element_text(color = "black"),
      axis.text  = ggplot2::element_text(color = "black")
    )
}
