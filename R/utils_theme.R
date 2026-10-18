# shared ggplot2 theme for all plot functions


#' Colour tokens for the stats2data theme
#'
#' @format Named character vector of hex colours.
#'
#' @noRd
.s2d_ink <- c(
  ink       = "#111111",  # primary text
  ink_muted = "#555555",  # secondary text
  rule      = "#333333",  # axis lines, panel frame and tick marks
  grid      = "#E4E4E7",  # hairline grid
  panel     = "#FFFFFF"   # panel and plot background
)


#' Clamp a font size
#'
#' Sizes derived from \code{base_size} are clamped so that unusual
#' \code{base_size} values cannot silently produce non-compliant figures.
#'
#' @param x Numeric; requested point size.
#'
#' @return Numeric; \code{x} constrained to \code{[8, 14]}.
#'
#' @noRd
.s2d_pt <- function(x) min(max(x, 8), 14)


#' Internal APA-style theme for stats2data plots
#'
#' Not exported; used internally by \code{plot_convergence} (including its
#' \code{ratio = TRUE} variant), \code{plot_cooling}, \code{plot_fit}, and
#' \code{plot_stability}.
#'
#' @param base_size Numeric; base font size
#' @param base_family Character; font family
#' @param frame Character; how the data area is delimited
#' @param grid Character; which major grid lines to draw.
#'
#' @return A \code{\link[ggplot2]{theme}} object.
#'
#' @noRd
theme_stats2data <- function(base_size = 12,
                             base_family = "",
                             frame = c("box", "L", "none"),
                             grid = c("none")) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting. ",
      "Install it with install.packages(\"ggplot2\").",
      call. = FALSE
    )
  }
  if (!is.numeric(base_size) || length(base_size) != 1L ||
       !is.finite(base_size) || base_size <= 0) {
    stop("`base_size` must be a single positive number.", call. = FALSE)
  }
  if (!is.character(base_family) || length(base_family) != 1L ||
       is.na(base_family)) {
    stop("`base_family` must be a single character string.", call. = FALSE)
  }
  frame <- match.arg(frame)
  grid  <- match.arg(grid)

  size_title    <- .s2d_pt(base_size + 2)
  size_subtitle <- .s2d_pt(base_size)
  size_axis_ttl <- .s2d_pt(base_size)
  size_axis_txt <- .s2d_pt(base_size - 1)
  size_strip    <- .s2d_pt(base_size - 1)
  size_legend   <- .s2d_pt(base_size - 1)
  size_caption  <- .s2d_pt(base_size - 2)

  grid_line <- ggplot2::element_line(
    color     = .s2d_ink[["grid"]],
    linewidth = 0.3
  )
  blank <- ggplot2::element_blank()

  panel_frame <- if (frame == "box") {
    ggplot2::element_rect(
       fill      = NA,
      color     = .s2d_ink[["rule"]],
      linewidth = 0.3
    )
  } else {
    blank
  }
  axis_rule <- if (frame == "L") {
    ggplot2::element_line(
      color = .s2d_ink[["rule"]], linewidth = 0.3, lineend = "square"
    )
  } else {
     blank
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(

      plot.background  = ggplot2::element_rect(
         fill = .s2d_ink[["panel"]], color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = .s2d_ink[["panel"]], color = NA
      ),
      panel.border     = panel_frame,

      panel.grid.major.x = if (grid %in% c("both", "x")) grid_line else blank,
       panel.grid.major.y = if (grid %in% c("both", "y")) grid_line else blank,
      panel.grid.minor   = blank,

      axis.line = axis_rule,
      axis.ticks = ggplot2::element_line(
        color = .s2d_ink[["rule"]], linewidth = 0.3
      ),
      axis.ticks.length = grid::unit(3, "pt"),
       axis.title = ggplot2::element_text(
        color = .s2d_ink[["ink"]], size = size_axis_ttl
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 0.6 * base_size)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.6 * base_size)
      ),
       axis.text = ggplot2::element_text(
        color = .s2d_ink[["ink"]], size = size_axis_txt
      ),

      plot.title.position = "plot",
       plot.title = ggplot2::element_text(
        face   = "bold",
        size   = size_title,
         hjust  = 0,
        color  = .s2d_ink[["ink"]],
        margin = ggplot2::margin(b = 0.35 * base_size)
      ),
      plot.subtitle = ggplot2::element_text(
        size   = size_subtitle,
        hjust  = 0,
         color  = .s2d_ink[["ink_muted"]],
        margin = ggplot2::margin(b = 0.75 * base_size)
      ),

      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(
        size   = size_caption,
         hjust  = 0,
        color  = .s2d_ink[["ink_muted"]],
        margin = ggplot2::margin(t = 0.75 * base_size)
      ),

      legend.background = blank,
       legend.key        = blank,
      legend.title      = ggplot2::element_text(
        size = size_legend, color = .s2d_ink[["ink_muted"]]
      ),
      legend.text = ggplot2::element_text(
        size = size_legend, color = .s2d_ink[["ink"]]
      ),
      legend.key.size = grid::unit(1.1 * base_size, "pt"),
       legend.margin   = ggplot2::margin(t = 0.25 * base_size),

      strip.background = blank,
      strip.text = ggplot2::element_text(
        face   = "bold",
        size   = size_strip,
        hjust  = 0,
        color  = .s2d_ink[["ink"]],
        margin = ggplot2::margin(b = 0.35 * base_size)
      ),
      panel.spacing = grid::unit(1.25 * base_size, "pt"),

      plot.margin = ggplot2::margin(
        t = base_size, r = base_size, b = base_size, l = base_size
      )
    )
}
