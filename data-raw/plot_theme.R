### plot themes and helpers ####

library(ggplot2)
library(patchwork)
library(dplyr)

moment_plane_file <- "/Users/lortz/Desktop/PhD/Research/simdata/stats2data/data-raw/moment_plane_graph.R"
if (!exists("cullen_frey_template") && file.exists(moment_plane_file)) {
  source(moment_plane_file)
}


#### setups ####

fs <- 10

heat_cols <- c(low = "#EEF1F5", mid = "#8C96A6", high = "#1E2733")

source_cols <- c("Original" = "grey80", "Simulation" = "grey30")


#### themes ####

theme_sim <- function(base_size = fs, fonts_only = FALSE) {
  fonts <- theme(
    text            = element_text(size = base_size, family = "serif"),
    axis.title      = element_text(size = base_size),
     axis.title.x    = element_text(size = base_size),
    axis.title.y    = element_text(size = base_size),
   axis.text       = element_text(size = base_size - 1),
    plot.title      = element_text(size = base_size),
    strip.text      = element_text(size = base_size),
   strip.text.x    = element_text(size = base_size),
    strip.text.y    = element_text(size = base_size),
    strip.text.y.right = element_text(size = base_size, angle = -90),
    legend.text     = element_text(size = base_size - 1),
    legend.title    = element_text(size = base_size - 1),
    legend.key.size = unit(base_size, "pt")
  )
  if (fonts_only) return(fonts)

  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", hjust = 0),
       plot.subtitle    = element_text(size = base_size - 1, color = "gray30"),
      plot.tag         = element_text(face = "bold", size = base_size + 2),
      axis.title       = element_text(color = "black"),
      axis.text        = element_text(color = "black"),
     strip.background = element_rect(fill = "gray90", color = "gray50"),
      strip.text       = element_text(face = "bold"),
      legend.position  = "bottom",
      legend.title     = element_blank()
    ) +
    fonts
}

font_theme <- theme_sim(fs, fonts_only = TRUE)

no_strips <- theme(strip.text       = element_blank(),
                   strip.text.x     = element_blank(),
                strip.text.y     = element_blank(),
                   strip.background = element_blank())


#### labels ####

var_lab <- function(v, suffix = NULL) {
  base <- sub("^([A-Za-z]+)([0-9]+)$", "\\1[\\2]", v)
txt  <- if (is.null(suffix)) base else paste0(base, " ~ ", suffix)
  parse(text = txt)[[1]]
}


#### moment plane ####

make_heat <- function(skew, exkurt, dist) {
  data.frame(skewness2 = skew^2, kurtosis = exkurt + 3, dist = dist) |>
 filter(is.finite(skewness2), is.finite(kurtosis), is.finite(dist),
           skewness2 <= 4, skewness2 > .001, kurtosis >= 1, kurtosis <= 10)
}

plot_cf_heat <- function(heat, discrete, col_mid, limits = NULL, title = NULL,
                         base_size = fs) {
  cullen_frey_template(discrete = discrete, base_size = base_size, background = list(
    stat_summary_2d(
      data        = heat,
        mapping     = aes(x = skewness2, y = kurtosis, z = dist),
      fun         = mean,
     bins        = 50,
      inherit.aes = FALSE,
      alpha       = 0.85
    ),
    scale_fill_gradient2(
      low      = heat_cols[["low"]],
      mid      = heat_cols[["mid"]],
      high     = heat_cols[["high"]],
    midpoint = col_mid,
      limits   = limits,
      guide    = "none"
    ),
    ggnewscale::new_scale_fill()
  )) +
    labs(title = title)
}

heat_legend <- function(heat, col_mid, limits = NULL, legend_title = NULL,
                        base_size = fs) {
  p <- ggplot(heat, aes(x = skewness2, y = kurtosis, z = dist)) +
    stat_summary_2d(fun = mean, bins = 50, alpha = 0.85) +
    scale_fill_gradient2(
      low      = heat_cols[["low"]],
      mid      = heat_cols[["mid"]],
     high     = heat_cols[["high"]],
      midpoint = col_mid,
      limits   = limits,
     name     = legend_title,
      guide    = guide_colourbar(title.position = "top", title.hjust = 0.5)
    ) +
    theme_classic(base_size = base_size) +
    theme(legend.position = "bottom") +
    theme_sim(base_size, fonts_only = TRUE) +
    theme(legend.key.width  = unit(15, "mm"),
          legend.key.height = unit(3, "mm"))
g   <- ggplotGrob(p)
box <- g$grobs[grep("guide-box", g$layout$name)]
  box[!vapply(box, inherits, logical(1), "zeroGrob")][[1]]
}

assemble_heat <- function(p_left, p_right, legends, legend_height = 0.12) {
  bottom <- if (length(legends) == 1L) {
    wrap_elements(full = legends[[1]])
  } else {
 wrap_elements(full = legends[[1]]) + wrap_elements(full = legends[[2]])
  }
  ((p_left + p_right + plot_layout(axis_titles = "collect_x")) / bottom) +
    plot_layout(heights = c(1, legend_height))
}

make_overlay <- function(true_skew, true_exkurt, ach_skew, ach_exkurt) {
  bind_rows(
   data.frame(skewness2 = true_skew^2, kurtosis = true_exkurt + 3, moments = "True"),
    data.frame(skewness2 = ach_skew^2,  kurtosis = ach_exkurt + 3,  moments = "Achieved")
  ) |>
    filter(is.finite(skewness2), is.finite(kurtosis)) |>   # drop degenerate / NA
    mutate(moments = factor(moments, levels = c("True", "Achieved")))
}

plot_cf_overlay <- function(overlay, discrete, base_size = fs) {
  cullen_frey_template(discrete = discrete, base_size = base_size, background = list(
    geom_point(
      data        = overlay,
 mapping     = aes(x = skewness2, y = kurtosis, colour = moments),
      inherit.aes = FALSE,
      size = 0.1, alpha = 0.1, na.rm = TRUE,
      show.legend = FALSE
    ),
    scale_colour_manual(
      values = c("True" = "grey80", "Achieved" = "grey30")
    )
  ))
}

