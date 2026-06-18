cullen_frey_template <- function(discrete = FALSE,
                                 xmax = 4,
                                 kurtmax = 10,
                                 base_size = 12) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }

  # Small padding so symbols at x = 0 are fully visible
  xpad <- 0.06 * xmax

  # Helper: clip a curve branch at xmax and interpolate endpoint exactly at xmax
  clip_branch_at_xmax <- function(x, y, xmax) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]

    ord <- order(x)
    x <- x[ord]
    y <- y[ord]

    keep <- x <= xmax
    x_keep <- x[keep]
    y_keep <- y[keep]

    if (length(x_keep) > 0 && max(x_keep) < xmax && any(x > xmax)) {
      idx_left <- max(which(x < xmax))
      idx_right <- min(which(x > xmax))

      if (length(idx_left) > 0 && length(idx_right) > 0 &&
          is.finite(idx_left) && is.finite(idx_right)) {
        y_xmax <- stats::approx(
          x = x[c(idx_left, idx_right)],
          y = y[c(idx_left, idx_right)],
          xout = xmax
        )$y

        x_keep <- c(x_keep, xmax)
        y_keep <- c(y_keep, y_xmax)
      }
    }

    data.frame(
      skewness2 = x_keep,
      kurtosis = y_keep
    )
  }

  p <- ggplot2::ggplot()

  if (!discrete) {

    # -----------------------------------------------------------------------
    # Beta distribution region
    # -----------------------------------------------------------------------
    p_beta <- exp(-100)
    lq <- seq(-100, 100, by = 0.1)
    q <- exp(lq)

    beta_s2_a <- (4 * (q - p_beta)^2 * (p_beta + q + 1)) /
      ((p_beta + q + 2)^2 * p_beta * q)

    beta_kurt_a <- 3 * (p_beta + q + 1) *
      (p_beta * q * (p_beta + q - 6) + 2 * (p_beta + q)^2) /
      (p_beta * q * (p_beta + q + 2) * (p_beta + q + 3))

    p_beta <- exp(100)
    q <- exp(lq)

    beta_s2_b <- (4 * (q - p_beta)^2 * (p_beta + q + 1)) /
      ((p_beta + q + 2)^2 * p_beta * q)

    beta_kurt_b <- 3 * (p_beta + q + 1) *
      (p_beta * q * (p_beta + q - 6) + 2 * (p_beta + q)^2) /
      (p_beta * q * (p_beta + q + 2) * (p_beta + q + 3))

    beta_a <- clip_branch_at_xmax(beta_s2_a, beta_kurt_a, xmax)
    beta_b <- clip_branch_at_xmax(beta_s2_b, beta_kurt_b, xmax)

    beta_a <- beta_a[beta_a$kurtosis <= kurtmax, , drop = FALSE]
    beta_b <- beta_b[beta_b$kurtosis <= kurtmax, , drop = FALSE]

    beta_df <- rbind(
      beta_a,
      beta_b[nrow(beta_b):1, , drop = FALSE]
    )
    beta_df$distribution <- "beta"

    # -----------------------------------------------------------------------
    # Gamma distribution curve
    # -----------------------------------------------------------------------
    lshape <- seq(-100, 100, by = 0.1)
    shape <- exp(lshape)

    gamma_df <- data.frame(
      skewness2 = 4 / shape,
      kurtosis = 3 + 6 / shape,
      distribution = "gamma"
    )
    gamma_df <- gamma_df[
      is.finite(gamma_df$skewness2) &
        is.finite(gamma_df$kurtosis) &
        gamma_df$skewness2 <= xmax &
        gamma_df$kurtosis <= kurtmax,
    ]

    # -----------------------------------------------------------------------
    # Lognormal distribution curve
    # -----------------------------------------------------------------------
    lshape <- seq(-100, 100, by = 0.1)
    shape <- exp(lshape)
    es2 <- exp(shape^2)

    lognormal_df <- data.frame(
      skewness2 = (es2 + 2)^2 * (es2 - 1),
      kurtosis = es2^4 + 2 * es2^3 + 3 * es2^2 - 3,
      distribution = "lognormal"
    )
    lognormal_df <- lognormal_df[
      is.finite(lognormal_df$skewness2) &
        is.finite(lognormal_df$kurtosis) &
        lognormal_df$skewness2 <= xmax &
        lognormal_df$kurtosis <= kurtmax,
    ]

    line_df <- rbind(gamma_df, lognormal_df)

    # -----------------------------------------------------------------------
    # Theoretical reference points
    # -----------------------------------------------------------------------
    point_df <- data.frame(
      skewness2 = c(0, 0, 4, 0),
      kurtosis = c(3, 9 / 5, 9, 4.2),
      distribution = c("normal", "uniform", "exponential", "logistic")
    )

    p <- p +
      # background first
      ggplot2::geom_polygon(
        data = beta_df,
        ggplot2::aes(
          x = skewness2,
          y = kurtosis,
          fill = distribution
        ),
        alpha = 0.55,
        colour = NA
      ) +
      # curves next
      ggplot2::geom_path(
        data = line_df,
        ggplot2::aes(
          x = skewness2,
          y = kurtosis,
          linetype = distribution
        ),
        linewidth = 0.7
      ) +
      # symbols on top
      ggplot2::geom_point(
        data = point_df,
        ggplot2::aes(
          x = skewness2,
          y = kurtosis,
          shape = distribution
        ),
        size = 3.4,
        stroke = 1.2
      ) +
      ggplot2::scale_fill_manual(
        name = "Theoretical",
        values = c("beta" = "grey80"),
        breaks = c("beta")
      ) +
      ggplot2::scale_linetype_manual(
        name = "Theoretical",
        values = c(
          "gamma" = "dashed",
          "lognormal" = "dotted"
        ),
        breaks = c("lognormal", "gamma")
      ) +
      ggplot2::scale_shape_manual(
        name = "Theoretical",
        values = c(
          "normal" = 8,
          "uniform" = 2,
          "exponential" = 7,
          "logistic" = 3
        ),
        breaks = c("normal", "uniform", "exponential", "logistic")
      )

  } else {

    # -----------------------------------------------------------------------
    # Negative binomial region
    # -----------------------------------------------------------------------
    p_nb <- exp(-10)
    lr <- seq(-100, 100, by = 0.1)
    r <- exp(lr)

    nb_s2_a <- (2 - p_nb)^2 / (r * (1 - p_nb))
    nb_kurt_a <- 3 + 6 / r + p_nb^2 / (r * (1 - p_nb))

    p_nb <- 1 - exp(-10)
    lr <- seq(100, -100, by = -0.1)
    r <- exp(lr)

    nb_s2_b <- (2 - p_nb)^2 / (r * (1 - p_nb))
    nb_kurt_b <- 3 + 6 / r + p_nb^2 / (r * (1 - p_nb))

    nb_a <- clip_branch_at_xmax(nb_s2_a, nb_kurt_a, xmax)
    nb_b <- clip_branch_at_xmax(nb_s2_b, nb_kurt_b, xmax)

    nb_a <- nb_a[nb_a$kurtosis <= kurtmax, , drop = FALSE]
    nb_b <- nb_b[nb_b$kurtosis <= kurtmax, , drop = FALSE]

    nb_df <- rbind(
      nb_a,
      nb_b[nrow(nb_b):1, , drop = FALSE]
    )
    nb_df$distribution <- "negative binomial"

    # -----------------------------------------------------------------------
    # Poisson distribution curve
    # -----------------------------------------------------------------------
    llambda <- seq(-100, 100, by = 0.1)
    lambda <- exp(llambda)

    poisson_df <- data.frame(
      skewness2 = 1 / lambda,
      kurtosis = 3 + 1 / lambda,
      distribution = "Poisson"
    )
    poisson_df <- poisson_df[
      is.finite(poisson_df$skewness2) &
        is.finite(poisson_df$kurtosis) &
        poisson_df$skewness2 <= xmax &
        poisson_df$kurtosis <= kurtmax,
    ]

    # -----------------------------------------------------------------------
    # Theoretical reference point
    # -----------------------------------------------------------------------
    point_df <- data.frame(
      skewness2 = 0,
      kurtosis = 3,
      distribution = "normal"
    )

    p <- p +
      # background first
      ggplot2::geom_polygon(
        data = nb_df,
        ggplot2::aes(
          x = skewness2,
          y = kurtosis,
          fill = distribution
        ),
        alpha = 0.55,
        colour = NA
      ) +
      # curve next
      ggplot2::geom_path(
        data = poisson_df,
        ggplot2::aes(
          x = skewness2,
          y = kurtosis,
          linetype = distribution
        ),
        linewidth = 0.7
      ) +
      # symbol on top
      ggplot2::geom_point(
        data = point_df,
        ggplot2::aes(
          x = skewness2,
          y = kurtosis,
          shape = distribution
        ),
        size = 3.4,
        stroke = 1.2
      ) +
      ggplot2::scale_fill_manual(
        name = "Theoretical",
        values = c("negative binomial" = "grey80"),
        breaks = c("negative binomial")
      ) +
      ggplot2::scale_linetype_manual(
        name = "Theoretical",
        values = c("Poisson" = "dashed"),
        breaks = c("Poisson")
      ) +
      ggplot2::scale_shape_manual(
        name = "Theoretical",
        values = c("normal" = 8),
        breaks = c("normal")
      )
  }

  p +
    ggplot2::scale_x_continuous(
      limits = c(-xpad, xmax),
      breaks = seq(0, floor(xmax), by = 1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_y_reverse(
      limits = c(kurtmax, 1),
      breaks = seq(1, kurtmax, by = 1),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::labs(
      title = "",
      x = "square of skewness",
      y = "kurtosis"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(-xpad, xmax),
      ylim = c(kurtmax, 1),
      clip = "off"
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      legend.position = c(0.76, 0.72),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = base_size * 0.8),
      legend.text = ggplot2::element_text(size = base_size * 0.75),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.margin = ggplot2::margin(t = 8, r = 18, b = 8, l = 18)
    )
}
