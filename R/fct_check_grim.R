#' Check plausibility of a reported mean with the GRIM test
#'
#' Performs the GRIM (Granularity-Related Inconsistency of Means) test
#' (Brown & Heathers, 2017) to assess whether a reported mean is numerically
#' possible given the sample size and number of decimal places.
#'
#' @param n Integer. Sample size; must be a single positive whole number.
#' @param target_mean Numeric. Reported mean to be tested for plausibility.
#' @param decimals Integer. Number of decimal places in the reported mean.
#' @param tol Numeric. thresh for rounding errors; a single non-negative
#'   value. Default is \code{.Machine$double.eps^0.5}.
#'
#' @return A list with components:
#' \describe{
#'   \item{test}{Logical. \code{TRUE} if the reported mean is plausible.}
#'   \item{grim_mean}{Numeric. If the test fails, the nearest plausible mean
#'     (rounded to \code{decimals}); otherwise the original
#'     \code{target_mean}.}
#' }
#'
#' @examples
#' \dontrun{
#' check_grim(n = 10, target_mean = 3.7, decimals = 1)
#' check_grim(n = 10, target_mean = 3.74, decimals = 2)
#' }
#'
#' @export
check_grim <- function(n,
                       target_mean,
                       decimals,
                       tol = .Machine$double.eps^0.5) {

  # ---- input validation ---------------------------------------------------
  if (!is.numeric(n) || length(n) != 1L || n <= 0 || n != as.integer(n))
    stop("`n` must be a single positive integer.", call. = FALSE)
  if (!is.numeric(target_mean) || length(target_mean) != 1L)
    stop("`target_mean` must be a single numeric value.", call. = FALSE)
  if (!is.numeric(decimals) || length(decimals) != 1L ||
      decimals < 0 || decimals != as.integer(decimals))
    stop("`decimals` must be a single non-negative integer.", call. = FALSE)
  if (!is.numeric(tol) || length(tol) != 1L || tol < 0)
    stop("`tol` must be a single non-negative numeric value.", call. = FALSE)

  # ---- GRIM computation -------------------------------------------------
  total_points   <- round(target_mean * n)
  possible_mean  <- total_points / n
  diff           <- abs(target_mean - possible_mean)
  allowed_margin <- (0.1^decimals) / 2 + tol

  if (diff > allowed_margin) {
    adjusted_mean <- round(possible_mean, decimals)
    list(test = FALSE, grim_mean = adjusted_mean)
  } else {
    list(test = TRUE, grim_mean = target_mean)
  }
}
