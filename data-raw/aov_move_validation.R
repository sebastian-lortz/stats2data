## ============================================================================
## Move validation v3: stratum-clean redesign
##
## Tests the proposed move set:
##   - move_between (unchanged from package)
##   - move_within_stratum(W) (new, parameterised over within-factor subset)
##
## For each (design, move):
##   - Predicts which effects should touch from stratum theory:
##       move_between           -> effects with W(E) = empty
##       move_within_stratum(W) -> effects with W(E) = W   (W as factor set)
##   - Empirically applies the move many times and measures F-value change
##     per effect via afex::aov_car / car::Anova
##   - Diffs predicted vs observed and flags mismatches
##
## Use this both as a regression check before/after the package patches
## and as a one-button correctness test for any future move added.
## ============================================================================

suppressPackageStartupMessages({
  library(stats2data)
  library(afex)
  library(car)
})

# ---------------------------------------------------------------------------
# F-value extraction (afex for any within, lm + car::Anova for purely between)
# ---------------------------------------------------------------------------
make_aov_formula <- function(factor_type) {
  fnames <- paste0("Factor", seq_along(factor_type))
  bnames <- fnames[factor_type == "between"]
  wnames <- fnames[factor_type == "within"]
  bhalf  <- if (length(bnames)) paste(bnames, collapse = " * ") else "1"
  if (length(wnames)) {
    err <- paste0("Error(ID/(", paste(wnames, collapse = " * "), "))")
    stats::as.formula(paste("outcome ~", bhalf, "+", err))
  } else {
    stats::as.formula(paste("outcome ~", paste(fnames, collapse = " * ")))
  }
}

compute_F <- function(candidate, structure, factor_type) {
  d <- structure$df
  d$outcome <- candidate
  d$ID <- factor(d$ID)
  for (col in grep("^Factor", names(d), value = TRUE)) d[[col]] <- factor(d[[col]])
  formula <- make_aov_formula(factor_type)

  if (any(factor_type == "within")) {
    fit <- suppressMessages(afex::aov_car(
      formula, data = d, factorize = TRUE, type = 3,
      anova_table = list(es = "none")
    ))
    setNames(fit$anova_table[["F"]], rownames(fit$anova_table))
  } else {
    old <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(old))
    fit <- stats::lm(formula, data = d)
    tab <- car::Anova(fit, type = 3)
    keep <- !rownames(tab) %in% c("(Intercept)", "Residuals")
    setNames(tab[keep, "F value"], rownames(tab)[keep])
  }
}

# ---------------------------------------------------------------------------
# Stratum theory: predict which effects each move should touch
# ---------------------------------------------------------------------------
parse_effect <- function(eff_label) {
  parts <- strsplit(eff_label, ":", fixed = TRUE)[[1]]
  as.integer(sub("^Factor", "", parts))
}

# Stratum label for printing: Subject, Subject:Factor2, Subject:Factor2:Factor3, ...
stratum_label <- function(kind, W_pos, widx) {
  if (kind == "between") return("Subject")
  paste0("Subject:", paste0("Factor", widx[W_pos], collapse = ":"))
}

# Predicted touch vector (TRUE for effects in the move's stratum).
predict_touched <- function(effect_labels, factor_type, kind, W_pos = NULL) {
  widx <- which(factor_type == "within")
  out  <- setNames(logical(length(effect_labels)), effect_labels)
  for (i in seq_along(effect_labels)) {
    facs <- parse_effect(effect_labels[i])
    W_E  <- intersect(facs, widx)
    out[i] <- if (kind == "between") {
      length(W_E) == 0L
    } else {
      setequal(W_E, widx[W_pos])
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Test runner
# ---------------------------------------------------------------------------
test_move_F <- function(move_caller, levels, factor_type, S = 80,
                        n_trials = 100, range = c(0, 100),
                        integer = FALSE, seed = 1) {
  set.seed(seed)
  structure <- stats2data:::build_aov_structure(S, levels, factor_type)
  cand <- stats::runif(nrow(structure$df), range[1], range[2])
  if (integer) cand <- round(cand)

  base_F  <- compute_F(cand, structure, factor_type)
  delta_F <- matrix(NA_real_, n_trials, length(base_F),
                    dimnames = list(NULL, names(base_F)))
  declined <- 0L
  for (i in seq_len(n_trials)) {
    nc <- move_caller(cand, integer, structure, range)
    if (identical(nc, cand)) { declined <- declined + 1L; next }
    delta_F[i, ] <- compute_F(nc, structure, factor_type) - base_F
  }
  ok <- !is.na(delta_F[, 1])
  if (!any(ok)) return(NULL)
  ad <- abs(delta_F[ok, , drop = FALSE])
  list(
    n_accepted = sum(ok),
    n_declined = declined,
    touch_rate = colMeans(ad > 1e-6),
    median_dF  = apply(ad, 2, median),
    p95_dF     = apply(ad, 2, quantile, .95),
    base_F     = base_F
  )
}

# ---------------------------------------------------------------------------
# Design grid
# ---------------------------------------------------------------------------
designs <- list(
  "2x2 between"            = list(c(2,2),   c("between","between")),
  "2x2 within"             = list(c(2,2),   c("within","within")),
  "2x2 mixed (B,W)"        = list(c(2,2),   c("between","within")),
  "3x3 within"             = list(c(3,3),   c("within","within")),
  "2x3 within"             = list(c(2,3),   c("within","within")),
  "2x2x2 within"           = list(c(2,2,2), rep("within",3)),
  "2x2x2 mixed (B,W,W)"    = list(c(2,2,2), c("between","within","within"))
)

# ---------------------------------------------------------------------------
# Main loop: enumerate all stratum-targeted moves per design, run test,
# diff against prediction.
# ---------------------------------------------------------------------------
cat("Validation v3 — stratum-clean moves\n")
cat("Per (design, move): predicted vs observed touch pattern.\n")
cat("'.' = orthogonal to that stratum, 'X' = in that stratum.\n")
cat("Mismatches flagged with ***.\n\n")

overall_pass <- TRUE
results <- list()

for (dn in names(designs)) {
  lvls  <- designs[[dn]][[1]]; ftype <- designs[[dn]][[2]]
  has_b <- any(ftype == "between"); has_w <- any(ftype == "within")
  widx  <- which(ftype == "within")

  # build the move list
  move_specs <- list()
  if (has_b) {
    move_specs[[length(move_specs) + 1L]] <- list(
      kind   = "between",
      W_pos  = NULL,
      caller = function(c, i, s, r) stats2data:::move_between(c, i, s, r)
    )
  }
  if (has_w) {
    K <- length(widx)
    for (size in seq_len(K)) {
      for (W in utils::combn(K, size, simplify = FALSE)) {
        local({
          Wcap <- W
          move_specs[[length(move_specs) + 1L]] <<- list(
            kind   = "within",
            W_pos  = Wcap,
            caller = function(c, i, s, r) move_within_stratum(c, Wcap, i, s, r)
          )
        })
      }
    }
  }

  for (ms in move_specs) {
    strat <- stratum_label(ms$kind, ms$W_pos, widx)
    label <- if (ms$kind == "between") "move_between"
    else sprintf("move_within_stratum(W=c(%s))", paste(ms$W_pos, collapse=","))
    cat(sprintf("=== %-22s | %s  -> stratum %s ===\n", dn, label, strat))

    res <- tryCatch(test_move_F(ms$caller, lvls, ftype),
                    error = function(e) list(error = conditionMessage(e)))
    if (!is.null(res$error)) { cat("  ERROR:", res$error, "\n\n"); next }
    if (is.null(res))        { cat("  (no accepted moves)\n\n"); next }

    pred  <- predict_touched(names(res$touch_rate), ftype, ms$kind, ms$W_pos)
    obs   <- res$touch_rate > 0.5
    match <- pred == obs

    df <- data.frame(
      effect       = names(res$touch_rate),
      predicted    = ifelse(pred, "X", "."),
      touch_rate   = round(res$touch_rate, 3),
      observed     = ifelse(obs, "X", "."),
      median_dF    = round(res$median_dF, 5),
      p95_dF       = round(res$p95_dF, 5),
      ok           = ifelse(match, "ok", "***"),
      stringsAsFactors = FALSE
    )
    print(df, row.names = FALSE)
    cat(sprintf("  accepted=%d / declined=%d\n", res$n_accepted, res$n_declined))

    if (!all(match)) {
      overall_pass <- FALSE
      cat("  *** MISMATCH on:",
          paste(names(res$touch_rate)[!match], collapse = ", "), "\n")
    }
    cat("\n")

    results[[paste(dn, label, sep = " :: ")]] <- list(
      stratum = strat, prediction = pred, observed = res
    )
  }
}

cat(strrep("=", 60), "\n", sep = "")
cat(if (overall_pass) "ALL TESTS PASSED — moves are stratum-clean across designs.\n"
    else              "*** SOME TESTS FAILED — see *** rows above.\n")
cat(strrep("=", 60), "\n", sep = "")

saveRDS(results, "data-raw\results\move_validation.rds")
cat("Full results -> move_validation.rds\n")
