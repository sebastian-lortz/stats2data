### Merge AOV job results into a single object

root_dir <- "/Users/lortz/Desktop/PhD/Research/simdata/stats2data"
save_dir <- file.path(root_dir, "data-raw", "results")

# helper
stack_targ <- function(nm) {
  comps   <- lapply(parts, `[[`, nm)
  first   <- comps[[1]]
   varying <- intersect(c("group_means", "f_vals", "range"), names(first))
   out     <- first
  for (v in varying) out[[v]] <- do.call(rbind, lapply(comps, `[[`, v))
  out
}

files <- sort(list.files(save_dir, "3e4_aov_mc_res_\\d+.*$", full.names = TRUE))
 parts <- lapply(files, readRDS)
n_cond_total <- sum(vapply(parts, function(p) ncol(p$b_cont_err), integer(1)))
 cond_nm      <- sprintf("cond_%03d", seq_len(n_cond_total))
aov_mc_res <- list(
  data         = unlist(lapply(parts, `[[`, "data"), recursive = FALSE),
  true_coh_f   = do.call(rbind, lapply(parts, `[[`, "true_coh_f")),
  true_means   = do.call(rbind, lapply(parts, `[[`, "true_means")),
  true_moments = unlist(lapply(parts, `[[`, "true_moments"), recursive = FALSE),
  subject_sd   = unlist(lapply(parts, `[[`, "subject_sd")),
  b_cont_targ  = stack_targ("b_cont_targ"),  # same helper as before
   b_int_targ   = stack_targ("b_int_targ"),
  w_cont_targ  = stack_targ("w_cont_targ"),
  w_int_targ   = stack_targ("w_int_targ"),
  m_cont_targ  = stack_targ("m_cont_targ"),
   m_int_targ   = stack_targ("m_int_targ"),
  b_cont_err   = do.call(cbind, lapply(parts, `[[`, "b_cont_err")),
  w_cont_err   = do.call(cbind, lapply(parts, `[[`, "w_cont_err")),
  m_cont_err   = do.call(cbind, lapply(parts, `[[`, "m_cont_err")),
  b_int_err    = do.call(cbind, lapply(parts, `[[`, "b_int_err")),
   w_int_err    = do.call(cbind, lapply(parts, `[[`, "w_int_err")),
  m_int_err    = do.call(cbind, lapply(parts, `[[`, "m_int_err"))
)

for (nm in c("b_cont_err","w_cont_err","m_cont_err",
             "b_int_err","w_int_err","m_int_err")) {
  colnames(aov_mc_res[[nm]]) <- cond_nm
}
names(aov_mc_res$data)         <- cond_nm
names(aov_mc_res$true_moments) <- cond_nm
 names(aov_mc_res$subject_sd)   <- cond_nm
rownames(aov_mc_res$true_coh_f) <- cond_nm
rownames(aov_mc_res$true_means) <- cond_nm

# Save merged object
saveRDS(aov_mc_res, file.path(save_dir, "aov_mc_res"))
