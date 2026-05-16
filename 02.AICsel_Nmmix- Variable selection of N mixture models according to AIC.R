# ============================================================
# Backward AIC selection for unmarked::pcount() (N-mixture)
# Uses unique names so it will not clash with your other scripts.
#
# - pcount expects a double-RHS formula:  ~ det_covs ~ state_covs
# - Drops 1 term at a time (backward) choosing the drop that most
#   improves AIC.
# - component = "det", "state", or "both"
# - keep_det / keep_state protect terms from being dropped
# - Computes AIC robustly from optim output if stats::AIC() fails
# ============================================================

pcAIC_as_double_rhs <- function(det_formula, state_formula) {
  df <- paste(deparse(det_formula), collapse = " ")
  sf <- paste(deparse(state_formula), collapse = " ")
  stats::as.formula(paste(df, sf))
}

pcAIC_make_one_sided <- function(term_labels) {
  if (length(term_labels) == 0) return(stats::as.formula("~ 1"))
  stats::as.formula(paste("~", paste(term_labels, collapse = " + ")))
}

pcAIC_get_terms <- function(one_sided_formula) {
  attr(stats::terms(one_sided_formula), "term.labels")
}

pcAIC_get_aic <- function(fit) {
  # 1) Try stats::AIC() if it works
  a <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
  if (is.numeric(a) && length(a) == 1 && is.finite(a)) return(a)
  
  # 2) Compute from optim output: AIC = 2*NLL + 2*k, k = length(opt$par)
  sn <- tryCatch(methods::slotNames(fit), error = function(e) character(0))
  if ("opt" %in% sn) {
    opt <- tryCatch(methods::slot(fit, "opt"), error = function(e) NULL)
    if (!is.null(opt) && !is.null(opt$value) && is.finite(opt$value) &&
        !is.null(opt$par) && length(opt$par) > 0) {
      nll <- opt$value
      k <- length(opt$par)
      return(2 * nll + 2 * k)
    }
  }
  
  stop("pcAIC_get_aic: Could not extract or compute AIC for this pcount fit.")
}

backward_pcount_AIC_pcAIC <- function(det_formula,
                                      state_formula,
                                      data,
                                      K,
                                      mixture = "NB",
                                      threads = 1,
                                      component = c("det", "state", "both"),
                                      keep_det = character(0),
                                      keep_state = character(0),
                                      max_steps = 200,
                                      verbose = TRUE,
                                      tol = 1e-8,
                                      ...) {
  component <- match.arg(component)
  
  pcAIC_fit_once <- function(df, sf) {
    form <- pcAIC_as_double_rhs(df, sf)
    unmarked::pcount(
      formula = form,
      data = data,
      K = K,
      mixture = mixture,
      threads = threads,
      ...
    )
  }
  
  det_terms   <- pcAIC_get_terms(det_formula)
  state_terms <- pcAIC_get_terms(state_formula)
  
  keep_det   <- intersect(keep_det, det_terms)
  keep_state <- intersect(keep_state, state_terms)
  
  cur_det_terms   <- det_terms
  cur_state_terms <- state_terms
  
  cur_fit <- pcAIC_fit_once(
    pcAIC_make_one_sided(cur_det_terms),
    pcAIC_make_one_sided(cur_state_terms)
  )
  cur_aic <- pcAIC_get_aic(cur_fit)
  
  trace <- data.frame(
    step = 0,
    removed_side = NA_character_,
    removed_term = NA_character_,
    aic = cur_aic,
    stringsAsFactors = FALSE
  )
  
  models <- list(cur_fit)
  
  for (step in seq_len(max_steps)) {
    cand <- data.frame(side = character(0), term = character(0), stringsAsFactors = FALSE)
    
    if (component %in% c("det", "both")) {
      det_cand <- setdiff(cur_det_terms, keep_det)
      if (length(det_cand) > 0) {
        cand <- rbind(cand, data.frame(side = rep("det", length(det_cand)), term = det_cand))
      }
    }
    
    if (component %in% c("state", "both")) {
      state_cand <- setdiff(cur_state_terms, keep_state)
      if (length(state_cand) > 0) {
        cand <- rbind(cand, data.frame(side = rep("state", length(state_cand)), term = state_cand))
      }
    }
    
    if (nrow(cand) == 0) break
    
    best_i <- NA_integer_
    best_fit <- NULL
    best_aic <- Inf
    
    for (i in seq_len(nrow(cand))) {
      side_i <- cand$side[i]
      term_i <- cand$term[i]
      
      new_det_terms <- cur_det_terms
      new_state_terms <- cur_state_terms
      
      if (side_i == "det") {
        new_det_terms <- setdiff(new_det_terms, term_i)
      } else {
        new_state_terms <- setdiff(new_state_terms, term_i)
      }
      
      fit_i <- tryCatch(
        pcAIC_fit_once(
          pcAIC_make_one_sided(new_det_terms),
          pcAIC_make_one_sided(new_state_terms)
        ),
        error = function(e) NULL
      )
      if (is.null(fit_i)) next
      
      aic_i <- tryCatch(pcAIC_get_aic(fit_i), error = function(e) Inf)
      if (is.finite(aic_i) && aic_i < best_aic) {
        best_aic <- aic_i
        best_fit <- fit_i
        best_i <- i
      }
    }
    
    if (!is.finite(best_aic) || is.na(best_i)) break
    
    # Stop if no improvement
    if (best_aic >= cur_aic - tol) break
    
    removed_side <- cand$side[best_i]
    removed_term <- cand$term[best_i]
    
    if (removed_side == "det") {
      cur_det_terms <- setdiff(cur_det_terms, removed_term)
    } else {
      cur_state_terms <- setdiff(cur_state_terms, removed_term)
    }
    
    cur_fit <- best_fit
    cur_aic <- best_aic
    
    if (verbose) {
      cat("Step", step, "removed:", removed_side, "::", removed_term, "AIC:", cur_aic, "\n")
    }
    
    trace <- rbind(
      trace,
      data.frame(
        step = step,
        removed_side = removed_side,
        removed_term = removed_term,
        aic = cur_aic,
        stringsAsFactors = FALSE
      )
    )
    models[[length(models) + 1]] <- cur_fit
  }
  
  list(
    best_model = cur_fit,
    best_det_formula   = pcAIC_make_one_sided(cur_det_terms),
    best_state_formula = pcAIC_make_one_sided(cur_state_terms),
    trace = trace,
    models = models
  )
}

# ============================================================
# Example call
# ============================================================
# det_f <- ~ scale(Canopy_height) + scale(Biomass_above) + season
# state_f <- ~ 1  # or the other way around depending on your intention
#
# sel_pc_aic <- backward_pcount_AIC_pcAIC(
#   det_formula   = det_f,
#   state_formula = state_f,
#   data = umf.sus.NM22,
#   K = K_use,
#   mixture = "NB",
#   threads = 8,
#   component = "det",
#   keep_det = "season"
# )
#
# summary(sel_pc_aic$best_model)
# sel_pc_aic$best_det_formula
# sel_pc_aic$trace
