### Packages
library(tidyverse)
library(survey)
library(stargazer)
library(haven)


### Custom Functions ------------------------
mediation_function_standard <- function(dvs, ivs, mediators, controls, des,
                                        dat, out = NULL) {
  standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)  # Standardization function
  
  # Standardize IVs, mediators, and controls (but not DVs)
  dat_std <- dat  # Copy dataset
  vars_to_standardize <- unique(c(ivs, mediators, controls))
  
  for (var in vars_to_standardize) {
    if (var %in% names(dat_std)) {
      dat_std[[var]] <- standardize(dat_std[[var]])
    }
  }
  
  mediation_results <- list()
  
  ## Step 1: Predict mediators (M ~ IV + Linked Fate + Controls)
  mediator_models <- list()
  for (M in mediators) {  
    for (X in ivs) {  
      form_mediator <- as.formula(paste(M, " ~ ", X, "+", paste(controls, collapse = " + ")))
      mediator_model <- svyglm(form_mediator, design = des, family = gaussian(),
                               data = dat_std)
      
      mediator_models[[paste0("IV_", X, "_Med_", M)]] <- mediator_model
    }
  }
  
  ## Step 2: Predict DV (Y ~ IV + M + Linked Fate + Controls)
  outcome_models <- list()
  for (Y in dvs) {  
    for (M in mediators) {  
      for (X in ivs) {  
        form_outcome <- as.formula(paste(Y, " ~ ", X, "+", M, "+", paste(controls, collapse = " + ")))
        outcome_model <- svyglm(form_outcome, design = des, family = gaussian(),
                                data = dat_std)
        
        outcome_models[[paste0("DV_", Y, "_IV_", X, "_Med_", M)]] <- outcome_model
      }
    }
  }
  
  mediation_results$mediator_models <- mediator_models
  mediation_results$outcome_models <- outcome_models
  
  # Assign to global environment
  if (!is.null(out)) {
    if (!is.character(out)) stop("Argument 'out' must be a character string")
    assign(out, mediation_results, envir = .GlobalEnv)
  } else {
    return(mediation_results)
  }
}
