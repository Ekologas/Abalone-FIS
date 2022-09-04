#' stan helper functions
#'
#' See
#' https://github.com/betanalpha/knitr_case_studies/blob/master/rstan_workflow/stan_utility.R

#' Check divergence of samples
checkDivergence <- function(fit) {
    sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
    divergent <- do.call(rbind, sampler_params)[, 'divergent__']
    n <- sum(divergent)
    if (n > 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

# Checks the potential scale reduction factors
checkRhat <- function(fit) {
    fit_summary <- summary(fit, probs = c(0.5))$summary
    N <- dim(fit_summary)[[1]]
    
    no_warning <- TRUE
    for (n in 1:N) {
        rhat <- fit_summary[, 6][n]
        if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
            print(sprintf('Rhat for parameter %s is %s!',
                          rownames(fit_summary)[n], rhat))
            no_warning <- FALSE
        }
    }
    return(no_warning)
}
