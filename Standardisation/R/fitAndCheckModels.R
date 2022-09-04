#' Fits stan models and performs some basic checks.
#'
#' This function fits stan models with a default set of control parameters. If
#' the model appears not to converge, the control parameters are adjusted
#' slightly. This is the default, this can be turned off using the check control
#' parameter.
#'
#' This function can be quite compute intensive as it may run models multiple
#' times.
fitAndCheckModels <- function(stan_data, model, check = TRUE) {
    samples <- sampling(
        object = model, data = stan_data, iter = 2000, chains = 4,
        control = list(adapt_delta = 0.9)
    )
    if(!check) return(samples)
    ## Perform some checks to see if model has approximately converged.
    ## First check on divergences. Increase adapt_delta in steps.
    ad_seq <- c(0.9, 0.95, 0.98, 0.99)
    no_divs <- checkDivergence(samples)
    ad_num <- 1
    ad_len <- length(ad_seq)
    while (!no_divs && ad_num < ad_len) {
        ad_num <- ad_num + 1
        samples <- sampling(
            object = model, data = stan_data, iter = 2000, chains = 4,
            control = list(adapt_delta = ad_seq[ad_num])
        )
        no_divs <- checkDivergence(samples)
    }
    ## If max divergence, say so
    max_div <- ifelse(ad_num == ad_len, TRUE, FALSE)
    ## Now check on Rhats. Add more samples by doubling iter, but only up to
    ## 8000...
    iters <- c(2000, 4000, 8000)
    no_rhat <- checkRhat(samples)
    rhat_num <- 1
    rhat_len <- 3
    while (!no_rhat && rhat_num < rhat_len) {
        rhat_num <- rhat_num + 1
        samples <- sampling(
            object = model, data = stan_data, iter = iters[rhat_num],
            chains = 4, control = list(adapt_delta = ad_seq[ad_num])
        )
        no_rhat <- checkRhat(samples)
    }
    ## If bad rhats, say so
    max_rhats <- ifelse(rhat_num == rhat_len, TRUE, FALSE)
    list(samples = samples, max_div = max_div, max_rhats = max_rhats)
}

#' Loop over all models, and create appropriate outputs.
#'
#' This function takes the raw CPUE data and a given SMU, and fits the sequence
#' of three models (two for Cliffy Group) to the data. The LOO comparison table
#' is saved, as well as some appropriate figures for reports. The full model
#' output is not saved by default, as it is considerably large (can be great
#' than 1 GB).
createModels <- function(smu, data, min_year, max_year,
                         save_output = FALSE) {
    stan_data <- createStanData(
        data = data,
        smu = smu,
        year_cutoff = min_year,
        pred_year = seq(min_year, max_year, by = 1)
    )
    ## Load/compile stan models
    ## Model with year/reef/diver effects
    model_year <- stan_model(here("stan", "smu-year.stan"))
    ## Model as above with year by reef effects
    model_year_reef <- stan_model(here("stan", "smu-year-reef.stan"))
    ## Model as above with year by diver effects
    model_year_reef_diver <- stan_model(here("stan", "smu-year-reef-diver.stan"))
    ## Load models for Cliffy Group (single reef, so no reef effects)
    ## Model with year/diver effects
    cliffy_year <- stan_model(here("stan", "cliffy-year.stan"))
    ## Model as above with year by diver effects
    cliffy_year_diver <- stan_model(here("stan", "cliffy-year-diver.stan"))

    ## Fit models
    if (smu == 'CLIFFY GROUP') {
        samples_year <- fitAndCheckModels(
            stan_data, cliffy_year
        )
        samples_year_diver<- fitAndCheckModels(
            stan_data, cliffy_year_diver
        )
        ## Calculate LOOIC
        ll_year <- extract_log_lik(samples_year$samples,
                                   "loglik")
        loo_year <- loo(ll_year)
        ll_year_diver <- extract_log_lik(samples_year_diver$samples,
                                         "loglik")
        loo_year_diver <- loo(ll_year_diver)
        looTab <- as.data.frame(compare(
            loo_year, loo_year, loo_year_diver)) %>%
            mutate(mID = rownames(.))
        names(looTab) <- c("LOOIC", "se(LOOIC)", "ELPD", "se(ELPD)", "Eff. P",
                           "se(Eff. P)", "mID")
        ## Algorithmically figure out which is best...
        ord <- looTab$mID
        looTab <- looTab[-which(ord == "loo_year")[1],]
        ord <- looTab$mID
        diffs <- rbind(
            rep(NA, 2),
            compare(get(ord[2]), get(ord[1]))
        ) %>%
            as_data_frame() %>%
            mutate(mID = ord)
        looTab <- left_join(looTab, diffs, by = "mID")
        loo_lookup <- data_frame(
            mID = sort(looTab[['mID']]),
            Model = paste("Model", 1:2)
        )
        ## Put differences on LOOIC scale (LOOIC = -2*ELPD)
        looTab <- looTab %>%
            mutate(elpd_diff = 2 * elpd_diff,
                   se = sqrt(2) * se) %>%
            rename(`$\\Delta$LOOIC` = elpd_diff,
                   `se($\\Delta$LOOIC)` = se) %>%
            left_join(., loo_lookup, by = "mID") %>%
            select(Model, everything())
        ## Confirm 'best model' as that which reduces LOOIC by over 2 sd
        best <- case_when(
            looTab[2, 9] > 2 * looTab[2, 10] ~ looTab[1, 8],
            TRUE ~ looTab[2, 8]
        )
    } else {
        samples_year <- fitAndCheckModels(
            stan_data, model_year
        )
        samples_year_reef <- fitAndCheckModels(
            stan_data, model_year_reef
        )
        samples_year_reef_diver <- fitAndCheckModels(
            stan_data, model_year_reef_diver
        )
        ## Calculate LOOIC
        ll_year <- extract_log_lik(samples_year$samples,
                                   "loglik")
        loo_year <- loo(ll_year)
        ll_year_reef <- extract_log_lik(samples_year_reef$samples,
                                        "loglik")
        loo_year_reef <- loo(ll_year_reef)
        ll_year_reef_diver <- extract_log_lik(samples_year_reef_diver$samples,
                                              "loglik")
        loo_year_reef_diver <- loo(ll_year_reef_diver)
        looTab <- as.data.frame(compare(
            loo_year, loo_year_reef, loo_year_reef_diver)) %>%
            mutate(mID = rownames(.))
        names(looTab) <- c("LOOIC", "se(LOOIC)", "ELPD", "se(ELPD)", "Eff. P",
                           "se(Eff. P)", "mID")
        ## Algorithmically figure out which is best...
        ord <- looTab$mID
        diffs <- rbind(
            rep(NA, 2),
            compare(get(ord[2]), get(ord[1])),
            compare(get(ord[3]), get(ord[2]))
        ) %>%
            as_data_frame() %>%
            mutate(mID = ord)
        looTab <- left_join(looTab, diffs, by = "mID")
        loo_lookup <- data_frame(
            mID = sort(looTab[['mID']]),
            Model = paste("Model", 1:3)
        )
        ## Put differences on LOOIC scale (LOOIC = -2*ELPD)
        looTab <- looTab %>%
            mutate(elpd_diff = 2 * elpd_diff,
                   se = sqrt(2) * se) %>%
            rename(`$\\Delta$LOOIC` = elpd_diff,
                   `se($\\Delta$LOOIC)` = se) %>%
            left_join(., loo_lookup, by = "mID") %>%
            select(Model, everything())
        ## Confirm 'best model' as that which reduces LOOIC by over 2 sd
        best <- case_when(
            looTab[2, 9] > 2 * looTab[2, 10] ~ looTab[1, 8],
            looTab[3, 9] > 2 * looTab[3, 10] ~ looTab[2, 8],
            TRUE ~ looTab[3, 8]
        )
    }
    ## Send the best model off for graphics post-processing.
    createGraphics(
        stan_data, get(gsub("loo", "samples", best))[['samples']],
        model_id = best, min_year = min_year, smu = smu
    )
    smu_nm <- gsub(" ", "_", tolower(smu))
    ## If save_output TRUE, save output
    if (save_output) {
        saveRDS(
            get(gsub("loo", "samples", best))[['samples']],
            file = here("data", paste0(smu_nm, "_model_data.rds"))
        )
    }
    ## Save loo table
    saveRDS(
        looTab %>% select(-mID),
        file = here("data", paste0(smu_nm, "_loo.rds"))
    )
    ## Save best model label
    saveRDS(
        best,
        file = here("data", paste0(smu_nm, "_best_model.rds"))
    )
    ## Save best model diagnostics
    saveRDS(
        list(max_div = get(gsub("loo", "samples", best))[['max_div']],
             max_rhats = get(gsub("loo", "samples", best))[['max_rhats']]),
        file = here("data", paste0(smu_nm, "_best_model_diags.rds"))
    )
}

#' Create graphics for chosen model
#'
#' This function creates some graphics for the chosen model. Graphics include
#' standardised CPUE over time, and MCMC diagnostics for some key parameters.
createGraphics <- function(data, model_data, model_id, min_year, smu) {
    ## Summarise actual data
    data <- data[c("year", "diver", "reef", "volume", "effort")] %>%
        as_data_frame() %>%
        mutate(
            cpue = volume / effort,
            year = year + min_year - 1
        ) %>%
        group_by(year) %>%
        summarise(mean_cpue = exp(mean(log(cpue))))

    ## Now calculate standardised cpue
    std_cpue <- extract(model_data, "new_cpue")$new_cpue
    std_cpue <- std_cpue %>%
        as_data_frame() %>%
        gather(year, cpue) %>%
        mutate(year = as.integer(gsub("[aA-zZ]", "", year)) + min_year - 1) %>%
        group_by(year) %>%
        summarise(ll1 = quantile(cpue, 0.1, names = FALSE),
                  ll2 = quantile(cpue, 0.25, names = FALSE),
                  med = quantile(cpue, 0.5, names = FALSE),
                  ul2 = quantile(cpue, 0.75, names = FALSE),
                  ul1 = quantile(cpue, 0.9, names = FALSE))

    ## Create graphic
    pl_cpue <- ggplot(std_cpue, aes(x = year, xend = year, y = med)) +
        geom_segment(data = std_cpue, aes(y = ll1, yend = ul1),
                     colour = "lightblue", lineend = "round") +
        geom_segment(data = std_cpue, aes(y = ll2, yend = ul2),
                     colour = "darkblue", size = 1.5, lineend = "round") +
        geom_point(data = data, aes(x = year, y = mean_cpue),
                   shape = 21, size = 4, colour = "black", fill = "white") +
        geom_point(colour = "darkblue", fill = "lightblue",
                   size = 4, shape = 21) +
        xlab("Year") +
        ylab("Standardised CPUE")

    ## MCMC convergences of key parameters: alpha, and standard deviations
    pl_alpha <- traceplot(model_data, par = "alpha")
    pl_year <- traceplot(model_data, par = "sigma_year")
    pl_diver <- traceplot(model_data, par = "sigma_diver")
    if (model_id == "loo_year") {
        pl_reef <- traceplot(model_data, par = "sigma_reef")
        plot_list <- list(pl_cpue = pl_cpue, pl_alpha = pl_alpha,
                          pl_year = pl_year, pl_reef = pl_reef,
                          pl_diver = pl_diver)
    } else if (model_id == "loo_year_reef") {
        pl_reef <- traceplot(model_data, par = "sigma_reef")
        pl_year_reef <- traceplot(model_data, par = "sigma_year_reef", ncol = 2)
        plot_list <- list(pl_cpue = pl_cpue, pl_alpha = pl_alpha,
                          pl_year = pl_year, pl_reef = pl_reef,
                          pl_diver = pl_diver, pl_year_reef = pl_year_reef)
    } else if (model_id == "loo_year_reef_diver") {
        pl_reef <- traceplot(model_data, par = "sigma_reef")
        pl_year_reef <- traceplot(model_data, par = "sigma_year_reef", ncol = 2)
        pl_year_diver <- traceplot(model_data, par = "sigma_year_diver", ncol = 2)
        plot_list <- list(pl_cpue = pl_cpue, pl_alpha = pl_alpha,
                          pl_year = pl_year, pl_reef = pl_reef,
                          pl_diver = pl_diver, pl_year_reef = pl_year_reef,
                          pl_year_diver = pl_year_diver)
    } else if (model_id == "loo_year" && smu == "CLIFFY GROUP") {
        plot_list <- list(pl_cpue = pl_cpue, pl_alpha = pl_alpha,
                          pl_year = pl_year, pl_diver = pl_diver)
    } else if (model_id == "loo_year_diver") {
        pl_year_diver <- traceplot(model_data, par = "sigma_year_diver", ncol = 2)
        plot_list <- list(pl_cpue = pl_cpue, pl_alpha = pl_alpha,
                          pl_year = pl_year, pl_diver = pl_diver,
                          pl_year_diver = pl_year_diver)
    }
    ## Save plots
    smu_nm <- gsub(" ", "_", tolower(smu))
    for (j in seq_len(length(plot_list))) {
        pl_name <- here("figs", paste0(smu_nm, "_", names(plot_list)[j], ".png"))
        if (names(plot_list)[j] %in% c("pl_year_reef", "pl_year_diver")) {
            ht <- 2 * 14 / (1 + sqrt(5))
        } else {
            ht <- 14 / (1 + sqrt(5))
        }
        ggsave(pl_name, plot_list[[j]], width = 7, height = ht)
    }
    saveRDS(std_cpue,
            file = here("data", paste0(smu_nm, "_std_cpue", ".rds")))
}
