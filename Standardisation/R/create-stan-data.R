################################################################################
################################################################################
## Title: Create Stan data
## Author: Steve Lane, interadata
## Date: Sunday, 01 April 2018
## Synopsis: Creates data lists for Stan models.
## Time-stamp: <2018-04-02 12:10:49 (slane)>
################################################################################
################################################################################
## This function creates the data restricted to year/SMU, and aggregates the
## catch and effort data by reef code. Filter as well, just in case.
smu_year_model <- function(data, smu, year) {
    data <- data %>%
        filter(SMU == smu, QuotaYear == year, !is.na(Effort), Effort > 0) %>%
        mutate(
            volume = Blacklip + Greenlip
        ) %>%
        group_by(ReefCode, Diver) %>%
        summarise(volume = sum(volume), effort = sum(Effort))
    ## Create unique integer codes
    reef_lookup <- data_frame(
        ReefCode = unique(data$ReefCode),
        Reef_ID = seq_len(length(ReefCode))
    )
    diver_lookup <- data_frame(
        Diver = unique(data$Diver),
        Diver_ID = seq_len(length(Diver))
    )
    data <- data %>%
        left_join(., reef_lookup, by = "ReefCode") %>%
        left_join(., diver_lookup, by = "Diver")
    ## Output list of data for Stan
    list(n = nrow(data),
         n_reefs = nrow(reef_lookup),
         n_divers = nrow(diver_lookup),
         reef = data[['Reef_ID']],
         diver = data[['Diver_ID']],
         volume = data[['volume']],
         effort = data[['effort']])
}

## This function creates the data restricted to year/SMU, and leaves the
## catch and effort data by reef code as individual observations. Filter as
## well, just in case.
smu_year_model_ind <- function(data, smu, year) {
    data <- data %>%
        filter(SMU == smu, QuotaYear == year, !is.na(Effort), Effort > 0) %>%
        mutate(
            volume = Blacklip + Greenlip
        )
    ## Create unique integer codes
    reef_lookup <- data_frame(
        ReefCode = unique(data$ReefCode),
        Reef_ID = seq_len(length(ReefCode))
    )
    diver_lookup <- data_frame(
        Diver = unique(data$Diver),
        Diver_ID = seq_len(length(Diver))
    )
    data <- data %>%
        left_join(., reef_lookup, by = "ReefCode") %>%
        left_join(., diver_lookup, by = "Diver")
    ## Output list of data for Stan
    list(n = nrow(data),
         n_reefs = nrow(reef_lookup),
         n_divers = nrow(diver_lookup),
         reef = data[['Reef_ID']],
         diver = data[['Diver_ID']],
         volume = data[['volume']],
         effort = data[['Effort']])
}

## This function creates the data restricted to SMU, aggregating catch and
## effort data by reef code and diver. Interaction terms will be created using
## matrices inside Stan, so no need to set them up here. Filter as well, just in
## case, and allow a year cutoff for simplicity (if NULL uses all years).
## Furthermore, need a year/s for standardising. If NULL (default) will use the
## latest year in the data.
smu_model <- function(data, smu, year_cutoff = NULL, pred_year = NULL) {
    year_cutoff <- ifelse(is.null(year_cutoff), min(data$QuotaYear),
                          year_cutoff)
    data <- data %>%
        filter(SMU == smu, !is.na(Effort), Effort > 0,
               QuotaYear >= year_cutoff) %>%
        mutate(
            volume = Blacklip + Greenlip
        ) %>%
        group_by(QuotaYear, ReefCode, Diver) %>%
        summarise(volume = sum(volume), effort = sum(Effort))
    ## Create unique integer codes
    reef_lookup <- data_frame(
        ReefCode = unique(data$ReefCode),
        Reef_ID = seq_len(length(ReefCode))
    )
    diver_lookup <- data_frame(
        Diver = unique(data$Diver),
        Diver_ID = seq_len(length(Diver))
    )
    data <- data %>%
        left_join(., reef_lookup, by = "ReefCode") %>%
        left_join(., diver_lookup, by = "Diver") %>%
        mutate(Year_ID = QuotaYear - year_cutoff + 1)
    ## Year preds
    if(is.null(pred_year)) {
        pred_year <- array(max(data[['Year_ID']]), 1)
        n_pred_year <- 1
    } else {
        n_pred_year <- length(pred_year)
        pred_year <- array(pred_year - year_cutoff + 1, n_pred_year)
    }
    ## Output list of data for Stan
    list(n = nrow(data),
         n_reefs = nrow(reef_lookup),
         n_divers = nrow(diver_lookup),
         n_years = max(data[['Year_ID']]),
         reef = data[['Reef_ID']],
         diver = data[['Diver_ID']],
         year = data[['Year_ID']],
         volume = data[['volume']],
         effort = data[['effort']],
         n_pred_year = n_pred_year,
         pred_year = pred_year)
}
