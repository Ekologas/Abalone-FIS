################################################################################
################################################################################
## Title: Create Models
## Author: Steve Lane
## Date: Tuesday, 03 April 2018
## Synopsis: This script loops over a collection of spatial management units
## (SMU's) to produce modelling outputs for CPUE standardisation. The outputs
## are saved, and then post-processed via the cpue-standardisation report.
## Time-stamp: <2018-04-08 11:51:15 (slane)>
################################################################################
################################################################################
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(parallel)
library(rstan)
library(loo)
library(ggplot2)
library(rmarkdown)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 2)

## Load required scripts
source(here("R", "createStanData.R"))
source(here("R", "stan-helpers.R"))
source(here("R", "fitAndCheckModels.R"))
source(here("R", "themeVFA.R"))
theme_set(themeVFA(16))

## Load data
data_file <- read_excel(
    here("data-raw", "RawCatchEffortUnfiltered_1979_2016_DiverPFN.xlsx")
)
SMU <- c('Airport', 'BACK BEACHES', 'CAPE LIPTRAP', 'CAPE OTWAY',
         'CLIFFY GROUP', 'FLINDERS', 'Julia Percy Island', 'KILCUNDA',
         'Mallacoota Central', 'Mallacoota East', 'Mallacoota Large',
         'Mallacoota Small', 'Mallacoota West', 'Marlo', 'PHILLIP ISLAND',
         'Port Fairy', 'Portland', 'PROM EASTSIDE', 'PROM WESTSIDE',
         'SHIPWRECK COAST', 'SURFCOAST', 'Warrnambool')

## Create data/reports directory for modelling outputs it it doesn't exist.
if(!dir.exists(here("data"))) dir.create(here("data"))
if(!dir.exists(here("reports"))) dir.create(here("reports"))

## Function to fit models and produce output
fitMod <- function(smu, data_file, min_year, max_year, save_output = FALSE) {
    ## Create the models
    createModels(
        smu = smu,
        data = data_file,
        min_year = min_year,
        max_year = max_year,
        save_output = FALSE
    )
    sm <- gsub(" ", "_", tolower(smu))
    infile <- here("Rmd", "cpue-standardisation-reports.Rmd")
    outfile <- here("reports", paste0("standardisation_report_", sm, ".html"))
    render(infile, output_file = outfile,
           params = list(input_data = data_file, smu = smu, min_year = min_year,
                         max_year = max_year))
    outfile <- here("reports", paste0("standardisation_report_", sm, ".docx"))
    render(infile, output_file = outfile, output_format = "word_document",
           params = list(input_data = data_file, smu = smu, min_year = min_year,
                         max_year = max_year))
}

## Fit models (restricted to years 2000 and above)
## Due to the types of models, this can be quite time consuming, 30-60 minutes
## per SMU.
## Uncomment and run the sapply command to fit all SMU's
min_year <- 2000
max_year <- max(data_file$QuotaYear)
## sapply(SMU, fitMod, data_file = data_file, min_year = min_year,
##        max_year = max_year)
