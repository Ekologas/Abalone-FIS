################################################################################
################################################################################
## Title: Create Outlier Reports
## Author: Steve Lane, interadata
## Date: Saturday, 24 March 2018
## Synopsis: This script loops over a collection of spatial management units
## (SMU's) to produce multiple html reports of possible diver outliers within
## each SMU.
## Time-stamp: <2018-03-25 14:53:17 (slane)>
################################################################################
################################################################################
library(here)
library(rmarkdown)
data_file <- here(
    "data-raw",
    "RawCatchEffortUnfiltered_1979_2016_DiverPFN.xlsx"
)
SMU <- c('Airport', 'BACK BEACHES', 'CAPE LIPTRAP', 'CAPE OTWAY',
         'CLIFFY GROUP', 'FLINDERS', 'Julia Percy Island', 'KILCUNDA',
         'Mallacoota Central', 'Mallacoota East', 'Mallacoota Large',
         'Mallacoota Small', 'Mallacoota West', 'Marlo', 'PHILLIP ISLAND',
         'Port Fairy', 'Portland', 'PROM EASTSIDE', 'PROM WESTSIDE',
         'SHIPWRECK COAST', 'SURFCOAST', 'Warrnambool')

## Create reports directory if it doesn't exist
if(!dir.exists(here("reports"))) dir.create(here("reports"))

## Function to run rmarkdown script and save output to reports directory
create_report <- function(smu, datafile) {
    SMU <- gsub(" ", "_", tolower(smu))
    infile <- here("Rmd", "cpue-qa-reports.Rmd")
    outfile <- here("reports", paste0("candidate_outliers_", SMU, ".html"))
    render(infile, output_file = outfile,
           params = list(input_data = datafile, smu = smu))
}

## Loop over the SMU's and create the reports. This command is commented out, so
## that it must be run interactively. Just copy the following into the R console
## to create the reports.
## sapply(SMU, create_report, datafile = data_file)

################################################################################
################################################################################
## Begin Section: The following code creates docx output. If you wish for this,
## uncomment the sapply line.
################################################################################
################################################################################
## Function to run rmarkdown script and save output to reports directory
create_report_word <- function(smu, datafile) {
    SMU <- gsub(" ", "_", tolower(smu))
    infile <- here("Rmd", "cpue-qa-reports.Rmd")
    outfile <- here("reports", paste0("candidate_outliers_", SMU, ".docx"))
    render(infile, output_file = outfile, output_format = "word_document",
           params = list(input_data = datafile, smu = smu))
}

## Loop over the SMU's and create the reports (uncomment for docx reports). This
## command is commented out, so that it must be run interactively. Just copy the
## following into the R console to create the reports.
## sapply(SMU, create_report_word, datafile = data_file)
