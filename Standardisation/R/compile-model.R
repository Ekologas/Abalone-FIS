#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
################################################################################
################################################################################
## Title: Compile model
## Author: Steve Lane, interadata
## Time-stamp: <2018-04-01 11:11:40 (slane)>
## Synopsis: Script that compiles the stan model. Designed to be called
## from the Makefile, it requires the model name to be set on the command line,
## or prior to sourcing the script.
################################################################################
################################################################################
if(length(args) != 1){
    stop("One argument must be supplied: stan model name.\nRscript compile-model.R mname=model-name", call. = FALSE)
} else {
    hasOpt <- grepl("=", args)
    argLocal <- strsplit(args[hasOpt], "=")
    for(i in seq_along(argLocal)){
        value <- NA
        tryCatch(value <- as.double(argLocal[[i]][2]), warning = function(e){})
        if(!is.na(value)){
            ## Assume int/double
            assign(argLocal[[i]][1], value, inherits = TRUE)
        } else {
            assign(argLocal[[i]][1], argLocal[[i]][2], inherits = TRUE)
        }
    }
}
library(rstan)
## Compile stan model
rstan_options(auto_write = TRUE)
model <- stan_model(paste0("../stan/", mname, ".stan"))
################################################################################
################################################################################
