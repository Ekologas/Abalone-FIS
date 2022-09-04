## Dockerfile For VFA Reporting
FROM rocker/verse:3.4.3
LABEL maintainer="Steve Lane"
LABEL email="steve@interadata.io"

## Update and install linux libraries
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libgsl0-dev

## Add in extra required MRAN packages
RUN . /etc/environment \
  && install2.r --error --repos $MRAN --deps TRUE \
    here metafor

## Add in rstan and loo
RUN . /etc/environment \ 
  && install2.r --error --repos $MRAN --deps TRUE \
    rstan loo

## Add in harvestStrategy package
RUN . /etc/environment \
  && Rscript --no-save --no-restore -e \
    "devtools::install_bitbucket('interadata/harvestStrategy', build_vignettes = TRUE)"

# Remove unnecessary tmp files
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Run rstudio on port 8787
CMD ["/init"]
