# Abalone Harvest Strategy

## Description

This repository is used to hold analytical code for Harry Gorfine on behalf of the Victorian Fisheries Authority. The primary purpose of this repository is to develop and document the features of the `harvestStrategy` R package, as well as the candidate outlier detection and catch per unit effort standardisation reports.

Sitting alongside this repository is an R package `harvestStrategy` which implements the harvest strategy code.

## Docker

Provided in this collection of scripts is a Dockerfile that can be used for running the provided code (see the appropriate reports in the manuscripts directory). It also installs the `harvestStrategy` package.

First, you'll need to install docker; for instructions on how to do that for Windows, see the [official documentation](https://docs.docker.com/docker-for-windows/install/).

Once docker is installed, you need to build an appropriate image. To do this, open up a `cmd` shell or `powershell`. Navigate to the directory where this README file resides. At the command line, run the following:

```sh
docker build --rm -t vfa-analysis docker/
```

This will take some time --- docker will need to download approximately 2 GB of images, and then build the appropriate packages (within the container).

Once the container has been built, you are ready to run it, and link it to the appropriately directory so that output is saved. Once again, you should make sure that you are in the appropriate directory on the command line. You then run the following command to start the container (all on one line):

``` sh
docker run --rm -t -v C:/Users/harry/vfa-analysis:/home/rstudio -p 8787:8787 vfa-analysis
```

Here `C:/Users/harry/vfa-analysis` is the directory where all the scripts are saved, i.e. the location of this README file --- important: make sure you change that location appropriately!

Once you have set the docker container running, it will start an Rstudio server; simply point your web browser to [http://localhost:8787](http://localhost:8787) and you will be in an Rstudio session where you can load and run all the scripts.
