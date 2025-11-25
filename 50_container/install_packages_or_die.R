#!/usr/bin/env Rscript
library(pak)

n_cores <- parallel::detectCores()
options(Ncpus = n_cores)

args = commandArgs(trailingOnly=TRUE)

#install.packages(args[1], dependencies=TRUE, type="source", repos=args[2]);
pak::pak(c(args[1]))

if ( ! library(args[1], character.only=TRUE, logical.return=TRUE) ) {
  quit(status=1, save='no')
}

#Use like below
#ADD install_packages_or_die.R /
#RUN Rscript --no-save install_packages_or_die.R profvis devtools memoise
#RUN Rscript --no-save install_packages_or_die.R memoise nosuchpackage

# #1 answer to
#https://stackoverflow.com/questions/26244530/how-do-i-make-install-packages-return-an-error-if-an-r-package-cannot-be-install
