#!/usr/bin/env Rscript

# From http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile/1189826#1189826
r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)
rm(r)

install.packages("jsonlite")
