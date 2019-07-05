
### ACCESS for ELLs SOAP and Web Files

################################################################################
### open and process raw data file
################################################################################

rm(list = ls())
library(stringi)
library(lubridate)
library(Hmisc)
library(tidyverse)

# open files
raw <- read.csv("ADDED DEMO NM_Summative_StudRR_File_2019-07_05.csv",
                header = TRUE, stringsAsFactors = FALSE)
dat <- raw

schools <- read.csv("Master Schools 2019 V3.csv", 
                    header = TRUE, stringsAsFactors = FALSE)

################################################################################
## recode variables

# test_schnumb, STARS_schnumb
dat$District.Number <- gsub("NM", "", dat$District.Number)
dat$District.Number <- as.numeric(dat$District.Number)
dat$test_schnumb <- dat$District.Number * 1000 + dat$School.Number
dat$STARS_schnumb <- dat$S_DISTRICT_CODE * 1000 + dat$S_LOCATION_CODE

# distcode
dat$distcode <- dat$District.Number

# distname
dat$distname <- schools$distname[match(dat$distcode, schools$distcode)]

# schcode
dat$schcode <- dat$School.Number

# schname
dat$schname <- schools$schname[match(dat$test_schnumb, schools$schnumb)]

# stid
dat$stid <- dat$STUID
range(dat$stid)

# last
dat$last <- dat$S_LASTNAME

# first
dat$first <- dat$S_FIRSTNAME
