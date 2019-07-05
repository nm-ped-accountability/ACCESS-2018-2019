
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
raw <- read.csv("ADDED DEMO NM_Summative_StudRR_File_2019-06-26.csv",
                header = TRUE, stringsAsFactors = FALSE)
dat <- raw

schools <- read.csv("Master Schools 2019 V3.csv", 
                    header = TRUE, stringsAsFactors = FALSE)

################################################################################
## recode variables

# schnumb