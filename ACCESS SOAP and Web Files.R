
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
schools <- schools[schools$ï..SY == 2019, ]

################################################################################
## recode variables

# test_schnumb
dat$District.Number <- gsub("NM", "", dat$District.Number)
dat$District.Number <- as.numeric(dat$District.Number)
dat$test_schnumb <- dat$District.Number * 1000 + dat$School.Number
# test_schnumb will be used

# STARS_schnumb
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

# mi
dat$mi <- dat$S_MIDDLE_NAME
dat$mi <- toupper(dat$mi)
table(dat$mi)
dat$mi <- gsub("-", "", dat$mi)
dat$mi <- gsub("'", "", dat$mi)
dat$mi <- gsub("\\$", "", dat$mi)
dat$mi <- gsub("\\.", "", dat$mi)
dat$mi <- gsub("0", "", dat$mi)
dat$mi <- gsub("NULL", "", dat$mi)
table(dat$mi)

# dob
dat$dob <- dat$S_DOB
dat$dob <- mdy(dat$dob)
str(dat$dob)

# test_grade
dat$test_grade_listen <- dat$Cluster...Listening
table(dat$test_grade_listen)
dat$test_grade_read <- dat$Cluster...Reading
table(dat$test_grade_read)
dat$test_grade_speak <- dat$Cluster...Speaking
table(dat$test_grade_speak)
dat$test_grade_write <- dat$Cluster...Writing
table(dat$test_grade_write)

# STARS_grade
dat$STARS_grade <- dat$S_GRADE
dat$STARS_grade <- gsub("KF", "0", dat$STARS_grade)
dat$STARS_grade <- as.numeric(dat$STARS_grade)
table(dat$STARS_grade)

# eth
dat$eth <- dat$S_ETNICITY
table(dat$eth)
table(dat$S_HISPANIC_INDICATOR)
dat$eth[dat$S_HISPANIC_INDICATOR == "Yes"] <- "Hispanic"
dat$eth[dat$eth == "Native Hawaiian or Other Pacific Islander"] <- "Asian"
dat$eth[dat$eth == "Black or African American"] <- "African American"
dat$eth[dat$eth == "American Indian/Alaskan Native"] <- "Native American"
table(dat$eth)

# gender
dat$gender <- dat$S_GENDER
table(dat$gender)

# swd
dat$swd <- dat$S_SPECIAL_ED
table(dat$swd)
dat$swd[dat$swd == "Y"] <- "Students with Disabilities"
dat$swd[dat$swd == "N"] <- "Non SWD"
table(dat$swd)

# frl
table(dat$S_FRLP)
dat$frl[dat$S_FRLP == "F"] <- "Economically Disadvantaged"
dat$frl[dat$S_FRLP == "R"] <- "Economically Disadvantaged"
dat$frl[dat$S_FRLP == "N"] <- "Non ED"
table(dat$frl)

# ell
dat$ell[dat$S_ELL_STATUS == "Y"] <- "English Learners"
dat$ell[dat$S_ELL_STATUS == "N"] <- "Non EL"
table(dat$ell)
# 911 students are classified as "Non ELL"

# migrant
dat$migrant[dat$S_MIGRANT == "Y"] <- "Migrants"
dat$migrant[dat$S_MIGRANT == "N"] <- "Non Migrant"
dat$migrant[dat$S_MIGRANT == "NULL"] <- "Non Migrant"
table(dat$migrant)

# military
# active, national guard, researve
table(dat$S_MILITARY)
dat$military[dat$S_MILITARY == "Active"] <- "Military"
dat$military[dat$S_MILITARY == "National Guard"] <- "Military"
dat$military[dat$S_MILITARY == "Reserve"] <- "Non Military"
dat$military[dat$S_MILITARY == "NULL"] <- "Non Military"
table(dat$military)

# homeless
table(dat$S_HOMELESS)
dat$homeless <- dat$S_HOMELESS
dat$homeless <- gsub("NULL", "Not Homeless", dat$homeless)
dat$homeless <- gsub("Student is not homeless", "Not Homeless", dat$homeless)
table(dat$homeless)

# foster
table(dat$S_FOSTER)
dat$foster[dat$S_FOSTER == "Y"] <- "Foster Care"
dat$foster[dat$S_FOSTER == "NULL"] <- "Not Foster Care"
table(dat$foster)

# test name
dat$testname <- "ACCESS"

# subtest
dat$subtest <- "ACCESS"

# test language
dat$testlang <- "E"

# accommodation
table(dat$BR...Accommodation)
dat$accommodation[dat$BR...Accommodation == "Y"] <- 1 #Braille, values of C(contracted) and U(uncontracted) are also possible
dat$accommodation[dat$EM...Accommodation == "Y"] <- 1 #extended testing of a test domain over multiple days
dat$accommodation[dat$ES...Accommodation == "Y"] <- 1 #extended speaking test response time
dat$accommodation[dat$ET...Accommodation == "Y"] <- 1 #extended testing time within the school day
dat$accommodation[dat$HI...Accommodation == "Y"] <- 1 #human reader for items
dat$accommodation[dat$HR...Accommodation == "Y"] <- 1 #human reader for response options
dat$accommodation[dat$LP...Accommodation == "Y"] <- 1 #large print
dat$accommodation[dat$MC...Accommodation == "Y"] <- 1 #manual control of item audio
dat$accommodation[dat$NS...Accommodation == "Y"] <- 1 #test may be administered in a non-school setting
dat$accommodation[dat$RA...Accommodation == "Y"] <- 1 #repeate item audio
dat$accommodation[dat$RD...Accommodation == "Y"] <- 1 #student responds using a recording device, which is played back and transcribed by the student
dat$accommodation[dat$RI...Accommodation == "Y"] <- 1 #human reader for repeat of items
dat$accommodation[dat$RR...Accommodation == "Y"] <- 1 #human reader for repeat of response options
dat$accommodation[dat$SD...Accommodation == "Y"] <- 1 #interpreter signs test directions in ASL
dat$accommodation[dat$SR...Accommodation == "Y"] <- 1 #scribe
dat$accommodation[dat$WD...Accommodation == "Y"] <- 1 #word processor
dat$accommodation[is.na(dat$accommodation)] <- 0
table(dat$accommodation)
