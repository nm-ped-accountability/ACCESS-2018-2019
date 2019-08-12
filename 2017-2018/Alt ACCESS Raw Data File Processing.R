
### Process Alt ACCESS raw data files 2017-2018

################################################################################
### open and process raw data file
################################################################################

rm(list = ls())
library(stringi)
library(lubridate)
library(Hmisc)
library(tidyverse)

# open files
raw <- read.csv("ADDED DEMO NM_Alternate_StudRR_File 2019-07-18.csv",
                header = TRUE, stringsAsFactors = FALSE)
dat <- raw
nrow(dat)
# 2019: 540

schools <- read.csv("Master Schools 2019 V3.csv", 
                    header = TRUE, stringsAsFactors = FALSE)
schools <- schools[schools$ï..SY == 2019, ]
nrow(schools)
# 2019: 1802

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
dat[is.na(dat$distname), ] #none

# schcode
dat$schcode <- dat$School.Number

# schname
dat$schname <- schools$schname[match(dat$test_schnumb, schools$schnumb)]
dat[is.na(dat$schname), ] #none

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
table(dat$S_GRADE)
dat$STARS_grade <- dat$S_GRADE

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
# 2 students are not special ed

# plan 504
dat$plan504 <- dat$S_PLAN504
table(dat$plan504)
dat$plan504 <- gsub("Yes", "Plan 504", dat$plan504)
dat$plan504 <- gsub("No", "Not Plan 504", dat$plan504)
dat$plan504 <- gsub("NULL", "Not Plan 504", dat$plan504)
table(dat$plan504)

# frl
table(dat$S_FRLP)
dat$frl[dat$S_FRLP == "F"] <- "Economically Disadvantaged"
dat$frl[dat$S_FRLP == "R"] <- "Economically Disadvantaged"
dat$frl[dat$S_FRLP == "N"] <- "Non ED"
table(dat$frl)

# ell
table(dat$S_ELL_STATUS)
table(dat$S_ELL_LEVEL)
dat[dat$S_ELL_STATUS == "Y" & dat$S_ELL_LEVEL != 1, ] #none
dat$ell[dat$S_ELL_STATUS == "Y"] <- "English Learners"
dat$ell[dat$S_ELL_STATUS == "N"] <- "Non EL"
table(dat$ell)
# 29 students are classified as "Non ELL"

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
dat$testname <- "Alt ACCESS"

# subtest
dat$subtest <- "Alt ACCESS"

# test language
dat$testlang <- "E"

# accommodation
table(dat$BR...Accommodation) #Braille, Y, C(contracted), U(uncontracted) possible
dat$accommodation[dat$BR...Accommodation == "Y"] <- 1 

table(dat$EM...Accommodation) #extended testing of a test domain over multiple days
dat$accommodation[dat$EM...Accommodation == "Y"] <- 1 

table(dat$ES...Accommodation) #extended speaking test response time
dat$accommodation[dat$ES...Accommodation == "Y"] <- 1 

table(dat$ET...Accommodation) #extended testing time within the school day
dat$accommodation[dat$ET...Accommodation == "Y"] <- 1 

table(dat$HI...Accommodation) #human reader for items
dat$accommodation[dat$HI...Accommodation == "Y"] <- 1 

table(dat$HR...Accommodation) #human reader for response options
dat$accommodation[dat$HR...Accommodation == "Y"] <- 1 

table(dat$LP...Accommodation) #large print
dat$accommodation[dat$LP...Accommodation == "Y"] <- 1 

table(dat$MC...Accommodation) #manual control of item audio
dat$accommodation[dat$MC...Accommodation == "Y"] <- 1 

table(dat$NS...Accommodation) #test may be administered in a non-school setting
dat$accommodation[dat$NS...Accommodation == "Y"] <- 1

table(dat$RA...Accommodation) #repeate item audio
dat$accommodation[dat$RA...Accommodation == "Y"] <- 1

table(dat$RD...Accommodation) #student responds using a recording device, which is played back and transcribed by the student
dat$accommodation[dat$RD...Accommodation == "Y"] <- 1 

table(dat$RI...Accommodation) #human reader for repeat of items
dat$accommodation[dat$RI...Accommodation == "Y"] <- 1

table(dat$RR...Accommodation) #human reader for repeat of response options
dat$accommodation[dat$RR...Accommodation == "Y"] <- 1 

table(dat$SD...Accommodation) #interpreter signs test directions in ASL
dat$accommodation[dat$SD...Accommodation == "Y"] <- 1 

table(dat$SR...Accommodation) #scribe
dat$accommodation[dat$SR...Accommodation == "Y"] <- 1 

table(dat$WD...Accommodation) #word processor
dat$accommodation[dat$WD...Accommodation == "Y"] <- 1

table(dat$Accommodations...Test.directions)
dat$accommodation[dat$Accommodations...Test.directions == "Y"] <- 1

table(dat$Accommodations...Presentation.Format)
dat$accommodation[dat$Accommodations...Presentation.Format == "Y"] <- 1

table(dat$Accommodations...Response.Format)
dat$accommodation[dat$Accommodations...Response.Format == "Y"] <- 1

table(dat$Accommodations...Setting.format.environment)
dat$accommodation[dat$Accommodations...Setting.format.environment == "Y"] <- 1

table(dat$Accommodations...Timing.scheduling)
dat$accommodation[dat$Accommodations...Timing.scheduling == "Y"] <- 1

table(dat$Accommodations...Other)
dat$accommodation[dat$Accommodations...Other == "Y"] <- 1

dat$accommodation[is.na(dat$accommodation)] <- 0
table(dat$accommodation)

# mode
dat$mode <- "Paper"

# testbookid
dat$testbookid <- dat$Unique.DRC.Student.ID

# SS
dat$SS_listen <- dat$Listening.Scale.Score
dat$SS_read <- dat$Reading.Scale.Score
dat$SS_speak <- dat$Speaking.Scale.Score
dat$SS_write <- dat$Writing.Scale.Score
dat$SS_comprehension <- dat$Comprehension.Score
dat$SS_oral <- dat$Oral.Scale.Score
dat$SS_literacy <- dat$Literacy.Scale.Score
dat$SS_composite <- dat$Composite..Overall..Scale.Score
range(dat$SS_listen, na.rm = TRUE)
range(dat$SS_read, na.rm = TRUE)
range(dat$SS_speak, na.rm = TRUE)
range(dat$SS_write, na.rm = TRUE)
range(dat$SS_comprehension, na.rm = TRUE)
range(dat$SS_oral, na.rm = TRUE)
range(dat$SS_literacy, na.rm = TRUE)
range(dat$SS_composite, na.rm = TRUE)

# PL
dat$PL_listen <- dat$Listening.Proficiency.Level
dat$PL_read <- dat$Reading.Proficiency.Level
dat$PL_speak <- dat$Speaking.Proficiency.Level
dat$PL_write <- dat$Writing.Proficiency.Level
dat$PL_comprehension <- dat$Comprehension.Proficiency.Level
dat$PL_oral <- dat$Oral.Proficiency.Level
dat$PL_literacy <- dat$Literacy.Proficiency.Level
dat$PL_composite <- dat$Composite..Overall..Proficiency.Level
table(dat$PL_listen)
table(dat$PL_read)
table(dat$PL_speak)
table(dat$PL_write)
table(dat$PL_comprehension)
table(dat$PL_oral)
table(dat$PL_literacy)
table(dat$PL_composite)

# proficient
dat$proficient[dat$PL_composite == "P1" | dat$PL_composite == "P2"] <- 1
dat$proficient[is.na(dat$proficient)] <- 0
table(dat$proficient)

# valid
# listening
dat$valid_listen <- 1
dat$valid_listen[dat$Do.Not.Score.Code...Listening == "ABS"] <- 0 #absent
dat$valid_listen[dat$Do.Not.Score.Code...Listening == "INV"] <- 0 #invalid
dat$valid_listen[dat$Do.Not.Score.Code...Listening == "DEC"] <- 0 #declined
dat$valid_listen[dat$Do.Not.Score.Code...Listening == "SPD"] <- 2 #deferred special ed/504
table(dat$Do.Not.Score.Code...Listening)
table(dat$valid_listen) 
# 2019: 14
sum(is.na(dat$SS_listen)) 
# 2019: 24
dat[dat$valid_listen == 1 & is.na(dat$SS_listen), ]
dat[dat$valid_listen == 0 & !is.na(dat$SS_listen), ] #none

# reading
dat$valid_read <- 1
dat$valid_read[dat$Do.Not.Score.Code...Reading == "ABS"] <- 0
dat$valid_read[dat$Do.Not.Score.Code...Reading == "INV"] <- 0
dat$valid_read[dat$Do.Not.Score.Code...Reading == "DEC"] <- 0
dat$valid_read[dat$Do.Not.Score.Code...Reading == "SPD"] <- 2
table(dat$Do.Not.Score.Code...Reading)
table(dat$valid_read)
# 2019: 10
sum(is.na(dat$SS_read))
# 2019: 19
dat[dat$valid_read == 1 & is.na(dat$SS_read), ]
dat[dat$valid_read == 0 & !is.na(dat$SS_read), ] #none

# speaking
dat$valid_speak <- 1
dat$valid_speak[dat$Do.Not.Score.Code...Speaking == "ABS"] <- 0
dat$valid_speak[dat$Do.Not.Score.Code...Speaking == "INV"] <- 0
dat$valid_speak[dat$Do.Not.Score.Code...Speaking == "DEC"] <- 0
dat$valid_speak[dat$Do.Not.Score.Code...Speaking == "SPD"] <- 2
table(dat$Do.Not.Score.Code...Speaking)
table(dat$valid_speak)
# 2019: 13
sum(is.na(dat$SS_speak))
# 2019: 16
dat[dat$valid_speak == 1 & is.na(dat$SS_speak), ]
dat[dat$valid_speak == 0 & !is.na(dat$SS_speak), ] #none

# writing
dat$valid_write <- 1
dat$valid_write[dat$Do.Not.Score.Code...Writing == "ABS"] <- 0
dat$valid_write[dat$Do.Not.Score.Code...Writing == "INV"] <- 0
dat$valid_write[dat$Do.Not.Score.Code...Writing == "DEC"] <- 0
dat$valid_write[dat$Do.Not.Score.Code...Writing == "SPD"] <- 2
table(dat$Do.Not.Score.Code...Writing)
table(dat$valid_write)
# 2019: 7
sum(is.na(dat$SS_write))
# 2019: 23
dat[dat$valid_write == 1 & is.na(dat$SS_write), ]
dat[dat$valid_write == 0 & !is.na(dat$SS_write), ] #none

# snapshot date
dat$status <- dat$STATUS
table(dat$status)
# 2019: all from the current year


################################################################################
## process invalid records and save file

# tally missing domains
dat$missing_listen[is.na(dat$SS_listen)] <- 1
dat$missing_listen[!is.na(dat$SS_listen)] <- 0
table(dat$missing_listen)
# 2019: 24
dat$missing_read[is.na(dat$SS_read)] <- 1
dat$missing_read[!is.na(dat$SS_read)] <- 0
table(dat$missing_read)
# 2019: 19
dat$missing_speak[is.na(dat$SS_speak)] <- 1
dat$missing_speak[!is.na(dat$SS_speak)] <- 0
table(dat$missing_speak)
# 2019: 16
dat$missing_write[is.na(dat$SS_write)] <- 1
dat$missing_write[!is.na(dat$SS_write)] <- 0
table(dat$missing_write)
# 2019: 23
dat$missing_domains <- rowSums(dat[, c("missing_listen", 
                                       "missing_read", 
                                       "missing_speak", 
                                       "missing_write")])
table(dat$missing_domains)
table(dat$missing_domains, dat$plan504)
table(dat$missing_domains, dat$swd)

# tabulate missing domains and invalidation codes
table(dat$missing_listen, dat$valid_listen)
table(dat$missing_listen, dat$accommodation)
dat$missing_domains[dat$missing_listen == 1 & dat$valid_listen == 2]
dat$accommodation[dat$missing_listen == 1 & dat$valid_listen == 2]
# 2019: 7 cases had SPD

table(dat$missing_read, dat$valid_read)
dat$missing_domains[dat$missing_read == 1 & dat$valid_read == 2]
# 2019: 3 cases had SPD

table(dat$missing_speak, dat$valid_speak)
dat$missing_domains[dat$missing_speak == 1 & dat$valid_speak == 2]
# 2019: 8 cases had SPD

table(dat$missing_write, dat$valid_write)
dat$missing_domains[dat$missing_write == 1 & dat$valid_write == 2]
# 2019: 2 cases had SPD

# remove extra columns
names(dat)
dat <- dat[c(214:272)]
names(dat)

# save student-level file
current_date <- Sys.Date()

file_name <- paste0("Alt ACCESS for ELLs 2018-2019 Cleaned ", 
                    current_date, ".csv")

write.csv(dat, file = file_name, row.names = FALSE)

nrow(dat) 
# 2019: 540

################################################################################
# remove student who are missing composite scores
dat <- dat[dat$PL_composite != "", ]
dat <- dat[!is.na(dat$test_schnumb), ]
nrow(dat)
# 2019: 495

# save student-level file with complete cases
current_date <- Sys.Date()

file_name <- paste0("Alt ACCESS for ELLs 2018-2019 Complete Cases ",
                    current_date, ".csv")

write.csv(dat, file = file_name, row.names = FALSE)