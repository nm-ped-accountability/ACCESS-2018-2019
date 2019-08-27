
### Process ACCESS raw data files 2017-2018

################################################################################
### open and process raw data file
################################################################################

rm(list = ls())
library(stringi)
library(lubridate)
library(Hmisc)
library(tidyverse)

# open files
raw <- read.csv("NM_Summative_StudRR_File 2017-2018 Cleaned 2019-08-12.csv",
                header = TRUE, stringsAsFactors = FALSE)
dat <- raw
nrow(dat)
# 2018: 48998

schools <- read.csv("Master Schools 2019 V3.csv", 
                    header = TRUE, stringsAsFactors = FALSE)
schools <- schools[schools$ï..SY == 2018, ]
nrow(schools)
# 2019: 1788


################################################################################
## recode variables

# test_schnumb
dat$District.Number <- gsub("NM", "", dat$District.Number)
dat$District.Number <- as.numeric(dat$District.Number)
dat$test_schnumb <- dat$District.Number * 1000 + dat$School.Number
# test_schnumb will be used
nrow(dat[is.na(dat$test_schnumb), ])

# distcode
dat$distcode <- dat$District.Number
nrow(dat[is.na(dat$distcode), ])

# distname
dat$distname <- schools$distname[match(dat$distcode, schools$distcode)]
nrow(dat[is.na(dat$distname), ])

# schcode
dat$schcode <- dat$School.Number
nrow(dat[is.na(dat$schcode), ])

# schname
dat$schname <- schools$schname[match(dat$test_schnumb, schools$schnumb)]
nrow(dat[is.na(dat$schname), ])

# stid
dat$stid <- dat$State.Student.ID
range(dat$stid, na.rm = TRUE) #valid range
nrow(dat[is.na(dat$stid), ]) #2 missing values

# last
dat$last <- dat$Student.Last.Name
nrow(dat[is.na(dat$last), ])

# first
dat$first <- dat$Student.First.Name
nrow(dat[is.na(dat$first), ])

# mi
dat$mi <- dat$Student.Middle.Name
dat$mi <- toupper(dat$mi)
table(dat$mi)
dat$mi <- gsub("-", "", dat$mi)
dat$mi <- gsub("'", "", dat$mi)
dat$mi <- gsub("\\$", "", dat$mi)
dat$mi <- gsub("\\.", "", dat$mi)
dat$mi <- gsub("0", "", dat$mi)
dat$mi <- gsub("NULL", "", dat$mi)
dat$mi <- gsub(" ", "", dat$mi)
table(dat$mi)

# dob
dat$dob <- dat$Birth.Date
str(dat$dob)
dat$dob <- mdy(dat$dob)
str(dat$dob)
nrow(dat[is.na(dat$dob), ]) #2 missing values

# test_grade
dat$grade <- dat$Grade
dat$test_grade_listen <- dat$Cluster...Listening
table(dat$test_grade_listen)
dat$test_grade_read <- dat$Cluster...Reading
table(dat$test_grade_read)
dat$test_grade_speak <- dat$Cluster...Speaking
table(dat$test_grade_speak)
dat$test_grade_write <- dat$Cluster...Writing
table(dat$test_grade_write)
nrow(dat[is.na(dat$test_grade_listen), ]) #108
nrow(dat[is.na(dat$test_grade_read), ]) #164
nrow(dat[is.na(dat$test_grade_speak), ]) #666
nrow(dat[is.na(dat$test_grade_write), ]) #634

# eth
dat$eth[dat$Race...American.Indian.Alaskan.Native == "Y"] <- "Native American"
dat$eth[dat$Race...Asian == "Y"] <- "Asian"
dat$eth[dat$Race...Pacific.Islander.Hawaiian == "Y"] <- "Asian"
dat$eth[dat$Race...Black.African.American == "Y"] <- "African American"
dat$eth[dat$Race...White == "Y"] <- "Caucasian"
dat$eth[dat$Ethnicity...Hispanic.Latino == "Y"] <- "Hispanic"
table(dat$eth)
nrow(dat[is.na(dat$eth), ]) #2
dat[is.na(dat$eth), ] #the two students who can't be found in STARS

# gender
dat$gender[dat$Gender == "F"] <- "Female"
dat$gender[dat$Gender == "M"] <- "Male"
table(dat$gender)
nrow(dat[is.na(dat$gender), ]) #2

# swd
dat$swd <- dat$IEP.Status
table(dat$swd)
dat$swd[dat$swd == "Y"] <- "Students with Disabilities"
dat$swd[dat$swd == ""] <- "Non SWD"
table(dat$swd)
nrow(dat[is.na(dat$swd), ])

# plan 504
table(dat$X504.Plan)
dat$plan504[dat$X504.Plan == "Y"] <- "Plan 504"
dat$plan504[dat$X504.Plan == ""] <- "Not Plan 504"
table(dat$plan504)
nrow(dat[is.na(dat$plan504), ])

# migrant
table(dat$Migrant)
dat$migrant[dat$Migrant == "Y"] <- "Migrants"
dat$migrant[dat$Migrant == ""] <- "Non Migrant"
table(dat$migrant)
nrow(dat[is.na(dat$migrant), ])

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

# mode
table(dat$Mode.of.Administration...Listening)
dat$mode_listen <- dat$Mode.of.Administration...Listening
table(dat$Mode.of.Administration...Reading)
dat$mode_read <- dat$Mode.of.Administration...Reading
table(dat$Mode.of.Administration...Speaking)
dat$mode_speak <- dat$Mode.of.Administration...Speaking
table(dat$Mode.of.Administration...Writing)
dat$mode_write <- dat$Mode.of.Administration...Writing

# testbookid
dat$testbookid <- dat$Unique.DRC.Student.ID

# SS
dat$SS_listen <- dat$Listening.Scale.Score
dat$SS_read <- dat$Reading.Scale.Score
dat$SS_speak <- dat$Speaking.Scale.Score
dat$SS_write <- dat$Writing.Scale.Score
dat$SS_comprehension <- dat$Comprehension.Scale.Score
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
dat$PL_integer[dat$PL_composite >= 1 & dat$PL_composite < 2] <- 1
dat$PL_integer[dat$PL_composite >= 2 & dat$PL_composite < 3] <- 2
dat$PL_integer[dat$PL_composite >= 3 & dat$PL_composite < 4] <- 3
dat$PL_integer[dat$PL_composite >= 4 & dat$PL_composite < 5] <- 4
dat$PL_integer[dat$PL_composite >= 5 & dat$PL_composite < 6] <- 5
dat$PL_integer[dat$PL_composite >= 6] <- 6
range(dat$PL_listen, na.rm = TRUE)
range(dat$PL_read, na.rm = TRUE)
range(dat$PL_speak, na.rm = TRUE)
range(dat$PL_write, na.rm = TRUE)
range(dat$PL_comprehension, na.rm = TRUE)
range(dat$PL_oral, na.rm = TRUE)
range(dat$PL_listen, na.rm = TRUE)
range(dat$PL_composite, na.rm = TRUE)
range(dat$PL_integer, na.rm = TRUE)

# proficient
dat$proficient[dat$PL_composite >= 5.0] <- 1
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
sum(is.na(dat$SS_listen)) #2018: 150
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
sum(is.na(dat$SS_read)) #2018: 199
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
sum(is.na(dat$SS_speak)) #2018: 391
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
sum(is.na(dat$SS_write)) #2018: 668
dat[dat$valid_write == 1 & is.na(dat$SS_write), ]
dat[dat$valid_write == 0 & !is.na(dat$SS_write), ] #none

################################################################################
## process invalid records and save file

# tally missing domains
dat$missing_listen[is.na(dat$SS_listen)] <- 1
dat$missing_listen[!is.na(dat$SS_listen)] <- 0
table(dat$missing_listen)
# 2018: 150
# 2019: 151

dat$missing_read[is.na(dat$SS_read)] <- 1
dat$missing_read[!is.na(dat$SS_read)] <- 0
table(dat$missing_read)
# 2018: 199
# 2019: 206

dat$missing_speak[is.na(dat$SS_speak)] <- 1
dat$missing_speak[!is.na(dat$SS_speak)] <- 0
table(dat$missing_speak)
# 2018: 691
# 2019: 703

dat$missing_write[is.na(dat$SS_write)] <- 1
dat$missing_write[!is.na(dat$SS_write)] <- 0
table(dat$missing_write)
# 2018: 668
# 2019: 675

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
# 2018: 15 cases had SPD
# 2019: 7 cases had SPD

table(dat$missing_read, dat$valid_read)
dat$missing_domains[dat$missing_read == 1 & dat$valid_read == 2]
# 2018: 13 cases had SPD
# 2019: 0 cases had SPD

table(dat$missing_speak, dat$valid_speak)
dat$missing_domains[dat$missing_speak == 1 & dat$valid_speak == 2]
# 2018: 10 cases had SPD
# 2019: 12 cases had SPD

table(dat$missing_write, dat$valid_write)
dat$missing_domains[dat$missing_write == 1 & dat$valid_write == 2]
# 2018: 9 cases had SPD
# 2019: 4 cases had SPD

# remove extra columns
dat <- dat[c(163:218)]
names(dat)

# save student-level file
current_date <- Sys.Date()

file_name <- paste0("ACCESS for ELLs 2017-2018 Cleaned ", current_date, ".csv")

write.csv(dat, file = file_name, row.names = FALSE)

nrow(dat)
# 2018: 48998
# 2019: 51178

################################################################################
# remove student who are missing composite scores
dat <- dat[!is.na(dat$PL_composite), ]
dat <- dat[!is.na(dat$test_schnumb), ]
nrow(dat)
# 2018: 48054
# 2019: 50208

# save student-level file with complete cases
current_date <- Sys.Date()

file_name <- paste0("ACCESS for ELLs 2017-2018 Complete Cases ", 
                    current_date, ".csv")

write.csv(dat, file = file_name, row.names = FALSE)

