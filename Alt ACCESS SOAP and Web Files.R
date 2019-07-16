
### Alt ACCESS for ELLs SOAP and Web Files
# SY 2018-2019

################################################################################
### open and process raw data file
################################################################################

rm(list = ls())
library(stringi)
library(lubridate)
library(Hmisc)
library(tidyverse)

# open files
raw <- read.csv("ADDED DEMO NM_Alternate_StudRR_File_2019-07_16.csv",
                header = TRUE, stringsAsFactors = FALSE)
dat <- raw
nrow(dat)
# 2019: 542

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
dat$proficient[dat$PL_composite == "P1" | 
                   dat$PL_composite == "P2" | 
                   dat$PL_composite == "P3"] <- 1
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
write.csv(dat, "Alt ACCESS for ELLs 2018-2019_Cleaned_07162019.csv",
          row.names = FALSE, quote = FALSE, na = "")
nrow(dat) 
# 2019: 542

################################################################################
# remove student who are missing composite scores
dat <- dat[dat$PL_composite != "", ]
dat <- dat[!is.na(dat$test_schnumb), ]
nrow(dat)
# 2019: 497

write.csv(dat, "Alt ACCESS for ELLs 2018-2019 Complete Cases_07162019.csv",
          row.names = FALSE, quote = FALSE, na = "")

################################################################################
### calculate rates for SOAP and web files
################################################################################
dat$allstudents <- "All Students"
dat$statecode <- 999

groups <- c("allstudents", "gender", "eth", "swd", "frl", 
            "ell", "migrant", "homeless", "military", "foster")

table(dat$PL_composite)
dat$a1[dat$PL_composite == "A1"] <- 1
dat$a1[is.na(dat$a1)] <- 0
sum(dat$a1)
dat$a2[dat$PL_composite == "A2"] <- 1
dat$a2[is.na(dat$a2)] <- 0
sum(dat$a2)
dat$a3[dat$PL_composite == "A3"] <- 1
dat$a3[is.na(dat$a3)] <- 0
sum(dat$a3)
dat$p1[dat$PL_composite == "P1"] <- 1
dat$p1[is.na(dat$p1)] <- 0
sum(dat$p1)
dat$p2[dat$PL_composite == "P2"] <- 1
dat$p2[is.na(dat$p2)] <- 0
sum(dat$p2)
dat$p3[dat$PL_composite == "P3"] <- 1
dat$p3[is.na(dat$p3)] <- 0
sum(dat$p3)


rate <- function(dataset, code) {
    Rates <- data.frame()
    
    for (group in groups) {
        GroupRate <- dataset %>%
            select(code, group, a1, a2, a3, p1, p2, p3, proficient) %>%
            group_by(dataset[[code]], dataset[[group]]) %>%
            summarise(NStudents = n(),
                      A1 = (sum(a1) / NStudents) * 100,
                      A2 = (sum(a2) / NStudents) * 100,
                      A3 = (sum(a3) / NStudents) * 100,
                      P1 = (sum(p1) / NStudents) * 100,
                      P2 = (sum(p2) / NStudents) * 100,
                      P3 = (sum(p3) / NStudents) * 100,
                      A123 = ((sum(a1) + sum(a2) + sum(a3)) / NStudents) * 100,
                      P123 = ((sum(p1) + sum(p2) + sum(p3)) / NStudents) * 100,
                      ProficiencyRate = (sum(proficient) / NStudents * 100))
        names(GroupRate) <- c("Code", "Group", "NStudents", 
                              "A1", "A2", "A3", "P1", "P2", "P3",
                              "A123", "P123", "ProficiencyRate")
        
        GroupRate <- GroupRate[GroupRate$Code != 999999, ]
        Rates <- rbind(GroupRate, Rates)
    }
    Rates
}

# state rates
stateRates <- rate(dat, "statecode")
stateRates$schnumb <- 999999
stateRates$DistrictCode <- 999
stateRates$SchoolCode <- 999
stateRates$SORT <- 1

# district rates
districtRates <- rate(dat, "distcode")
districtRates$schnumb <- districtRates$Code * 1000
districtRates$DistrictCode <- districtRates$Code
districtRates$SchoolCode <- 0
districtRates$SORT <- 2

# school rates
schoolRates <- rate(dat, "test_schnumb")
schoolRates$schnumb <- schoolRates$Code
schoolRates$DistrictCode <- floor(schoolRates$Code / 1000)
schoolRates$SchoolCode <- schoolRates$Code - (schoolRates$DistrictCode * 1000)
schoolRates$SORT <- 3


################################################################################
### merging, formatting, masking
################################################################################
all <- rbind(stateRates, districtRates, schoolRates)

# sort codes for subgroups
table(all$Group)
all$SORTCODE[all$Group == "All Students"] <- 1
all$SORTCODE[all$Group == "Female"] <- 2
all$SORTCODE[all$Group == "Male"] <- 3
all$SORTCODE[all$Group == "Caucasian"] <- 4
all$SORTCODE[all$Group == "African American"] <- 5
all$SORTCODE[all$Group == "Hispanic"] <- 6
all$SORTCODE[all$Group == "Asian"] <- 7
all$SORTCODE[all$Group == "Native American"] <- 8
all$SORTCODE[all$Group == "Economically Disadvantaged"] <- 9
all$SORTCODE[all$Group == "Students with Disabilities"] <- 10
all$SORTCODE[all$Group == "English Learners"] <- 11
all$SORTCODE[all$Group == "Migrant"] <- 12
all$SORTCODE[all$Group == "Homeless"] <- 13
all$SORTCODE[all$Group == "Military"] <- 14
all$SORTCODE[all$Group == "Foster Care"] <- 15
table(all$SORTCODE)
# ELs will be removed from the files, since all students should be ELs
# SWD will be removed from the files, since all students should be SWDs.

# add district and school names
all$DistrictName <- schools$distname[match(all$DistrictCode, schools$distcode)]
all$SchoolName <- schools$schname[match(all$schnumb, schools$schnumb)]
all$DistrictName[all$SORT == 1] <- "Statewide"
all$SchoolName[all$SORT == 1] <- "All Students"
all$SchoolName[all$SORT == 2] <- "Districtwide"

# check for missing district and school names
all <- all[!is.na(all$schnumb), ]
all[is.na(all$DistrictName), ] #none
all$schnumb[is.na(all$SchoolName)] #none

################################################################################
# SOAP file
SOAP <- all[c("schnumb", "DistrictCode", "DistrictName", 
              "SchoolCode", "SchoolName", "Group", "NStudents",
              "A1", "A2","A3", "P1", "P2", "P3",
              "ProficiencyRate", "SORTCODE", "SORT")]

# remove entries that do not have sortcodes
SOAP <- SOAP[!is.na(SOAP$SORTCODE), ]
# remove entries for ELs since all students should be ELs
SOAP <- SOAP[SOAP$SORTCODE != 11, ]
# remove entries for SWDs since all students should be SWDs
SOAP <- SOAP[SOAP$SORTCODE != 10, ]
nrow(SOAP)
# 2019: 1057

# remove district-level rates for state charter schools
# except for 542 Mission Achievement and Success, since there are two schools
SOAP <- SOAP[!(SOAP$DistrictCode > 500 & SOAP$SchoolName == "Districtwide"), ]
nrow(SOAP)
# 2019: 1035

# this function rounds 0.5 up
round2 <- function(x, digits) {
    posneg <- sign(x)
    z <- abs(x) * (10 ^ digits)
    z <- z + 0.5
    z <- as.numeric(as.character(z))
    z <- trunc(z)
    z <- z / (10 ^ digits)
    z * posneg
}

head(SOAP)
SOAP$A1 <- round2(SOAP$A1, digits = 1)
SOAP$A2 <- round2(SOAP$A2, digits = 1)
SOAP$A3 <- round2(SOAP$A3, digits = 1)
SOAP$P1 <- round2(SOAP$P1, digits = 1)
SOAP$P2 <- round2(SOAP$P2, digits = 1)
SOAP$P3 <- round2(SOAP$P3, digits = 1)
SOAP$ProficiencyRate <- round2(SOAP$ProficiencyRate, digits = 1)
head(SOAP)

# sorting
SOAP <- SOAP[order(SOAP$SORT, SOAP$schnumb, SOAP$SORTCODE), ]
SOAP$SORT <- NULL
SOAP$SORTCODE <- NULL

write.csv(SOAP, "Alt ACCESS for ELLs UNMASKED SOAP 2018-2019 07162019.csv",
          row.names = FALSE, quote = FALSE, na = "")


################################################################################
# web file
web <- all[c("schnumb", "DistrictCode", "DistrictName", 
             "SchoolCode", "SchoolName", "Group", "NStudents",
             "A123", "P123", "SORTCODE", "SORT")]

# remove state charters' district-level rates
# remove non-state charter schools' school-level rates
# remove subgroup rates
web <- web %>%
    filter(SORTCODE == 1) %>%
    filter(!(DistrictName == "State Charter" & SchoolName == "Districtwide")) %>%
    filter((SchoolName %in% c("Districtwide", "All Students")) | 
               DistrictName == "State Charter") %>%
    filter(SchoolCode != 998) #homebound
web$SORTCODE <- NULL
nrow(web)
# 2019: 37

# round to integers
head(web)
web$A123 <- round2(web$A123, digits = 0)
web$P123 <- round2(web$P123, digits = 0)
head(web)

# check totals
web$total <- rowSums(web[, c("A123", "P123")])
range(web$total) #100
web$total <- NULL


###############################################
## masking

# remove records with fewer than 10 students
nrow(web) 
web <- web[web$NStudents >= 10, ]
nrow(web)
# 2019: 12

mask <- function(dataset, level) {
    masked <- data.frame()
    
    for (row in 1:nrow(dataset)) {
        row <- dataset[row, ]
        
        # N = 301 or higher
        if (row$NStudents > 300) {
            row$pct[row[[level]] >= 99] <- "GE 99"
            row$pct[row[[level]] <= 1] <- "LE 1"
            row$pct[row[[level]] < 99 & row[[level]] > 1] <- row[[level]]
        }
        
        # N = 201-300
        else if (row$NStudents > 200 & row$NStudents <= 300) {
            row$pct[row[[level]] >= 98] <- "GE 98"
            row$pct[row[[level]] <= 2] <- "LE 2"
            row$pct[row[[level]] < 98 & row[[level]] > 2] <- row[[level]]
        }
        
        # N = 101-200
        else if (row$NStudents > 100 & row$NStudents <= 200) {
            row$pct[row[[level]] < 3] <- "LE 2"
            row$pct[row[[level]] >= 3 & row[[level]] < 5] <- "3-4"
            row$pct[row[[level]] >= 5 & row[[level]] < 10] <- "5-9"
            row$pct[row[[level]] >= 10 & row[[level]] < 15] <- "10-14"
            row$pct[row[[level]] >= 15 & row[[level]] < 20] <- "15-19"
            row$pct[row[[level]] >= 20 & row[[level]] < 25] <- "20-24"
            row$pct[row[[level]] >= 25 & row[[level]] < 30] <- "25-29"
            row$pct[row[[level]] >= 30 & row[[level]] < 35] <- "30-34"
            row$pct[row[[level]] >= 35 & row[[level]] < 40] <- "35-39"
            row$pct[row[[level]] >= 40 & row[[level]] < 45] <- "40-44"
            row$pct[row[[level]] >= 45 & row[[level]] < 50] <- "45-49"
            row$pct[row[[level]] >= 50 & row[[level]] < 55] <- "50-54"
            row$pct[row[[level]] >= 55 & row[[level]] < 60] <- "55-59"
            row$pct[row[[level]] >= 60 & row[[level]] < 65] <- "60-64"
            row$pct[row[[level]] >= 65 & row[[level]] < 70] <- "65-69"
            row$pct[row[[level]] >= 70 & row[[level]] < 75] <- "70-74"
            row$pct[row[[level]] >= 75 & row[[level]] < 80] <- "75-79"
            row$pct[row[[level]] >= 80 & row[[level]] < 85] <- "80-84"
            row$pct[row[[level]] >= 85 & row[[level]] < 90] <- "85-89"
            row$pct[row[[level]] >= 90 & row[[level]] < 95] <- "90-94"
            row$pct[row[[level]] >= 95 & row[[level]] < 98] <- "95-97"
            row$pct[row[[level]] >= 98] <- "GE 98"
        }
        
        # N = 41-100
        else if (row$NStudents > 40 & row$NStudents <= 100) {
            row$pct[row[[level]] < 6] <- "LE 5"
            row$pct[row[[level]] >= 6 & row[[level]] < 10] <- "6-9"
            row$pct[row[[level]] >= 10 & row[[level]] < 15] <- "10-14"
            row$pct[row[[level]] >= 15 & row[[level]] < 20] <- "15-19"
            row$pct[row[[level]] >= 20 & row[[level]] < 25] <- "20-24"
            row$pct[row[[level]] >= 25 & row[[level]] < 30] <- "25-29"
            row$pct[row[[level]] >= 30 & row[[level]] < 35] <- "30-34"
            row$pct[row[[level]] >= 35 & row[[level]] < 40] <- "35-39"
            row$pct[row[[level]] >= 40 & row[[level]] < 45] <- "40-44"
            row$pct[row[[level]] >= 45 & row[[level]] < 50] <- "45-49"
            row$pct[row[[level]] >= 50 & row[[level]] < 55] <- "50-54"
            row$pct[row[[level]] >= 55 & row[[level]] < 60] <- "55-59"
            row$pct[row[[level]] >= 60 & row[[level]] < 65] <- "60-64"
            row$pct[row[[level]] >= 65 & row[[level]] < 70] <- "65-69"
            row$pct[row[[level]] >= 70 & row[[level]] < 75] <- "70-74"
            row$pct[row[[level]] >= 75 & row[[level]] < 80] <- "75-79"
            row$pct[row[[level]] >= 80 & row[[level]] < 85] <- "80-84"
            row$pct[row[[level]] >= 85 & row[[level]] < 90] <- "85-89"
            row$pct[row[[level]] >= 90 & row[[level]] < 95] <- "90-94"
            row$pct[row[[level]] >= 95] <- "GE 95"
        }
        
        # N = 21-40
        else if (row$NStudents > 20 & row$NStudents <= 40) {
            row$pct[row[[level]] < 11] <- "LE 10"
            row$pct[row[[level]] >= 11 & row[[level]] < 20] <- "11-19"
            row$pct[row[[level]] >= 20 & row[[level]] < 30] <- "20-29"
            row$pct[row[[level]] >= 30 & row[[level]] < 40] <- "30-39"
            row$pct[row[[level]] >= 40 & row[[level]] < 50] <- "40-49"
            row$pct[row[[level]] >= 50 & row[[level]] < 60] <- "50-59"
            row$pct[row[[level]] >= 60 & row[[level]] < 70] <- "60-69"
            row$pct[row[[level]] >= 70 & row[[level]] < 80] <- "70-79"
            row$pct[row[[level]] >= 80 & row[[level]] < 90] <- "80-89"
            row$pct[row[[level]] >= 90] <- "GE 90"
        }
        
        # N = 10-20
        else {
            row$pct[row[[level]] < 21] <- "LE 20"
            row$pct[row[[level]] >= 21 & row[[level]] < 30] <- "21-29"
            row$pct[row[[level]] >= 30 & row[[level]] < 40] <- "30-39"
            row$pct[row[[level]] >= 40 & row[[level]] < 50] <- "40-49"
            row$pct[row[[level]] >= 50 & row[[level]] < 60] <- "50-59"
            row$pct[row[[level]] >= 60 & row[[level]] < 70] <- "60-69"
            row$pct[row[[level]] >= 70 & row[[level]] < 80] <- "70-79"
            row$pct[row[[level]] >= 80] <- "GE 80"
        }
        masked <- rbind(row, masked)    
    }
    masked <- masked[order(masked$SORT, masked$schnumb), ]
}

A123 <- mask(web, "A123")
colnames(A123)[11] <- "A_123"

P123 <- mask(web, "P123")
colnames(P123)[11] <- "P_123"

# merge files
webfile <- cbind(A123, P123[c(11)])
head(webfile)

final <- webfile[c("schnumb", "DistrictName", "SchoolName", "A123", "P123")]
head(final)

# save output
write.csv(final, "ACCESS for ELLs MASKED Web 2018-2019 07162019.csv",
          row.names = FALSE)


################################################################################
### the end
################################################################################
