
# ACCESS for ELLs

# Merge records for students who have two separate entries
# (1) listening, reading, speaking
# (2) writing

rm(list = ls())
library(stringi)
library(Hmisc)
library(tidyverse)
options(tibble.print_max = Inf)

lookup <- read_csv("ACCESS Scale Score to Proficiency Level Look-up Table.csv")

# rounding
round2 <- function(x, digits) {
    posneg <- sign(x)
    z <- abs(x) * (10 ^ digits)
    z <- z + 0.5
    z <- as.numeric(as.character(z))
    z <- trunc(z)
    z <- z / (10 ^ digits)
    z * posneg
}

# merge scores
merge <- function(student_ID, DRC_3domains, DRC_writing) {
    
    # cluster
    dups$Cluster...Writing[dups$Unique.DRC.Student.ID == DRC_3domains] <- 
        dups$Cluster...Writing[dups$Unique.DRC.Student.ID == DRC_writing]
    
    # scale score
    dups$Writing.Scale.Score[dups$Unique.DRC.Student.ID == DRC_3domains] <- 
        dups$Writing.Scale.Score[dups$Unique.DRC.Student.ID == DRC_writing]
    
    # proficiency level
    dups$Writing.Proficiency.Level[dups$Unique.DRC.Student.ID == DRC_3domains] <- 
        dups$Writing.Proficiency.Level[dups$Unique.DRC.Student.ID == DRC_writing]
    
    # remove the record with only writing scale score
    dups <- dups[dups$Unique.DRC.Student.ID != DRC_writing, ]
    
    dups$manual_fix[dups$State.Student.ID == student_ID] <- 2
    
    # calculate literacy and composite scale scores
    listen_ss <- dups$Listening.Scale.Score[dups$State.Student.ID == student_ID]
    read_ss <- dups$Reading.Scale.Score[dups$State.Student.ID == student_ID]
    speak_ss <- dups$Speaking.Scale.Score[dups$State.Student.ID == student_ID]
    write_ss <- dups$Writing.Scale.Score[dups$State.Student.ID == student_ID]
    
    dups$Literacy.Scale.Score[dups$State.Student.ID == student_ID] <- 
        round2((0.5 * read_ss) + (0.5 * write_ss), digits = 0)
    
    dups$Composite..Overall..Scale.Score[dups$State.Student.ID == student_ID] <- 
        round2((0.35 * read_ss) + 
                   (0.35 * write_ss) + 
                   (0.15 * listen_ss) + 
                   (0.15 * speak_ss), digits = 0)
    
    # look up literacy proficiency level
    level <- dups$Cluster...Reading[dups$State.Student.ID == student_ID]
    grade <- dups$Grade[dups$State.Student.ID == student_ID]
    literacy_ss <- dups$Literacy.Scale.Score[dups$State.Student.ID == student_ID]
    
    literacy_pl <- lookup$PL[lookup$Level == level &
                                 lookup$Grade == grade &
                                 lookup$Subject == "Literacy" &
                                 lookup$SS == literacy_ss]
    
    dups$Literacy.Proficiency.Level[dups$State.Student.ID == student_ID] <- literacy_pl
    
    # look up composite proficiency level
    level <- dups$Cluster...Listening[dups$State.Student.ID == student_ID]
    grade <- dups$Grade[dups$State.Student.ID == student_ID]
    composite_ss <- dups$Composite..Overall..Scale.Score[dups$State.Student.ID == student_ID]
    
    composite_pl <- lookup$PL[lookup$Level == level &
                                  lookup$Grade == grade &
                                  lookup$Subject == "Overall" &
                                  lookup$SS == composite_ss]
    
    dups$Composite..Overall..Proficiency.Level[dups$State.Student.ID == student_ID] <- composite_pl
    
    # return the data frame
    dups
}

