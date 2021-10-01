#### Load Libraries and data ####

library(RSQLite) #Use SQLite database to store and read data
library(dplyr) # Data manipulation
library(tidyverse) #Data science package
library(readstata13) #Read Stata files into R
library(xlsx)

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository) # Required for file.choose() function

#read stata files

#cover <- read.dta13("data/secure/stata/cover_factors.dta")
#household <- read.dta13("data/secure/stata/Household_factors.dta")
#person <- read.dta13("data/secure/stata/Person_factors.dta")
#livestock <- read.dta13("data/secure/stata/Livestock_roster_new.dta")
#appliances <- read.dta13("data/secure/stata/roster_appliances.dta")
#communication <- read.dta13("data/secure/stata/roster_communication.dta")
#memberDied <- read.dta13("data/secure/stata/roster_member_died.dta")
#transport <- read.dta13("data/secure/stata/roster_transport.dta")

# Establish connection to the Census 2020 SQLite database
mydb <- dbConnect(RSQLite::SQLite(), "data/secure/sqlite/census2020.SQLite")

#Write Stata files into the SQLite database
#dbWriteTable(mydb, "cover", cover, overwrite = TRUE)
#dbWriteTable(mydb, "household", household, overwrite = TRUE)
#dbWriteTable(mydb, "person", person, overwrite = TRUE)
#dbWriteTable(mydb, "livestock", livestock, overwrite = TRUE)
#dbWriteTable(mydb, "appliances", appliances, overwrite = TRUE)
#dbWriteTable(mydb, "communication", communication, overwrite = TRUE)
#dbWriteTable(mydb, "memberDied", memberDied, overwrite = TRUE)
#dbWriteTable(mydb, "transport", transport, overwrite = TRUE)

#### Read data from SQLite database and run summary ####

provPop <- dbGetQuery(mydb, "SELECT province, 
                                    sum(province_factor) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province ")

acPop <- dbGetQuery(mydb, "SELECT area_council,
                                  sum(ac_factor) as population
                           FROM person
                           WHERE can_enumerate = 1
                           GROUP BY area_council")

write.csv(acPop, "test.csv")


pop_hh <- dbGetQuery(mydb, "SELECT province,
                                   round(sum(hhld_ac_factor * hhsize)) as population
                            FROM household
                            WHERE can_enumerate = 1
                            GROUP BY province ")


livestock <- dbGetQuery(mydb, "SELECT household.area_council,
                                      livestock.livestock_id,
                                      round(sum(household.hhld_ac_factor * livestock.num_livestock)) as totallivestock  
                              FROM household
                              INNER JOIN livestock ON household.interview_key = livestock.interview_key
                              WHERE household.can_enumerate = 1
                              GROUP BY household.area_council, livestock.livestock_id
                              ORDER BY household.island
                        ")


livestockpivot <- livestock %>%
  pivot_wider(names_from = livestock_id, values_from = totallivestock, values_fill = 0) %>%
  ungroup()

#View(livestockpivot)

#### Write tables to Excel ####
write.csv(provPop, "data/open/team1/provPop.csv", row.names = FALSE)
write.csv(acPop, "data/open/team1/acPop.csv", row.names = FALSE)
write.csv(livestock, "data/open/team1/livestock.csv", row.names = FALSE)
write.csv(livestockpivot, "data/open/team1/livestockpivot.csv", row.names = FALSE)










