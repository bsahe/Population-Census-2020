#### Load Libraries and data ####

library(RSQLite) #Use SQLite database to store and read data
library(dplyr) # Data manipulation
library(tidyverse) #Data science package
library(tidyr) #Data science package
library(readstata13) #Read Stata files into R

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository) # Required for file.choose() function

# Establish connection to the SQLite databases
mydb <- dbConnect(RSQLite::SQLite(), "data/secure/sqlite/census2020.SQLite")
t6 <- dbConnect(RSQLite::SQLite(), "data/open/team6/team6.SQLite")

 #### Read data from SQLite database and run summary ####
tab7_1 <- dbGetQuery(mydb, "SELECT area_council,province, urban_rural, work_income, absent_job, work_last_2wks__1,work_last_2wks__2,   
                                    round(sum(ac_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province, urban_rural, work_income, absent_job, work_last_2wks__1, work_last_2wks__2 ")

tab7_2 <- dbGetQuery(mydb, "SELECT area_council,province, urban_rural,sex, work_income, absent_job, work_last_2wks__1,work_last_2wks__2,   
                                    round(sum(ac_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province, urban_rural,sex, work_income, absent_job, work_last_2wks__1, work_last_2wks__2 ")

tab7_3 <- dbGetQuery(mydb, "SELECT area_council,province, urban_rural,sex, work_income, absent_job, work_last_2wks__1,work_last_2wks__2,   
                                    round(sum(ac_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province, urban_rural,sex, work_income, absent_job, work_last_2wks__1, work_last_2wks__2 ")

tab7_3 <- dbGetQuery(mydb, "SELECT area_council,province, urban_rural,sex, work_income,
                                    round(sum(ac_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province, urban_rural,sex, work_income")
