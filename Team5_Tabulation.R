#### Load Libraries and data ####

library(RSQLite) #Use SQLite database to store and read data
library(dplyr) # Data manipulation
library(tidyverse) #Data science package
library(tidyr) #Data science package
#library(readstata13) #Read Stata files into R

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository) # Required for file.choose() function


# Establish connection to the SQLite databases
mydb <- dbConnect(RSQLite::SQLite(), "data/secure/sqlite/census2020.SQLite")
t5 <- dbConnect(RSQLite::SQLite(), "data/open/team5/team5.SQLite")

#### Read data from SQLite database and run summary ####


Tab6.1 <- dbGetQuery(mydb, "SELECT province, sex, area_council, urban_rural, round(sum(province_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1 and attending_school = 'Yes'
                             GROUP BY province, sex, area_council, urban_rural")

Tab6.1Pivot <- Tab6.1 %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population, values_fill = 0) %>%
  ungroup()

Tab6.2 <- dbGetQuery(mydb, "SELECT age, sex, round(sum(province_factor)) as population
                     FROM person
                     WHERE can_enumerate = 1 and attending_school = 'Yes'
                     GROUP BY age, sex")

Tab6.2Pivot <- Tab6.2 %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population, values_fill = 0) %>%
  ungroup()

Tab6.3 <- dbGetQuery(mydb, "SELECT sex, level_attending_School, age, area_council, province, round(sum(province_factor)) as population
                     FROM person
                     WHERE can_enumerate = 1 and attending_School = 'Yes'
                     GROUP BY sex, level_attending_School, age, area_council, province")

Tab6.3Pivot <- Tab6.3 %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = level_attending_school, values_from = population, values_fill = 0) %>%
  ungroup()

Tab6.4 <- dbGetQuery(mydb, "SELECT sex, level_attending_School, age, area_council, province, round(sum(province_factor)) as population
                     FROM person
                     WHERE can_enumerate = 1 and attending_School = 'Yes'
                     GROUP BY sex, level_attending_School, age, area_council, province")

#write table to SQLite database
dbWriteTable(t5, "provSexPivot",provSexPivot, overwrite = TRUE)
                     

 

acPop <- dbGetQuery(mydb, "SELECT area_council,
                                  sum(ac_factor) as population
                           FROM person
                           WHERE can_enumerate = 1
                           GROUP BY area_council")

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

#Uaing pivot_wider to cross-tabulate with 2 variables
livestockpivot <- livestock %>%
  pivot_wider(names_from = livestock_id, values_from = totallivestock, values_fill = 0) %>%
  ungroup()

#### Write computed tables to teams SQLite database #### 
dbWriteTable(t1, "provPop", provPop, overwrite=TRUE)
dbWriteTable(t1, "acPop", acPop, overwrite=TRUE)
dbWriteTable(t1, "livestockpivot", livestockpivot, overwrite=TRUE)
