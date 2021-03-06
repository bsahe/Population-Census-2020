#### Load Libraries and data ####

library(RSQLite) #Use SQLite database to store and read data
library(dplyr) # Data manipulation
library(tidyverse) #Data science package
library(tidyr) #Data science package
#library(readstata13) #Read Stata files into R

#Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository) # Required for file.choose() function


# Establish connection to the SQLite databases
mydb <- dbConnect(RSQLite::SQLite(), "data/secure/sqlite/census2020.SQLite")
t4 <- dbConnect(RSQLite::SQLite(), "data/open/team1/team4.SQLite")



#### Read data from SQLite database and run summary ####
provPop <- dbGetQuery(mydb, "SELECT province, 
                                    sum(province_factor) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province ")
provsex <- dbGetQuery(mydb, "SELECT province, sex, round(sum(province_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province, sex ")
provsexpivot <- provsex %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population,values_fill= 0) %>%
  ungroup()

acPop <- dbGetQuery(mydb, "SELECT area_council, sex,
                           round(sum(ac_factor)) as population
                           FROM person
                           WHERE can_enumerate= 1
                           GROUP BY area_council")
provacpivot <- acPop %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population,values_fill= 0) %>%
  ungroup()
dbWriteTable(t4, "provacpivot", provacpivot, overwrite = TRUE)

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
page<- dbGetQuery(mydb,"SELECT * FROM person")
page$age_groupe_5_yr <-""
person <- page %>% 
  mutate(
    # Create categories
    age_group_5_yr = dplyr::case_when(
      age >= 0 & age < 5  ~ "0-4",
      age >= 5 & age < 10 ~ "5-9",
      age >= 10 & age < 15 ~ "10-14",
      age >= 15 & age < 20 ~ "15-19",
      age >= 20 & age < 25 ~ "20-24",
      age >= 25 & age < 30 ~ "25-29",
      age >= 30 & age < 35 ~ "30-34",
      age >= 35 & age < 40 ~ "35-39",
      age >= 40 & age < 45 ~ "40-44",
      age >= 45 & age < 50 ~ "45-49",
      age >= 50 & age < 55 ~ "50-54",
      age >= 55 & age < 60 ~ "55-59",
      age >= 60 & age < 65 ~ "60-64",
      age >= 65 & age < 70 ~ "65-69",
      
      
      age >= 70           ~ "70 +"
    ),
  )
#seeing

healthSeeing <- dbGetQuery(mydb,"SELECT seeing, sex,area_council,urban_rural,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age ")

causeOfdiffseeing <- dbGetQuery(mydb,"SELECT cause_of_diff_seeing,sex,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by sex,age_5yr_grp_80 ")

seeingDisability_acquired <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN seeing='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN seeing='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN seeing='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN seeing='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Acquired'
                                      GROUP BY province, area_council, urban_rural
                                ")

seeingDisability_congenital <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN seeing='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN seeing='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN seeing='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN seeing='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Congenital'
                                      GROUP BY province, area_council, urban_rural
                                ")


seeingDisability_oldAge <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN seeing='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN seeing='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN seeing='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN seeing='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Related to old age'
                                      GROUP BY province, area_council, urban_rural
                                ")


#hearing

healthhearing <- dbGetQuery(mydb,"SELECT hearing, sex,area_council,urban_rural,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age_5yr_grp_80 ")


causeOfdiffHearing <- dbGetQuery(mydb,"SELECT cause_of_diff_hearing,sex,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by sex, age")

hearingDisability_acquired <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN hearing='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN hearing='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN hearing='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN hearing='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Acquired'
                                      GROUP BY province, area_council, urban_rural
                                ")

hearingDisability_congenital <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN hearing='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN hearing='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN hearing='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN hearing='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Congenital'
                                      GROUP BY province, area_council, urban_rural
                                ")


hearingDisability_oldAge <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN hearing='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN hearing='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN hearing='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN hearing='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Related to old age'
                                      GROUP BY province, area_council, urban_rural
                                ")

#walking

healthwalking <- dbGetQuery(mydb,"SELECT walking, sex,area_council,urban_rural,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age")

causeOfdiffwalking <- dbGetQuery(mydb,"SELECT cause_of_diff_walking,sex,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by sex,age_5yr_grp_80 ")

#filter
walkingDisability_acquired <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN walking='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN walking='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN walking='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN walking='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Acquired'
                                      GROUP BY province, area_council, urban_rural
                                ")

walkingDisability_congenital <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN walking='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN walking='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN walking='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN walking='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Congenital'
                                      GROUP BY province, area_council, urban_rural
                                ")


walkingDisability_oldAge <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN walking='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN walking='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN walking='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN walking='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_walking ='Related to old age'
                                      GROUP BY province, area_council, urban_rural
                                ")

#remembering

healthremembering <- dbGetQuery(mydb,"SELECT remembering, sex,area_council,urban_rural,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age_5yr_grp_80 ")

causeOfdifflearning <- dbGetQuery(mydb,"SELECT cause_of_diff_learning,sex,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by sex,age_5yr_grp_80 ")

rememberingDisability_acquired <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN remembering='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN remembering='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN remembering='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN remembering='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_learning ='Acquired'
                                      GROUP BY province, area_council, urban_rural
                                ")
rememberingDisability_congenital <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN remembering='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN remembering='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN remembering='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN remembering='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_learning ='Congenital'
                                      GROUP BY province, area_council, urban_rural
                                ")


rememberingDisability_oldAge <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN remembering='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN remembering='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN remembering='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN remembering='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_learning ='Related to old age'
                                      GROUP BY province, area_council, urban_rural
                                ")


#selfcare

healthselfcare <- dbGetQuery(mydb,"SELECT selfcare, sex,area_council,urban_rural,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age_5yr_grp_80 ")

causeOfdiffselfcare <- dbGetQuery(mydb,"SELECT cause_of_diff_selfcare,sex,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by sex,age_5yr_grp_80 ")

selfcareDisability_acquired <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN seeing='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN seeing='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN seeing='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN seeing='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_selfcare ='Acquired'
                                      GROUP BY province, area_council, urban_rural
                                ")

selfcareDisability_congenital <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN selfcare='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN selfcare='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN selfcare='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN selfcare='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_selfcare ='Congenital'
                                      GROUP BY province, area_council, urban_rural
                                ")


selfcareDisability_oldAge <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN selfcare='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN selfcare='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN selfcare='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN selfcare='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_selfcare ='Related to old age'
                                      GROUP BY province, area_council, urban_rural
                                ")
#communication

healthcommunication <- dbGetQuery(mydb,"SELECT communication, sex,area_council,urban_rural,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age_5yr_grp_80 ")


causeOfdiffcommunication <- dbGetQuery(mydb,"SELECT cause_of_diff_communicating,sex,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by sex,age_5yr_grp_80 ")

communicationDisability_acquired <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN communication='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN communication='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN communication='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN communication='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_communicating ='Acquired'
                                      GROUP BY province, area_council, urban_rural
                                ")
communicationDisability_congenital <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN communication='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN communication='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN communication='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN communication='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_communicating ='Congenital'
                                      GROUP BY province, area_council, urban_rural
                                ")


communicationDisability_oldAge <- dbGetQuery(mydb, "SELECT province, area_council, urban_rural,
                                            ROUND(SUM(CASE WHEN selfcare='Yes, lots_of_difficulty' THEN ac_factor ELSE 0 END)) as yesLotsofDifficulty,                
                                            ROUND(SUM(CASE WHEN selfcare='Yes, some difficulty' THEN ac_factor ELSE 0 END)) as yesSomeDifficulty,
                                            ROUND(SUM(CASE WHEN selfcare='No, no difficulty' THEN ac_factor ELSE 0 END)) as noNoDifficulty,
                                            ROUND(SUM(CASE WHEN selfcare='Cannot do at all' THEN ac_factor ELSE 0 END)) as cannotDoAtAll
                                      FROM person
                                      WHERE can_enumerate = 1 and cause_of_diff_communicating ='Related to old age'
                                      GROUP BY province, area_council, urban_rural
                                ")
#consumption1

consumption1 <- dbGetQuery(mydb,"SELECT consumption__1, sex,area_council,urban_rural,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age ")
#consumption2
consumption2 <- dbGetQuery(mydb,"SELECT consumption__2, sex,area_council,urban_rural,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age ")
#consumption3
consumption3 <- dbGetQuery(mydb,"SELECT consumption__3, sex,area_council,urban_rural,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age ")
#consumption4
consumption4 <- dbGetQuery(mydb,"SELECT consumption__4, sex,area_council,urban_rural,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age ")
#treatedBetnets
treatedBetnets <- dbGetQuery(mydb,"SELECT treated_bednets, sex,area_council,urban_rural,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural ")


#Uaing pivot_wider to cross-tabulate with 2 variables
livestockpivot <- livestock %>%
  pivot_wider(names_from = livestock_id, values_from = totallivestock, values_fill = 0) %>%
  ungroup()

#### Write computed tables to teams SQLite database #### 
dbWriteTable(t4, "provPop", provPop, overwrite=TRUE)
dbWriteTable(t1, "acPop", acPop, overwrite=TRUE)
dbWriteTable(t1, "livestockpivot", livestockpivot, overwrite=TRUE)
dbWriteTable(t4, "healthSeeing", healthSeeing, overwrite=TRUE)
dbWriteTable(t4, "causeOfdiffseeing", causeOfdiffseeing, overwrite=TRUE)
dbWriteTable(t4, "healthhearing", healthhearing, overwrite=TRUE)
dbWriteTable(t4, "causeOfdiffHearing", causeOfdiffHearing, overwrite=TRUE)
dbWriteTable(t4, "healthwalking", healthwalking, overwrite=TRUE)
dbWriteTable(t4, "causeOfdiffwalking", causeOfdiffwalking, overwrite=TRUE)
dbWriteTable(t4, "healthremembering", healthremembering, overwrite=TRUE)
dbWriteTable(t4, "causeOfdifflearning", causeOfdifflearning, overwrite=TRUE)
dbWriteTable(t4, "healthselfcare",healthselfcare, overwrite=TRUE)
dbWriteTable(t4, "causeOfdiffselfcare", causeOfdiffselfcare, overwrite=TRUE)
dbWriteTable(t4, "healthcommunication", healthcommunication, overwrite=TRUE)
dbWriteTable(t4, "causeOfdiffcommunication", causeOfdiffcommunication, overwrite=TRUE)
dbWriteTable(t4, "consumption1", consumption1, overwrite=TRUE)
dbWriteTable(t4, "consumption2", consumption2, overwrite=TRUE)
dbWriteTable(t4, "consumption3", consumption3, overwrite=TRUE)
dbWriteTable(t4, "consumption4", consumption4, overwrite=TRUE)
dbWriteTable(t4, "treatedBetnets", treatedBetnets, overwrite=TRUE)
