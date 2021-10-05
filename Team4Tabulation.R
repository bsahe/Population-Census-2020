#### Load Libraries and data ####
###HEALTH TEAM4

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
t4 <- dbConnect(RSQLite::SQLite(), "data/open/team4/team4.SQLite")



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


#Table 5.1 seeing


healthSeeing <- dbGetQuery(mydb,"SELECT seeing, sex,area_council,urban_rural,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age ")

#Table 5.2 difficulty in seeing

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


#Table 5.3 hearing

healthhearing <- dbGetQuery(mydb,"SELECT hearing, sex,area_council,urban_rural,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age_5yr_grp_80 ")

#Table 5.4 difficulty in hearing

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

#Table 5.5 walking

healthwalking <- dbGetQuery(mydb,"SELECT walking, sex,area_council,urban_rural,age,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age")

#Table 5.6 difficulty in walking

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

#Table 5.7 remembering

healthremembering <- dbGetQuery(mydb,"SELECT remembering, sex,area_council,urban_rural,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age_5yr_grp_80 ")

#Table 5.8 difficulty in remembering 

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


#Table 5.9 selfcare

healthselfcare <- dbGetQuery(mydb,"SELECT selfcare, sex,area_council,urban_rural,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age_5yr_grp_80 ")

#Table 5.10 difficulty in selfcare

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
#Table 5.11 communication

healthcommunication <- dbGetQuery(mydb,"SELECT communication, sex,area_council,urban_rural,age_5yr_grp_80,
                           round(sum(province_factor)) as population
                           FROM person
                           where can_enumerate= 1
                           group by area_council,sex, urban_rural,age_5yr_grp_80 ")

#Table 5.12 difficulty in communication


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

sleptUnderBetnets <- dbGetQuery(mydb,"SELECT slept_under_bednets,sex, area_council,urban_rural,
                                round(sum(province_factor)) as population
                                FROM person
                                where can_enumerate= 1
                                group by area_council,sex,urban_rural")


#Uaing pivot_wider to cross-tabulate with 2 variables
livestockpivot <- livestock %>%
  pivot_wider(names_from = livestock_id, values_from = totallivestock, values_fill = 0) %>%
  ungroup()

#### Write computed tables to teams SQLite database #### 
dbWriteTable(t4, "provPop", provPop, overwrite=TRUE)
dbWriteTable(t4, "acPop", acPop, overwrite=TRUE)
dbWriteTable(t4, "livestockpivot", livestockpivot, overwrite=TRUE)

dbWriteTable(t4, "healthSeeing", healthSeeing, overwrite=TRUE)
write.csv(healthSeeing, file = "healthSeeing.csv")
dbWriteTable(t4, "causeOfdiffseeing", causeOfdiffseeing, overwrite=TRUE)
write.csv(causeOfdiffseeing, file = "causeOfdiffseeing.csv")
dbWriteTable(t4, "seeingDisability_acquired", seeingDisability_acquired, overwrite=TRUE)
write.csv(seeingDisability_acquired, file = "seeingDisability_acquired.csv")
dbWriteTable(t4, "seeingDisability_congenital", seeingDisability_congenital, overwrite=TRUE)
write.csv(seeingDisability_congenital, file = "seeingDisability_congenital.csv")
dbWriteTable(t4, "seeingDisability_oldAge", seeingDisability_oldAge, overwrite=TRUE)
write.csv(seeingDisability_oldAge, file = "seeingDisability_oldAge.csv")

dbWriteTable(t4, "healthhearing", healthhearing, overwrite=TRUE)
write.csv(healthhearing, file = "healthhearing.csv")
dbWriteTable(t4, "causeOfdiffHearing", causeOfdiffHearing, overwrite=TRUE)
write.csv(causeOfdiffHearing, file = "causeOfdiffHearing.csv")
dbWriteTable(t4, "hearingDisability_acquired", hearingDisability_acquired, overwrite=TRUE)
write.csv(hearingDisability_acquired, file = "hearingDisability_acquired.csv")
dbWriteTable(t4, "hearingDisability_congenital",hearingDisability_congenital, overwrite=TRUE)
write.csv(hearingDisability_congenital, file = "hearingDisability_congenital.csv")
dbWriteTable(t4, "hearingDisability_oldAge", hearingDisability_oldAge, overwrite=TRUE)
write.csv(hearingDisability_oldAge, file = "hearingDisability_oldAge.csv")

dbWriteTable(t4, "healthwalking", healthwalking, overwrite=TRUE)
write.csv(healthwalking, file = "healthwalking.csv")
dbWriteTable(t4, "causeOfdiffwalking", causeOfdiffwalking, overwrite=TRUE)
write.csv(causeOfdiffwalking, file = "causeOfdiffwalking.csv")
dbWriteTable(t4, "walkingDisability_acquired", walkingDisability_acquired, overwrite=TRUE)
write.csv(walkingDisability_acquired, file = "walkingDisability_acquired.csv")
dbWriteTable(t4, "walkingDisability_congenital",walkingDisability_congenital, overwrite=TRUE)
write.csv(walkingDisability_congenital, file = "walkingDisability_congenital.csv")
dbWriteTable(t4, "walkingDisability_oldAge", walkingDisability_oldAge, overwrite=TRUE)
write.csv(walkingDisability_oldAge, file = "walkingDisability_oldAge.csv")

dbWriteTable(t4, "healthremembering", healthremembering, overwrite=TRUE)
write.csv(healthremembering, file = "healthremembering.csv")
dbWriteTable(t4, "causeOfdifflearning", causeOfdifflearning, overwrite=TRUE)
write.csv(causeOfdifflearning, file = "causeOfdifflearning.csv")
dbWriteTable(t4, "rememberingDisability_acquired", rememberingDisability_acquired, overwrite=TRUE)
write.csv(rememberingDisability_acquired, file = "rememberingDisability_acquired.csv")
dbWriteTable(t4, "rememberingDisability_congenital",rememberingDisability_congenital, overwrite=TRUE)
write.csv(rememberingDisability_congenital, file = "rememberingDisability_congenital.csv")
dbWriteTable(t4, "rememberingDisability_oldAge", rememberingDisability_oldAge, overwrite=TRUE)
write.csv(rememberingDisability_oldAge, file = "rememberingDisability_oldAge.csv")

dbWriteTable(t4, "healthselfcare",healthselfcare, overwrite=TRUE)
write.csv(healthselfcare, file = "healthselfcare.csv")
dbWriteTable(t4, "causeOfdiffselfcare", causeOfdiffselfcare, overwrite=TRUE)
write.csv(causeOfdiffselfcare, file = "causeOfdiffselfcare.csv")
dbWriteTable(t4, "selfcareDisability_acquired", selfcareDisability_acquired, overwrite=TRUE)
write.csv(selfcareDisability_acquired, file = "selfcareDisability_acquired.csv")
dbWriteTable(t4, "selfcareDisability_congenital",selfcareDisability_congenital, overwrite=TRUE)
write.csv(selfcareDisability_congenital, file = "selfcareDisability_congenital.csv")
dbWriteTable(t4, "selfcareDisability_oldAge", selfcareDisability_oldAge, overwrite=TRUE)
write.csv(selfcareDisability_oldAge, file = "selfcareDisability_oldAge.csv")

dbWriteTable(t4, "healthcommunication", healthcommunication, overwrite=TRUE)
write.csv(healthcommunication, file = "healthcommunication.csv")
dbWriteTable(t4, "causeOfdiffcommunication", causeOfdiffcommunication, overwrite=TRUE)
write.csv(causeOfdiffcommunication, file = "causeOfdiffcommunication.csv")
dbWriteTable(t4, "communicationDisability_acquired", communicationDisability_acquired, overwrite=TRUE)
write.csv(communicationDisability_acquired, file = "communicationDisability_acquired.csv")
dbWriteTable(t4, "communicationDisability_congenital",communicationDisability_congenital, overwrite=TRUE)
write.csv(communicationDisability_congenital, file = "communicationDisability_congenital.csv")
dbWriteTable(t4, "communicationDisability_oldAge", communicationDisability_oldAge, overwrite=TRUE)
write.csv(communicationDisability_oldAge, file = "communicationDisability_oldAge.csv")


dbWriteTable(t4, "consumption1", consumption1, overwrite=TRUE)
write.csv(consumption1, file = "consumption1.csv")
dbWriteTable(t4, "consumption2", consumption2, overwrite=TRUE)
write.csv(consumption2, file = "consumption2.csv")
dbWriteTable(t4, "consumption3", consumption3, overwrite=TRUE)
write.csv(consumption3, file = "consumption3.csv")
dbWriteTable(t4, "consumption4", consumption4, overwrite=TRUE)
write.csv(consumption4, file = "consumption4.csv")

dbWriteTable(t4, "treatedBetnets", treatedBetnets, overwrite=TRUE)
write.csv(treatedBetnets, file = "treatedBetnets.csv")
dbWriteTable(t4, "sleptUnderBetnets", sleptUnderBetnets, overwrite=TRUE)
write.csv(sleptUnderBetnets, file = "sleptUnderBetnets.csv")


