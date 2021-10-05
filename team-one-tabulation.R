#### Load Libraries and data ####
#install.packages('tidyverse')
#install.packages('dplyr')
#install.packages('tidyr')
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
t1 <- dbConnect(RSQLite::SQLite(), "data/open/team1/team1.SQLite")

#### Read data from SQLite database and run summary ####

### Geography ####

##Table 1.1: Total population by Province and Census year, population density, Vanuatu: 1967-2020
provPop <- dbGetQuery(mydb, "SELECT province, hhld_type, 
                                    round(sum(ac_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province, hhld_type ") #Do not remove NAs. 

##Table 1.1: Total population by Area Council, Vanuatu
areaConcilPopSex <- dbGetQuery(mydb, "SELECT area_council, sex, hhld_type, 
                                    round(sum(ac_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY area_council, sex ") 

#areaCouncilPopAge <- dbGetQuery(mydb, "SELECT area_council, age_5yr_grp_80, 
#                                   round(sum(ac_factor)) as population
#                             FROM person
#                             WHERE can_enumerate = 1
#                             GROUP BY area_council, age_5yr_grp_80")

#dbWriteTable(t1, "areaConcilPopSex", areaConcilPopSex, overwrite=TRUE)
#dbWriteTable(t1, "areaCouncilPopAge", areaCouncilPopAge, overwrite=TRUE)

#write.csv(areaConcilPopSex,"C:\\Census 2020\\acPopSex.csv", row.names = FALSE)
#write.csv(areaCouncilPopAge,"C:\\Census 2020\\acPopAge.csv", row.names = FALSE)

##Table 1.2: Total population by place of residence and sex, and number of people living in households by household type
residPopProv <- dbGetQuery(mydb, "SELECT residence, sex, hhld_type, area_council, urban_rural, 
                                    round(sum(ac_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY  residence, hhld_type, area_council ") #Total population by residence and sex, householdtype
#Do not remove NAs

#### Age Indicator ####

##Table 2.1: Total population by 5-year age groups, Area Council by province and urban-rural residence
popUsualResidenceHhtype <- dbGetQuery(mydb, "SELECT residence, province,
                                               age_5yr_grp_80,
                                               urban_rural,area_council,
                                              sum (ac_factor) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by area_council, age_5yr_grp_80") #population by place of residence, sex and hhldtype, Does this apply for the household factor.                                  ")
gh <- sum(popUsualResidenceHhtype$population, na.rm = TRUE)
 format(gh, scientific = FALSE)

#residence, province,age_5yr_grp_80,urban_rural,

##Table 2.2: Total male population by 5-year age groups, Area Council by province and urban-rural residence 
## & Table 2.3 : Total female population by 5-year age groups, Area Council by province and urban-rural residence

totsexProv <- dbGetQuery(mydb, "SELECT province, age_5yr_grp_80,
                                 area_council, urban_rural,
                                 ROUND(SUM(CASE WHEN sex = 'Male' THEN ac_factor ELSE 0 END)) as male,
                                 ROUND(SUM(CASE WHEN sex = 'Female' THEN ac_factor ELSE 0 END)) as female
                                 FROM person
                                 WHERE can_enumerate = 1
                                 GROUP BY  area_council") #Total Male population by 5 year age group


##Table 2.4: Usual resident population by 5-year age groups and by province, Area Council
residAgeGrp <- dbGetQuery(mydb, "SELECT  residence,age_5yr_grp_80,area_council,province,hhld_type,
                                      round(sum(ac_factor)) as population
                                      FROM person
                                      WHERE can_enumerate = 1
                                      GROUP BY age_5yr_grp_80,area_council, province") #Usual Resisdence by 5 yr age group, province,council.

fh <- sum(residAgeGrp$population, na.rm = TRUE)
format(gh, scientific = FALSE)

residAgeGrpPov <- residAgeGrp %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = residence, values_from = population, values_fill = 0) %>%
  ungroup()

##Table 2.5: Usual resident population by 5-year age groups by province and Sex
reAgeGrpSex <- dbGetQuery(mydb, "SELECT  residence,age_5yr_grp_80,sex,province,hhld_type,
                                      round(sum(ac_factor)) as population
                                      FROM person
                                      WHERE can_enumerate = 1
                                      GROUP BY age_5yr_grp_80,sex, province, hhld_type") #Usual Resisdence by 5 yr age group, sex, province.

ih <- sum(reAgeGrpSex$population, na.rm = TRUE)
format(gh, scientific = FALSE)

residAgeGrpSexPov <- reAgeGrpSex %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = residence, values_from = population, values_fill = 0) %>%
  ungroup()


##Table 2.6: Usual resident population by single age and sex, and province
resdPopSexProv <- dbGetQuery(mydb, "SELECT age,sex,residence,province,
                                      round(sum(ac_factor)) as population
                                      FROM person
                                      WHERE can_enumerate = 1
                         GROUP BY age,sex,residence,province") #Residenc population by sex, single age and province.\

resdPopSexAc <- dbGetQuery(mydb, "SELECT age,sex,residence,area_council,
                                      round(sum(ac_factor)) as population
                                      FROM person
                                      WHERE can_enumerate = 1
                         GROUP BY age,sex,residence,area_council") 


resdPopSexPov <- resdPopSexProv %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = residence, values_from = population, values_fill = 0) %>%
  ungroup()

resdPopSexAcPov <- resdPopSexAc %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = residence, values_from = population, values_fill = 0) %>%
  ungroup()

#### Demography ####

##Table 8.1.1: Total population by 5-year age group, whether biological mother is still alive and by province 
bioMotherpop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80, province, birth_mother_alive,
                                               round (sum (ac_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by age_5yr_grp_80, province, birth_mother_alive") #population by 5 year age, Biological Mother still Alive

bioMomProv <- bioMotherpop %>%
 filter(population != "NA") %>%
 pivot_wider(names_from = birth_mother_alive, values_from = population, values_fill = 0) %>%
 ungroup()


##Table8.1.2: Total Population by 5-year age group, whether biological father is still alive and by province 
bioFatherpop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80, province, father_alive,
                                               round (sum (ac_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by age_5yr_grp_80, province, father_alive") #population by 5 year age, Biological father still Alive


#bioDadProv <- bioFatherpop %>%
# filter(population != "NA") %>%
# pivot_wider(names_from = father_alive, values_from = population, values_fill = 0) %>%
# ungroup()

##Table 8.2.1: Female population 15 years and over older by 5-year age group and whether ever given birth by province and urban-rural 
#Group to check this table for this figures.
femGivenBirthPop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80,ever_given_birth, totfemale, urban_rural,province, hhld_type,
                                               round (sum (ac_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by age_5yr_grp_80,totfemale, ever_given_birth, urban_rural, province, hhld_type") #Female population 15 years and older, given birth.

##Table 8.2.2: Female population 15 years and overolder by 5-year age group and total number of children ever born alive by province and urban-rural 
birthChildAlivePop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80,totfemale,age,ever_given_birth, urban_rural,province, hhld_type,
                                               round (sum (ac_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by age_5yr_grp_80,age,ever_given_birth, urban_rural, province, hhld_type") 

##Table 8.2.3: Female population 15 years and over older by 5-year age group and total number of children dead by province and urban-rural 
#Female population 15 years and over older by 5-year age group and total number of children dead by province and urban-rural 
femBirthChildDeadPop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80, males_died, females_died, age, urban_rural,province, hhld_type,
                                               round (sum (ac_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP BY age_5yr_grp_80, males_died, females_died, age, urban_rural, province, hhld_type") 

##Table 8.2.4: year of birth of last child born by sex of child, by province and urban-rural residence of mother.
# year of birth of last child born by sex of child, by province and urban-rural residence of mother

femBirthChildDeadPop <- dbGetQuery(mydb, "SELECT  last_child_born_mm, last_born_sex, province, sex,hhld_type,
                                                urban_rural,last_child_born_yrs,
                                               round (sum (ac_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP BY  last_child_born_mm, last_born_sex, province, sex,
                                                urban_rural, last_child_born_yrs, hhld_type")  # Double check on this figure


### Trial ####

#provSex <- dbGetQuery(mydb, "SELECT province,
#                                    sex,
#                                    round(sum(province_factor))as population 
#                              FROM person
#                              WHERE can_enumerate = 1
#                              GROUP BY province, sex") #Total population by provinces

#acPop <- dbGetQuery(mydb, "SELECT area_council,
#                                  sum(ac_factor) as population
#                           FROM person
#                           WHERE can_enumerate = 1
#                           GROUP BY area_council")

#pop_hh <- dbGetQuery(mydb, "SELECT province,
#                                   round(sum(hhld_ac_factor * hhsize)) as population
#                            FROM household
#                            WHERE can_enumerate = 1
#                            GROUP BY province ")


#livestock <- dbGetQuery(mydb, "SELECT household.area_council,
#                                      livestock.livestock_id,
#                                      round(sum(household.hhld_ac_factor * livestock.num_livestock)) as totallivestock  
#                              FROM household
#                              INNER JOIN livestock ON household.interview_key = livestock.interview_key
#                              WHERE household.can_enumerate = 1
#                              GROUP BY household.area_council, livestock.livestock_id
#                             ORDER BY household.island
#                        ")

####Using pivot_wider to cross-tabulate with 2 variables####
livestockpivot <- livestock %>%
  pivot_wider(names_from = livestock_id, values_from = totallivestock, values_fill = 0) %>%
  ungroup()

provSexPivot <- provSex %>%
  filter(population != "NA") %>%
  pivot_wider(names_from = sex, values_from = population, values_fill = 0) %>%
  ungroup()


#### Write computed tables to teams SQLite database #### 
dbWriteTable(t1, "provSexPivot", provSexPivot, overwrite=TRUE)
dbWriteTable(t1, "acPop", acPop, overwrite=TRUE)
dbWriteTable(t1, "livestockpivot", livestockpivot, overwrite=TRUE)
dbWriteTable(t1, "provSexPivot", provSexPivot, overwrite=TRUE)