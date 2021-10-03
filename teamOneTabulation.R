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
provPop <- dbGetQuery(mydb, "SELECT province, age_5yr_grp_80, sex, hhld_type, 
                                    round(sum(province_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province ") # Work on that tomorrow


popFiveYearAgeGrp <- dbGetQuery(mydb, "SELECT  
                                  age_5yr_grp_80,area_council,
                                  urban_rural,province,
                                round(sum(province_factor)) as population
                                FROM person
                                WHERE can_enumerate = 1
                                GROUP BY age_5yr_grp_80,area_council,
                                  urban_rural,province") #practice run for the 5year age group

##Table 1.2: Total population by place of residence and sex, and number of people living in households by household type
residPopProv <- dbGetQuery(mydb, "SELECT province, residence, sex, hhld_type, area_council, urban_rural, 
                                    round(sum(province_factor)) as population
                             FROM person
                             WHERE can_enumerate = 1
                             GROUP BY province, residence, sex, hhld_type, area_council, urban_rural ") #Total population by residence and sex, householdtype

#### Age Indicator ####

##Table 2.1: Total population by 5-year age groups, Area Council by province and urban-rural residence
popUsualResidenceHhtype <- dbGetQuery(mydb, "SELECT residence,
                                               sex,
                                               hhld_type,area_council,
                                               round(sum (province_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by residence, hhld_type,sex, area_council") #population by place of residence, sex and hhldtype, Does this apply for the household factor.                                  ")

##Table 2.2: Total male population by 5-year age groups, Area Council by province and urban-rural residence 
totMaleProv <- dbGetQuery(mydb, "SELECT totmale, age_5yr_grp_80,
                                 area_council,province, urban_rural,
                                 round(sum(province_factor)) as totalMalePop
                                 FROM person
                                 WHERE can_enumerate = 1
                                 GROUP BY totmale, urban_rural,age_5yr_grp_80, province") #Total Male population by 5 year age group

##Table 2.3: Total female population by 5-year age groups, Area Council by province and urban-rural residence
totFemaleProv <- dbGetQuery(mydb, "SELECT totfemale, age_5yr_grp_80,
                                 area_council,province,urban_rural,
                                 round(sum(province_factor)) as totalFemalePop
                                 FROM person
                                 WHERE can_enumerate = 1
                                 GROUP BY totfemale, age_5yr_grp_80, urban_rural,province")  #Total Female population by 5 year age group

##Table 2.4: Usual resident population by 5-year age groups and by province, Area Council
residAgeGrp <- dbGetQuery(mydb, "SELECT  residence,age_5yr_grp_80,area_council,province,
                                      round(sum(province_factor)) as population
                                      FROM person
                                      WHERE can_enumerate = 1
                                      GROUP BY age_5yr_grp_80,area_council,province") #Usual Resisdence by 5 yr age group, province,council.

##Table 2.5: Usual resident population by 5-year age groups, Area Council by province and Sex
reAgeGrpSex <- dbGetQuery(mydb, "SELECT  residence,age_5yr_grp_80,sex,province,
                                      round(sum(province_factor)) as population
                                      FROM person
                                      WHERE can_enumerate = 1
                                      GROUP BY age_5yr_grp_80,sex,province") #Usual Resisdence by 5 yr age group, sex, province.

##Table 2.6: Usual resident population by single age and sex, and province
resdPopSex <- dbGetQuery(mydb, "SELECT age,sex,residence,province,
                                      round(sum(province_factor)) as population
                                      FROM person
                                      WHERE can_enumerate = 1
                                      GROUP BY age,sex,residence,province") #Residenc population by sex, single age and province.
#### Demography ####

##Table 8.1.1: Total population by 5-year age group, whether biological mother is still alive and by province 
bioMotherpop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80, province, birth_mother_alive,
                                               round (sum (province_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by age_5yr_grp_80, province, birth_mother_alive") #population by 5 year age, Biological Mother still Alive

##Table8.1.2: Total Population by 5-year age group, whether biological father is still alive and by province 
bioFatherpop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80, province, father_alive,
                                               round (sum (province_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by age_5yr_grp_80, province, father_alive") #population by 5 year age, Biological father still Alive

##Table 8.2.11.2: Female population 15 years and over older by 5-year age group and whether ever given birth by province and urban-rural 
#Group to check this table for this figures.
femGivenBirthPop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80,age,ever_given_birth, totfemale, urban_rural,province,
                                               round (sum (province_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by age_5yr_grp_80,age,totfemale, ever_given_birth, urban_rural, province") #Female population 15 years and older, given birth.

##Table 8.2.2: Female population 15 years and overolder by 5-year age group and total number of children ever born alive by province and urban-rural 
birthChildAlivePop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80,totfemale,age,ever_given_birth, urban_rural,province,
                                               round (sum (province_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP by age_5yr_grp_80,age,ever_given_birth, urban_rural, province") .

##Table 8.2.3: Female population 15 years and over older by 5-year age group and total number of children dead by province and urban-rural 
#Female population 15 years and over older by 5-year age group and total number of children dead by province and urban-rural 
femBirthChildDeadPop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80, males_died, females_died, age, urban_rural,province,
                                               round (sum (province_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP BY age_5yr_grp_80, males_died, females_died, age, urban_rural, province") 

##Table 8.2.5: year of birth of last child born by sex of child, by province and urban-rural residence of mother.
# year of birth of last child born by sex of child, by province and urban-rural residence of mother
femBirthChildDeadPop <- dbGetQuery(mydb, "SELECT age_5yr_grp_80, last_child_born_mm, last_born_sex, province, sex,
                                                urban_rural,
                                               round (sum (province_factor)) as population
                                               FROM person
                                               WHERE can_enumerate = 1
                                               GROUP BY age_5yr_grp_80, last_child_born_mm, last_born_sex, province, sex,
                                                urban_rural")  # Double check on this figure




### Trial ####
provSex <- dbGetQuery(mydb, "SELECT province,
                                    sex,
                                    round(sum(province_factor))as population 
                              FROM person
                              WHERE can_enumerate = 1
                              GROUP BY province, sex") #Total population by provinces

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