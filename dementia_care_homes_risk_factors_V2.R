
# Start up ----------------------------------------------------------------


library(RODBC)
library(tidyverse)
library(lubridate)
library(readxl)

source("S:/WMC_ - 0911- Wales Multimorbidity Cohort (WMC)_ COVID-19/Chris Emmerson/login_box.R")
login <- getLogin('emmersoc') #Username is optional - omit and it will prompt for username.
channel <- odbcConnect('PR_SAIL',login[1],login[2])
rm(login)


# bring in data -----------------------------------------------------------
#set start amd end 
period_start_date  <- as.Date("2020-09-01 00:00:00")
period_end_date  <- as.Date("2020-12-31 23:59:59")
incubation_period <- 14



#code brings in cohort for 21 Jan 2021
rm(full_cohort_2020_21_JAN)  
full_cohort_2020_21_JAN <- sqlQuery(channel, "SELECT ALF_E,
         CCH_CAREHOME_RALF_INCEPTION,
         HEALTHBOARD_INCEPTION,
         FROM_DT, TO_DT,
         WOB,
         DOD,
         GNDR_CD,
         GP_START_DATE,
         MOVE_OUT_DATE,
         COHORT_END_DATE

FROM SAILWMC_V.C19_COHORT20_20210121
WHERE  CCH_CAREHOME_RALF_INCEPTION IS NOT NULL" )

#add age
full_cohort_2020_21_JAN <- full_cohort_2020_21_JAN %>% 
  mutate(age_period_start= time_length(difftime(period_start_date, WOB), "years")) %>% 
  mutate(age_period_Start_whole_no=trunc(time_length(difftime(period_start_date, WOB), "years")))

#remove dates of death and 'To' dates after period end (inc to dates of "9999-01-01")
full_cohort_2020_21_JAN$DOD[full_cohort_2020_21_JAN$DOD>period_end_date]<-NA
full_cohort_2020_21_JAN$MOVE_OUT_DATE[full_cohort_2020_21_JAN$MOVE_OUT_DATE>period_end_date]<-NA

# convert GP START DATE Col into marker for no GP data 
full_cohort_2020_21_JAN <- full_cohort_2020_21_JAN %>% 
  mutate(GP_DATA_AVAILABLE = if_else(!is.na(GP_START_DATE), 1, 0)) 
full_cohort_2020_21_JAN<- full_cohort_2020_21_JAN %>% 
  select(-GP_START_DATE)

#age category for start of period 
full_cohort_2020_21_JAN<-full_cohort_2020_21_JAN %>% 
  mutate(AGE_CAT=case_when(age_period_start>=90 ~ "over 90",
                           age_period_start>=85 ~ "85-89",
                           age_period_start>=80 ~ "80-84",
                           age_period_start>=75 ~ "75-79",
                           age_period_start>=70 ~ "70-74",
                           age_period_start>=65 ~ "65-69",
                           age_period_start>=60 ~ "60-64",
                           TRUE ~ "under 60")) 
full_cohort_2020_21_JAN$AGE_CAT<- as_factor(full_cohort_2020_21_JAN$AGE_CAT)
full_cohort_2020_21_JAN$AGE_CAT<- fct_relevel(full_cohort_2020_21_JAN$AGE_CAT,
                                         c("under 60","60-64","65-69","70-74", 
                                           "75-79","80-84","85-89","over 90" 
                                            ))


# IMPORT mortality 2020 ---------------------------------------------------
rm(mortality_data_2020)
mortality_data_2020<- sqlQuery(channel, "SELECT ALF_E,
  DOD

FROM SAILWMC_V.C19_COHORT20_MORTALITY")


mortality_data_2020<-mortality_data_2020 %>% 
  filter(DOD <=period_end_date & DOD>="2020-01-01") 

#join to full cohort
full_cohort_2020_21_JAN<- left_join(full_cohort_2020_21_JAN, mortality_data_2020, by="ALF_E") 

#collate the date of death on the two datasets, using mortality data 
#  as the main date - also remove legacy fields
full_cohort_2020_21_JAN<-full_cohort_2020_21_JAN %>% 
  mutate(Date_death= case_when(!is.na(DOD.y) ~ DOD.y,
                               !is.na(DOD.x) ~ DOD.x,
                               TRUE ~ DOD.y)) %>% 
  select(-c(DOD.x, DOD.y))


# IMPORT SDeC 2020 --------------------------------------------------------

rm(dementia_cohort_2020)
dementia_cohort_2020 <- sqlQuery(channel,  "SELECT  
                ALF_E,
                DEM_DT,
        DEMENTIA_FLAG,
        ALZHEIMER_FLAG,
        VASCULAR_FLAG
        
FROM SAILWMC_V.C19_COHORT_SDEC_COHORT_POPULATION
WHERE DEMENTIA_FLAG =1
AND  (DEATH_DT   IS NULL 
OR DEATH_DT >= '2020-01-01')
AND DEM_DT <='2020-12-31'")                            
          


#join dementia data to full cohort
full_cohort_2020_21_JAN <- left_join(full_cohort_2020_21_JAN, dementia_cohort_2020, by="ALF_E")

#replace NAs in Dementia Flag field with zeros
full_cohort_2020_21_JAN <- full_cohort_2020_21_JAN %>% 
  mutate(DEMENTIA_FLAG=if_else(is.na(DEMENTIA_FLAG), 0, 1)) 


# calculate time between dementia diagnosis and period start in months, 
#   with all diagnoses within the period calculated as 0 months since diagnosis
full_cohort_2020_21_JAN<-full_cohort_2020_21_JAN %>% 
  mutate(time_since_dem_diag=time_length(difftime(period_start_date, DEM_DT), "months")) 
full_cohort_2020_21_JAN$time_since_dem_diag[full_cohort_2020_21_JAN$time_since_dem_diag<0]<-0

# IMPORT PEDW data --------------------------------------------------------
rm(PEDW_spell_data_2020)
PEDW_spell_data_2020 <- sqlQuery(channel, 
                                 "SELECT ALF_E, ADMIS_DT,  DISCH_DT,  SPELL_DUR 
FROM SAILWMC_V.C19_COHORT_PEDW_SPELL
WHERE ADMIS_DT >= '2020-01-01'")

#this returns all those spells associated with a care home resident
PEDW_spell_data_2020<- PEDW_spell_data_2020 %>% 
  filter(ADMIS_DT<=period_end_date)

PEDW_spell_data_2020 <- PEDW_spell_data_2020 %>% 
  semi_join(full_cohort_2020_21_JAN, by="ALF_E") 


# to start, remove those in hosp all the time 

#this removes all those who spend the entire study period in hospital from full cohort...
full_cohort_2020_21_JAN <-PEDW_spell_data_2020 %>% 
  filter(ADMIS_DT<=period_start_date & DISCH_DT>=period_end_date) %>% 
  group_by(ALF_E) %>% 
  select(ALF_E) %>% 
  mutate(hosp_entire_period=1) %>% 
  right_join(full_cohort_2020_21_JAN, by="ALF_E") %>% 
  filter(is.na(hosp_entire_period)) %>% 
  select(-hosp_entire_period)

#....and from PEDW group
PEDW_spell_data_2020<-PEDW_spell_data_2020 %>% 
  filter(ADMIS_DT<=period_start_date & DISCH_DT>=period_end_date) %>% 
  group_by(ALF_E) %>% 
  select(ALF_E) %>% 
  mutate(hosp_entire_period=1) %>% 
  right_join(PEDW_spell_data_2020, by="ALF_E") %>% 
  filter(is.na(hosp_entire_period)) %>% 
  select(ALF_E,ADMIS_DT, DISCH_DT)

# now replace the end date if NA wiht period end date
PEDW_spell_data_2020<-PEDW_spell_data_2020 %>% 
  mutate(DISCH_DT=if_else(is.na(DISCH_DT), period_end_date, DISCH_DT))

#now we need to check for spells and move out dates
# this removes all instances where the individual died or moved out before admis date
PEDW_spell_data_2020<- PEDW_spell_data_2020 %>% 
  left_join(select(full_cohort_2020_21_JAN, 
                   ALF_E,
                   MOVE_OUT_DATE,
                   Date_death), by="ALF_E") %>% 
  mutate(moved_out_before_hosp=if_else((MOVE_OUT_DATE<ADMIS_DT | Date_death<ADMIS_DT), 1,0)) %>% 
  filter(moved_out_before_hosp==0 | is.na(moved_out_before_hosp)) %>% 
  select(ALF_E,ADMIS_DT, DISCH_DT )


#now remove all spells with no time within the study period to create study period dataset
rm(PEDW_spell_data_2020_study_period)
PEDW_spell_data_2020_study_period<-PEDW_spell_data_2020 %>% 
  filter(ADMIS_DT>period_start_date | DISCH_DT>period_start_date) 


# now replace the start date and end date as appropriate to calculate time in hosp during period
full_cohort_2020_21_JAN<-PEDW_spell_data_2020_study_period %>% 
  filter(ADMIS_DT>period_start_date | DISCH_DT>period_start_date) %>% 
  mutate(start_date=if_else(ADMIS_DT>period_start_date, ADMIS_DT, period_start_date)) %>% 
  mutate(end_date=if_else(DISCH_DT<period_end_date, DISCH_DT, period_end_date)) %>% 
  mutate(hosp_time=time_length (difftime(end_date, start_date), "days")) %>%
  group_by(ALF_E) %>%  
  summarise(time_hosp_study_period=sum(hosp_time)) %>% 
  mutate(hosp_admis_study_period=1) %>% 
  filter(time_hosp_study_period<time_length (difftime(period_end_date, period_start_date), "days")) %>% 
  right_join(full_cohort_2020_21_JAN, by="ALF_E") 


# IMPORT COVID test results -----------------------------------------------

rm(COVID_test_results_2020)
COVID_test_results_2020 <- sqlQuery(channel, "SELECT REC_ID_E,
SPCM_COLLECTED_DT,
ALF_E,
COVID19TESTRESULT
FROM SAILWMC_V.C19_COHORT_PATD_DF_COVID_LIMS_TESTRESULTS")


#only include test results of those who apper on the care home cohort
COVID_test_results_2020 <- COVID_test_results_2020 %>% 
  filter(SPCM_COLLECTED_DT>="2020-01-01" & SPCM_COLLECTED_DT<=period_end_date) %>% 
  semi_join(full_cohort_2020_21_JAN, by="ALF_E")


# IDENTIFY hospital acquired cases ----------------------------------------

# this links identifies every +ve test result in someone in hospital
#     and identifies the subset tested after more than 7 days, who 
#     can be seen as hospital acquired
rm(tests_in_hospital)
tests_in_hospital <- PEDW_spell_data_2020 %>% 
  left_join(COVID_test_results_2020, by="ALF_E") %>% 
  filter(COVID19TESTRESULT=="Positive") %>% 
  mutate(tested_in_hosp=if_else(SPCM_COLLECTED_DT>=ADMIS_DT &
                                  SPCM_COLLECTED_DT<DISCH_DT &
                                  !is.na(DISCH_DT),
                                1,0)) %>% 
  mutate(assume_hosp_acquired=if_else(SPCM_COLLECTED_DT>ADMIS_DT+incubation_period &
                                        SPCM_COLLECTED_DT<DISCH_DT &
                                        !is.na(DISCH_DT),
                                      1,0)) %>% 
  filter(tested_in_hosp==1)


#join the data on hospital testing to the main test results
COVID_test_results_2020 <- tests_in_hospital %>% 
  select(REC_ID_E, tested_in_hosp, assume_hosp_acquired) %>% 
  right_join(COVID_test_results_2020,  by="REC_ID_E") %>% 
  select(-ALF_E.x ) %>% 
  rename(ALF_E=ALF_E.y)
#and replace NAs with 0 
COVID_test_results_2020$tested_in_hosp[is.na(COVID_test_results_2020$tested_in_hosp)]<-0
COVID_test_results_2020$assume_hosp_acquired[is.na(COVID_test_results_2020$assume_hosp_acquired)]<-0


# IDENTIFY date first pos test and no of tests ----------------------------

#total number of tests 
full_cohort_2020_21_JAN <- COVID_test_results_2020 %>% 
  group_by(ALF_E) %>% 
  summarise(total_tests=n()) %>% 
  right_join(full_cohort_2020_21_JAN, by="ALF_E")


full_cohort_2020_21_JAN <- COVID_test_results_2020 %>% 
  filter(COVID19TESTRESULT=="Positive") %>% 
  group_by(ALF_E) %>% 
  summarise(REC_ID_E_first_pos_test=REC_ID_E[which.min(SPCM_COLLECTED_DT)], 
            date_first_pos_test=SPCM_COLLECTED_DT[which.min(SPCM_COLLECTED_DT)],
            first_pos_test_in_hosp=tested_in_hosp[which.min(SPCM_COLLECTED_DT)],
            first_pos_test_hosp_acquired=assume_hosp_acquired[which.min(SPCM_COLLECTED_DT)]
  ) %>% 
  right_join(full_cohort_2020_21_JAN, by="ALF_E") 

full_cohort_2020_21_JAN <- full_cohort_2020_21_JAN %>% 
  mutate(any_pos_test=if_else(is.na(date_first_pos_test), 0,1)) 


#identify whether test is in study period (also filters for hosp acquired)

full_cohort_2020_21_JAN<-full_cohort_2020_21_JAN %>% 
  mutate(pos_test_in_study_period= if_else((date_first_pos_test>period_start_date & 
                                              first_pos_test_hosp_acquired==0),1,0))
full_cohort_2020_21_JAN$pos_test_in_study_period[is.na(full_cohort_2020_21_JAN$pos_test_in_study_period)]<-0  


#this removes all first test results recorded later than TO-DT or Date-death + incubation period
full_cohort_2020_21_JAN <- full_cohort_2020_21_JAN %>% 
  mutate(test_after_death=if_else(date_first_pos_test>Date_death+incubation_period, 1,0)) %>% 
  mutate(test_after_MOVE_OUT_DATE=if_else(date_first_pos_test>MOVE_OUT_DATE+incubation_period, 1,0)) %>% 
  mutate(test_to_remove= if_else(test_after_death==1 | test_after_MOVE_OUT_DATE==1, 1,0)) %>% 
  mutate(date_first_pos_test=case_when((is.na(test_to_remove) | test_to_remove==0) ~ date_first_pos_test)) %>% 
  select(-c(test_after_death,test_after_MOVE_OUT_DATE,test_to_remove )) 


# IDENTIFY - index and secondary spread and susceptible period start-------------------------------------
# first create a set of all first pos tests by care home
all_pos_test_dates_by_carehome<- full_cohort_2020_21_JAN %>% 
  select(CCH_CAREHOME_RALF_INCEPTION, date_first_pos_test) %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION, date_first_pos_test) %>% 
  filter(!is.na(date_first_pos_test))

#now create and join columns to identify index and secondary cases
full_cohort_2020_21_JAN <- full_cohort_2020_21_JAN %>% 
  filter(!is.na(date_first_pos_test)) %>% 
  select(ALF_E, CCH_CAREHOME_RALF_INCEPTION, date_first_pos_test) %>% 
  left_join(all_pos_test_dates_by_carehome, by="CCH_CAREHOME_RALF_INCEPTION") %>% 
  mutate(date_first_pos_test=date_first_pos_test.x,
         risk_start=date_first_pos_test.y+1,
         risk_end=date_first_pos_test.y+(incubation_period*2)) %>% 
  select(-c(date_first_pos_test.x, date_first_pos_test.y)) %>% 
  rowwise() %>% 
  mutate(test_in_risk_period = if_else(between(date_first_pos_test, risk_start, risk_end),1,0)) %>% 
  mutate(test_prior = if_else(date_first_pos_test>risk_start, 1,0)) %>% 
  group_by(ALF_E) %>%
  summarise(out_break_preceding_pos_test=if_else(sum(test_in_risk_period)>0, 1,0),
            total_tests_2_inc_preceding=sum(test_in_risk_period)) %>% 
  right_join(full_cohort_2020_21_JAN, by="ALF_E") 


#this identifies those who are susceptible at the start of the study period (i.e. no previous +ve test)
full_cohort_2020_21_JAN<-full_cohort_2020_21_JAN %>% 
  mutate(susceptible_period_start=if_else(date_first_pos_test<period_start_date, 0,1)) 
full_cohort_2020_21_JAN$susceptible_period_start[is.na(full_cohort_2020_21_JAN$susceptible_period_start)]<-1

# IMPORT care homes data --------------------------------------------------

rm(care_home_details, care_home_RALF)
#this brings in the details of the care homes, such as services and size
care_home_details <- sqlQuery(channel, 
                              "SELECT *
                                  FROM SAILWMC_V.C19_COHORT_CARE_CARE_HOMES")

care_home_details<- care_home_details%>%
  select(SYSTEM_ID_E, LA, SERVICES,LINKED_CARE_HOMES,PLACES, HOUSING_TYPE:LSOA11CODE)

# this care home details dataset does not include the RALF needed to 
# link to the resident dataset
# this code brings in the RALF data
care_home_RALF <- sqlQuery(channel, 
                           "SELECT *
  FROM SAILWMC_V.C19_COHORT_CARE_CARE_HOMES_RALF")

# There are a small number of RALFs with multiple system IDs
# This selects the most recently associated SYSTEM-IDE

care_home_RALF<- care_home_RALF %>% 
  group_by(RALF_E) %>% 
  summarise(SYSTEM_ID_E=SYSTEM_ID_E[which.max(AVAIL_FROM_DT)],
            AVAIL_FROM_DT=AVAIL_FROM_DT[which.max(AVAIL_FROM_DT)]) 


#this joins the RALF data to the main care home data by SYSTEM_ID_E
care_home_details<-inner_join(care_home_details, care_home_RALF, by="SYSTEM_ID_E")
care_home_details<- care_home_details %>% 
  select(SYSTEM_ID_E:RALF_E)

#add a field to identify homes with nursing 
care_home_details <- care_home_details %>% 
  mutate(nursing_provision=if_else(str_detect(SERVICES, "nursing"), 1,0)) 


# PERIOD cohort ------------------------------------------------------
#this creates a set of all those registered in a care home at the period start

#this removes records of all those who moved from their listed address or died before the period start
period_cohort<-full_cohort_2020_21_JAN %>% 
  filter((is.na(MOVE_OUT_DATE) | MOVE_OUT_DATE>=period_start_date) & (is.na(Date_death) | Date_death>=period_start_date))  

# this calculates the amount of days registered at the CH
period_cohort<-period_cohort %>% 
  select(ALF_E, FROM_DT, MOVE_OUT_DATE, Date_death) %>% 
  mutate(start_date=if_else(FROM_DT>period_start_date, FROM_DT, period_start_date)) %>% 
  mutate(end_date=if_else(MOVE_OUT_DATE<period_end_date, MOVE_OUT_DATE, period_end_date)) %>% 
  mutate(time_in_study_period=time_length (difftime(end_date, start_date), "days")) %>% 
  select(ALF_E, time_in_study_period) %>% 
  right_join(period_cohort, by="ALF_E") 

#this creates a field that subtracts the hosp time from overall time
period_cohort<-period_cohort %>% 
  mutate(time_in_residence=(time_in_study_period-time_hosp_study_period)) %>% 
  mutate(time_in_residence=(if_else(is.na(time_in_residence), 
                                    time_in_study_period, time_in_residence))) %>% 
  select(ALF_E,time_in_residence) %>% 
  right_join(period_cohort, by="ALF_E") 

# CREATE final care homes dataset -----------------------------------------

#number positive during and pre study period, total residents cohort strt and date first pos
care_home_details<-full_cohort_2020_21_JAN %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(residents_cohort_start=n(),
            total_res_positive_during_period=sum(!is.na(date_first_pos_test[date_first_pos_test>period_start_date])),
            total_res_positive=sum(any_pos_test),
            date_first_pos_test_in_home=min(date_first_pos_test, na.rm = TRUE)) %>% 
  mutate(outbreak_in_study_period=if_else(total_res_positive_during_period>0,1,0)) %>% 
  right_join(care_home_details, by=c("CCH_CAREHOME_RALF_INCEPTION"="RALF_E"))
care_home_details$total_res_positive[is.na(care_home_details$total_res_positive)]<-0
care_home_details$total_res_positive_during_period[is.na(care_home_details$total_res_positive_during_period)]<-0
care_home_details$outbreak_in_study_period[is.na(care_home_details$outbreak_in_study_period)]<-0

# number residents at start period 
care_home_details<-period_cohort %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(total_residents_start_study_period=n()) %>% 
  right_join(care_home_details, by="CCH_CAREHOME_RALF_INCEPTION") 

# now calculate number of susceptible (no pos test prior to study period)
care_home_details<-period_cohort %>%
  filter(is.na(date_first_pos_test) | date_first_pos_test>=period_start_date) %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(susceptible_start_study_period=n()) %>% 
  right_join(care_home_details, by="CCH_CAREHOME_RALF_INCEPTION") 
care_home_details$susceptible_start_study_period[is.na(care_home_details$susceptible_start_study_period)]<-0

#calculate the number of residents with a dementia diganosis at start of period 
#   (NB - inlcudes those diagnosed within the period)
care_home_details<-period_cohort %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(total_dementia_flag_start_study_period=sum(DEMENTIA_FLAG)) %>% 
  right_join(care_home_details, by="CCH_CAREHOME_RALF_INCEPTION")
care_home_details$total_dementia_flag_start_study_period[is.na(care_home_details$total_dementia_flag_start_study_period)]<-0

#proportions of residents susceptible and with dementia flag at period start
care_home_details<-care_home_details %>% 
  mutate(prop_dem_flag_period_start=total_dementia_flag_start_study_period/total_residents_start_study_period) %>% 
  mutate(prop_susceptible_period_start= susceptible_start_study_period/ total_residents_start_study_period ) 

care_home_details$prop_dem_flag_period_start[is.na(care_home_details$prop_dem_flag_period_start)]<-0
care_home_details$prop_susceptible_period_start[is.na(care_home_details$prop_susceptible_period_start)]<-0

#proportion of those with GP data available in residence
care_home_details<-period_cohort %>%
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(GP_data_home=sum(GP_DATA_AVAILABLE),
            prop_GP_DATA=(GP_data_home/n())) %>% 
  right_join(care_home_details, by="CCH_CAREHOME_RALF_INCEPTION") 



#categorise homes by size
care_home_details<-care_home_details %>% 
  mutate(size_cat=case_when(PLACES>=50 ~ "50 plus",
                            PLACES>=25 ~ "25-49",
                            PLACES>=10 ~ "10-24",
                            PLACES<10 ~ "less than 10",
                            TRUE ~ "not recorded")) 

care_home_details$size_cat<- as_factor(care_home_details$size_cat)
care_home_details$size_cat<- fct_relevel(care_home_details$size_cat,
                                         c("less than 10", "10-24","25-49","50 plus","not recorded"))

#categorise by num residents identified at start period, prop with dementia and prop susceptible
care_home_details<-care_home_details %>% 
  mutate(identified_res_start_period_cat=case_when(total_residents_start_study_period>=50 ~ "50 plus",
                                                   total_residents_start_study_period>=25 ~ "25-49",
                                                   total_residents_start_study_period>=10 ~ "10-24",
                                                   total_residents_start_study_period<10 ~ "less than 10")) 
care_home_details$identified_res_start_period_cat<- as_factor(care_home_details$identified_res_start_period_cat)
care_home_details$identified_res_start_period_cat<- fct_relevel(care_home_details$identified_res_start_period_cat,
                                         c("less than 10", "10-24","25-49","50 plus"))


care_home_details<-care_home_details %>% 
  mutate(prop_dementia_flag_start_study_period_cat=case_when(prop_dem_flag_period_start>=0.5 ~ "50%+",
                                                             prop_dem_flag_period_start>0 ~ "<50%",
                                                             TRUE ~ "zero")) 
care_home_details$prop_dementia_flag_start_study_period_cat<- as_factor(care_home_details$prop_dementia_flag_start_study_period_cat)
care_home_details$prop_dementia_flag_start_study_period_cat<- fct_relevel(care_home_details$prop_dementia_flag_start_study_period_cat,
                                                                c("zero", "<50%", "50%+"))

care_home_details<-care_home_details %>% 
  mutate(prop_susceptible_period_start_cat=case_when(prop_susceptible_period_start==1 ~ "All susceptible",
                                                     prop_susceptible_period_start==0 ~ "None susceptible",
                                                     TRUE ~ "Proportion susceptible"))
care_home_details$prop_susceptible_period_start_cat<-as_factor(care_home_details$prop_susceptible_period_start_cat)
care_home_details$prop_susceptible_period_start_cat<-fct_relevel(care_home_details$prop_susceptible_period_start_cat,
                                                                 c("None susceptible", "Proportion susceptible","All susceptible"))

# communal areas
care_home_details<-care_home_details %>% 
 mutate(COMMUNAL_AREAS_M2=as.integer(COMMUNAL_AREAS_M2))

care_home_details<-care_home_details %>% 
  mutate(COMMUNAL_AREAS_M2_CAT=case_when(is.na(COMMUNAL_AREAS_M2) ~"not recorded",
                                        COMMUNAL_AREAS_M2>500 ~ "500+",
                                         COMMUNAL_AREAS_M2>200 ~ "200+",
                                         COMMUNAL_AREAS_M2>100 ~ "100+",
                                         COMMUNAL_AREAS_M2>50 ~ "50+",
                                         TRUE ~ "<50")) 
care_home_details$COMMUNAL_AREAS_M2_CAT<-as_factor(care_home_details$COMMUNAL_AREAS_M2_CAT)
care_home_details$COMMUNAL_AREAS_M2_CAT<-fct_relevel(care_home_details$COMMUNAL_AREAS_M2_CAT,
                                                     c("<50", "50+","100+","200+","500+" ))

# FINAL study set ---------------------------------------------------------

#join care home details 
period_cohort<-period_cohort %>% 
  left_join(care_home_details, by="CCH_CAREHOME_RALF_INCEPTION") 

glimpse(period_cohort)

# join frailty scores

frailty_scores<- as_tibble(read.csv("efi.csv"))

frailty_scores <-frailty_scores %>% 
  select(ALF_E, FRAILTY, FRAILTY_DEF)

#create frailty scores as a factor
frailty_scores$FRAILTY_DEF<-as_factor(frailty_scores$FRAILTY_DEF)
frailty_scores$FRAILTY_DEF<- fct_relevel(frailty_scores$FRAILTY_DEF,
                                              c("Fit", "Mild", "Moderate", "Severe" 
                                              ))

period_cohort<-left_join(period_cohort, frailty_scores, by="ALF_E") 

# making the HB field complete
#remove all spaces in LA listings
period_cohort<-period_cohort %>% 
  mutate(LA=str_replace_all(LA, " ", "")) %>% 
  mutate(DEMENTIA_SERVICE=str_replace_all(DEMENTIA_SERVICE, " ", "")) 
period_cohort$DEMENTIA_SERVICE[is.na(period_cohort$DEMENTIA_SERVICE)]<-"Unknown" 



  
# create a new field that interpolates LA values where HB is NA
period_cohort<-period_cohort %>%
  mutate(HB_final=case_when(!is.na(HEALTHBOARD_INCEPTION)~HEALTHBOARD_INCEPTION,
                            TRUE ~ LA  ))
#replace the LA values with HB values 
period_cohort$HB_final[period_cohort$HB_final=="Cardiff"]<-"Cardiff and Vale University Health Board"
period_cohort$HB_final[period_cohort$HB_final=="Conwy"]<-"Betsi Cadwaladr University Health Board"
period_cohort$HB_final[period_cohort$HB_final=="MerthyrTydfil"]<-"Cwm Taf Morgannwg University Health Board"
period_cohort$HB_final[period_cohort$HB_final=="NeathPortTalbot"]<-"Swansea Bay University Health Board"
period_cohort$HB_final[period_cohort$HB_final=="RhonddaCynonTaf"]<-"Cwm Taf Morgannwg University Health Board"
period_cohort$HB_final[period_cohort$HB_final=="Swansea"]<-"Swansea Bay University Health Board"
period_cohort$HB_final[period_cohort$HB_final=="ValeofGlamorgan"]<-"Cardiff and Vale University Health Board"


# select final fields for analysis and 
study_cohort<-period_cohort %>% 
    filter(GP_DATA_AVAILABLE==1) %>% 
  filter(susceptible_period_start==1)%>% 
  filter(time_in_residence>=14) %>% 
  
  select(ALF_E, GNDR_CD, AGE=age_period_start, AGE_CAT,  DT_DEATH=Date_death,
         
         DEMENTIA_FLAG, TIME_DEMENTIA=time_since_dem_diag, GP_DATA_AVAILABLE,
         
         STUDY_TIME=time_in_residence,
         
         TOTAL_TESTS=total_tests,
         
         POS_TEST=pos_test_in_study_period, DATE_POS_TEST=date_first_pos_test,
         
         POS_TEST_DURING_OUTBREAK=out_break_preceding_pos_test,
         POS_RES_IN_OUTBREAK_PRECEDING_POS_TEST=total_tests_2_inc_preceding,
         
         CCH_CAREHOME_RALF_INCEPTION, HEALTH_BOARD=HB_final,
         PLACES,PLACES_CAT=size_cat,
         
         RES_IDENTIFIED=total_residents_start_study_period,
         RES_IDENTIFIED_CAT=identified_res_start_period_cat,
         
         DEM_IDENTIFIED=total_dementia_flag_start_study_period,
         DEM_PROP=  prop_dem_flag_period_start, 
         
         DEM_PROP_CAT= prop_dementia_flag_start_study_period_cat,
         
         FRAILTY,
         FRAILTY_DEF,
         
         RES_GP_DATA=GP_data_home, GP_DATA_PROP=prop_GP_DATA,
         
         
         SUSCEPTIBLE=susceptible_start_study_period, 
         PROP_SUSCEPTIBLE=prop_susceptible_period_start ,
         PROP_SUSCEPTIBLE_CAT=prop_susceptible_period_start_cat,
         
         CH_OUTBREAK=outbreak_in_study_period ,
         
         
         DEMENTIA_SERVICE,
         NURSING=nursing_provision,
         COMMUNAL_AREAS_M2, COMMUNAL_AREAS_M2_CAT 
         
               ) %>% 
  mutate(DEM_PCT= (DEM_PROP*100)) %>% 
  mutate(SUSCEPTIBLE_PCT=(PROP_SUSCEPTIBLE*100)) 

#create GNDR as Sex, character variable
study_cohort<-study_cohort %>% 
  mutate(SEX=case_when(GNDR_CD=='1' ~ "Male", 
            GNDR_CD=='2' ~ "Female"))

study_cohort   <- study_cohort%>% 
  select(ALF_E:DEM_PROP, DEM_PCT, DEM_PROP_CAT:PROP_SUSCEPTIBLE, SUSCEPTIBLE_PCT,
         PROP_SUSCEPTIBLE_CAT :COMMUNAL_AREAS_M2_CAT, SEX) 

#partition dementia prop categories 
study_cohort<-study_cohort %>% 
  mutate(DEM_PROP_2=case_when(DEM_PROP==0 ~ '0%',
                              DEM_PROP<0.25 ~ '<25%',
                              DEM_PROP<0.50 ~ '<50%',
                              DEM_PROP<0.75 ~ '<75%',
                              TRUE ~ '>75%')) 

study_cohort$DEM_PROP_2<-as_factor(study_cohort$DEM_PROP_2)
study_cohort$DEM_PROP_2<-fct_relevel(study_cohort$DEM_PROP_2, c('0%','<25%','<50%','<75%','>75%'))

#secondary age cat by 10 years 
study_cohort<-study_cohort %>% 
  mutate(AGE_CAT_10_YRS=case_when(AGE>=90 ~ "over 90",
                           AGE>=80 ~ "80-89",
                           AGE>=70 ~ "70-79",
                           AGE>=60 ~ "60-69",
                           TRUE ~ "under 60")) 
study_cohort$AGE_CAT_10_YRS<- as_factor(study_cohort$AGE_CAT_10_YRS)
study_cohort$AGE_CAT_10_YRS<- fct_relevel(study_cohort$AGE_CAT_10_YRS,
                                              c("under 60","60-69",
                                                "70-79","80-89", "over 90"))
#   identify those for which care home DEMENTIA status was unknown consistently 
study_cohort$DEMENTIA_SERVICE[is.na(study_cohort$DEMENTIA_SERVICE)]<-"Unknown" 
# creating a binary dementia category
study_cohort<-study_cohort %>% 
  mutate(DEM_PROP_CAT2=case_when(DEM_PROP_CAT=="50%+" ~ "50%+",
                                 TRUE ~ "<50%")) 