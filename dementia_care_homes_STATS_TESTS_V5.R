#summary statistics on exclusions
#total residents on 1 Sept 2020, inc check for no duplicate records 
period_cohort%>% 
  summarise(n(), n_distinct(ALF_E), n_distinct(CCH_CAREHOME_RALF_INCEPTION))

#############################
#
#
# CONSORT TABLE
#
#############################


# check and remove no GP record
period_cohort %>% 
  group_by(GP_DATA_AVAILABLE) %>% 
  summarise(n(), n_distinct(ALF_E), n_distinct(CCH_CAREHOME_RALF_INCEPTION))

CONSORT_TABLE<-period_cohort %>% 
  filter(GP_DATA_AVAILABLE==1)

# check and remove not susceptible 
CONSORT_TABLE%>%
group_by(susceptible_period_start)%>% 
  summarise(total_residents=n(), homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION))

CONSORT_TABLE<-CONSORT_TABLE%>%
  filter(susceptible_period_start==1)

CONSORT_TABLE %>% 
  summarise(n())

# time in residence
CONSORT_TABLE%>%
  group_by(time_in_residence>=14) %>% 
  summarise(total_residents=n(), homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION))

CONSORT_TABLE<-CONSORT_TABLE%>%
  filter(time_in_residence>=14) 

CONSORT_TABLE %>% 
  summarise(n())

#identifying and removing homes with residents all under 65
care_home_total_res<-study_cohort %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(total_res=n() )

care_home_under65<-study_cohort %>% 
  filter(AGE<65) %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(under_65=n() )

care_home_by_age<- left_join(care_home_total_res, care_home_under65)
care_homes_100pct_u65_list<-care_home_by_age %>% 
  mutate(prop_u65=under_65/total_res) %>% 
  filter(prop_u65==1) %>% 
  select (CCH_CAREHOME_RALF_INCEPTION)



CONSORT_TABLE<-anti_join(study_cohort, care_homes_100pct_u65_list) 
CONSORT_TABLE %>% 
  summarise(n())


# those without nursing provision 
CONSORT_TABLE %>% 
  group_by(is.na(NURSING))%>% 
  summarise(n())

CONSORT_TABLE<-CONSORT_TABLE %>% 
  filter(!is.na(NURSING))

CONSORT_TABLE %>% 
  summarise(n())

#############################
#
# FINAL filter of study cohort 
#
#
####################

#NB - GP data, susceptible and time in residence already accounted for in prev script
# this removes all those in homes entirely <65 and any nursing not known
study_cohort <-anti_join(study_cohort, care_homes_100pct_u65_list)

study_cohort<-study_cohort %>% 
  filter(!is.na(NURSING))
  
study_cohort %>% 
  summarise(residents=n(), care_homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION))

################
#
# GP data available
# additional statistical analysis 
#
################################

period_cohort%>%
  filter(GP_DATA_AVAILABLE==0) %>% 
  summarise(total_residents=n(), homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION))

#analysis of those with no GP data compared to those we have it for
# how many homes were all residents removed from analysis 
period_cohort%>%
  group_by(prop_GP_DATA) %>% 
  summarise(total_residents=n(), homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION))

# how many residents in how many homes had no GP data available
period_cohort%>%
  group_by(GP_DATA_AVAILABLE) %>% 
  summarise(total_residents=n(), 
            homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION),
            ave_age=mean(age_period_start))

period_cohort%>%
  group_by(GP_DATA_AVAILABLE, GNDR_CD) %>% 
  summarise(total=n()) %>% 
  mutate(freq=total/sum(total))

period_cohort$GNDR_CD<-as_factor(period_cohort$GNDR_CD)
period_cohort$GP_DATA_AVAILABLE <-as_factor(period_cohort$GP_DATA_AVAILABLE )
period_cohort$DEMENTIA_FLAG <-as_factor(period_cohort$DEMENTIA_FLAG )

# create table for chi square
GNDR_by_GP_data_table<-data.frame(period_cohort$GNDR_CD,
                                  period_cohort$GP_DATA_AVAILABLE)
GNDR_by_GP_data_table=table(GNDR_by_GP_data_table)
GNDR_by_GP_data_table

#chi square
print(chisq.test(GNDR_by_GP_data_table))

# t-test for age of cohort
age_vector_GP_data <-period_cohort %>% 
  filter(GP_DATA_AVAILABLE==1) %>% 
  select(age_period_start)
  
age_vector_no_GP_data<-period_cohort %>% 
  filter(GP_DATA_AVAILABLE==0) %>% 
  select(age_period_start)

t.test(age_vector_GP_data, age_vector_no_GP_data)

# dementia diagnosis
period_cohort %>% 
  group_by(GP_DATA_AVAILABLE, DEMENTIA_FLAG) %>% 
  summarise(total=n()) %>% 
  mutate(freq=total/sum(total))

# create table for chi square
DEM_DIAG_by_GP_data_table<-data.frame(period_cohort$DEMENTIA_FLAG,
                                  period_cohort$GP_DATA_AVAILABLE)
DEM_DIAG_by_GP_data_table=table(DEM_DIAG_by_GP_data_table)
DEM_DIAG_by_GP_data_table

#chi square
print(chisq.test(DEM_DIAG_by_GP_data_table))

#by health board
period_cohort %>% 
  group_by(HB_final,GP_DATA_AVAILABLE ) %>% 
  summarise(total=n()) %>% 
  mutate(freq=total/sum(total))

HB_by_GP_data_table<-data.frame(period_cohort$HB_final,
                                period_cohort$GP_DATA_AVAILABLE )
HB_by_GP_data_table=table(HB_by_GP_data_table)
HB_by_GP_data_table

#chi square
print(chisq.test(HB_by_GP_data_table))

#by size
period_cohort %>% 
  group_by(identified_res_start_period_cat   ,GP_DATA_AVAILABLE ) %>% 
  summarise(total=n()) %>% 
  mutate(freq=total/sum(total))

Identified_by_GP_data_table<-data.frame(period_cohort$identified_res_start_period_cat ,
                                        period_cohort$GP_DATA_AVAILABLE )
Identified_by_GP_data_table=table(Identified_by_GP_data_table)
Identified_by_GP_data_table

#chi square
print(chisq.test(Identified_by_GP_data_table))

levels(period_cohort$identified_res_start_period_cat)


#total numbers in homes
study_cohort %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise (n(), mean(RES_IDENTIFIED), max_capacity=mean(PLACES), sum(SUSCEPTIBLE))

#homes wihout dementia residents
study_cohort %>% 
  group_by(DEM_PROP_2,POS_TEST ) %>% 
  summarise(total=n()) %>% 
  mutate(freq=total/sum(total))


#############################
#                           #
# BASIC DESCRIPTIVE STATS   #
#                           #
#############################
# total positive test
study_cohort %>% 
  group_by(POS_TEST ) %>% 
  summarise(total=n())%>% 
  mutate(pct=round(total/sum(total)*100,digits= 1) )

#total dementia 
study_cohort %>% 
  group_by(DEMENTIA_FLAG ) %>% 
  summarise(total=n())%>% 
  
#prop dementia  
study_cohort %>% 
  group_by(DEM_PROP_2) %>% 
  summarise (individuals=n(), homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION)) %>% 
  mutate(pct=round(individuals/sum(individuals)*100,digits= 1) ) %>% 
  mutate(homes_pct=round(homes/sum(homes)*100,digits= 1) )

#susceptibiltiy
study_cohort %>% 
  group_by(PROP_SUSCEPTIBLE_CAT) %>% 
  summarise (individuals=n(), homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION)) %>% 
  mutate(pct=round(individuals/sum(individuals)*100,digits= 1) ) %>% 
  mutate(homes_pct=round(homes/sum(homes)*100,digits= 1) )

study_cohort %>% 
  filter(SUSCEPTIBLE_PCT<75) %>% 
  summarise (individuals=n(), homes=n_distinct(CCH_CAREHOME_RALF_INCEPTION)) %>% 
  mutate(pct=round(individuals/sum(individuals)*100,digits= 1) ) %>% 
  mutate(homes_pct=round(homes/sum(homes)*100,digits= 1) ) 

#mean number of places in care home
study_cohort %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(reg_places=mean(PLACES), all_1_Sept=mean(RES_IDENTIFIED), all_analysis=n()) %>%
  summarise(n(), mean(reg_places), mean( all_1_Sept), mean(all_analysis))

################################
#
# total positive test - figures for table 2 desc
#
###############################

study_cohort %>% 
  group_by(RES_IDENTIFIED_CAT , POS_TEST  ) %>% 
  summarise(total=n())%>% 
  mutate(pct=round(total/sum(total)*100,digits= 1) )

#for continuous variables
study_cohort %>% 
  group_by(POS_TEST  ) %>% 
  summarise(total=n(), mean(AGE))




#data avaiable for communal space 
study_cohort %>% 
  group_by(POS_TEST, is.na(COMMUNAL_AREAS_M2)) %>% 
  summarise(total=n()) %>% 
  mutate(pct=round(total/sum(total)*100,digits= 1) ) 

#homes with no residents with dementia 

study_cohort %>% 
  group_by(DEM_PCT) %>% 
  summarise(n(), n_distinct(CCH_CAREHOME_RALF_INCEPTION)) 

#number of dementia flagged individuals by different categories of home
study_cohort %>% 
  group_by(DEMENTIA_SERVICE, DEMENTIA_FLAG) %>% 
  summarise(n()) 

#chi squared for dementia 
chi_sq_table_Demflag_Demserv<-table(study_cohort$DEMENTIA_SERVICE, study_cohort$DEMENTIA_FLAG)
print(chi_sq_table_Demflag_Demserv)

print(chisq.test(chi_sq_table_Demflag_Demserv))
# VARIABLES FOR MODELS 
#   OUTCOME VARIABLE
#     COVID-19 Diagnosis 
#   LEVEL 1 VARIABLES
#     Gender, Age, Dementia,FRAILTY  
#   LEVEL 2 VARIABLES
#     HB, Nursing, Dementia service, size, Prop with dementia, Prop susceptible
#   INTERACTIONS
#   Dementia/Dementia Service; Size/Dementia


library(lme4)
library(sjPlot)
library(sjstats)
library(pscl)
library(broom)
library(caret)
library(car)

##########################
#                        #
# COLLINEARITY           #
#                        #
##########################

#checking for collinearity between covariates that may be associated
collinearity_check_dementia<-glmer(POS_TEST ~  DEMENTIA_FLAG+DEMENTIA_SERVICE + DEM_PROP+ 
                            (1 | CCH_CAREHOME_RALF_INCEPTION)+
                            (1 | HEALTH_BOARD), 
                          family=binomial, 
                          data=study_cohort  )
car::vif(collinearity_check_dementia)

# places and residents identified
collinearity_check_CH_size<-glmer(POS_TEST ~  RES_IDENTIFIED+PLACES+ 
                            (1 | CCH_CAREHOME_RALF_INCEPTION)+
                            (1 | HEALTH_BOARD), 
                          family=binomial, 
                          data=study_cohort  )
car::vif(collinearity_check_CH_size)

# age, dementia and frailty
collinearity_check_age.dem.frailty<-glmer(POS_TEST ~  AGE+ DEMENTIA_FLAG+FRAILTY_DEF+
                                    (1 | CCH_CAREHOME_RALF_INCEPTION)+
                                    (1 | HEALTH_BOARD), 
                                  family=binomial, 
                                  data=study_cohort  )
car::vif(collinearity_check_age.dem.frailty)

collinearity_check_age.dem.frailty<-glmer(POS_TEST ~  RES_IDENTIFIED_CAT + PLACES_CA
                                            (1 | CCH_CAREHOME_RALF_INCEPTION)+
                                            (1 | HEALTH_BOARD), 
                                          family=binomial, 
                                          data=study_cohort  )
car::vif(collinearity_check_age.dem.frailty)


#############################
#                           #
#   UNIVARIABLE TESTS       #
#                           #
#############################

#basic model 
BASIC_UNIVARIABLE<- glm(POS_TEST ~ HEALTH_BOARD , 
                        family=binomial, 
                        data=study_cohort)
summary(BASIC_UNIVARIABLE)
tab_model(BASIC_UNIVARIABLE)

# MLM regression, univariable 
MLM_UNIVARIABLE <- glmer(POS_TEST ~ COMMUNAL_AREAS_M2_CAT  +   
                         (1 | CCH_CAREHOME_RALF_INCEPTION) + 
                         (1 | HEALTH_BOARD),  
                         family=binomial, 
                         data=study_cohort)
summary(MLM_UNIVARIABLE)
tab_model(MLM_UNIVARIABLE)


##############################
#                            #   
# build the model            #
#                            #
##############################

# intercept only 2 levels
intercept_only_model_2L<-glmer(POS_TEST ~ (1 | CCH_CAREHOME_RALF_INCEPTION),  
                            family=binomial, 
                            data=study_cohort)
summary(intercept_only_model_2L)
tab_model(intercept_only_model_2L)

#intercept only 3 levels
intercept_only_model_3L<-glmer(POS_TEST ~ (1 | CCH_CAREHOME_RALF_INCEPTION)+
                                 (1 | HEALTH_BOARD),  
                               family=binomial, 
                               data=study_cohort)
summary(intercept_only_model_3L)
tab_model(intercept_only_model_3L)

# demontrating the 3L model is a better fit than 2 level
anova(intercept_only_model_2L, intercept_only_model_3L)
anova(add_dem.pct_2L, add_dem.pct_3L)

# add category with prop dementia diagnosis
add_dem.cat_3L<-glmer(POS_TEST ~ DEM_PROP_2+(1 | CCH_CAREHOME_RALF_INCEPTION)+
                        (1 | HEALTH_BOARD),  
                      family=binomial, 
                      data=study_cohort)
summary(add_dem.cat_3L)
tab_model(add_dem.cat_3L)
anova(intercept_only_model_3L, add_dem.cat_3L)

# add category with prop dementia diagnosis +age
add_dem.cat.age_3L<-glmer(POS_TEST ~ DEM_PROP_2+AGE +(1 | CCH_CAREHOME_RALF_INCEPTION)+
                        (1 | HEALTH_BOARD),  
                      family=binomial, 
                      data=study_cohort)
summary(add_dem.cat.age_3L)
tab_model(add_dem.cat.age_3L)
anova(add_dem.cat_3L, add_dem.cat.age_3L)

# add category with prop dementia diagnosis +res identified
add_dem.cat.ResID_int_3L<-glmer(POS_TEST ~ DEM_PROP_2+RES_IDENTIFIED+
                            (1 | CCH_CAREHOME_RALF_INCEPTION)+
                            (1 | HEALTH_BOARD),  
                          family=binomial, 
                          data=study_cohort)
summary(add_dem.cat.ResID_int_3L)
tab_model(add_dem.cat.ResID_int_3L)
anova(add_dem.cat_3L, add_dem.cat.ResID_int_3L)

# add category with prop dementia diagnosis +res identified
add_dem.cat.Dem_Serv_3L<-glmer(POS_TEST ~ DEM_PROP_2+DEMENTIA_SERVICE+
                                  (1 | CCH_CAREHOME_RALF_INCEPTION)+
                                  (1 | HEALTH_BOARD),  
                                family=binomial, 
                                data=study_cohort)
summary(add_dem.cat.Dem_Serv_3L)
tab_model(add_dem.cat.Dem_Serv_3L)
anova(add_dem.cat_3L, add_dem.cat.Dem_Serv_3L)

#create variable for non-specialist service only, and test
study_cohort<-study_cohort %>% 
  mutate(DEM_NON_SPEC_BINARY=case_when(DEMENTIA_SERVICE=="NonSpecialist" ~ "NonSpecialist",
                                       TRUE ~ "Other"))
add_dem.cat.Dem_Serv_nonspec_3L<-glmer(POS_TEST ~ DEM_PROP_2+DEM_NON_SPEC_BINARY+
                                 (1 | CCH_CAREHOME_RALF_INCEPTION)+
                                 (1 | HEALTH_BOARD),  
                               family=binomial, 
                               data=study_cohort)
summary(add_dem.cat.Dem_Serv_nonspec_3L)
tab_model(add_dem.cat.Dem_Serv_nonspec_3L)
anova(add_dem.cat.Dem_Serv_nonspec_3L)






# add category with dementia diagnosis + frailty
add_dem.cat.frailty_3L<-glmer(POS_TEST ~ DEM_PROP_2+FRAILTY_SEVERE+
                              (1 | CCH_CAREHOME_RALF_INCEPTION)+
                            (1 | HEALTH_BOARD),  
                          family=binomial, 
                          data=study_cohort)
summary(add_dem.cat.frailty_3L)
tab_model(add_dem.cat.frailty_3L)
anova(add_dem.cat_3L, add_dem.cat.frailty_3L)

#create a cateogry to separate out 

# add category with dementia diagnosis + dementia service
add_dem.cat.dem.serve_3L<-glmer(POS_TEST ~ DEM_PROP_2+ANY_DEM_SERVICE  +
                                (1 | CCH_CAREHOME_RALF_INCEPTION)+
                                (1 | HEALTH_BOARD),  
                              family=binomial, 
                              data=study_cohort)
summary(add_dem.cat.dem.serve_3L)
tab_model(add_dem.cat.dem.serve_3L)
anova(add_dem.cat_3L, add_dem.cat.dem.serve_3L)

# add category with dementia diagnosis + residents identified
add_dem.cat.age.res_identified_3L<-glmer(POS_TEST ~ DEM_PROP_2+RES_IDENTIFIED+AGE+
                                       (1 | CCH_CAREHOME_RALF_INCEPTION)+
                                (1 | HEALTH_BOARD),  
                              family=binomial, 
                              data=study_cohort)
summary(add_dem.cat.age.res_identified_3L)
tab_model(add_dem.cat.age.res_identified_3L)
anova(add_dem.cat.age_3L, add_dem.cat.age.res_identified_3L)

# try for places and dementia in a 2 level model
add_dem.cat.res_identified_2L<-glmer(POS_TEST ~ DEM_PROP_2+RES_IDENTIFIED_CAT+
                                       (1 | CCH_CAREHOME_RALF_INCEPTION),  
                                     family=binomial, 
                                     data=study_cohort)
summary(add_dem.cat.res_identified_2L)
tab_model(add_dem.cat.res_identified_2L)
anova(add_dem.cat_3L, add_dem.cat.res_identified_2L)



##WITH % dementia diganosis

# add % with dementia diagnosis (continuous)
add_dem.pct_3L<-glmer(POS_TEST ~ DEM_PCT+(1 | CCH_CAREHOME_RALF_INCEPTION)+
                          (1 | HEALTH_BOARD),  
                        family=binomial, 
                        data=study_cohort)
summary(add_dem.pct_3L)
tab_model(add_dem.pct_3L)
anova(intercept_only_model_3L, add_dem.pct_3L)

# add % with dementia diagnosis (continuous) + Residents identified 
add_dem.pct_resID_3L<-glmer(POS_TEST ~ DEM_PCT+
                              RES_IDENTIFIED+(1 | CCH_CAREHOME_RALF_INCEPTION)+
                        (1 | HEALTH_BOARD),  
                      family=binomial, 
                      data=study_cohort)
summary(add_dem.pct_resID_3L)
tab_model(add_dem.pct_resID_3L)
anova(intercept_only_model_3L, add_dem.pct_3L, add_dem.pct_resID_3L)

# add % with dementia diagnosis + age
add_dem.pct_age_3L<-glmer(POS_TEST ~ DEM_PCT+AGE+(1 | CCH_CAREHOME_RALF_INCEPTION)+
                              (1 | HEALTH_BOARD),  
                            family=binomial, 
                            data=study_cohort)
summary(add_dem.pct_age_3L)
tab_model(add_dem.pct_age_3L)
anova(intercept_only_model_3L, add_dem.pct_3L, add_dem.pct_age_3L)


# add % with dementia diagnosis + frailty
add_dem.pct_frailty_3L<-glmer(POS_TEST ~ DEM_PCT+FRAILTY_DEF+(1 | CCH_CAREHOME_RALF_INCEPTION)+
                            (1 | HEALTH_BOARD),  
                          family=binomial, 
                          data=study_cohort)
summary(add_dem.pct_frailty_3L)
tab_model(add_dem.pct_frailty_3L)
anova(intercept_only_model_3L, add_dem.pct_3L, add_dem.pct_frailty_3L)


# add % with dementia diagnosis + gender
add_dem.pct_gender_3L<-glmer(POS_TEST ~ DEM_PCT+GNDR_CD+(1 | CCH_CAREHOME_RALF_INCEPTION)+
                                (1 | HEALTH_BOARD),  
                              family=binomial, 
                              data=study_cohort)
summary(add_dem.pct_gender_3L)
tab_model(add_dem.pct_gender_3L)
anova(intercept_only_model_3L, add_dem.pct_3L, add_dem.pct_gender_3L)



# add % with dementia diagnosis (continuous) + Dementia service 
add_dem.pct_frailty_dem.service_3L<-glmer(POS_TEST ~ DEM_PCT+ DEMENTIA_SERVICE+FRAILTY_DEF+
                     (1 | CCH_CAREHOME_RALF_INCEPTION)+
                       (1 | HEALTH_BOARD),
                   family=binomial, 
                   data=study_cohort)
summary(add_dem.pct_frailty_dem.service_3L)
tab_model(add_dem.pct_frailty_dem.service_3L)
anova(intercept_only_model_3L, add_dem.pct_3L, add_dem.pct_frailty_dem.service_3L)

# add % with dementia diagnosis - continuous + HB + res identified cont
#DOES NOT CONVERGE
add_dem.pct_HB_3L<-glmer(POS_TEST ~ DEM_PCT+ HEALTH_BOARD+
                        (1 | CCH_CAREHOME_RALF_INCEPTION)+
                          (1 | HEALTH_BOARD),
                      family=binomial, 
                      data=study_cohort)
summary(add_dem.pct_HB_3L)
tab_model(add_dem.pct_HB_3L)
anova(intercept_only_model_3L, add_dem.pct_3L, add_dem.pct_HB_3Ls)

#################################
#                               #
#    ADDITIONAL TESTS           #
#                               #
#################################

# sensitivity test - remove those homes with no dementia diagnosis

glimpse(study_cohort)

study_cohort_minus_homes_no_dem<-study_cohort %>% 
  filter(DEM_PROP !=0) 
  
study_cohort_minus_homes_no_dem %>% 
  group_by(POS_TEST, DEMENTIA_FLAG) %>% 
  summarise(n())

#basic 
BASIC_UNIVARIABLE<- glm(POS_TEST ~ FRAILTY_ANY , 
                        family=binomial, 
                        data=study_cohort_minus_homes_no_dem)
summary(BASIC_UNIVARIABLE)
tab_model(BASIC_UNIVARIABLE)

# MLM regression, univariable 
MLM_UNIVARIABLE <- glmer(POS_TEST ~ FRAILTY_ANY  +   
                           (1 | CCH_CAREHOME_RALF_INCEPTION) + 
                           (1 | HEALTH_BOARD),  
                         family=binomial, 
                         data=study_cohort_minus_homes_no_dem)
summary(MLM_UNIVARIABLE)
tab_model(MLM_UNIVARIABLE)

#scatterplot of numbers ascertained v max capacity
study_cohort %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(Max_registered_capacity=mean(PLACES), Residents_identified=mean(RES_IDENTIFIED  )) %>% 
  ggplot(aes(x=Max_registered_capacity, y=Residents_identified ))+
  geom_point()+
  geom_smooth(method = lm)+
  geom_abline()

# calculate correlation coefficient
correlation_places_identified_dataset<-study_cohort %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(Max_registered_capacity=mean(PLACES), Residents_identified=mean(RES_IDENTIFIED  ))

view(correlation_places_identified_dataset)

cor(correlation_places_identified_dataset$Max_registered_capacity, 
    correlation_places_identified_dataset$Residents_identified)
cor.test(correlation_places_identified_dataset$Max_registered_capacity, 
         correlation_places_identified_dataset$Residents_identified)



study_cohort %>% 
  filter(SUSCEPTIBLE_PCT<75) %>% 
  summarise(n(), n_distinct(CCH_CAREHOME_RALF_INCEPTION))


# rescale frailty
glimpse(study_cohort)

study_cohort<-study_cohort %>% 
  mutate(FRAILTY_ANY=case_when(FRAILTY==0 ~ 0,
                               TRUE ~ 1))

study_cohort<-study_cohort %>% 
  mutate(FRAILTY_SEVERE=case_when(FRAILTY==3 ~ 1,
                               TRUE ~ 0))


# any dementia service 
study_cohort<-study_cohort %>% 
  mutate(SPECIALIST_DEM_SERVICE=case_when(DEMENTIA_SERVICE=="Specialist" ~ 1,
                               TRUE ~ 0))

study_cohort<-study_cohort %>% 
  mutate(ANY_DEM_SERVICE=case_when(DEMENTIA_SERVICE=="Specialist" ~ 1,
                                   DEMENTIA_SERVICE=="NonSpecialist" ~ 1,
                                          TRUE ~ 0))


#frailty with proportion
frailty_proportions_by_care_home<-study_cohort %>% 
  group_by(CCH_CAREHOME_RALF_INCEPTION) %>% 
  summarise(total_any_frailty_start_study_period=sum(FRAILTY_ANY), 
            total_residents=n(), 
            proportion_any_frailty=total_any_frailty_start_study_period/total_residents) %>% 
  arrange(proportion_any_frailty)


frailty_proportions_by_care_home %>% 
  mutate(PROP_ANY_FRAILTY_CAT=case_when(proportion_any_frailty>0.5 ~ "More thadn ")

study_cohort %>% 
  mutate(FRAILTY_ANY_PROP=)