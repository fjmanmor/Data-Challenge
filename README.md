# Data-Challenge
Survival Analysis in R



##########
##########
#Holmusk
#Data Challenge
##########
##########
#F. Javier
#Manzano M.
#FJMM
##########
##########



##########
##########
#Install packages
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(knitr)
library(gtsummary)
library(Rcpp)
devtools::install_github("zabore/ezfun")
##########
##########



##########
##########
#Import data
#"Event_duration.csv" data = ed
ed <- read.csv("Event_duration.csv")
#Information about if the event happened to a patient (1) or if patient was 
#censored (0) and the durations for each case from the start of therapy
#View(ed)
#Rename the second and fourth column
names(ed)[2] <- "bleedingEvent"
#Specific event after the start of the treatment: Bleeding
#bleedingEvent: 1=event & 0=censored
names(ed)[4] <- "treatmentVariable_ed"

##########

#"Patient_characteristics.csv" data = pc
pc <- read.csv("Patient_characteristics.csv")
#Information about socio-demographics of patients, diagnosis, lab values, 
#and other existing therapies patients are on
#View(pc)
#Rename the second column
names(pc)[2] <- "treatmentVariable_pc"
##########
##########


##########
##########
#Data type/structure &
#some changes (first part)
#ed
dim(ed)
#There are 19284 rows/observations
str(ed)
#From X To Factor
ed$patient_id <- as.factor(ed$patient_id)
ed$bleedingEvent <- as.numeric(ed$bleedingEvent)
ed$treatmentVariable_ed <- as.factor(ed$treatmentVariable_ed)

##########

dim(pc)
#There are 19284 rows/observations
str(pc)
#From X To Factor
pc$patient_id <- as.factor(pc$patient_id)
pc$treatmentVariable_pc <- as.factor(pc$treatmentVariable_pc)
pc$sex <- as.factor(pc$sex)
pc$other_drugs_1 <- as.factor(pc$other_drugs_1)
pc$other_drugs_2 <- as.factor(pc$other_drugs_2)
pc$other_drugs_3 <- as.factor(pc$other_drugs_3)
pc$other_drugs_4 <- as.factor(pc$other_drugs_4)
pc$other_drugs_5 <- as.factor(pc$other_drugs_5)
pc$other_drugs_6 <- as.factor(pc$other_drugs_6)
pc$other_drugs_7 <- as.factor(pc$other_drugs_7)
pc$other_drugs_8 <- as.factor(pc$other_drugs_8)
pc$diagnosis_1 <- as.factor(pc$diagnosis_1)
pc$diagnosis_2 <- as.factor(pc$diagnosis_2)
pc$diagnosis_3 <- as.factor(pc$diagnosis_3)
pc$diagnosis_4 <- as.factor(pc$diagnosis_4)
pc$diagnosis_5 <- as.factor(pc$diagnosis_5)
pc$diagnosis_6 <- as.factor(pc$diagnosis_6)
pc$diagnosis_7 <- as.factor(pc$diagnosis_7)
pc$diagnosis_8 <- as.factor(pc$diagnosis_8)
pc$diagnosis_9 <- as.factor(pc$diagnosis_9)
pc$diagnosis_10 <- as.factor(pc$diagnosis_10)
pc$diagnosis_11 <- as.factor(pc$diagnosis_11)
pc$diagnosis_12 <- as.factor(pc$diagnosis_12)
pc$diagnosis_13 <- as.factor(pc$diagnosis_13)
pc$diagnosis_14 <- as.factor(pc$diagnosis_14)
pc$diagnosis_15 <- as.factor(pc$diagnosis_15)
pc$Diag_Score_1 <- as.factor(pc$Diag_Score_1)
pc$Diag_Score_2 <- as.factor(pc$Diag_Score_2)
##########
##########


##########
##########
#Data type/structure &
#some changes (second part)
#Join: ed & pc
#Key field: "patient_id"
ed_pc_join <- inner_join(ed, pc, by = "patient_id")
#View(ed_pc_join)
dim(ed_pc_join)
#There are 15868 rows/observations
#Create variable that compares if the records are equal (or different) between "treatmentVariable_ed" 
#and "treatmentVariable_pc": "com"
ed_pc_join$com <- ifelse(ed_pc_join$treatmentVariable_ed == ed_pc_join$treatmentVariable_pc,
                         1,
                         0)
ed_pc_join$com <- as.factor(ed_pc_join$com)
summary(ed_pc_join$com)
#ed_pc_join$com = 1 = 8057 rows/observations (OK)
#ed_pc_join$com = 0 = 7811 rows/observations (possible inconsistency/error)
#Records with (possible) inconsistency
rpi <- ed_pc_join %>% filter(com == 0)
#View(rpi)
dim(rpi)
#There are 7811 rows/observations with (possible) inconsistency/error
#49% of the data with a (possible) inconsistency
#Differences were found between "treatmentVariable_ed" and "treatmentVariable_pc" entries,
#when they should be equal since the patient could not receive both drugs at the same time

##########

#Consequently, it was decided to continue with the records that do not present this type of 
#inconsistency
records <- ed_pc_join %>% filter(com == 1)
#View(records)
dim(records)
#There are 8057 rows/observations
#51% of the data with a (possible) inconsistency
str(records)
##########
##########


##########
##########
#Some descriptive statistics
#In general
summary(records)

##########

#bleedingEvent
summary(records$bleedingEvent)
#1 = Event = 1569
#0 = Censored = 6488
#Graph
ggplot(records, aes(x = bleedingEvent)) +
      geom_bar(fill = "#4d94ff")

##########

#duration_in_years
summary(records$duration_in_years)
#min: 0.00000
#median: 0.09589
#mean: 0.35523
#max: 2.00000
#Graph
hist(records$duration_in_years, col = "blue")
#Create a variable in months with "duration_in_years": "months"
records$yearsInMonths <- (records$duration_in_years * 12)
#Create a variable in days with "duration_in_years": "days"
records$yearsInDays <- records$duration_in_years * 365

##########

#treatmentVariable: Drug A & Drug B
#treatmentVariable_ed (or treatmentVariable_pc)
#Both show the same results
summary(records$treatmentVariable_ed)
summary(records$treatmentVariable_pc)
#Patiens with Drug_A: 3106
paste(round(((3106 * 100) / 8057), 1), "%") 
#38.6%
#Patiens with Drug_B: 4951
paste(round(((4951 * 100) / 8057), 1), "%") 
#61.4%
#Difference
4951 - 3106
#1845 patiens with Drug_B
#Patiens with Drug_B > Patiens with Drug_A
#Graph
ggplot(records, aes(x = treatmentVariable_ed)) +
        geom_bar(fill = "#4d94ff")

##########

#sex
summary(records$sex)
#1 (man): 4737
#2 (woman): 3320 
#Graph
ggplot(records, aes(x = sex)) +
        geom_bar(fill = "#4d94ff")

##########

#age
summary(records$age)
median(records$age)
#Create an age categories (adult & senior): "ageCat"
#1) adult = 22 - 79
#2) senior = 79+
records$ageCat <- case_when(records$age >= 22 & records$age <= 79 ~ "adult",
                            records$age >= 80 ~ "senior")
records$ageCat <- as.factor(records$ageCat)
summary(records$ageCat)
#Graph 1
hist(records$age)
#Graph 2
ggplot(records, aes(x = ageCat)) +
       geom_bar(fill = "#4d94ff")

##########

#Some contingency tables
#bleedingEvent & treatmentVariable
table(records$bleedingEvent, records$treatmentVariable_ed)
#sex & treatmentVariable
table(records$sex, records$treatmentVariable_ed)
#ageCat & treatmentVariable
table(records$ageCat, records$treatmentVariable_ed)

##########

records$treatmentVariable <- records$treatmentVariable_ed

##########
##########


##########
##########
#Survival Analysis (SA)
#SA no. 1 (with all data)
dim(records)
#There are 8057 rows/observations
summary(as.factor(records$bleedingEvent))
#0 = 6488     &     1 = 1569
summary(records$treatmentVariable)
#Drug_A = 3106     &     Drug_B = 4951
summary(records$sex)
#1 (M) = 4737     &     2 (F) = 3320
summary(records$ageCat)
#adult = 4304    &    senior = 3753

#Kaplan-Meier (KM) Method: the survival probability from observed survival time
sa_km_1 <- survfit(Surv(records$yearsInDays, records$bleedingEvent) ~ records$treatmentVariable,
                   data = records)
#Survival curve
x11()
ggsurvplot(sa_km_1, pval = TRUE, conf.int = FALSE, risk.table = TRUE, risk.table.col = "strata", 
           linetype = "strata", surv.median.line = "hv", ggtheme = theme_survminer(), 
           palette = c("#3385ff", "#80ff80"), xlab = "Days")
#
x11()
ggsurvplot(sa_km_1, pval = TRUE, conf.int = FALSE, conf.int.style = "step", 
           ggtheme = theme_survminer(), risk.table = "abs_pct", risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE, surv.median.line = "hv",
           legend.labs =  c("Drug A", "Drug B"), palette = c("#3385ff", "#80ff80"),
           xlab = "Days")
#X-axis: time in days
#Y-axis: probability of surviving or the proportion of people surviving
#Blue Line: Drug A
#Green Line: Drug B
#When time = 0: survival probability is 1 (100% of the patients are alive)
#When time = 700: probability of survival is 0.50 (or 50%) for treatmentVariable Drug B 
#and 0.51-052 (or 51-52%) for treatmentVariable Drug A.
summary(sa_km_1)$table
#The median survival is 702 days for Drug B
#The median survival is unknown for Drug A (¿?)
#Those results are not clear enough
#Furthermore, this difference is not statistically significant (p = 0.56) between 
#two drugs

#Log-Rank test
#Comparing two groups
#H0: No difference between the two survival curves (two drugs)
#H1: Difference between the two survival curves (two drugs)
test_sa_km_1 <- survdiff(Surv(records$yearsInDays, records$bleedingEvent) ~ records$treatmentVariable, 
                          data = records)
test_sa_km_1
#Do not reject the H0
#No difference between the two survival curves (two drugs)

##########

#NA´s in "lab_#2to8"
sum(is.na(records$lab_1))
#0
sum(is.na(records$lab_2))
#5295
sum(is.na(records$lab_3))
#7599
sum(is.na(records$lab_4))
#7123
sum(is.na(records$lab_5))
#3917
sum(is.na(records$lab_6))
#66
sum(is.na(records$lab_7))
#1344
sum(is.na(records$lab_8))
#2479

#Cox  Method (Hazard ratios): to fit univariable and multivariable regression models and 
#survival outcomes
sa_cox_1 <- coxph(Surv(records$yearsInDays, records$bleedingEvent) ~ records$treatmentVariable +
                        records$sex + records$ageCat +
                        records$other_drugs_1 + records$other_drugs_2 + records$other_drugs_3 + 
                        records$other_drugs_4 + records$other_drugs_5 + records$other_drugs_6 + 
                        records$other_drugs_7 + records$other_drugs_8 + 
                        records$diagnosis_1 + records$diagnosis_2 + records$diagnosis_3 + records$diagnosis_4 +
                        records$diagnosis_5 + records$diagnosis_6 + records$diagnosis_7 + records$diagnosis_8 +
                        records$diagnosis_9 + records$diagnosis_10 + records$diagnosis_11 + records$diagnosis_12 +
                        records$diagnosis_13 + records$diagnosis_14 + records$diagnosis_15 + 
                        records$lab_1 +
                        records$Diag_Score_1 + records$Diag_Score_2, 
                        data = records)
sa_cox_1
#Exponential
exp_sa_cox_1 <- tidy(sa_cox_1, exp = TRUE)
kable(exp_sa_cox_1)
#Statistically significant at 5%: None
#Statistically significant at 10%
#records$other_drugs_5
#records$diagnosis_3
#records$diagnosis_4
#records$diagnosis_6
#records$diagnosis_11
#records$lab_1
#records$Diag_Score_2
##########
##########


##########
##########
#Survival Analysis (SA)
#SA no. 2
#Stratified Random Sample: "bleedingEvent"
#bleedingEvent
summary(as.factor(records$bleedingEvent))
#0 = 6488     &     1 = 1569
#Stratified Random Sample: "bleedingEvent"
records_be <- records %>% 
                          group_by(bleedingEvent) %>%
                                                      sample_n(1569)
#View(records_be)
summary(as.factor(records_be$bleedingEvent))
#0 = 1569     &     1 = 1569
#n = 3138
summary(as.factor(records_be$treatmentVariable))
#Drug_A  = 1199        &     Drug_B  = 1939

#Kaplan-Meier (KM) Method
sa_km_2 <- survfit(Surv(records_be$yearsInDays, records_be$bleedingEvent) ~ records_be$treatmentVariable,
                   data = records_be)
#Survival curve
x11()
ggsurvplot(sa_km_2, pval = TRUE, conf.int = FALSE, risk.table = TRUE, risk.table.col = "strata", 
           linetype = "strata", surv.median.line = "hv", ggtheme = theme_survminer(), 
           palette = c("#3385ff", "#80ff80"), xlab = "Days")
#
x11()
ggsurvplot(sa_km_2, pval = TRUE, conf.int = FALSE, conf.int.style = "step", 
           ggtheme = theme_survminer(), risk.table = "abs_pct", risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE, surv.median.line = "hv",
           legend.labs =  c("Drug A", "Drug B"), palette = c("#3385ff", "#80ff80"),
           xlab = "Days")
#X-axis: time in days
#Y-axis: probability of surviving or the proportion of people surviving
#Blue Line: Drug A
#Green Line: Drug B
#When time = 0: survival probability is 1 (100% of the patients are alive)
#When time = 150: probability of survival is 0.50 (or 50%) for treatmentVariable Drug A 
#When time = 143: probability of survival is 0.50 (or 50%) for treatmentVariable Drug B
summary(sa_km_2)$table
#The median survival is 150 days for Drug A
#The median survival is 143 days for Drug B 
#Those results suggest a good survival for Drug A compared to Drug B
#Although, this difference is not statistically significant (p = 0.26) between 
#two drugs

#Log-Rank test
#Comparing two groups
#H0: No difference between the two survival curves (two drugs)
#H1: Difference between the two survival curves (two drugs)
test_sa_km_2 <- survdiff(Surv(records_be$yearsInDays, records_be$bleedingEvent) ~ records_be$treatmentVariable, 
                         data = records_be)
test_sa_km_2
#Do not reject the H0
#No difference between the two survival curves (two drugs)

##########

#Cox  Method (Hazard ratios)
sa_cox_2 <- coxph(Surv(records_be$yearsInDays, records_be$bleedingEvent) ~ records_be$treatmentVariable +
                    records_be$sex + records_be$ageCat +
                    records_be$other_drugs_1 + records_be$other_drugs_2 + records_be$other_drugs_3 + 
                    records_be$other_drugs_4 + records_be$other_drugs_5 + records_be$other_drugs_6 + 
                    records_be$other_drugs_7 + records_be$other_drugs_8 + 
                    records_be$diagnosis_1 + records_be$diagnosis_2 + records_be$diagnosis_3 + records_be$diagnosis_4 +
                    records_be$diagnosis_5 + records_be$diagnosis_6 + records_be$diagnosis_7 + records_be$diagnosis_8 +
                    records_be$diagnosis_9 + records_be$diagnosis_10 + records_be$diagnosis_11 + records_be$diagnosis_12 +
                    records_be$diagnosis_13 + records_be$diagnosis_14 + records_be$diagnosis_15 + 
                    records_be$lab_1 +
                    records_be$Diag_Score_1 + records_be$Diag_Score_2, 
                    data = records_be)
sa_cox_2
#Exponential
exp_sa_cox_2 <- tidy(sa_cox_2, exp = TRUE)
kable(exp_sa_cox_2)
#Statistically significant at 5%:
#records_be$other_drugs_1
##########
##########


##########
##########
#Survival Analysis (SA)
#SA no. 3
#Stratified Random Sample: "treatmentVariable"
#treatmentVariable
summary(records$treatmentVariable)
#Drug_A = 3106     &     Drug_B = 4951
#Stratified Random Sample: "treatmentVariable" & "bleedingEvent"
records_tv <- records %>% 
                          group_by(treatmentVariable, bleedingEvent) %>%
                                                                        sample_n(1553, replace = TRUE)
#View(records_tv)
summary(records_tv$treatmentVariable)
#Drug_A = 3106     &     Drug_B = 3106
#n = 6212
summary(as.factor(records_tv$bleedingEvent))
#0  = 3106        &     1  = 3106

#Kaplan-Meier (KM) Method
sa_km_3 <- survfit(Surv(records_tv$yearsInDays, records_tv$bleedingEvent) ~ records_tv$treatmentVariable,
                   data = records_tv)
#Survival curve
x11()
ggsurvplot(sa_km_3, pval = TRUE, conf.int = FALSE, risk.table = TRUE, risk.table.col = "strata", 
           linetype = "strata", surv.median.line = "hv", ggtheme = theme_survminer(), 
           palette = c("#3385ff", "#80ff80"), xlab = "Days")
#
x11()
ggsurvplot(sa_km_3, pval = TRUE, conf.int = FALSE, conf.int.style = "step", 
           ggtheme = theme_survminer(), risk.table = "abs_pct", risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE, surv.median.line = "hv",
           legend.labs =  c("Drug A", "Drug B"), palette = c("#3385ff", "#80ff80"),
           xlab = "Days")
#X-axis: time in days
#Y-axis: probability of surviving or the proportion of people surviving
#Blue Line: Drug A
#Green Line: Drug B
#When time = 0: survival probability is 1 (100% of the patients are alive)
#When time = 152: probability of survival is 0.50 (or 50%) for treatmentVariable Drug A 
#When time = 141: probability of survival is 0.50 (or 50%) for treatmentVariable Drug B
summary(sa_km_3)$table
#The median survival is 152 days for Drug A
#The median survival is 141 days for Drug B 
#Those results suggest a good survival for Drug A compared to Drug B
#Although, this difference is not statistically significant (p = 0.52) between 
#two drugs

#Log-Rank test
#Comparing two groups
#H0: No difference between the two survival curves (two drugs)
#H1: Difference between the two survival curves (two drugs)
test_sa_km_3 <- survdiff(Surv(records_tv$yearsInDays, records_tv$bleedingEvent) ~ records_tv$treatmentVariable, 
                         data = records_tv)
test_sa_km_3
#Do not reject the H0
#No difference between the two survival curves (two drugs)

##########

#Cox  Method (Hazard ratios)
sa_cox_3 <- coxph(Surv(records_tv$yearsInDays, records_tv$bleedingEvent) ~ records_tv$treatmentVariable +
                    records_tv$sex + records_tv$ageCat +
                    records_tv$other_drugs_1 + records_tv$other_drugs_2 + records_tv$other_drugs_3 + 
                    records_tv$other_drugs_4 + records_tv$other_drugs_5 + records_tv$other_drugs_6 + 
                    records_tv$other_drugs_7 + records_tv$other_drugs_8 + 
                    records_tv$diagnosis_1 + records_tv$diagnosis_2 + records_tv$diagnosis_3 + records_tv$diagnosis_4 +
                    records_tv$diagnosis_5 + records_tv$diagnosis_6 + records_tv$diagnosis_7 + records_tv$diagnosis_8 +
                    records_tv$diagnosis_9 + records_tv$diagnosis_10 + records_tv$diagnosis_11 + records_tv$diagnosis_12 +
                    records_tv$diagnosis_13 + records_tv$diagnosis_14 + records_tv$diagnosis_15 + 
                    records_tv$lab_1 +
                    records_tv$Diag_Score_1 + records_tv$Diag_Score_2, 
                    data = records_tv)
sa_cox_3
#Exponential
exp_sa_cox_3 <- tidy(sa_cox_3, exp = TRUE)
kable(exp_sa_cox_3)
#Statistically significant at 5%:
#records_be$other_drugs_3
#records_be$other_drugs_8
#records_tv$diagnosis_4
#records_tv$diagnosis_6
#records_tv$diagnosis_8
#records_tv$diagnosis_13
#records_tv$diagnosis_14
##########
##########


##########
##########
#Survival Analysis (SA)
#SA no. 4
#Stratified Random Sample: "sex"
#sex
summary(records$sex)
#1 (M) = 4737     &     2 (F) = 3320
#Stratified Random Sample: "sex", "treatmentVarible" & "bleedingEvent"
records_sex <- records %>% 
                          group_by(sex, treatmentVariable, bleedingEvent) %>%
                                                                            sample_n(830, replace = TRUE)
#View(records_sex)
summary(records_sex$sex)
#1 (M) = 3320     &     2 (F) = 3320
#n = 6640
summary(records_sex$treatmentVariable)
summary(as.factor(records_sex$bleedingEvent))

#Kaplan-Meier (KM) Method
sa_km_4 <- survfit(Surv(records_sex$yearsInDays, records_sex$bleedingEvent) ~ records_sex$treatmentVariable,
                   data = records_sex)
#Survival curve
x11()
ggsurvplot(sa_km_4, pval = TRUE, conf.int = FALSE, risk.table = TRUE, risk.table.col = "strata", 
           linetype = "strata", surv.median.line = "hv", ggtheme = theme_survminer(), 
           palette = c("#3385ff", "#80ff80"), xlab = "Days")
#
x11()
ggsurvplot(sa_km_4, pval = TRUE, conf.int = FALSE, conf.int.style = "step", 
           ggtheme = theme_survminer(), risk.table = "abs_pct", risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE, surv.median.line = "hv",
           legend.labs =  c("Drug A", "Drug B"), palette = c("#3385ff", "#80ff80"),
           xlab = "Days")
#X-axis: time in days
#Y-axis: probability of surviving or the proportion of people surviving
#Blue Line: Drug A
#Green Line: Drug B
#When time = 0: survival probability is 1 (100% of the patients are alive)
#When time = 151: probability of survival is 0.50 (or 50%) for treatmentVariable Drug A 
#When time = 158: probability of survival is 0.50 (or 50%) for treatmentVariable Drug B
summary(sa_km_4)$table
#The median survival is 151 days for Drug A
#The median survival is 158 days for Drug B
#Those results suggest a good survival for Drug B compared to Drug A
#Although, this difference is not statistically significant (p = 0.19) between 
#two drugs

#Log-Rank test
#Comparing two groups
#H0: No difference between the two survival curves (two drugs)
#H1: Difference between the two survival curves (two drugs)
test_sa_km_4 <- survdiff(Surv(records_sex$yearsInDays, records_sex$bleedingEvent) ~ records_sex$treatmentVariable, 
                         data = records_sex)
test_sa_km_4
#Do not reject the H0
#No difference between the two survival curves (two drugs)

##########

#Cox  Method (Hazard ratios)
sa_cox_4 <- coxph(Surv(records_sex$yearsInDays, records_sex$bleedingEvent) ~ records_sex$treatmentVariable +
                    records_sex$sex + records_sex$ageCat +
                    records_sex$other_drugs_1 + records_sex$other_drugs_2 + records_sex$other_drugs_3 + 
                    records_sex$other_drugs_4 + records_sex$other_drugs_5 + records_sex$other_drugs_6 + 
                    records_sex$other_drugs_7 + records_sex$other_drugs_8 + 
                    records_sex$diagnosis_1 + records_sex$diagnosis_2 + records_sex$diagnosis_3 + records_sex$diagnosis_4 +
                    records_sex$diagnosis_5 + records_sex$diagnosis_6 + records_sex$diagnosis_7 + records_sex$diagnosis_8 +
                    records_sex$diagnosis_9 + records_sex$diagnosis_10 + records_sex$diagnosis_11 + records_sex$diagnosis_12 +
                    records_sex$diagnosis_13 + records_sex$diagnosis_14 + records_sex$diagnosis_15 + 
                    records_sex$lab_1 +
                    records_sex$Diag_Score_1 + records_sex$Diag_Score_2, 
                  data = records_sex)
sa_cox_4
#Exponential
exp_sa_cox_4 <- tidy(sa_cox_4, exp = TRUE)
kable(exp_sa_cox_4)
#Statistically significant at 5%:
#records_sex$treatmentVariableDrug_B
#records_be$other_drugs_3
#records_sex$other_drugs_5
#records_sex$other_drugs_8
#records_tv$diagnosis_3
#records_tv$diagnosis_4
#records_tv$diagnosis_9
#records_tv$diagnosis_14
#records_sex$Diag_Score_2
##########
##########


##########
##########
#Survival Analysis (SA)
#SA no. 5
#Stratified Random Sample: "ageCat"
#ageCat
summary(records$ageCat)
#adult = 4304    &     senior = 3753
#Stratified Random Sample: "ageCat", "sex", "treatmentVariable" & "bleedingEvent"
records_ageCat <- records %>% 
                              group_by(ageCat, sex, treatmentVariable, bleedingEvent) %>%
                                                                                          sample_n(469, replace = TRUE)


#View(records_ageCat)
summary(records_ageCat$ageCat)
#adult = 3752    &     senior = 3752
#n = 7504

#Kaplan-Meier (KM) Method
sa_km_5 <- survfit(Surv(records_ageCat$yearsInDays, records_ageCat$bleedingEvent) ~ records_ageCat$treatmentVariable,
                   data = records_ageCat)
#Survival curve
x11()
ggsurvplot(sa_km_5, pval = TRUE, conf.int = FALSE, risk.table = TRUE, risk.table.col = "strata", 
           linetype = "strata", surv.median.line = "hv", ggtheme = theme_survminer(), 
           palette = c("#3385ff", "#80ff80"), xlab = "Days")
#
x11()
ggsurvplot(sa_km_5, pval = TRUE, conf.int = FALSE, conf.int.style = "step", 
           ggtheme = theme_survminer(), risk.table = "abs_pct", risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE, surv.median.line = "hv",
           legend.labs =  c("Drug A", "Drug B"), palette = c("#3385ff", "#80ff80"),
           xlab = "Days")
#X-axis: time in days
#Y-axis: probability of surviving or the proportion of people surviving
#Blue Line: Drug A
#Green Line: Drug B
#When time = 0: survival probability is 1 (100% of the patients are alive)
#When time = 145: probability of survival is 0.50 (or 50%) for treatmentVariable Drug A 
#When time = 150: probability of survival is 0.50 (or 50%) for treatmentVariable Drug B
summary(sa_km_5)$table
#The median survival is 145 days for Drug A
#The median survival is 150 days for Drug B 
#Those results suggest a good survival for Drug B compared to Drug A
#Although, this difference is not statistically significant (p = 0.14) between 
#two drugs

#Log-Rank test
#Comparing two groups
#H0: No difference between the two survival curves (two drugs)
#H1: Difference between the two survival curves (two drugs)
test_sa_km_5 <- survdiff(Surv(records_ageCat$yearsInDays, records_ageCat$bleedingEvent) ~ records_ageCat$treatmentVariable, 
                         data = records_ageCat)
test_sa_km_5
#Do not reject the H0
#No difference between the two survival curves (two drugs)

##########

#Cox  Method (Hazard ratios)
sa_cox_5 <- coxph(Surv(records_ageCat$yearsInDays, records_ageCat$bleedingEvent) ~ records_ageCat$treatmentVariable +
                    records_ageCat$sex + records_ageCat$ageCat +
                    records_ageCat$other_drugs_1 + records_ageCat$other_drugs_2 + records_ageCat$other_drugs_3 + 
                    records_ageCat$other_drugs_4 + records_ageCat$other_drugs_5 + records_ageCat$other_drugs_6 + 
                    records_ageCat$other_drugs_7 + records_ageCat$other_drugs_8 + 
                    records_ageCat$diagnosis_1 + records_ageCat$diagnosis_2 + records_ageCat$diagnosis_3 + records_ageCat$diagnosis_4 +
                    records_ageCat$diagnosis_5 + records_ageCat$diagnosis_6 + records_ageCat$diagnosis_7 + records_ageCat$diagnosis_8 +
                    records_ageCat$diagnosis_9 + records_ageCat$diagnosis_10 + records_ageCat$diagnosis_11 + records_ageCat$diagnosis_12 +
                    records_ageCat$diagnosis_13 + records_ageCat$diagnosis_14 + records_ageCat$diagnosis_15 + 
                    records_ageCat$lab_1 +
                    records_ageCat$Diag_Score_1 + records_ageCat$Diag_Score_2, 
                    data = records_ageCat)
sa_cox_5
#Exponential
exp_sa_cox_5 <- tidy(sa_cox_5, exp = TRUE)
kable(exp_sa_cox_5)
#Statistically significant at 5%:
#records_sex$treatmentVariable
#records_sex$other_drugs_5
#records_sex$other_drugs_8
#records_tv$diagnosis_4
##########
##########


##########

#Difference between this n (7504) and n original (8506)
8057 - 7504
#553 rows/observations

##########
##########
