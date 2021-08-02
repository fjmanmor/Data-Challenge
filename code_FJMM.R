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
library(cobalt)
library(psych)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(knitr)
library(gtsummary)
library(Rcpp)
library(MatchIt)
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
##########
##########



##########
##########
#Data type/structure &
#some changes (second part)
#Join: ed & pc
#Key field: "patient_id"
data <- inner_join(ed, pc, by = "patient_id")
#View(data)
dim(data)
#There are 19284 rows/observations
##########
##########



##########
##########
#Some descriptive statistics
#In general
summary(data)

##########

#bleedingEvent
summary(as.factor(data$bleedingEvent))
#1 = Event = 3577
#0 = Censored = 15707
#Graph
ggplot(data, aes(x = bleedingEvent)) +
  geom_bar(fill = "#4d94ff")

##########

#duration_in_years
summary(data$duration_in_years)
#min: 0.00000
#median: 0.09589
#mean: 0.35523
#max: 2.00000
#Graph
hist(data$duration_in_years, col = "black")

##########

#treatmentVariable: Drug A & Drug B
#treatmentVariable_ed (or treatmentVariable_pc)
#Both show the same results
summary(data$treatmentVariable_ed)
summary(data$treatmentVariable_pc)
#Patiens with Drug_A: 8518
paste(round(((8518 * 100) / 19284), 1), "%") 
#44.2%
#Patiens with Drug_B: 10766
paste(round(((10766 * 100) / 19284), 1), "%") 
#55.8%
#Difference
10766 - 8518
#2248 patiens with Drug_B
#Patiens with Drug_B > Patiens with Drug_A
#Graph
ggplot(data, aes(x = treatmentVariable_ed)) +
  geom_bar(fill = "#4d94ff")

##########

#sex
summary(data$sex)
#1 (man): 11386
#2 (woman): 7898 
#Graph
ggplot(data, aes(x = sex)) +
  geom_bar(fill = "#4d94ff")

##########

#age
summary(data$age)
#Graph
hist(data$age)

##########

#other_drugs_1 (od1) to other_drugs_8 (od8)
c(od1 = summary(data$other_drugs_1), 
  od2 = summary(data$other_drugs_2),
  od3 = summary(data$other_drugs_3),
  od4 = summary(data$other_drugs_4),
  od5 = summary(data$other_drugs_5),
  od6 = summary(data$other_drugs_6),
  od7 = summary(data$other_drugs_7),
  od8 = summary(data$other_drugs_8)
  )
#Unbalanced data between Yes and No

#diagnosis_1 (d1) to diagnosis_15 (d15)
c(d1 = summary(data$diagnosis_1), 
  d2 = summary(data$diagnosis_2),
  d3 = summary(data$diagnosis_3),
  d4 = summary(data$diagnosis_4),
  d5 = summary(data$diagnosis_5),
  d6 = summary(data$diagnosis_6),
  d7 = summary(data$diagnosis_7),
  d8 = summary(data$diagnosis_8),
  d9 = summary(data$diagnosis_9),
  d10 = summary(data$diagnosis_10),
  d11 = summary(data$diagnosis_11), 
  d12 = summary(data$diagnosis_12),
  d13 = summary(data$diagnosis_13),
  d14 = summary(data$diagnosis_14),
  d15 = summary(data$diagnosis_15)
  )
#Unbalanced data between Yes and No

#lab_1 to lab_8
summary(data$lab_1)
sum(is.na(data$lab_1))

summary(data$lab_2)
sum(is.na(data$lab_2))
#NA´s = 12633

summary(data$lab_3)
sum(is.na(data$lab_3))
#NA´s = 18133

summary(data$lab_3)
sum(is.na(data$lab_3))
#NA´s = 18133

summary(data$lab_4)
sum(is.na(data$lab_4))
#NA´s = 16985

summary(data$lab_5)
sum(is.na(data$lab_5))
#NA´s = 9257

summary(data$lab_6)
sum(is.na(data$lab_6))
#NA´s = 195

summary(data$lab_7)
sum(is.na(data$lab_7))
#NA´s = 3310

summary(data$lab_8)
sum(is.na(data$lab_8))
#NA´s = 5834

#Diag_Score_1 and Diag_Score_2
summary(data$Diag_Score_1)
summary(data$Diag_Score_2)

##########

#Some contingency tables
#bleedingEvent & treatmentVariable
table(data$bleedingEvent, data$treatmentVariable_ed)
#0 and Drug_A = 7014     &     0 and Drug_B = 8693
#1 and Drug_A = 1504     &     1 and Drug_B = 2073
#%
round(prop.table(table(data$bleedingEvent, data$treatmentVariable_ed)) * 100, 2)

#sex & treatmentVariable
table(data$sex, data$treatmentVariable_ed)
#1 and Drug_A = 5040     &     1 and Drug_B = 6346
#2 and Drug_A = 3478     &     2 and Drug_B = 4420
#%
round(prop.table(table(data$sex, data$treatmentVariable_ed)) * 100, 2)

#bleedingEvent, sex & treatmentVariable
ftable(data$bleedingEvent, data$sex, data$treatmentVariable_ed)
#%
round(prop.table(ftable(data$bleedingEvent, data$sex, data$treatmentVariable_ed)) * 100, 2) 

##########

#To avoid confusions
#treatmentVariable_ed = treatmentVariable_pc
#One variable: treatmentVariable
data$treatmentVariable <- data$treatmentVariable_ed
##########
##########



##########
##########
#1st Step: with unbalanced data (ud)
#Multivariate Cox Regression Analysis
#To fit multivariable regression models and identify 
#statistically significant variables
cox_0_ud <- coxph(Surv(data$duration_in_years, data$bleedingEvent) ~ data$treatmentVariable +
                       data$sex +
                       data$age + 
                       data$other_drugs_1 + data$other_drugs_2 + data$other_drugs_3 + data$other_drugs_4 +
                       data$other_drugs_5 + data$other_drugs_6 + data$other_drugs_7 + data$other_drugs_8 + 
                       data$diagnosis_1 + data$diagnosis_2 + data$diagnosis_3 + data$diagnosis_4 + data$diagnosis_5 +
                       data$diagnosis_6 + data$diagnosis_7 + data$diagnosis_8 + data$diagnosis_9 + data$diagnosis_10 + 
                       data$diagnosis_11 + data$diagnosis_12 + data$diagnosis_13 + data$diagnosis_14 + data$diagnosis_15 + 
                       data$lab_1 +
                       data$Diag_Score_1 + data$Diag_Score_2, 
                       data = data)
summary(cox_0_ud)
#Statistically significant at 5%
#age;
#other_drugs_2; other_drugs_3; other_drugs_8;
#diagnosis_1; diagnosis_4; diagnosis_6; diagnosis_8; diagnosis_9; 
#diagnosis_10; diagnosis_11; diagnosis_12; diagnosis_13; diagnosis_14;
#lab_1;
#Diag_Score_1 and Diag_Score_2

#Kaplan-Meier (KM) Method: the survival probability from observed survival time
km_0_ud <- survfit(Surv(data$duration_in_years, data$bleedingEvent) ~ 
                        data$treatmentVariable, data = data)
#Survival curve
x11()
ggsurvplot(km_0_ud, data = data, pval = TRUE, conf.int = FALSE, risk.table = TRUE, risk.table.col = "strata", 
           linetype = "strata", surv.median.line = "hv", ggtheme = theme_survminer(), 
           palette = c("#3385ff", "#80ff80"), xlab = "Years")
x11()
ggsurvplot(km_0_ud, data = data, pval = TRUE, conf.int = FALSE, conf.int.style = "step", 
           ggtheme = theme_survminer(), risk.table = "abs_pct", risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE, surv.median.line = "hv",
           legend.labs =  c("Drug A", "Drug B"), palette = c("#3385ff", "#80ff80"),
           xlab = "Years")
#X-axis: time in years
#Y-axis: probability of surviving or the proportion of people surviving
#Blue Line: Drug A
#Green Line: Drug B
summary(km_0_ud)$table
#Median survival not reached

#Log-Rank test
#Comparing two groups
#H0: No difference between the two survival curves (two drugs)
#H1: Difference between the two survival curves (two drugs)
test_km_0_ud <- survdiff(Surv(data$duration_in_years, data$bleedingEvent) ~ 
                           data$treatmentVariable, data = data)
test_km_0_ud
#p = 0.005
#Do reject the H0
#Difference between the two survival curves (two drugs)
#However, it did not have the edian survival

#Cox  Method with only statistically  significant variables (cox_0_ud)
cox_1_ud <- coxph(Surv(data$duration_in_years, data$bleedingEvent) ~ data$age + 
                    data$other_drugs_2 + data$other_drugs_3 + data$other_drugs_8 + 
                    data$diagnosis_1 + data$diagnosis_4 + data$diagnosis_6 + data$diagnosis_8 + data$diagnosis_9 +
                    data$diagnosis_10 + data$diagnosis_11 + data$diagnosis_12 + data$diagnosis_13 + data$diagnosis_14 + 
                    data$lab_1 + 
                    data$Diag_Score_1 + data$Diag_Score_2, 
                    data = data)
summary(cox_1_ud)
#Exponential
exp_cox_1_ud <- tidy(cox_1_ud, exp = TRUE)
kable(exp_cox_1_ud)
#Statistically significant at 5%:
#age;
#other_drugs_2; other_drugs_3; other_drugs_8;
#diagnosis_1; diagnosis_4; diagnosis_6; diagnosis_8; diagnosis_9; 
#diagnosis_10; diagnosis_11; diagnosis_12; diagnosis_13; diagnosis_14;
#lab_1;
#Diag_Score_1 and Diag_Score_2
##########
##########



##########
##########
#2nd Step
#Propensity Score Matching (PSM)
#Statistically significant at 5%
#age;
#other_drugs_2; other_drugs_3; other_drugs_8;
#diagnosis_1; diagnosis_4; diagnosis_6; diagnosis_8; diagnosis_9; 
#diagnosis_10; diagnosis_11; diagnosis_12; diagnosis_13; diagnosis_14;
#lab_1;
#Diag_Score_1 and Diag_Score_2
data1 <- data[, c("age", "other_drugs_2", "other_drugs_3", "other_drugs_8",
                   "diagnosis_1", "diagnosis_4", "diagnosis_6", "diagnosis_8", "diagnosis_9",
                   "diagnosis_10", "diagnosis_11", "diagnosis_12", "diagnosis_13", "diagnosis_14",
                   "lab_1",
                   "Diag_Score_1", "Diag_Score_2",
                   "duration_in_years",
                   "bleedingEvent",
                   "treatmentVariable")]

match1 <- matchit(data1$bleedingEvent ~ data1$age + 
                    data1$other_drugs_2 + data1$other_drugs_3 + data1$other_drugs_8 +
                    data1$diagnosis_1 + data1$diagnosis_4 + data1$diagnosis_6 + data1$diagnosis_8 + data1$diagnosis_9 +
                    data1$diagnosis_10 + data1$diagnosis_11 + data1$diagnosis_12 + data1$diagnosis_13 + data1$diagnosis_14 +
                    data1$lab_1 + 
                    data1$Diag_Score_1 + data1$Diag_Score_2 +
                    data1$treatmentVariable + data1$duration_in_years,
                    data = data1)
summary(match1)

#Plots
plot(match1, type = "jitter")
plot(match1, type = "hist")
x11()
love.plot(bal.tab(match1, binary = "std", m.threshold = 0.1),
          stat = "mean.diffs", abs = F, shapes = c("triangle filled", "circle filled"))

#Matching data
dataMatch <- match.data(match1, data = data, distance = "prop.score")
dim(dataMatch)
#There are 7154 rows/observations
#With PSM
dataMatch %>%
  group_by(bleedingEvent, treatmentVariable) %>%
  select(age, lab_1) %>%
  summarise_all(mean) %>%
  arrange(treatmentVariable)
#W/O PSM
data %>%
  group_by(bleedingEvent, treatmentVariable) %>%
  select(age, lab_1, duration_in_years) %>%
  summarise_all(mean) %>%
  arrange(treatmentVariable)

round(prop.table(ftable(dataMatch$bleedingEvent, dataMatch$sex, dataMatch$treatmentVariable_ed)) * 100, 2)
##########
##########



##########
##########
#3rd Step: with balanced data (bd)

#Kaplan-Meier (KM) Method: the survival probability from observed survival time
km_1_bd <- survfit(Surv(dataMatch$duration_in_years, dataMatch$bleedingEvent) ~ dataMatch$treatmentVariable,
                   data = dataMatch)
#Survival curve
x11()
ggsurvplot(km_1_bd, data = dataMatch, pval = TRUE, conf.int = FALSE, risk.table = TRUE, risk.table.col = "strata", 
           linetype = "strata", surv.median.line = "hv", ggtheme = theme_survminer(), 
           palette = c("#3385ff", "#80ff80"), xlab = "Years")
x11()
ggsurvplot(km_1_bd, pval = TRUE, conf.int = FALSE, conf.int.style = "step", 
           ggtheme = theme_survminer(), risk.table = "abs_pct", risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE, surv.median.line = "hv",
           legend.labs =  c("Drug A", "Drug B"), palette = c("#3385ff", "#80ff80"),
           xlab = "Years")
#X-axis: time in years
#Y-axis: probability of surviving or the proportion of people surviving
#Blue Line: Drug A
#Green Line: Drug B
#When time = 0: survival probability is 1 (100% of the patients are alive)
#When time = 0.40: probability of survival is 0.50 (or 50%) for treatmentVariable Drug A
#When time = 0.42: probability of survival is 0.50 (or 50%) for treatmentVariable Drug B 
summary(km_1_bd)$table
#The median survival is 0.40 years for Drug A
#The median survival is 0.42 years for Drug B
#Furthermore, this difference is not statistically significant (p = 0.31) between 
#two drugs

#Log-Rank test
#Comparing two groups
#H0: No difference between the two survival curves (two drugs)
#H1: Difference between the two survival curves (two drugs)
test_km_1_bd <- survdiff(Surv(dataMatch$duration_in_years, dataMatch$bleedingEvent) ~ dataMatch$treatmentVariable, 
                         data = dataMatch)
test_km_1_bd
#p = 0.3
#Do not reject the H0
#No difference between the two survival curves (two drugs)

##Cox  Method (Hazard ratios)
cox_1_bd <- coxph(Surv(dataMatch$duration_in_years, dataMatch$bleedingEvent) ~
                    dataMatch$treatmentVariable +
                    dataMatch$sex +
                    dataMatch$age + 
                    dataMatch$other_drugs_1 + dataMatch$other_drugs_2 + dataMatch$other_drugs_3 + dataMatch$other_drugs_4 +
                    dataMatch$other_drugs_5 + dataMatch$other_drugs_6 + dataMatch$other_drugs_7 + dataMatch$other_drugs_8 + 
                    dataMatch$diagnosis_1 + dataMatch$diagnosis_2 + dataMatch$diagnosis_3 + dataMatch$diagnosis_4 + dataMatch$diagnosis_5 +
                    dataMatch$diagnosis_6 + dataMatch$diagnosis_7 + dataMatch$diagnosis_8 + dataMatch$diagnosis_9 + dataMatch$diagnosis_10 + 
                    dataMatch$diagnosis_11 + dataMatch$diagnosis_12 + dataMatch$diagnosis_13 + dataMatch$diagnosis_14 + dataMatch$diagnosis_15 + 
                    dataMatch$lab_1 +
                    dataMatch$Diag_Score_1 + dataMatch$Diag_Score_2, 
                    data = dataMatch)
summary(cox_1_bd)
#Exponential
exp_cox_1_bd <- tidy(cox_1_bd, exp = TRUE)
kable(exp_cox_1_bd)
#Statistically significant at 10%:
#treatmentVariable;
#other_drugs_2;
#diagnosis_1, diagnosis_4 and diagnosis_5, and
#Diag_Score_2
#Statistically significant at 5%:
#other_drugs_7, other_drugs_8;
#diagnosis_2, diagnosis_9, diagnosis_11, diagnosis_12, diagnosis_14;
#lab_1, and
#Diag_Score_1


#Statistically significant at 5%
#age;
#other_drugs_2; other_drugs_3; other_drugs_8;
#diagnosis_1; diagnosis_4; diagnosis_6; diagnosis_8; diagnosis_9; 
#diagnosis_10; diagnosis_11; diagnosis_12; diagnosis_13; diagnosis_14;
#lab_1;
#Diag_Score_1 and Diag_Score_2
cox_2_bd <- coxph(Surv(dataMatch$duration_in_years, dataMatch$bleedingEvent) ~
                    dataMatch$age + 
                    dataMatch$other_drugs_2 + dataMatch$other_drugs_3 +
                    dataMatch$other_drugs_8 + 
                    dataMatch$diagnosis_1 + dataMatch$diagnosis_4 +
                    dataMatch$diagnosis_6 + dataMatch$diagnosis_8 + dataMatch$diagnosis_9 + dataMatch$diagnosis_10 + 
                    dataMatch$diagnosis_11 + dataMatch$diagnosis_12 + dataMatch$diagnosis_13 + dataMatch$diagnosis_14 +
                    dataMatch$lab_1 +
                    dataMatch$Diag_Score_1 + dataMatch$Diag_Score_2, 
                  data = dataMatch)
summary(cox_2_bd)
#Exponential
exp_cox_2_bd <- tidy(cox_2_bd, exp = TRUE)
kable(exp_cox_2_bd)
#Statistically significant at 5%
#other_drugs_2; other_drugs_8;
#diagnosis_9; diagnosis_12; diagnosis_14;
#Diag_Score_1, and Diag_Score_2

#Statistically significant at 10%
#diagnosis_1; diagnosis_4; diagnosis_11;
#lab_1;
##########
##########