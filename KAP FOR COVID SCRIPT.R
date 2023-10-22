############## KAP for COVID SCRIPT #################
############# Author: Prabhjot Juttla

#### Import data set #######
sourceDir(RSCRIPTS)
install.packages("tidyverse")
library(tidyverse)
install.packages("janitor")
library(janitor)
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)


setwd("C:/Users/Prabhjot/OneDrive/Documents/Magoma's work/KAP for COVID/kap final I am serious")
kap_for_covid <- import("KAP for Covid Results 1.xlsx")


kap_for_covid <- KAP_for_Covid_Results_1


# Clean column names  -----------------------------------------------------


#### Clean column names 
kap_for_covid_clean <- kap_for_covid %>%
  rename(timestamp = Timestamp,
         new_timestamp = `Timestamp new`,
    work_place = `Where do you work?`,
         sex = Sex,
         age = `Age in completed years`,
         cadre = Cadre,
         education = `Highest level of education`,
         information_sources = `Which are your sources of information on COVID-19? (tick all that apply)`,
         main_symptoms = `The main clinical symptoms of COVID-19 are (tick all that apply)`,
         knowledge_q1 = `There is currently no cure for COVID19, but early symptomatic and supportive treatment can help most patients recover from the infection.`,
         knowledge_q2 = `Most persons with COVID-19 will develop severe cases.`,
         knowledge_q3 = `Eating or being in contact with wild animals would result in the COVID-19 virus infection`,
         knowledge_q4 = `Persons with COVID-19 cannot transmit the virus to others when a fever is not present`,
         knowledge_q5 = `The COVID-19 virus spreads via respiratory droplets of infected individuals`,
         knowledge_q6 = `Wearing medical masks can prevent one from acquiring infection by the COVID-19 virus`,
         knowledge_q7 = `It is not necessary for children and young adults to take measures to prevent the infection by the COVID-19 virus`,
         knowledge_q8 = `Isolation and treatment of people who are infected with the COVID-19 virus are effective ways to reduce the spread of the virus`,
         knowledge_q9 = `In Kiambu County, the main facility for isolation of COVID-19 patients is:`,
         knowledge_q10 = `When at work, if I come across a patient with a high index of suspicion for COVID-19, what should i do? (tick all that apply)`,
         knowledge_q11 = `Data for a suspected COVID-19 patient is entered in: (tick all that apply)`,
         att_q1 = `Black race is protective towards COVID-19 disease`,
         att_q2 = `Wearing a well fitting mask is effective in preventing COVID-19`,
         att_q3 = `Using normal soap for hand washing can prevent you from getting COVID-19.`,
         att_q4 = `When a patient has signs and symptoms of COVID-19, i can confidently participate in the management of the patient`,
         att_q5 = `In my everyday work, i feel confident that i am safe and protected from contracting COVID-19`,
         att_q6 = `I am well trained on the use of PPE to protect me from contracting COVID-19`,
         att_q7 = `i am confident about the vaccine production process for the COVID-19 vaccine.`,
         att_q8 = `When the vaccine comes, i will take the vaccine.`,
         att_q9 = `Kenya is in a good position to contain COVID-19.`,
         att_q10 = `Kiambu county is in a good position to contain COVID-19`,
         att_q11 = `At the beginning of the COVID-19 pandemic, Kenya handled the pandemic well.`,
         att_q12 = `At the beginning of the COVID-19 pandemic, Kiambu County handled the pandemic well.`,
         att_q13 = `Currently, Kenya is handling the pandemic well.`,
         att_q14 = `Currently, Kiambu County is handling the pandemic well`,
         practise_q1 =`In the last 1 week, I have worn a mask when in contact with patients.`,
         practise_q2 =`In the last 1 week, I have worn PPE when in contact with patients`,
         practise_q3 =`In the last 1 week, I have refrained from shaking hands`,
         practise_q4 =`In the last 1 week, I have washed my hands before and after handling each patient`,
         practise_q5 =`In the last 1 week, I have avoided patients with signs and symptoms suggestive of COVID-19`,
         practise_q6 =`In the last 1 week, I have taken time to sensitize patients and their families on COVID-19`,
         att_q15 = `From my interaction with patients, I believe that the Kiambu county community is well sensitised on COVID-19`)


#### Select only columns with the data required
### Remove columns with qualitative data 
cleaned_kap_for_covid <- subset(kap_for_covid_clean, select = c(new_timestamp,
                                                                work_place,
                                                                sex,
                                                                age,
                                                                cadre,
                                                                education,
                                                                information_sources,
                                                                main_symptoms,
                                                                knowledge_q1,
                                                                knowledge_q2,
                                                                knowledge_q3,
                                                                knowledge_q4,
                                                                knowledge_q5,
                                                                knowledge_q6,
                                                                knowledge_q7,
                                                                knowledge_q8,
                                                                knowledge_q9,
                                                                knowledge_q10,
                                                                knowledge_q11,
                                                                att_q1,
                                                                att_q2,
                                                                att_q3,
                                                                att_q4,
                                                                att_q5,
                                                                att_q6,
                                                                att_q7,
                                                                att_q8,
                                                                att_q9,
                                                                att_q10,
                                                                att_q11,
                                                                att_q12,
                                                                att_q13,
                                                                att_q14,
                                                                practise_q1,
                                                                practise_q2,
                                                                practise_q3,
                                                                practise_q4,
                                                                practise_q5,
                                                                practise_q6,
                                                                att_q15))
#### Replace all the zero values (0) with NA
cleaned_kap_for_covid[cleaned_kap_for_covid == 0] <- NA

cleaned_kap_for_covid$work_place[cleaned_kap_for_covid$work_place == "Private Sector"] <- NA

str(cleaned_kap_for_covid)


########Proportions 

# Place of work proportions -----------------------------------------------


clean <- cleaned_kap_for_covid

clean <- clean %>% 
  rename(knowledge_q12 = main_symptoms)

clean$work_place <- as.factor(clean$work_place)

clean %>%               
  tabyl(work_place) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# sex proportions ---------------------------------------------------------
clean$sex <- as.factor(clean$sex)

clean %>%               
  tabyl(sex) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

# Age proportions ---------------------------------------------------------
clean$age <- as.factor(clean$age)

clean %>%               
  tabyl(age) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# HIghest level of education proportion -----------------------------------

clean$education <- gsub("Other",
                        "other", clean$education)
clean$education <- gsub("Masters",
                        "masters", clean$education)
clean$education <- gsub("PhD",
                        "phd", clean$education)
clean$education <- gsub("Diploma",
                        "diploma", clean$education)

# To clean the education for bachelors ----------------------------------
###Replace brackets and slashes with nothing 

clean$education <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "",
                        clean$education, perl=TRUE)
clean$education <- gsub("Bachelors",
                        "bachelors", clean$education)

clean$education <- as.factor(clean$education)

clean %>%               
  tabyl(education) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

# Cadre proportions -------------------------------------------------------
###Cadres must be first subdivided into further categories. 
## 1. Patient facing 
## 2. Non-patient facing 
## 3. Public health staff 

##Health facility staff who interact with patients directly (Patient Facing)
####Medical Officers /Consultants, Nurses, Clinical Officers, 
##Dental officers / Consultants
##Dental Technologists/ Community Oral Health Officers
##Pharmacists / Consultants
##Pharmaceutical staff (Technologist/Technician)
##Laboratory staff (Technologist/Technician)
##Orthopedic Technologist/Technician
##Nutritionists, Radiographers. Physiotherapists / Occupational Therapists
##Mortuary Attendants

####Health facility staff who do not interact with patients directly (Not-patient facing)
##Health administrative officers and staff
##Health supportive staff (secretaries, accountants, procurement etc)
##Medical Engineering Technologist/Technicians
##Health Records & Information Officers
##Medical Social Worker
##Ambulance Drivers
##HTS (HIV testing services)


######Public Health Staff
##Community health volunteers
##Health Promotion Officers
##Public Health Officers/Community health officers

#### RENAME THE CADRES INTO A NEW COLUMN
### To clean the names thus far 
clean2 <- clean

clean2$cadre <- gsub("Health Administrative officers and staff",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("Health Records and Information Officers and staff",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("Community Health Volunteers",
                          "public health", clean2$cadre)

clean2$cadre <- gsub("Medical Social Workers",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("Ambulance Drivers",
                          "nonpatient facing", clean2$cadre)


clean2$cadre <- gsub("Public Health Officers/Community Health Officers and staff",
                          "public health", clean2$cadre)

clean2$cadre <- gsub("Medical Engineering technologists/technicians",
                          "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("Health Promotion Officers",
                          "public health", clean2$cadre)



#####This is where is gets messed up

# To clean the stubborn names for cadres ----------------------------------
###Replace brackets and slashes with nothing 

clean2$cadre <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "",
                     clean2$cadre, perl=TRUE)

clean2$cadre <- gsub("Health supportive staff",
                     "nonpatient facing", clean2$cadre)

clean2$cadre <- gsub("HTS staff",
                          "nonpatient facing", clean2$cadre)


clean2$cadre <- gsub("Community Health Assistants/Community Health Extension Workers",
                          "public health", clean2$cadre)



# Patient facing ----------------------------------------------------------
clean2$cadre <- gsub("Mortuary Attendants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Mortuary attendants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Physiotherapists/Occupational Therapists",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Laboratory staff",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Pharmaceutical Staff",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Pharmaceutical Staff",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Medical Officers/Consultants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Nurses",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Clinical Officers",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Dental officers/Consultants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Dental Officers/Consultants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Dental Technologists/Community Oral Health Officers",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Pharmacists/Consultants",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Nutritionists",
                     "patient facing", clean2$cadre)

clean2$cadre <- gsub("Radiographers",
                     "patient facing", clean2$cadre)

clean2$cadre <- as.factor(clean2$cadre)
clean2 %>%               
  tabyl(cadre) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean <- clean2

##Case investigation form
#medicssss_suspected <- clean2 %>%
#  filter(clean2$cadre == "patient facing")

#medicssss_suspected$knowledge_q11 <- gsub("Case investigation form", "CIF",
#                                          medicssss_suspected$knowledge_q11)

#medicssss_suspected$cif <- str_count(medicssss_suspected$knowledge_q11,
 #                         "CIF")
#medicssss_suspected %>%               
#  tabyl(cif) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#medicssss_suspected$knowledge_q11 <- gsub("Patient notes", "patient notes",
#                                          medicssss_suspected$knowledge_q11)

#medicssss_suspected$pn <- str_count(medicssss_suspected$knowledge_q11,
         #                            "patient notes")
#medicssss_suspected %>%               
#  tabyl(pn) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#medicssss_suspected$register <- str_count(medicssss_suspected$knowledge_q11,
#                                    "Clinical register")
#medicssss_suspected %>%               
#  tabyl(register) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#medicssss_suspected$definition <- str_count(medicssss_suspected$knowledge_q11,
#                                          "Case definition form")
#medicssss_suspected %>%               
#  tabyl(definition) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#medicssss_suspected$idk <- str_count(medicssss_suspected$knowledge_q11,
#                                           "I dont know")
#medicssss_suspected %>%               
#  tabyl(idk) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#clean <- clean2


# To check knowledge q 11 acc to only medical cadres.. --------

suspected_covid <- clean

suspected_covid %>%               
  tabyl(knowledge_q11) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

# Sources of information --------------------------------------------------
clean$information_sources <- gsub("Official international health organisation sites and media e.g WHO, CDC",
                     "international health organisation", clean$information_sources)
clean$information_sources <- gsub("News Media e.g TV, radios, magazines, newspapers",
                                  "news media", clean$information_sources)
clean$information_sources <- gsub("Social Media e.g Whatsapp, Facebook, Twitter, Instagram",
                                  "social media", clean$information_sources)
clean$information_sources <- gsub("Official government sites and media e.g Ministry of health circulars",
                                  "government sites", clean$information_sources)
clean$information_sources <- gsub("Medical Journals and other research sites",
                                  "journals", clean$information_sources)
clean$information_sources <- gsub("Continuous medical education fora",
                                  "fora", clean$information_sources)

#clean <- clean[!is.na(clean$information_sources), ]

#clean %>% 
#  str_split(clean$information_sources,
#            pattern = ",")
 
#####
clean %>%               
  tabyl(information_sources) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

library(stringr)

clean$international <- str_count(clean$information_sources,
                                 "international health organisation")
clean$news <- str_count(clean$information_sources,
                                 "news media")
clean$social <- str_count(clean$information_sources,
                        "social media")
clean$govt <- str_count(clean$information_sources,
                        "government sites")
clean$journals <- str_count(clean$information_sources,
                        "journals")
clean$fora <- str_count(clean$information_sources,
                            "fora")

clean$international <- as.numeric(clean$international)
clean$news <- as.numeric(clean$news)
clean$social <- as.numeric(clean$social)
clean$govt <- as.numeric(clean$govt)
clean$journals <- as.numeric(clean$journals)
clean$fora <- as.numeric(clean$fora)

clean %>%               
  tabyl(international) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(news) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(social) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(govt) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(fora) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()
clean %>%               
  tabyl(journals) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

###Total responses
349+254+223+192+95+80

(349/1193)*100
(254/1193)*100
(223/1193)*100
(192/1193)*100
(95/1193)*100
(80/1193)*100

#### Filter cadres and demographic data 
## using dataframe 'clean'


# Public health -----------------------------------------------------------

public <- clean2 %>%
  filter(clean2$cadre == "public health")

public %>%               
  tabyl(sex) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(age) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(work_place) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(education) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(international) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(news) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(social) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(govt) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(fora) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(journals) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public$total_sources <- public$international + public$news + public$social + public$govt + public$fora + public$journals  

public %>%               
  tabyl(total_sources) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

public %>%               
  tabyl(knowledge_q9) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# Patient facers   ----------------------------------------------------------

patient_facers <- clean2 %>%
  filter(clean2$cadre == "patient facing")

patient_facers %>%               
  tabyl(sex) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(age) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(work_place) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(education) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(international) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(news) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(social) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(govt) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(fora) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(journals) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers$total_sources <- patient_facers$international + patient_facers$news + patient_facers$social + patient_facers$govt + patient_facers$fora + patient_facers$journals  

patient_facers %>%               
  tabyl(total_sources) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

patient_facers %>%               
  tabyl(knowledge_q9) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

# Non-patient facers----------------------------------------
nonpatient_facers <- clean2 %>%
  filter(clean2$cadre == "nonpatient facing")

nonpatient_facers %>%               
  tabyl(sex) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(age) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(work_place) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(education) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(international) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(news) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(social) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(govt) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(fora) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(journals) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers$total_sources <- nonpatient_facers$international + nonpatient_facers$news + nonpatient_facers$social + nonpatient_facers$govt + nonpatient_facers$fora + nonpatient_facers$journals  

nonpatient_facers %>%               
  tabyl(total_sources) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

nonpatient_facers %>%               
  tabyl(knowledge_q9) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()



# Online vs. hard copy ----------------------------------------------------


online <- clean %>%
  filter(clean$new_timestamp == "online")

hardcopy <- clean %>%
  filter(clean$new_timestamp == "hardcopy")


# online ------------------------------------------------------------------

#online %>%               
#  tabyl(sex) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#online %>%               
#  tabyl(age) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#online %>%               
#  tabyl(work_place) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#online %>%               
#  tabyl(education) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#online %>%               
#  tabyl(cadre) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

# hardcopy ----------------------------------------------------------------

#hardcopy %>%               
#  tabyl(sex) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#hardcopy %>%               
#  tabyl(age) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#hardcopy %>%               
#  tabyl(work_place) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#hardcopy %>%               
#  tabyl(education) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#hardcopy %>%               
#  tabyl(cadre) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

# Knowledge questions -----------------------------------------------------

clean$knowledge_q1 <- as.factor(clean$knowledge_q1)

clean %>%               
  tabyl(knowledge_q1) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q2 <- as.factor(clean$knowledge_q2)

clean %>%               
  tabyl(knowledge_q2) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q3 <- as.factor(clean$knowledge_q3)
clean %>%               
  tabyl(knowledge_q3) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q4 <- as.factor(clean$knowledge_q4)
clean %>%               
  tabyl(knowledge_q4) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q5 <- as.factor(clean$knowledge_q5)
clean %>%               
  tabyl(knowledge_q5) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q6 <- as.factor(clean$knowledge_q6)
clean %>%               
  tabyl(knowledge_q6) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q7 <- as.factor(clean$knowledge_q7)
clean %>%               
  tabyl(knowledge_q7) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q8 <- as.factor(clean$knowledge_q8)
clean %>%               
  tabyl(knowledge_q8) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$knowledge_q9 <- as.factor(clean$knowledge_q9)
clean %>%               
  tabyl(knowledge_q9) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

##### Split main symtpoms into individual strings 
str_split(string = cleaned_kap_for_covid$main_symptoms,
          pattern = ",")

##clean$main_symptoms <- as.factor(clean$main_symptoms)
##clean %>%               
#  tabyl(main_symptoms) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()
 
   ## did not work
clean$knowledge_q10 <- as.factor(clean$knowledge_q10)
clean %>%               
    tabyl(knowledge_q10) %>%       # tabulate counts and proportions by work place category
    adorn_pct_formatting()

clean$knowledge_q11 <- as.factor(clean$knowledge_q11)
clean %>%               
  tabyl(knowledge_q11) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# Attitude questions ------------------------------------------------------

###Remove "i dont see patients" 

clean$att_q1[clean$att_q1 == "i dont see patients"] <- NA
clean$att_q6[clean$att_q6 == "i dont see patients"] <- NA
clean$att_q6[clean$att_q6 == "i dont know"] <- NA
clean$att_q4[clean$att_q4 == "i dont know"] <- NA
clean$att_q4[clean$att_q4 == "i dont see patients"] <- NA
clean$att_q15[clean$att_q15 == "i dont see patients"] <- NA

##### For question 8, collapse "Agree" into "YES"
#### collapse "i dont know", "not sure" into "MAYBE"
#### collapse "Diagree" into "No"
#### to have three outputs: YES, NO, MAYBE 

clean$att_q8 <- gsub("Agree",
                          "Yes",
                     clean$att_q8)
clean$att_q8 <- gsub("I dont know",
                     "Maybe",
                     clean$att_q8)

clean$att_q8 <- gsub("Not sure",
                     "Maybe",
                     clean$att_q8)

clean$att_q8 <- gsub("Disagree",
                     "No",
                     clean$att_q8)

clean$att_q8 <- as.factor(clean$att_q8)
clean %>%               
  tabyl(att_q8) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q1 <- as.factor(clean$att_q1)
clean %>%               
  tabyl(att_q1) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q2 <- as.factor(clean$att_q2)
clean %>%               
  tabyl(att_q2) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q3 <- as.factor(clean$att_q3)
clean %>%               
  tabyl(att_q3) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q4 <- as.factor(clean$att_q4)
clean %>%               
  tabyl(att_q4) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q5 <- as.factor(clean$att_q5)
clean %>%               
  tabyl(att_q5) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q6 <- as.factor(clean$att_q6)
clean %>%               
  tabyl(att_q6) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q7 <- as.factor(clean$att_q7)
clean %>%               
  tabyl(att_q7) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q8 <- as.factor(clean$att_q8)
clean %>%               
  tabyl(att_q8) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q9 <- as.factor(clean$att_q9)
clean %>%               
  tabyl(att_q9) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q10 <- as.factor(clean$att_q10)
clean %>%               
  tabyl(att_q7) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q11 <- as.factor(clean$att_q11)
clean %>%               
  tabyl(att_q11) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q12 <- as.factor(clean$att_q12)
clean %>%               
  tabyl(att_q12) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q13 <- as.factor(clean$att_q13)
clean %>%               
  tabyl(att_q13) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q14 <- as.factor(clean$att_q14)
clean %>%               
  tabyl(att_q14) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()

clean$att_q15 <- as.factor(clean$att_q15)
clean %>%               
  tabyl(att_q15) %>%       # tabulate counts and proportions by work place category
  adorn_pct_formatting()


# Practise questions  -----------------------------------------------------

###Remove "i dont see patients" 

#clean3 <- clean2

###Make data set called clean33 only for patient facing cadres

#clean33 <- clean3 %>%
#  filter(clean3$cadre == "patient facing")


#clean33$practise_q1 <- as.factor(clean33$practise_q1)
#clean33 %>%               
#  tabyl(practise_q1) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()


#clean33$practise_q2 <- as.factor(clean33$practise_q2)
#clean33 %>%               
#  tabyl(practise_q2) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()

#clean33$practise_q3 <- as.factor(clean33$practise_q3)
#clean33 %>%               
#  tabyl(practise_q3) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()
#
#
#clean33$practise_q4 <- as.factor(clean33$practise_q4)
#clean33 %>%               
 # tabyl(practise_q4) %>%       # tabulate counts and proportions by work place category
  #adorn_pct_formatting()



#clean33$practise_q5 <- as.factor(clean33$practise_q5)
#clean33 %>%               
#  tabyl(practise_q5) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()
#
####remove those who dont see patients 


#clean33$practise_q6 <- as.factor(clean33$practise_q6)
#clean33 %>%               
#  tabyl(practise_q6) %>%       # tabulate counts and proportions by work place category
#  adorn_pct_formatting()


# New practise score ------------------------------------------------------

#### Changed practices analysis to "patient facing" only

practise_score_new <- clean


#### Drop the columns we dont need

#practise_score_new = subset(practise_score_new, select = c(cadre,
#                                                           practise_q1,
#                                                           practise_q2,
#                                                           practise_q3, 
#                                                           practise_q4,
#                                                           practise_q5,
#                                                           practise_q6))

####Q1:Worn mask, Always = 1,
## Occassional, Never = 0

practise_score_new %>%               
  tabyl(practise_q1) %>%       
  adorn_pct_formatting()

practise_score_new$practise_q1 <- gsub("Always",
                                       "1", practise_score_new$practise_q1)

practise_score_new$practise_q1 <- gsub("Never",
                                       "0", practise_score_new$practise_q1)

practise_score_new$practise_q1 <- gsub("Occasionally",
                                       "0", practise_score_new$practise_q1)

practise_score_new$practise_q1[practise_score_new$practise_q1 == "i dont see patients"] <- NA

####Q2:Worn PPE, Always = 1,
## Occassional, Never = 0
practise_score_new %>%               
  tabyl(practise_q2) %>%       
  adorn_pct_formatting()

practise_score_new$practise_q2 <- gsub("Always",
                                       "1", practise_score_new$practise_q2)
practise_score_new$practise_q2 <- gsub("Never",
                                       "0", practise_score_new$practise_q2)
practise_score_new$practise_q2 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q2)
practise_score_new$practise_q2[practise_score_new$practise_q2 == "i dont see patients"] <- NA

####Q3:Refrained from shaking hands, Always = 1,
## Occassional, Never = 0
practise_score_new %>%               
  tabyl(practise_q3) %>%       
  adorn_pct_formatting()

practise_score_new$practise_q3 <- gsub("Always",
                                       "1", practise_score_new$practise_q3)
practise_score_new$practise_q3 <- gsub("Never",
                                       "0", practise_score_new$practise_q3)
practise_score_new$practise_q3 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q3)


####Q4:Washed my hands before and after, Always = 1,
## Occassional, Never = 0
practise_score_new %>%               
  tabyl(practise_q4) %>%       
  adorn_pct_formatting()
practise_score_new$practise_q4 <- gsub("Always",
                                       "1", practise_score_new$practise_q4)
practise_score_new$practise_q4 <- gsub("Never",
                                       "0", practise_score_new$practise_q4)
practise_score_new$practise_q4 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q4)

practise_score_new$practise_q4[practise_score_new$practise_q4 == "i dont see patients"] <- NA

####Q5:Avoided patients with signs and symptoms, Never = 1,
## Occassional, Always = 0
practise_score_new %>%               
  tabyl(practise_q5) %>%       
  adorn_pct_formatting()#

practise_score_new$practise_q5 <- gsub("Always",
                                       "0", practise_score_new$practise_q5)
practise_score_new$practise_q5 <- gsub("Never",
                                       "1", practise_score_new$practise_q5)
practise_score_new$practise_q5 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q5)

practise_score_new$practise_q5[practise_score_new$practise_q5 == "i dont see patients"] <- NA

####Q6:time to sensitize patients, Always = 1,
## Occassional, Never = 0
practise_score_new %>%               
  tabyl(practise_q6) %>%       
  adorn_pct_formatting()

practise_score_new$practise_q6 <- gsub("Always",
                                       "1", practise_score_new$practise_q6)
practise_score_new$practise_q6 <- gsub("Never",
                                       "0", practise_score_new$practise_q6)
practise_score_new$practise_q6 <- gsub("Occasional",
                                       "0", practise_score_new$practise_q6)

practise_score_new$practise_q6[practise_score_new$practise_q6 == "i dont see patients"] <- NA

# Convert type to numeric for attitude ------------------------------------

practise_score_new$practise_q1 <- as.numeric(practise_score_new$practise_q1)
practise_score_new$practise_q2 <- as.numeric(practise_score_new$practise_q2)
practise_score_new$practise_q3 <- as.numeric(practise_score_new$practise_q3)
practise_score_new$practise_q4 <- as.numeric(practise_score_new$practise_q4)
practise_score_new$practise_q5 <- as.numeric(practise_score_new$practise_q5)
practise_score_new$practise_q6 <- as.numeric(practise_score_new$practise_q6)


# Create a total practise score column -----------------------------------
practise_score_new$total_practise_score <- rowSums(cbind(practise_score_new$practise_q1,
                                                         practise_score_new$practise_q2,
                                                         practise_score_new$practise_q3,
                                                         practise_score_new$practise_q4,
                                                         practise_score_new$practise_q5,
                                                         practise_score_new$practise_q6),
                                                   na.rm = FALSE)


# Determine NEW PRACTICE out of 15 possible points  --------------
###THESE ARE NEW RESULTS... 

mean(practise_score_new$total_practise_score, na.rm = TRUE)
sd(practise_score_new$total_practise_score, na.rm = TRUE)
summary(practise_score_new$total_practise_score, na.rm = TRUE)

practise_score_new$practise_type <- ifelse(test = practise_score_new$total_practise_score >= 4.588997,
                                           yes = "good",
                                           no = "bad")

practise_score_new$practise_type <- as.factor(practise_score_new$practise_type)
summary(practise_score_new$practise_type)


# Calculating the actual knowledge scores ---------------------------------
##Create a new data frame 

k_score <- clean

#### Drop the columns we dont need

k_score = subset(k_score, select = c(cadre,
                                     knowledge_q1,
                                     knowledge_q10,
                                     knowledge_q2,
                                     knowledge_q3,
                                     knowledge_q4,
                                     knowledge_q5,
                                     knowledge_q6,
                                     knowledge_q7,
                                     knowledge_q8,
                                     knowledge_q9,
                                     knowledge_q11,
                                     knowledge_q12))

####Q1: there is no effective cure: TRUE = 1, FALSE, IDK = 0
k_score$knowledge_q1 <- gsub("TRUE",
                     "1", k_score$knowledge_q1)

k_score$knowledge_q1 <- gsub("FALSE",
                             "0", k_score$knowledge_q1)

k_score$knowledge_q1 <- gsub("I dont know",
                             "0", k_score$knowledge_q1)


#Q2: Most persons will develop severe cases: FALSE = 1, TRUE/IDK = 0
k_score$knowledge_q2 <- gsub("TRUE",
                             "0", k_score$knowledge_q2)

k_score$knowledge_q2 <- gsub("FALSE",
                             "1", k_score$knowledge_q2)

k_score$knowledge_q2 <- gsub("I dont know",
                             "0", k_score$knowledge_q2)

#Q3: Eating or being close to animals: FALSE = 1, TRUE/IDK = 0
k_score$knowledge_q3 <- gsub("TRUE",
                             "0", k_score$knowledge_q3)

k_score$knowledge_q3 <- gsub("FALSE",
                             "1", k_score$knowledge_q3)

k_score$knowledge_q3 <- gsub("I dont know",
                             "0", k_score$knowledge_q3)

#Q4: Persons with COVID-19 cannot transmit: FALSE = 1, TRUE/IDK = 0
k_score$knowledge_q4 <- gsub("TRUE",
                             "0", k_score$knowledge_q4)

k_score$knowledge_q4 <- gsub("FALSE",
                             "1", k_score$knowledge_q4)

k_score$knowledge_q4 <- gsub("I dont know",
                             "0", k_score$knowledge_q4)

#Q5: Spreads via respiratory droplets: TRUE = 1, FALSE, IDK = 0
k_score$knowledge_q5 <- gsub("TRUE",
                             "1", k_score$knowledge_q5)

k_score$knowledge_q5 <- gsub("FALSE",
                             "0", k_score$knowledge_q5)

k_score$knowledge_q5 <- gsub("I dont know",
                             "0", k_score$knowledge_q5)

#Q6: Wearing masks can prevent one from acquiring: TRUE = 1, FALSE, IDK = 0
k_score$knowledge_q6 <- gsub("TRUE",
                             "1", k_score$knowledge_q6)

k_score$knowledge_q6 <- gsub("FALSE",
                             "0", k_score$knowledge_q6)

k_score$knowledge_q6 <- gsub("I dont know",
                             "0", k_score$knowledge_q6)

#Q7: Not necessary for kids to take measures: FALSE = 1, TRUE/IDK = 0
k_score$knowledge_q7 <- gsub("TRUE",
                             "0", k_score$knowledge_q7)

k_score$knowledge_q7 <- gsub("FALSE",
                             "1", k_score$knowledge_q7)

k_score$knowledge_q7 <- gsub("I dont know",
                             "0", k_score$knowledge_q7)

#Q8: Isolation & treatment are effective: TRUE = 1, FALSE, IDK = 0
k_score$knowledge_q8 <- gsub("TRUE",
                             "1", k_score$knowledge_q8)

k_score$knowledge_q8 <- gsub("FALSE",
                             "0", k_score$knowledge_q8)

k_score$knowledge_q8 <- gsub("I dont know",
                             "0", k_score$knowledge_q8)

#Q9: Kiambu county main facility: Tigoni = 1, anything else = 0
k_score$knowledge_q9 <- gsub("Tigoni L4 Hospital",
                             "1", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("Kiambu L4 Hospital",
                             "0", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("Thika L5 Hospital",
                             "0", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("Lari L4 Hospital",
                             "0", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("Gatundu L5 Hospital",
                             "0", k_score$knowledge_q9)

k_score$knowledge_q9 <- gsub("i dont know",
                             "0", k_score$knowledge_q9)

k_score %>%               
  tabyl(knowledge_q9) %>%       
  adorn_pct_formatting()

#Q10: High index of suspicion for COVID-19
#Give mask. Isolate somewhere, inform the surveillance officer
#This is the correct answer as at when we were collecting data – at that time.

k_score %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

k_score$knowledge_q10 <- gsub("Give the patient a mask",
                              "1", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Inform the disease surveillance coordinator",
                              "1", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Isolate the patient and the patient relatives",
                              "1", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Call the subcounty COVID hotline",
                             "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Send the patient and relatives to the holding area",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Find and use the case definition to decide",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Send the paitent to the lab for the COVID-19test",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("Send the paitent to the lab for the COVID-19test",
                              "0", k_score$knowledge_q10)

k_score %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

k_score$knowledge_q10 <- gsub("Inform the Medical Superintendent or hospital in charge",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- gsub("I dont know what to do",
                              "0", k_score$knowledge_q10)

k_score$knowledge_q10 <- sapply(strsplit(k_score$knowledge_q10,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

k_score %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()


# Patient facing, non-patient facing and environemntal health staff Q10 -------

patient_facing_q10 <- k_score %>%
  filter(k_score$cadre == "patient facing")

patient_facing_q10 %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

nonpatient_facing_q10 <- k_score %>%
  filter(k_score$cadre == "nonpatient facing")

nonpatient_facing_q10 %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

public_q10 <- k_score %>%
  filter(k_score$cadre == "public health")

public_q10 %>%               
  tabyl(knowledge_q10) %>%       
  adorn_pct_formatting()

# Q11: Data for suspected COVID-19 patients: patient notes/case investigation form = 1,
###    anything else = 0

k_score %>%               
  tabyl(knowledge_q11) %>%       
  adorn_pct_formatting()

k_score$knowledge_q11 <- gsub("Patient notes",
                              "1", k_score$knowledge_q11)

k_score$knowledge_q11 <- gsub("Case definition form",
                              "0", k_score$knowledge_q11)

k_score$knowledge_q11 <- gsub("Case investigation form",
                              "1", k_score$knowledge_q11)

k_score$knowledge_q11 <- gsub("Clinical register",
                              "0", k_score$knowledge_q11)

k_score %>%               
  tabyl(knowledge_q11) %>%       
  adorn_pct_formatting()

k_score$knowledge_q11 <- gsub("I dont know",
                              "0", k_score$knowledge_q11)

k_score$knowledge_q11 <- sapply(strsplit(k_score$knowledge_q11,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

k_score %>%               
  tabyl(knowledge_q11) %>%       
  adorn_pct_formatting()

patientsss <- k_score %>%
  filter(k_score$cadre == "patient facing")

patientsss %>%
  tabyl(knowledge_q11) %>%
  adorn_pct_formatting()

###Q12: Main symptoms of COVID-19: 
## Fever, smell disturbance and cough = 1 each, anything else = 0. 
k_score %>%               
  tabyl(knowledge_q12) %>%       
  adorn_pct_formatting()

k_score$knowledge_q12 <- gsub("Fever",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Cough",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Smell disturbance",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("smell disturbance",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Confusion",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Confusion",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "",
                              k_score$knowledge_q12, perl=TRUE)

k_score %>%               
  tabyl(knowledge_q12) %>%       
  adorn_pct_formatting()

k_score$knowledge_q12 <- gsub("Sore throat",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Diarrhoea",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Myalgia",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Sneezing",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Runny nose",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Headache",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Runny nose",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Hair loss",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Seizures",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("lack of taste",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("runny nose",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("sore throat",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("diarrhoea",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("cough",
                              "1", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Burning sensetional",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("General body pain",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("Skin color changes",
                              "0", k_score$knowledge_q12)

k_score$knowledge_q12 <- gsub("sneezing",
                              "0", k_score$knowledge_q12)

str(k_score$knowledge_q1)

######Create k_score2 to remove the zeroes first, then remove the commas using
### "replace"

#### knowledge q1, 2,3,4,5,6,7,8,9 all single, 
## so can be converted to numeric
#### knowledge q10, 11, 12 have commas. 

k_score$knowledge_q1 <- as.numeric(k_score$knowledge_q1)
k_score$knowledge_q2 <- as.numeric(k_score$knowledge_q2)
k_score$knowledge_q3 <- as.numeric(k_score$knowledge_q3)
k_score$knowledge_q4 <- as.numeric(k_score$knowledge_q4)
k_score$knowledge_q5 <- as.numeric(k_score$knowledge_q5)
k_score$knowledge_q6 <- as.numeric(k_score$knowledge_q6)
k_score$knowledge_q7 <- as.numeric(k_score$knowledge_q7)
k_score$knowledge_q8 <- as.numeric(k_score$knowledge_q8)
k_score$knowledge_q9 <- as.numeric(k_score$knowledge_q9)

### create a column adding up all the scores for Q1 to 9

###Separate the numbers in Q10, Q11, Q12
k_score$knowledge_q10 <- as.character(k_score$knowledge_q10)

k_score$knowledge_q10 <- sapply(strsplit(k_score$knowledge_q10,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

# Adding the values in the knowledge q11-13 -------------------------------
k_score$knowledge_q11 <- as.character(k_score$knowledge_q11)
k_score$knowledge_q12 <- as.character(k_score$knowledge_q12)


k_score$knowledge_q11 <- sapply(strsplit(k_score$knowledge_q11,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

k_score$knowledge_q12 <- sapply(strsplit(k_score$knowledge_q12,
                                         "[ ,]+"), function(i) sum(as.numeric(i)))

# Create a total knowledge score column -----------------------------------
k_score$total_kscore <- rowSums(cbind(k_score$knowledge_q1,
                                     k_score$knowledge_q10,
                                     k_score$knowledge_q2,
                                     k_score$knowledge_q3,
                                     k_score$knowledge_q4,
                                     k_score$knowledge_q5,
                                     k_score$knowledge_q6,
                                     k_score$knowledge_q7,
                                     k_score$knowledge_q8,
                                     k_score$knowledge_q9,
                                     k_score$knowledge_q11,
                                     k_score$knowledge_q12),
                                     na.rm = FALSE)


# Determine mean knowledge score out of 16 possible points  --------------

mean(k_score$total_kscore, na.rm = TRUE)
sd(k_score$total_kscore, na.rm = TRUE)
summary(k_score$total_kscore)


# hOW MANY ARE BELOW MEAN?  -----------------------------------------------

#### hOW MANY ARE BELOW MEAN? below == 11.6 = bad knowledge

k_score$knowledge_type <- ifelse(test = k_score$total_kscore >= 11.52,
                                 yes = "good",
                                 no = "bad")

k_score %>%               
  tabyl(knowledge_type) %>%       
  adorn_pct_formatting()

# Making attitude a numeric based on responses ----------------------------
##Create a new data frame 

att_score <- clean

#### Drop the columns we dont need

att_score = subset(att_score, select = c(att_q1,
                                         att_q2,
                                         att_q3,
                                         att_q4,
                                         att_q5,
                                         att_q6,
                                         att_q7,
                                         att_q8,
                                         att_q9,
                                         att_q10,
                                         att_q11,
                                         att_q12,
                                         att_q13,
                                         att_q14,
                                         att_q15
                                        ))

####Q1: Black race is protective: Strongly disagree/disagree = 1,
## Not sure, agree, strongly agree = 0
att_score$att_q1 <- gsub("Strongly disagree",
                             "1", att_score$att_q1)

att_score$att_q1 <- gsub("disagree",
                         "1", att_score$att_q1)

att_score$att_q1 <- gsub("strongly agree",
                         "0", att_score$att_q1)

att_score$att_q1 <- gsub("agree",
                         "0", att_score$att_q1)

att_score$att_q1 <- gsub("not sure",
                         "0", att_score$att_q1)

att_score$att_q1 <- gsub("i dont know",
                         "0", att_score$att_q1)

att_score %>%               
  tabyl(att_q1) %>%       
  adorn_pct_formatting()

####Q2: Wearing a well-fitting mask is protective: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0
att_score$att_q2 <- gsub("strongly agree",
                         "1", att_score$att_q2)

att_score$att_q2 <- gsub("agree",
                         "1", att_score$att_q2)

att_score$att_q2 <- gsub("Strongly disagree",
                         "0", att_score$att_q2)

att_score$att_q2 <- gsub("disagree",
                         "0", att_score$att_q2)
att_score$att_q2 <- gsub("dis1",
                         "0", att_score$att_q2)
att_score$att_q2 <- gsub("Strongly dis1",
                         "0", att_score$att_q2)
att_score$att_q2 <- gsub("Strongly 0",
                         "0", att_score$att_q2)

att_score$att_q2 <- gsub("not sure",
                         "0", att_score$att_q2)

att_score$att_q2 <- gsub("i dont know",
                         "0", att_score$att_q2)

att_score %>%               
  tabyl(att_q2) %>%       
  adorn_pct_formatting()

####Q3: Using normal soap: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0
att_score$att_q3 <- gsub("strongly agree",
                         "1", att_score$att_q3)

att_score$att_q3 <- gsub("agree",
                         "1", att_score$att_q3)

att_score$att_q3 <- gsub("Strongly disagree",
                         "0", att_score$att_q3)

att_score$att_q3 <- gsub("disagree",
                         "0", att_score$att_q3)
att_score$att_q3 <- gsub("dis1",
                         "0", att_score$att_q3)
att_score$att_q3 <- gsub("Strongly dis1",
                         "0", att_score$att_q3)

att_score$att_q3 <- gsub("not sure",
                         "0", att_score$att_q3)

att_score$att_q3 <- gsub("strongly 0",
                         "0", att_score$att_q3)

att_score$att_q3 <- gsub("i dont know",
                         "0", att_score$att_q3)

att_score %>%               
  tabyl(att_q3) %>%       
  adorn_pct_formatting()

####Q4: I can confidently manage a patient: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0
att_score$att_q4 <- gsub("Strongly agree",
                         "1", att_score$att_q4)
att_score$att_q4 <- gsub("Agree",
                         "1", att_score$att_q4)
att_score$att_q4 <- gsub("Strongly disagree",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Disagree",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Dis1",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Strongly dis1",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Not sure",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Strongly 0",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("Strongly 1",
                         "0", att_score$att_q4)
att_score$att_q4 <- gsub("i dont know",
                         "0", att_score$att_q4)

att_score$att_q4[att_score$att_q4 == "i dont see patients"] <- NA

att_score %>%               
  tabyl(att_q4) %>%       
  adorn_pct_formatting()

####Q5: I feel safe in my everyday work: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q5) %>%       
  adorn_pct_formatting()

att_score$att_q5 <- gsub("Strongly agree",
                         "1", att_score$att_q5)
att_score$att_q5 <- gsub("Agree",
                         "1", att_score$att_q5)
att_score$att_q5 <- gsub("Not sure",
                         "0", att_score$att_q5)
att_score$att_q5 <- gsub("Strongly disagree",
                         "0", att_score$att_q5)
att_score$att_q5 <- gsub("Disagree",
                         "0", att_score$att_q5)

####Q6: I am well trained: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q6) %>%       
  adorn_pct_formatting()

att_score$att_q6 <- gsub("Strongly agree",
                         "1", att_score$att_q6)
att_score$att_q6 <- gsub("Agree",
                         "1", att_score$att_q6)
att_score$att_q6 <- gsub("Not sure",
                         "0", att_score$att_q6)
att_score$att_q6 <- gsub("i dont know",
                         "0", att_score$att_q6)
att_score$att_q6 <- gsub("Strongly disagree",
                         "0", att_score$att_q6)
att_score$att_q6 <- gsub("Disagree",
                         "0", att_score$att_q6)

att_score$att_q6[att_score$att_q6 == "i dont see patients"] <- NA

####Q7: I am confident abt vaccine: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q7) %>%       
  adorn_pct_formatting()

att_score$att_q7 <- gsub("Strongly agree",
                         "1", att_score$att_q7)
att_score$att_q7 <- gsub("Agree",
                         "1", att_score$att_q7)
att_score$att_q7 <- gsub("Not sure",
                         "0", att_score$att_q7)

att_score$att_q7 <- gsub("Strongly disagree",
                         "0", att_score$att_q7)
att_score$att_q7 <- gsub("Disagree",
                         "0", att_score$att_q7)

####Q8: I am confident about vaccine: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q8) %>%       
  adorn_pct_formatting()

att_score$att_q8 <- gsub("Yes",
                         "1", att_score$att_q8)
att_score$att_q8 <- gsub("Agree",
                         "1", att_score$att_q8)
att_score$att_q8 <- gsub("Not sure",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("I dont know",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("Maybe",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("No",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("Strongly disagree",
                         "0", att_score$att_q8)
att_score$att_q8 <- gsub("Disagree",
                         "0", att_score$att_q8)

####Q9:Kenya is in a good position to contain COVID-19: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q9) %>%       
  adorn_pct_formatting()

att_score$att_q9 <- gsub("strongly agree",
                         "1", att_score$att_q9)
att_score$att_q9 <- gsub("agree",
                         "1", att_score$att_q9)
att_score$att_q9 <- gsub("not sure",
                         "0", att_score$att_q9)
att_score$att_q9 <- gsub("strongly dis1",
                         "0", att_score$att_q9)
att_score$att_q9 <- gsub("dis1",
                         "0", att_score$att_q9)

####Q10:Kiambu is in a good position to contain COVID-19: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q10) %>%       
  adorn_pct_formatting()

att_score$att_q10 <- gsub("strongly agree",
                         "1", att_score$att_q10)
att_score$att_q10 <- gsub("agree",
                         "1", att_score$att_q10)
att_score$att_q10 <- gsub("not sure",
                         "0", att_score$att_q10)
att_score$att_q10 <- gsub("Strongly 0",
                         "0", att_score$att_q10)
att_score$att_q10 <- gsub("dis1",
                         "0", att_score$att_q10)

####Q11:in the beginning, Kenya handled it well: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q11) %>%       
  adorn_pct_formatting()

att_score$att_q11 <- gsub("Strongly Agree",
                          "1", att_score$att_q11)
att_score$att_q11 <- gsub("Agree",
                          "1", att_score$att_q11)
att_score$att_q11 <- gsub("Neutral",
                          "0", att_score$att_q11)
att_score$att_q11 <- gsub("Disagree",
                          "0", att_score$att_q11)
att_score$att_q11 <- gsub("Strongly 0",
                          "0", att_score$att_q11)

####Q12:in the beginning, Kiambu handled it well: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q12) %>%       
  adorn_pct_formatting()

att_score$att_q12 <- gsub("strongly agree",
                          "1", att_score$att_q12)
att_score$att_q12 <- gsub("agree",
                          "1", att_score$att_q12)
att_score$att_q12 <- gsub("not sure",
                          "0", att_score$att_q12)
att_score$att_q12 <- gsub("dis1",
                          "0", att_score$att_q12)
att_score$att_q12 <- gsub("Strongly 0",
                          "0", att_score$att_q12)

####Q13:Currently, Kenya handling it well: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q13) %>%       
  adorn_pct_formatting()

att_score$att_q13 <- gsub("Strongly agree",
                          "1", att_score$att_q13)
att_score$att_q13 <- gsub("Agree",
                          "1", att_score$att_q13)
att_score$att_q13 <- gsub("Not sure",
                          "0", att_score$att_q13)
att_score$att_q13 <- gsub("Disagree",
                          "0", att_score$att_q13)
att_score$att_q13 <- gsub("Strongly disagree",
                          "0", att_score$att_q13)

####Q14:Currently, Kiambu handling it well: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q14) %>%       
  adorn_pct_formatting()

att_score$att_q14 <- gsub("Strongly agree",
                          "1", att_score$att_q14)
att_score$att_q14 <- gsub("Agree",
                          "1", att_score$att_q14)
att_score$att_q14 <- gsub("Not sure",
                          "0", att_score$att_q14)
att_score$att_q14 <- gsub("Disagree",
                          "0", att_score$att_q14)
att_score$att_q14 <- gsub("Strongly disagree",
                          "0", att_score$att_q14)


####Q13:From my interaction with patients: Strongly agree/agree = 1,
## Not sure, strongly disagree, disagree = 0

att_score %>%               
  tabyl(att_q15) %>%       
  adorn_pct_formatting()

att_score$att_q15 <- gsub("Strongly Agree",
                          "1", att_score$att_q15)
att_score$att_q15 <- gsub("Agree",
                          "1", att_score$att_q15)
att_score$att_q15 <- gsub("not sure",
                          "0", att_score$att_q15)
att_score$att_q15 <- gsub("disagree",
                          "0", att_score$att_q15)
att_score$att_q15 <- gsub("Strongly 0",
                          "0", att_score$att_q15)

att_score$att_q15[att_score$att_q15 == "i dont see patients"] <- NA



# Convert type to numeric for attitude ------------------------------------


att_score$att_q1 <- as.numeric(att_score$att_q1)
att_score$att_q2 <- as.numeric(att_score$att_q2)
att_score$att_q3 <- as.numeric(att_score$att_q3)
att_score$att_q4 <- as.numeric(att_score$att_q4)
att_score$att_q5 <- as.numeric(att_score$att_q5)
att_score$att_q6 <- as.numeric(att_score$att_q6)
att_score$att_q7 <- as.numeric(att_score$att_q7)
att_score$att_q8 <- as.numeric(att_score$att_q8)
att_score$att_q9 <- as.numeric(att_score$att_q9)
att_score$att_q10 <- as.numeric(att_score$att_q10)
att_score$att_q11 <- as.numeric(att_score$att_q11)
att_score$att_q12 <- as.numeric(att_score$att_q12)
att_score$att_q13 <- as.numeric(att_score$att_q13)
att_score$att_q14 <- as.numeric(att_score$att_q14)
att_score$att_q15 <- as.numeric(att_score$att_q15)

# Create a total attitude score column -----------------------------------
att_score$total_att_score <- rowSums(cbind(att_score$att_q1,
                                           att_score$att_q2,
                                           att_score$att_q3,
                                           att_score$att_q4,
                                           att_score$att_q5,
                                           att_score$att_q6,
                                           att_score$att_q7,
                                           att_score$att_q8,
                                           att_score$att_q9,
                                           att_score$att_q10,
                                           att_score$att_q11,
                                           att_score$att_q12,
                                           att_score$att_q13,
                                           att_score$att_q14,
                                           att_score$att_q15),
                                na.rm = FALSE)

# Determine attitude out of 15 possible points  --------------

mean(att_score$total_att_score, na.rm = TRUE)
sd(att_score$total_att_score, na.rm = TRUE)
summary(att_score$total_att_score, na.rm = TRUE)

att_score$att_type <- ifelse(test = att_score$total_att_score >= 8.565217,
                                 yes = "good",
                                 no = "bad")

att_score$att_type <- as.factor(att_score$att_type)
summary(att_score$att_type)

#OLD converting practise scores to numeric values  ---------------------------

#practise_score <- clean

#### Drop the columns we dont need

#practise_score = subset(practise_score, select = c(practise_q1,
  #                                                 practise_q2,
   #                                                practise_q3, 
    #                                               practise_q4,
     #                                              practise_q5,
      #                                             practise_q6))

####Q1:Worn mask, Always = 1,
## Occassional, Never = 0

#practise_score %>%               
 # tabyl(practise_q1) %>%       
  #adorn_pct_formatting()
#
#practise_score$practise_q1 <- gsub("Always",
 #                         "1", practise_score$practise_q1)
#
#practise_score$practise_q1 <- gsub("Never",
#                                   "0", practise_score$practise_q1)

#practise_score$practise_q1 <- gsub("Occasionally",
#                                   "0", practise_score$practise_q1)

#practise_score$practise_q1[practise_score$practise_q1 == "i dont see patients"] <- NA

####Q2:Worn PPE, Always = 1,
## Occassional, Never = 0
#practise_score %>%               
 # tabyl(practise_q2) %>%       
#  adorn_pct_formatting()
#practise_score$practise_q2 <- gsub("Always",
  #                                 "1", practise_score$practise_q2)
#practise_score$practise_q2 <- gsub("Never",
 #                                  "0", practise_score$practise_q2)
#practise_score$practise_q2 <- gsub("Occasional",
#                                   "0", practise_score$practise_q2)
#practise_score$practise_q2[practise_score$practise_q2 == "i dont see patients"] <- NA

####Q3:Refrained from shaking hands, Always = 1,
## Occassional, Never = 0
#practise_score %>%               
#  tabyl(practise_q3) %>%       
#  adorn_pct_formatting()
#practise_score$practise_q3 <- gsub("Always",
 #                                  "1", practise_score$practise_q3)
#practise_score$practise_q3 <- gsub("Never",
#                                   "0", practise_score$practise_q3)
#practise_score$practise_q3 <- gsub("Occasional",
#                                   "0", practise_score$practise_q3)


####Q4:Washed my hands before and after, Always = 1,
## Occassional, Never = 0
#practise_score %>%               
#  tabyl(practise_q4) %>%       
#  adorn_pct_formatting()
#practise_score$practise_q4 <- gsub("Always",
#                                   "1", practise_score$practise_q4)
#practise_score$practise_q4 <- gsub("Never",
#                                  "0", practise_score$practise_q4)
#practise_score$practise_q4 <- gsub("Occasional",
#                                   "0", practise_score$practise_q4)

#practise_score$practise_q4[practise_score$practise_q4 == "i dont see patients"] <- NA

####Q5:Avoided patients with signs and symptoms, Never = 1,
## Occassional, Always = 0
#practise_score %>%               
#  tabyl(practise_q5) %>%       
#  adorn_pct_formatting()
#practise_score$practise_q5 <- gsub("Always",
#                                   "0", practise_score$practise_q5)
#practise_score$practise_q5 <- gsub("Never",
#                                   "1", practise_score$practise_q5)
#practise_score$practise_q5 <- gsub("Occasional",
#                                   "0", practise_score$practise_q5)

#practise_score$practise_q5[practise_score$practise_q5 == "i dont see patients"] <- NA

####Q6:time to sensitize patients, Always = 1,
## Occassional, Never = 0
#practise_score %>%               
#  tabyl(practise_q6) %>%       
#  adorn_pct_formatting()
#practise_score$practise_q6 <- gsub("Always",
#                                   "1", practise_score$practise_q6)
#practise_score$practise_q6 <- gsub("Never",
 #                                  "0", practise_score$practise_q6)
#practise_score$practise_q6 <- gsub("Occasional",
                                   "0", practise_score$practise_q6)

#practise_score$practise_q6[practise_score$practise_q6 == "i dont see patients"] <- NA

# Convert type to numeric for attitude ------------------------------------

#practise_score$practise_q1 <- as.numeric(practise_score$practise_q1)
#practise_score$practise_q2 <- as.numeric(practise_score$practise_q2)
#practise_score$practise_q3 <- as.numeric(practise_score$practise_q3)
#practise_score$practise_q4 <- as.numeric(practise_score$practise_q4)
#practise_score$practise_q5 <- as.numeric(practise_score$practise_q5)
#practise_score$practise_q6 <- as.numeric(practise_score$practise_q6)


# Create a total practise score column -----------------------------------
#practise_score$total_practise_score <- rowSums(cbind(practise_score$practise_q1,
#                                               practise_score$practise_q2,
#                                               practise_score$practise_q3,
#                                               practise_score$practise_q4,
#                                               practise_score$practise_q5,
#                                               practise_score$practise_q6),
#                                             na.rm = FALSE)




# Make final data frame with demographics, knowledge/attitude/prac --------

final <- clean

final <- subset(final, select = c(new_timestamp,
                                  work_place,
                                  sex,
                                  age,
                                  cadre,
                                  education,
                                  international,
                                  news,
                                  fora,
                                  social,
                                  govt,
                                  journals))


# Make final data frame ---------------------------------------------------


final <- cbind(final, k_score)
final <- cbind(final, att_score)
final <- cbind(final, practise_score)
final <- cbind(final, clean$new_timestamp)

final <- cbind(final, practise_score_new)

# Now the combined_data data frame contains the original data plus the analysis results for patients and doctors



# Drop unneeded columns from the data frame -------------------------------

final = subset(final, select = c(new_timestamp,
  work_place,
                                 sex,
                                 age,
                                 cadre, 
                                 education,
                                 international,
                                 news,
                                 fora,
                                 social,
                                 govt,
                                 journals,
                                 total_kscore,
                                 knowledge_type,
                                 total_att_score,
                                 att_type,
                                 total_practise_score
                                 ))


# Add the information sources together ------------------------------------
final$all_sources_information <- rowSums(cbind(final$international,
                                               final$news,
                                               final$fora,
                                               final$social,
                                               final$govt,
                                               final$journals),
                                na.rm = FALSE)

##### So, how many sources of information do most respondents have? 
final$all_sources_information <- as.factor(final$all_sources_information)
final %>%               
  tabyl(all_sources_information) %>%       
  adorn_pct_formatting()

##### Replace knowledge score "good" with 1, "bad" with 0
final$knowledge_binary <- ifelse(test = final$knowledge_type == "good",
                                       yes = "1",
                                       no = "0")

##### Replace attitude score "good" with 1, "bad" with 0
final$att_binary <- ifelse(test = final$att_type == "good",
                                 yes = "1",
                                 no = "0")

##### Replace practise score "good" with 1, "bad" with 0
practise_score_new$practise_binary <- ifelse(test = practise_score_new$total_practise_score == "good",
                           yes = "1",
                           no = "0")


# Linear regression between K, A, P scores  -------------------------------

library(broom)        
library(lmtest)     
library(parameters)

#### KNowledge and attitude?

hist(final$total_kscore)
hist(final$total_att_score)
hist(final$total_practise_score)



# Linear regression!!!!!!!! -----------------------------------------------

# Odds and adjusted odds ratio for knowledge  -----------------------------
final$all_sources_information <- as.numeric(final$all_sources_information)
final$all_sources_information[is.na(final$all_sources_information)] <- 7

# Level 7 is taken as zero sources of information, changes codes 
final$sex <- as.factor(final$sex)
final$work_place <- as.factor(final$work_place)
final$age <- as.factor(final$age)
final$cadre <- as.factor(final$cadre)
final$education <- as.factor(final$education)
final$govt <- as.factor(final$govt)
final$news <- as.factor(final$news)
final$international <- as.factor(final$international)
final$social <- as.factor(final$social)
final$fora <- as.factor(final$fora)
final$journals <- as.factor(final$journals) 
final$all_sources_information <- as.factor(final$all_sources_information)
final$knowledge_binary <- as.factor(final$knowledge_binary)
final$att_binary <- as.factor(final$att_binary)
practise_score_new$practise_binary <- as.factor(practise_score_new$practise_binary)
final$new_timestamp <- as.factor(final$new_timestamp)


install.packages("epiDisplay")
library(epiDisplay)

final$education <- relevel(final$education, ref='bachelors')
final$cadre <- relevel(final$cadre, ref='public health')
final$sex <- relevel(final$sex, ref='Male')
final$all_sources_information <- relevel(final$all_sources_information, ref='zero')


# COR AND AOR FOR KNOWLEDGE SCORES ----------------------------------------

knowledge_model_1 <- glm(knowledge_binary ~ sex + 
                    age + work_place + 
                    education + cadre + govt + 
                    news + international + social + 
                    fora + journals + new_timestamp, 
                  family = "binomial", data = final)

knowledge_model_2 <- glm(knowledge_binary ~ sex + 
                           age + work_place + 
                           education + cadre + govt + 
                           news + international + social + 
                           fora + journals + new_timestamp + att_binary
                         + practise_type, 
                         family = "binomial", data = final)

logistic.display(knowledge_model_1)
logistic.display(knowledge_model_2)

knowledge_model_1 %>% as_flex_table()

# AIC method for KNOWLEDGE: Goodness of Fit testing ------------------------------------------------

#First model
step(knowledge_model_1, direction = "forward")


#Second model 
step(knowledge_model_2, direction = "forward")



# Attitude ----------------------------------------------------------------

att_model_1 <- glm(att_binary ~ sex + 
                           age + work_place + 
                           education + cadre + govt + 
                           news + international + social + 
                           fora + journals + new_timestamp, 
                         family = "binomial", data = final)

att_model_2 <- glm(att_binary ~ sex + 
                           age + work_place + 
                           education + cadre + govt + 
                           news + international + social + 
                           fora + journals + new_timestamp + knowledge_binary
                         + practise_type, 
                         family = "binomial", data = final)


# AIC for attitude: GOF testing -------------------------------------------

#First model
step(att_model_1, direction = "forward")


#Second model 
step(att_model_2, direction = "forward")



# PRACTISE COR AND AOR ----------------------------------------------------

# Define a function to create a flextable from the model summary
create_model_flextable <- function(model) {
  # Create a data frame with coefficients, confidence intervals, and AOR
  coef_data <- cbind(coef(model), confint(model), calculate_aor(model))
  colnames(coef_data) <- c("Coefficient", "Lower CI", "Upper CI", "AOR", "AOR Lower CI", "AOR Upper CI")
  
  # Create a flextable from the data frame
  flextable_data <- flextable(coef_data) %>%
    set_table_properties(width = .5, layout = "autofit") %>%
    compose(j = "AOR", value = as_paragraph(as_chunk("AOR"), get_column_as_paragraph("AOR", pattern = "\\d\\.\\d+")), part = "footer") %>%
    compose(j = "AOR Lower CI", value = as_paragraph(as_chunk("AOR Lower CI"), get_column_as_paragraph("AOR Lower CI", pattern = "\\d\\.\\d+")), part = "footer") %>%
    compose(j = "AOR Upper CI", value = as_paragraph(as_chunk("AOR Upper CI"), get_column_as_paragraph("AOR Upper CI", pattern = "\\d\\.\\d+")), part = "footer") %>%
    flextable::set_table_properties(layout = "autofit")
  
  return(flextable_data)
}

# Fit your GLM model
practise_model1 <- glm(practise_score_new$practise_binary ~ sex + 
                         age + work_place + education + cadre + govt + 
                         news + international + social + fora + journals + new_timestamp, 
                       family = "binomial", data = final)

# Create the flextable
model_flextable <- create_model_flextable(practise_model1)

# Create a Word document
doc <- read_docx()

# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(value = model_flextable)

# Save the Word document
print(doc, target = "model_summary.docx")



# Fit your GLM model
practise_model1 <- glm(practise_score_new$practise_binary ~ sex + 
                         age + work_place + education + cadre + govt + 
                         news + international + social + fora + journals + new_timestamp, 
                       family = "binomial", data = final)

# Print the comprehensive summary
print_comprehensive_summary(practise_model1)


practise_model1 <- glm(practise_score_new$practise_binary ~ sex + 
                     age + work_place + 
                     education + cadre + govt + 
                     news + international + social + 
                     fora + journals + new_timestamp, 
                   family = "binomial", data = final)

practise_model2 <- glm(practise_score_new$practise_binary ~ sex + 
                     age + work_place + 
                     education + cadre + govt + 
                     news + international + social + 
                     fora + journals + new_timestamp + knowledge_binary
                   + att_binary, 
                   family = "binomial", data = final)


# AIC for attitude: GOF testing -------------------------------------------

#First model
step(practise_model1, direction = "forward")


#Second model 
step(practise_model2, direction = "forward")

# FLEXTABLE  --------------------------------------------------------------

## ---- Univariate-knowledge     ------------------

knwoledge_univ_tab <- final %>% 
  dplyr::select(all_of(explanatory_vars), knowledge_binary) %>% 
  tbl_uvregression(                         
    method = glm,                           
    y = knowledge_binary,                           
    method.args = list(family = binomial),  
    exponentiate = TRUE,                   
    tidy_fun = broom.helpers::tidy_parameters
  ) 

## view univariate results table 
knwoledge_univ_tab %>% as_flex_table()


## ---- Multivariable-model-maternal-death ---------------------

# run a regression with all variables of interest
knwoledge_mv_tab <- explanatory_vars %>%  
  str_c(collapse = "+") %>%     
  str_c("knowledge_binary ~ ", .) %>%    
  glm(family = "binomial",      
      data = final) %>%
  step(direction = "forward", trace = FALSE) %>%
  tbl_regression(exponentiate = TRUE,
                 tidy_fun = broom.helpers::tidy_parameters)

knwoledge_mv_tab %>% as_flex_table()

## ---- combine-maternal-death-univariate-multivariable-results ------------
merged_table <- tbl_merge(
  tbls = list(knwoledge_univ_tab, knwoledge_mv_tab),                          # combine
  tab_spanner = c("**Univariate**", "**Multivariable**")) %>% 
  as_flex_table()

print()



# Exact p-values for reviewer: Was not successful...----------------------------------------


# Printing the Combined Univariate & Multivariate Output ------------------

doc <- read_docx() %>%
  body_add_flextable(value = knowledge_model_2) 

print(doc, target = "output_document_after_gestation_cat.docx")


# Correlation -------------------------------------------------------------

lm_K <- lm(total_kscore ~ total_att_score, data = final)
summary(lm_K)

corr_KandA <- cor.test(x=final$total_kscore, y=final$total_att_score,
                       method = 'spearman',
                       conf.level = TRUE)
corr_KandA

kpoints <- augment(lm_K)
## plot the data using age as the x-axis 
ggplot(kpoints, aes(x = total_kscore)) + 
  geom_point(aes(y = total_att_score)) + 
  geom_line(aes(y = .fitted), colour = "red")

knowledge <- ggplot(data = kpoints,   
       mapping = aes(     
         x = total_att_score,                       
         y = total_kscore,         
         color = total_kscore))+     
  geom_point(             
    shape = "diamond",      
    alpha = 3.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(                  
    method = "lm",              
    size = 2.5,
    color = "#c77d77") +
  theme_minimal() +
  labs(title = "Knowledge Vs. Attitude",
       y = "Knowledge Score",
       x = "Attitude Score",
       color = " ") +
  theme(legend.position = "bottom") + 
  theme(text = element_text(size = 34)) +
  theme(axis.title = element_text(size = 36))+
  theme(legend.text= element_text(size = 10)) +
  theme(plot.title = element_text(size = 40))

ggsave(knowledge,filename = "knowledge_edited_size_7.png",
       width=9, height=7)

## Knowledge and practise
lm_P <- lm(total_kscore ~ total_practise_score, data = final)
summary(lm_P)

corr_KandP <- cor.test(x=final$total_kscore, y=final$total_practise_score,
                       method = 'spearman',
                       conf.level = TRUE)
corr_KandP

predict(lm_P, data = total_practise_score, interval = 'confidence')

Ppoints <- augment(lm_P)
ggplot(Ppoints, aes(x = total_kscore)) + 
  geom_point(aes(y = total_practise_score)) + 
  geom_line(aes(y = .fitted), colour = "red")

practice <- ggplot(data = Ppoints,   
       mapping = aes(     
         x = total_practise_score,                       
         y = total_kscore,         
         color = total_kscore))+ 
  scale_y_continuous(expand = (expansion(0)),
                     limits = c(0, 16)) +
  scale_x_continuous(expand = (expansion(0)),
                     limits = c(0, 6)) +
  geom_point(
    colour = "#2c92e6",
    shape = "circle",      
    alpha = 3.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(                  
    method = "lm",              
    size = 2.5,
    color = "#b01515") +
  theme_minimal() +
  labs(title = "Knowledge Vs. Practice",
       y = "Knowledge Score",
       x = "Practice Score",
       color = " ") +
  theme(legend.position = " ") +
  theme(text = element_text(size = 34)) +
  theme(axis.title = element_text(size = 30))+
  theme(legend.text= element_text(size = 10)) +
  theme(plot.title = element_text(size = 37, face = "bold")) 

ggsave(practice, filename = "KNWOLEDGE_edited_size22.png",
       width=9, height=7)


# Make paned image --------------------------------------------------------

paned_image <- ggarrange(practice,
                         attitude,
                         labels = c("A", "B"),
                         font.label = 
                           list(size = 35,
                                colour = "black",
                                face = "bold"),
                         ncol = 1, nrow = 2)

ggsave(paned_image, filename = "paned2.png",
       width=12, height=18)

## Attitude and practise
lm_A <- lm(total_att_score ~ total_practise_score, data = final)
summary(lm_A)

Apoints <- augment(lm_A)

corr_AandP <- cor.test(x=final$total_att_score, y=final$total_practise_score,
                       method = 'spearman',
                       conf.level = TRUE)
corr_AandP

attitude <- ggplot(data = Apoints,   
       mapping = aes(     
         x = total_practise_score,                       
         y = total_att_score,         
         color = total_att_score))+ 
  scale_x_continuous(expand = (expansion(0)),
                               limits = c(0, 6))+
  geom_point(
    colour = "#2c92e6",
    shape = "circle",      
    alpha = 3.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(
    method = "lm", 
    size = 2.5,
    color = "#b01515") +
  theme_minimal() +
  labs(title = "Attitude Vs. Practice",
       y = "Attitude Score",
       x = "Practice Score",
       color = " ") +
  theme(legend.position = " ")+
  theme(text = element_text(size = 34)) +
  theme(axis.title = element_text(size = 30))+
  theme(legend.text= element_text(size = 10)) +
  theme(plot.title = element_text(size = 37, face = "bold"))


attitude

ggsave(attitude, filename = "att_edited_size_final1.png",
       width=9, height=7)


# Linear regression with information sources and knowledge  ---------------

final$all_sources_information <- as.numeric(final$all_sources_information)


# STOPPED HERE!!!! --------------------------------------------------------

## Knowledge and sources of information
lm_k_sources <- lm(total_kscore ~ all_sources_information, data = final)
summary(lm_k_sources)

corr_know <- cor.test(x=final$total_kscore, y=final$all_sources_information,
                       method = 'spearman',
                       conf.level = TRUE)
corr_know

KSpoints <- augment(lm_k_sources)
ggplot(KSpoints, aes(x = all_sources_information)) + 
  geom_point(aes(y = total_kscore)) + 
  geom_line(aes(y = .fitted), colour = "red")

knowledge11 <- ggplot(data = KSpoints,   
                   mapping = aes(     
                     x = all_sources_information,                       
                     y = total_kscore,         
                     color = all_sources_information))+     
  geom_point(             
    shape = "diamond",      
    alpha = 3.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(                  
    method = "lm",              
    size = 2.5,
    color = "#c77d77") +
  theme_minimal() +
  labs(title = "Knowledge Vs. Sources",
       y = "Knowledge Score",
       x = "Number of Information Sources",
       color = " ") +
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 34)) +
  theme(axis.title = element_text(size = 36))+
  theme(legend.text= element_text(size = 10)) +
  theme(plot.title = element_text(size = 40))

ggsave(knowledge11, filename = "know_1.png",
       width=9, height=7)

## Attitude and sources of information
lm_att_sources <- lm(total_att_score ~ all_sources_information, data = final)
summary(lm_att_sources)

corr_att <- cor.test(x=final$total_att_score, y=final$all_sources_information,
                      method = 'spearman',
                      conf.level = TRUE)
corr_att

ASpoints <- augment(lm_att_sources)
ggplot(ASpoints, aes(x = all_sources_information)) + 
  geom_point(aes(y = total_att_score)) + 
  geom_line(aes(y = .fitted), colour = "red")

attinfo <- ggplot(data = ASpoints,   
       mapping = aes(     
         x = all_sources_information,                       
         y = total_att_score,         
         color = all_sources_information))+     
  geom_point(             
    shape = "diamond",      
    alpha = 3.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(                  
    method = "lm",              
    size = 2.5,
    color = "#c77d77") +
  theme_minimal() +
  labs(title = "Attitude Vs. Sources",
       y = "Attitude Score",
       x = "Number of Information Sources",
       color = " ") +
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 34)) +
  theme(axis.title = element_text(size = 36))+
  theme(legend.text= element_text(size = 10)) +
  theme(plot.title = element_text(size = 40))

ggsave(attinfo, filename = "att_1.png",
       width=9, height=7)


## Practise and sources of information
lm_prac_sources <- lm(total_practise_score ~ all_sources_information,
                      data = final)
summary(lm_prac_sources)

corr_prac <- cor.test(x=final$total_practise_score,
                      y=final$all_sources_information,
                     method = 'spearman',
                     conf.level = TRUE)
corr_prac

PSpoints <- augment(lm_prac_sources)
ggplot(PSpoints, aes(x = all_sources_information)) + 
  geom_point(aes(y = total_practise_score)) + 
  geom_line(aes(y = .fitted), colour = "red")

practice <- ggplot(data = PSpoints,   
       mapping = aes(     
         x = all_sources_information,                       
         y = total_practise_score,         
         color = all_sources_information))+     
  geom_point(             
    shape = "diamond",      
    alpha = 3.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(                  
    method = "lm",              
    size = 2.5,
    color = "#c77d77") +
  theme_minimal() +
  labs(title = "Practice Vs. Sources",
       y = "Practise Score",
       x = "Number of Information Sources",
       color = " ") +
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 34)) +
  theme(axis.title = element_text(size = 36))+
  theme(legend.text= element_text(size = 10)) +
  theme(plot.title = element_text(size = 40))

ggsave(practice, filename = "prac_1.png",
       width=9, height=7)




# Linear regression curves ------------------------------------------------

install.packages("ggplot2")
library(ggplot2)

ggplot(KSpoints, aes(x = all_sources_information)) + 
  geom_point(aes(y = total_kscore)) + 
  geom_line(aes(y = .fitted), colour = "red")

ggplot(data = KSpoints, mapping = aes(x = all_sources_information,
                                      y = total_kscore))+  # set data and axes mapping
  geom_point(color = "darkgreen", size = 0.3, alpha = 0.7)+
  geom_line(aes(y = .fitted), colour = "blue") +
  theme_classic() +
  labs(title = "Knowledge Score Vs. Information Sources",
       y = "Knowledge Score",
       x = "Number of Information Sources")

ggplot(data = KSpoints,   
       mapping = aes(     
         x = all_sources_information,                       
         y = total_kscore,         
         color = all_sources_information))+     
  geom_point(             
    shape = "diamond",      
    alpha = 0.7,
    position=position_jitter(width=1,height=.1)) +
  geom_smooth(                  
      method = "lm",              
      size = 1.5,
      color = "#c77d77") +
  theme_minimal() +
  labs(title = "Knowledge Score Vs. Information Sources",
       y = "Knowledge Score",
       x = "Number of Information Sources",
       color = " ") +
  theme(legend.position = "bottom")



# Column graph for symptoms -----------------------------------------------

data <- data.frame(Percentage = c(91.9, 84.2, 66.0, 50.9, 46.6,
                                  44.1, 37.8, 31.3, 8.3, 6.3, 5.4),
                   Symptoms = c("Fever", "Cough", "Sore throat", "Sneezing",
                                "Anosmia", "Diarrhoea", "Rhinorrhea",
                                "Confusion","Hair loss", "Headache", "Myalgia"))
symptoms <- ggplot(data, 
                   aes(x = Symptoms, y = Percentage, 
                       fill = Symptoms)) +
  geom_bar(stat = "Identity", size = 0.5) +
  scale_fill_pizza() +
  theme_classic() +
  geom_col(width = .9, position = position_dodge(.3),
           color = "black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 100)) +
  labs(
    x = "Symptoms",
    y = "Percentage",
    title = "Main clinical symptoms of COVID-19") +
  theme(plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 25, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 18)),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(margin = margin(r = 22)),
    axis.ticks.x = element_blank(),
    legend.position = "none")

symptoms

ggsave(symptoms, filename = "symptoms11.png",
       width=20, height=12)




# ANALYSIS!!!!!!!!!!!!!!! IMPORTED FROM MPDSR -----------------------------




################################################END

# Note: all_sources_infomation: 
# real = coded ===> none = 2, 1 = 1, 2=3, 3=4. 4=5, 5=6, 6=7

k_modelinfo <- glm(knowledge_binary ~ govt + news + international + social + fora + journals, family = "binomial", data = final)
logistic.display(k_modelinfo)

k_modelatt <- glm(knowledge_binary ~ att_binary, family = "binomial", data = final)
logistic.display(k_modelatt)

oddratio.wald(final$knowledge_binary, final$att_binary)

oddsratio(final$knowledge_binary, final$`clean$real_timestamp`)



k_modelprac <- glm(knowledge_binary ~ practise_binary, family = "binomial", data = final)
logistic.display(k_modelprac)


# Factors associated with good attitude -----------------------------------

att_modelsex <- glm(att_binary ~ sex + age + work_place + education + cadre + all_sources_information + new_timestamp, family = "binomial", data = final)
logistic.display(att_modelsex)

oddsratio(final$att_binary, final$`clean$real_timestamp`)

# Note: all_sources_infomation: 
# real = coded ===> none = 2, 1 = 1, 2=3, 3=4. 4=5, 5=6, 6=7

att_modelinfo <- glm(att_binary ~ govt + news + international + social + fora + journals, family = "binomial", data = final)
logistic.display(att_modelinfo)


# Factors associated with good practise  ----------------------------------
prac_modelsex <- glm(practise_binary ~ sex + age + work_place + education + cadre + all_sources_information + `clean$real_timestamp`, family = "binomial", data = final)
logistic.display(prac_modelsex)

oddsratio(final$practise_binary, final$`clean$real_timestamp`)

# Note: all_sources_infomation: 
# real = coded ===> none = 2, 1 = 1, 2=3, 3=4. 4=5, 5=6, 6=7

prac_modelinfo <- glm(practise_binary ~ govt + news + international + social + fora + journals, family = "binomial", data = final)
logistic.display(prac_modelinfo)

install.packages("epitools")
library(epitools)

oddsratio.wald(final$practise_binary, final$social)



final %>%               
  tabyl(all_sources_information) %>%       
  adorn_pct_formatting()

online_good <- final %>%
  filter(final$real_timestamp == "online")

online_good %>%
  tabyl(knowledge_binary) %>%
  adorn_pct_formatting()

online_good %>%
  tabyl(international) %>%
  adorn_pct_formatting()

online_good %>%
  tabyl(news) %>%
  adorn_pct_formatting()

online_good %>%
  tabyl(fora) %>%
  adorn_pct_formatting()

online_good %>%
  tabyl(social) %>%
  adorn_pct_formatting()

online_good %>%
  tabyl(govt) %>%
  adorn_pct_formatting()

online_good %>%
  tabyl(journals) %>%
  adorn_pct_formatting()

online_good %>%
  tabyl(att_binary) %>%
  adorn_pct_formatting()

online_good %>%
  tabyl(practise_binary) %>%
  adorn_pct_formatting()

hardcopy_good <- final %>%
  filter(final$`clean$real_timestamp` == "hardcopy")

hardcopy_good %>%
  tabyl(knowledge_binary) %>%
  adorn_pct_formatting()

hardcopy_good %>%
  tabyl(att_binary) %>%
  adorn_pct_formatting()

hardcopy_good %>%
  tabyl(practise_binary) %>%
  adorn_pct_formatting()

hardcopy_good %>%
  tabyl(international) %>%
  adorn_pct_formatting()

hardcopy_good %>%
  tabyl(news) %>%
  adorn_pct_formatting()

hardcopy_good %>%
  tabyl(fora) %>%
  adorn_pct_formatting()

hardcopy_good %>%
  tabyl(social) %>%
  adorn_pct_formatting()

hardcopy_good %>%
  tabyl(govt) %>%
  adorn_pct_formatting()

hardcopy_good %>%
  tabyl(journals) %>%
  adorn_pct_formatting()

