### Clean data frame up

##pre: dac pulls from 2023 with full psychiatric ICD10 codes
##post: rds that contains beautifully curated all_icd10_dx column. 
## uses: Takes the dac pull, combines csvs, adds column with beautifully curated icd10 codes
## dependencies: dplyr

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)


######################
## FUNCTIONS #########
######################
####Extract unique ICD10 codes
### takes an empi, returns all the unique diagnostic codes.
### steps: 1) filter data frame by EMPI; 2) separate into 1 column per diagnosis; 3) remove doubles; 4) combine/paste into 1 column separated by semicolons; 5) return
get_all_unique_ICD10_diagnoses <- function(indiv_empi) {
  data_frame<-data_pull
  all_unique_diagnoses <- data_frame %>% 
    filter(str_detect(EMPI, indiv_empi)) %>% 
    dplyr::select(ICD10) %>%
    separate_rows(ICD10, sep = ";") %>%
    summarize(all_icd_codes = paste(unique(ICD10), collapse = ";"))
  #print(all_unique_diagnoses$all_icd_codes)
  return(all_unique_diagnoses$all_icd_codes)
}



######################
## CODE ##############
######################

#this goes through dac pulls, combines them, cleans them, and saves them as rds that can be loaded in other analyses. To clean up. 

#############################################################################################################################
### ***** MUST CHANGE THE HOMEDIR  AND THE PATHS TO DATA AND OUTPUT IN ORDER FOR THIS TO WORK ON A NEW PULL> THIS IS REALLY IMPORTANT ###
#############################################################################################################################

homedir <- "/Users/eballer/BBL/msanxiety/"

## output directory
output_file <- paste0(homedir, "/data/data_2023_pull_with_curated_icd10_codes.rds")

#data from dac pull with full icd10 lists
data_pt1 <- read.csv(paste0(homedir, "/data/dac/investigatingpatientsms2010to2023_full_psych_codes_pt1.csv"), sep = ",", header = TRUE) 
data_pt2 <- read.csv(paste0(homedir, "/data/dac/investigatingpatientsms2010to2023_full_psych_codes_pt2.csv"), sep = ",", header = TRUE) 

#dac pull with ms providers
dac_pull_2021_with_ms_providers <- read.csv(paste0(homedir, "/data/dac/investigatingdepressioninmspatients_dates_right_format.csv"), sep = ",", header = TRUE)
empi_and_provider <- dac_pull_2021_with_ms_providers %>% dplyr::select(EMPI, Provider) %>% mutate(EMPI = as.character(EMPI))

#msproviders
msproviders <- read.csv("/Users/eballer/BBL/general_emr_relevant_spreadsheets//msproviders_2024.csv", sep = ",", header = TRUE)

#data frame that only contains people with MS doctors
unique_empi_with_ms_providers <- empi_and_provider %>%
  mutate(Has.MS.Provider = ifelse(str_detect(Provider, paste(msproviders$Provider, collapse = "|")), 1, 0)) %>%
  filter(Has.MS.Provider == 1) %>%
  mutate(EMPI = as.character(EMPI)) %>%
  group_by(EMPI) %>%
  slice(1)

#combine data frames
data_pull <- rbind(data_pt1, data_pt2) %>% 
  mutate(EMPI = as.character(EMPI)) %>%
  mutate(all_icd10_dx = "NA")

#assemble new data frame and change names of ICD9/10 columns to reflect that they contain multiple dx

for(row in 1:dim(data_pull)[1]) {
  data_pull$all_icd10_dx[row] <- get_all_unique_ICD10_diagnoses(data_pull$EMPI[row])
}

columns_to_make_integer <- c("ACCESSION_NUM", "PAT_AGE_AT_EXAM","MRI_ENC_AGE","CURRENT_AGE","hemoglobin", "WBC","RBC","B12","FOLATE", "TSH", "RPR","CSFAPPEAR1","CSFAPPEAR4","CSFCOLOR1", "CSFCOLOR4","CSFTUBE","CSFTUBE4","PHQ.2","PHQ.9") #removed "VitD", not in new pull for some reason

#Get med info in
#read in BCP info, names of meds, est and prog and doses
bcp <- read.csv("/Users/eballer/BBL/medication_data/BCP_est_and_prog_levels_and_doses_all_brands.csv", sep = ",", header = TRUE)
bcp$bcp_name <- toupper(bcp$bcp_name)

#read in ms med info, names of meds, mechanism, and whether or not dmt
msmeds <- read.csv("/Users/eballer/BBL/medication_data/ms_medications_brand_and_generic.csv", sep = ",", header = TRUE)
msmeds$Medication <- toupper(msmeds$Medication)

#depression/psych meds from NAMI website 
psych_meds <- read.csv("/Users/eballer/BBL/medication_data/nami_psych_meds_antidepressants.csv", sep = ",", header = TRUE)
psych_meds$Medication.Name <- toupper(psych_meds$Medication.Name)

#depression/psych meds from NAMI website + neurontin, gabapentin, elavil, amitriptyline
#psych_meds_extended <- read.csv("/Users/eballer/BBL/medication_data/nami_psych_meds_antidepressants_plus_gabapentin_and_amitriptyline.csv", sep = ",", header = TRUE)

#psych meds from NAMI + medications used for anxiety (neurontin/gabapentin, elavil/amitriptyline, propranolol, nortriptyline/pamelor)
psych_meds_extended <- read.csv("/Users/eballer/BBL/medication_data/nami_psych_meds_antidepressants_plus_gabapentin_and_amitriptyline_benzos_beta_blockers.csv", sep = ",", header = TRUE)
psych_meds_extended$Medication.Name <- toupper(psych_meds_extended$Medication.Name)


#anxiety meds from chatgpt (cross referenced with psych meds)
anxiety_meds <- read.csv("/Users/eballer/BBL/medication_data/antianxiety_medications.csv", sep = ",", header = TRUE)
anxiety_meds$Medication <- toupper(anxiety_meds$Medication)
anxiety_meds_no_beta_blocker <- anxiety_meds %>% filter(Class != "Beta_blocker")

#get icd10 codes
icd10_codes <- read.csv("/Users/eballer/BBL/icd10_data/F-codes_01-99.csv", sep = ",", header = TRUE)

##########################
### some preprocessing ###
##########################
#we need to keep scans from people seen by an MS provider (n = 17067) who have an MS diagnostic code (n=16,830)
#we make a bunch of columns from character to integer, binarize sex and race, and put date into a nice format YYYYMMDD, the %m%d%y indicates what it started out as

#goal is to keep people who were seen by 
data_final_rds <-data_pull %>% 
  
  #clean demographics
  mutate(across(.cols = columns_to_make_integer, .fns = as.integer)) %>%
  mutate(sex_binarized = ifelse(SEX == "MALE", 1, 2)) %>%
  mutate(osex = ordered(sex_binarized,levels = c(1,2), labels = c("Male","Female"))) %>%
  mutate(race_binarized = ifelse(RACE == "WHITE", 1, 2)) %>%
  mutate(orace = ordered(race_binarized,levels = c(1,2), labels = c("White","Non-white"))) %>%
  mutate(EXAM_DATE = as.Date(BEGIN_EXAM_DTTM, format = "%m/%d/%y")) %>% 
  mutate(EXAM_DATE = gsub(EXAM_DATE, pattern = "-", replacement = "")) %>%
  mutate(BEGIN_EXAM_DTTM = as.Date(BEGIN_EXAM_DTTM, format = "%m/%d/%y")) %>% #make sure to actually save it as date object for later processing
  mutate(EMPI = as.factor(EMPI)) %>%
  
  #count number of diagnosis, meds, comorbidities
  mutate(number_of_dx = ifelse(!(is.na(ICD10) | ICD10 == "NULL"), str_count(ICD10, ";") + 1, 0)) %>% # new code
  mutate(number_of_dx_extended = ifelse(!(is.na(all_icd10_dx) | all_icd10_dx == "NULL"), str_count(all_icd10_dx, ";") + 1, 0)) %>% # new code
  mutate(number_of_psychiatric_comorbidities_extended = ifelse(!is.na(all_icd10_dx), str_count(all_icd10_dx, "F"), 0)) %>%
  mutate(number_of_anxiety_comorbidities_extended = ifelse(!is.na(all_icd10_dx), str_count(all_icd10_dx, "F4"), 0)) %>%
  mutate(number_of_mood_comorbidities_extended = ifelse(!is.na(all_icd10_dx), str_count(all_icd10_dx, "F3"), 0)) %>%
  mutate(number_of_medications = ifelse(!(is.na(Medication) | Medication == "NULL"), str_count(Medication, ";") + 1, 0)) %>% # new code
  
  #Create variables for who is on/off certain medications
  mutate(On.Bcp = ifelse(str_detect(Medication, paste(bcp$bcp_name, collapse = "|")), 1, 0)) %>%
  mutate(On.Psych.Meds = ifelse(str_detect(Medication, paste(psych_meds$Medication.Name, collapse = "|")), 1, 0)) %>%
  mutate(On.Psych.Meds.Extended = ifelse(str_detect(Medication, paste(psych_meds_extended$Medication.Name, collapse = "|")), 1, 0)) %>% #extended includes amitriptyline and gabapentin (and brands) in med list
  mutate(On.Antidepressants = ifelse(str_detect(Medication, paste(psych_meds$Medication[psych_meds$Antidepressant == "t"], collapse = "|")), 1, 0)) %>%
  mutate(On.Anxiolytics = ifelse(str_detect(Medication, paste(anxiety_meds$Medication, collapse = "|")), 1, 0)) %>%
  mutate(On.Anxiolytics_no_beta_blocker = ifelse(str_detect(Medication, paste(anxiety_meds_no_beta_blocker$Medication, collapse = "|")), 1, 0)) %>%
  mutate(On.Benzos = ifelse(str_detect(Medication, paste(anxiety_meds$Medication[anxiety_meds$Class == "Benzodiazepine"], collapse = "|")), 1, 0)) %>%
  mutate(On.Anti_cd20 = ifelse(str_detect(Medication, paste(msmeds$Medication[msmeds$Mechanism == "anti-cd20"], collapse = "|")), 1, 0)) %>%
  mutate(On.Interferon = ifelse(str_detect(Medication, paste(msmeds$Medication[msmeds$Mechanism == "Interferon"], collapse = "|")), 1, 0)) %>%
  mutate(On.Steroids = ifelse(str_detect(Medication, paste(msmeds$Medication[msmeds$Mechanism == "Steroid"], collapse = "|")), 1, 0)) %>%
  mutate(On.Dmt = ifelse(str_detect(Medication, paste(msmeds$Medication[msmeds$DMT == "y"], collapse = "|")), 1, 0)) %>%
  
  #Create variables based on PHQ2/9
  mutate(Has.PHQ2 = ifelse((PHQ.2 != "NULL" & !is.na(PHQ.2)), 1, 0)) %>%
  mutate(Has.PHQ9 = ifelse((PHQ.9 != "NULL" & !is.na(PHQ.9)), 1, 0)) %>%
  mutate(PHQ.2_modsev_dep_sxs = ifelse((Has.PHQ2==1) & PHQ.2 >= 3, 1, 0)) %>%
  mutate(PHQ.9_modsev_dep_sxs = ifelse((Has.PHQ9==1) & PHQ.9 >= 10, 1, 0)) %>%
  mutate(PHQ.2_mild_dep_sxs = ifelse((Has.PHQ2==1) & PHQ.2 > 0 & PHQ.2 < 3, 1, 0)) %>%
  mutate(PHQ.9_mild_dep_sxs = ifelse((Has.PHQ9==1) & PHQ.9 > 0 & PHQ.9 < 10, 1, 0)) %>%
  mutate(PHQ.2_zero = ifelse((Has.PHQ2==1) & PHQ.2 == 0, 1, 0)) %>%
  mutate(PHQ.9_zero = ifelse((Has.PHQ9==1) & PHQ.9 == 0, 1, 0)) %>%
  
  #Create variables based on psychiatric diagnoses
  mutate(Has.depdx = ifelse(grepl("F3[2:4]", all_icd10_dx), 1, 0)) %>%
  mutate(Has.anxietydx = ifelse(grepl("F4", all_icd10_dx), 1, 0)) %>%
  mutate(Has.CannabisUsedx = ifelse(grepl("F12", all_icd10_dx), 1, 0)) %>%
  mutate(Has.Anxietydx.Or.Antianxietymed = ifelse((Has.anxietydx==1) | (On.Anxiolytics==1),1,0)) %>%
  mutate(Has.Anxietydx.AND.Antianxietymed = ifelse((Has.anxietydx==1) & (On.Anxiolytics==1),1,0)) %>%
  mutate(dep_by_dx_phq = ifelse(((Has.depdx==1) | (PHQ.2_modsev_dep_sxs==1) | (PHQ.9_modsev_dep_sxs==1)),1,0)) %>%
  mutate(dep_by_dx_phq_antidep = ifelse(((Has.depdx==1) | (On.Antidepressants==1) | (PHQ.2_modsev_dep_sxs==1) | (PHQ.9_modsev_dep_sxs==1)),1,0)) %>%
  mutate(true_healthy = ifelse(((number_of_psychiatric_comorbidities_extended == 0) & (On.Psych.Meds.Extended==0) & ((Has.PHQ2==1 | Has.PHQ9==1) & ((PHQ.2_zero==1) | (PHQ.9_zero==1)))), 1, 0)) %>% # we can do this because we know that a person will either have a PHQ2 OR 9, not both
  mutate(dep_by_dx_phq_meds_healthy_phq0_no_psych_meds = ifelse((dep_by_dx_phq_antidep==1) | (true_healthy==1), 1, 0)) %>%
  
  #combine dataframes
  left_join(unique_empi_with_ms_providers, by = "EMPI") %>% 
  rowwise(ACCESSION_NUM) %>%
  
  #dep Diagnosis - for final analysis
  mutate(depGroupVar = ifelse(dep_by_dx_phq_antidep==1, 2, ifelse(dep_by_dx_phq_meds_healthy_phq0_no_psych_meds == 1, 1, 0))) %>% # dep = 2, healthy = 1, 0 for exclude
 
  #anxiety Diagnosis - for final analysis
  mutate(anxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed = ifelse((true_healthy==1), 1, ifelse((Has.Anxietydx.AND.Antianxietymed==1), 3, ifelse((On.Anxiolytics==1 | Has.anxietydx == 1), 2, 0)))) %>% #healthy = 1, anxiety alone + meds = 3, on.anxiolytics or anxiety meds = 2, unclassified = 0
  mutate(oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed = ordered(anxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed, levels = c(1,2,3,0), labels = c("true_healthy", "Has.Anxietydx.OR.Antianxiety.Meds", "Has.Anxiety.And.On.Anxiety.Meds.Inc.Dep", "unclassified"))) %>%
  
  # anxiety dose - for final analysis
  mutate(anxiety_dose = (as.numeric(anxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed) -1)) %>% #anxiety dose is 0 (no anxiety dx), 1 (anxietydx or anxiety meds), 2 (anxiety + anxiety meds), so take the factor and subtract 1
  ungroup() #n=4,957 unique people, n= 51,238 total



saveRDS(data_final_rds, file = output_file)


