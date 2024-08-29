require('visreg')
require('mgcv')
require('tableone')
require('dplyr')
require('plm')
require('MatchIt')
require('tidyr')
require('ggplot2')
require('reshape')
require('emmeans')
require('cowplot')
require('stringr')
require('visreg')
require('rasterVis')
require('lattice')
require('circlize')
## set directories ##
#local_wkdir <- '~/Google Drive/TDSlab/SCZ_gene_imaging/'
#remote_wkdir <- '~/Desktop/BBL/data/joy/BBL/studies/pnc/'

#############################
###Make Demographics Table###
#############################

#############################
####### Demographics ########
#############################

#### anxiety demographics

make_demographics_table_ms_anxiety_simple<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", 
                "Sex",
                "Age",
                "Has.anxietydx", 
                "On.Anxiolytics",
                "oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed",
                "anxiety_dose",
                "on_anti_cd20",
                "on_interferon",
                "on_steroids",
                "on_dmt",
                "PHQ2",
                "PHQ9",
                "volume_of_mimosa_lesions", 
                "mean_UF_vol",
                "proportion_volume_lost_per_total_network_size_sum") #Race 1 = caucasian, Sex 1 = M age = years
  
  
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  # for_parse <- paste0(data.frame(data_frame$race_binarized,
  for_parse <- paste0("data.frame(data_frame$RACE, 
                      data_frame$sex_binarized, 
                      data_frame$PAT_AGE_AT_EXAM, 
                      data_frame$Has.anxietydx, 
                      data_frame$On.Anxiolytics,
                      data_frame$oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed,
                      data_frame$anxiety_dose,
                      data_frame$On.Anti_cd20,
                      data_frame$On.Interferon,
                      data_frame$On.Steroids,
                      data_frame$On.Dmt,
                      data_frame$PHQ.2, 
                      data_frame$PHQ.9, 
                      data_frame$volume_of_mimosa_lesions, 
                      data_frame$mean_UF_vol,
                      data_frame$proportion_volume_lost_per_total_network_size_sum)")
  
  
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  # demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed", "Has.anxietydx","On.Anxiolytics","on_anti_cd20", "on_interferon", "on_steroids", "on_dmt")
  title <- c(paste0("Demographics_Anxiety"))
  
 
  #Groups - true healthy, anxiety OR anti anxiety meds, anxiety AND antianxiety meds
  demo_table_by_on_depAndAnxiety_true_healthy <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed"))
  print(demo_table_by_on_depAndAnxiety_true_healthy , showAllLevels = TRUE)
  
  
  return(demo_table_by_on_depAndAnxiety_true_healthy)
}

make_demographics_table_ms_w_promis_anxiety<- function(data_frame) {
  #subset demographics
  
  #subset demographics
  
  listVars <- c("Race", 
                "Sex",
                "Age",
                "Has.anxietydx", 
                "On.Anxiolytics",
                "oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed",
                "anxiety_dose",
                "on_anti_cd20",
                "on_interferon",
                "on_steroids",
                "on_dmt",
                "PHQ2",
                "PHQ9",
                "volume_of_mimosa_lesions", 
                "mean_UF_vol",
                "proportion_volume_lost_per_total_network_size_sum",
                "Quality_of_Life", 
                "Physical_Health", 
                "Mental_Health_and_Mood",
                "Social_Activities_Satisfaction",
                "Carrying_Out_Social_Activities",
                "Carrying_Out_Physical_Activities",
                "Emotional_Problems", 
                "Fatique_Average",
                "PostOp.PROMIS.Physical.Score",
                "PostOp.PROMIS.Mental.Score") #Race 1 = caucasian, Sex 1 = M age = years
  
  
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  # for_parse <- paste0(data.frame(data_frame$race_binarized,
  for_parse <- paste0("data.frame(data_frame$RACE, 
                      data_frame$sex_binarized, 
                      data_frame$PAT_AGE_AT_EXAM, 
                      data_frame$Has.anxietydx, 
                      data_frame$On.Anxiolytics,
                      data_frame$oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed,
                      data_frame$anxiety_dose,
                      data_frame$On.Anti_cd20,
                      data_frame$On.Interferon,
                      data_frame$On.Steroids,
                      data_frame$On.Dmt,
                      data_frame$PHQ.2, 
                      data_frame$PHQ.9, 
                      data_frame$volume_of_mimosa_lesions, 
                      data_frame$mean_UF_vol,
                      data_frame$proportion_volume_lost_per_total_network_size_sum,
                      data_frame$Quality_of_Life, 
                      data_frame$Physical_Health, 
                      data_frame$Mental_Health_and_Mood, 
                      data_frame$Social_Activities_Satisfaction,
                      data_frame$Carrying_Out_Social_Activities, 
                      data_frame$Carrying_Out_Physical_Activities, 
                      data_frame$Emotional_Problems, data_frame$Fatique_Average,
                      data_frame$PostOp.PROMIS.Physical.Score, 
                      data_frame$PostOp.PROMIS.Mental.Score)")
  
  
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  # demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed", "Has.anxietydx","On.Anxiolytics","on_anti_cd20", "on_interferon", "on_steroids", "on_dmt")
  title <- c(paste0("Demographics_Anxiety"))
  
  
  #Groups - true healthy, anxiety OR anti anxiety meds, anxiety AND antianxiety meds
  demo_table_by_on_depAndAnxiety_true_healthy <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed"))
  print(demo_table_by_on_depAndAnxiety_true_healthy , showAllLevels = TRUE)
  
  return(demo_table_by_on_depAndAnxiety_true_healthy)
}

make_demographics_table_ms_anxiety<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", 
                "Sex",
                "Age",
                "AnxietyGroupVar",
                "AnxietydxANDbenzoGroupVar",
                "Anxiety.And.Dep.GroupVar",
                "Has.Anxietydx.Or.Antianxietymed",
                "Has.Anxietydx.AND.Antianxietymed",
                "Has.Anxietydx.AND.AntianxietymedGroupVar",
                "oAnxiety.And.Dep.And.AnxietyMeds.TrueHealthy.GroupVar",
                "oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed",
                "Has.depdx", 
                "Has.anxietydx", 
                "On.Antidepressants",
                "On.Anxiolytics_no_beta_blocker",
                "On.Benzos",
                "on_anti_cd20",
                "on_interferon",
                "on_steroids",
                "on_dmt",
                "PHQ2",
                "PHQ9",
                "number_of_dx",
                "number_of_dx_extended",
                "number_of_psychiatric_comorbidities_extended",
                "number_of_anxiety_comorbidities_extended",
                "number_of_medications",
                "on_bcp",
                "volume_of_mimosa_lesions", 
                "sum_fascicle_vol_lost",
                "proportion_volume_lost_per_total_network_size_sum",
                "sum_fascicle_vol_lost_inanxietynet_all",
                "proportion_volume_lost_anxiety_net_by_network_size_sum",
                "sum_fascicle_vol_lost_inanxietynet_top_quartile",
                "proportion_volume_lost_anxiety_net_by_network_size_sum_top_25", 
                "sum_fascicle_vol_lost_nonanxiety_net_bottom_3_quartiles",
                "proportion_volume_lost_nonanxiety_net_by_network_size_sum_bottom_75",
                "proportion_volume_lost_depression_net_by_network_size_sum_top_25",
                "proportion_volume_lost_nondepression_net_by_network_size_sum_bottom_75",
                "proportion_fascicle_vol_lost_indep_and_anxiety_net_combined",
                "proportion_fascicle_vol_lost_indep_and_anxiety_net_overlap",
                "proportion_fascicle_vol_lost_indep_and_anxiety_net_overlap_no_CC",
                "proportion_fascicle_vol_lost_indepressionnet_no_anxiety_overlap",
                "proportion_fascicle_vol_lost_inanxietynet_no_depression_overlap") #Race 1 = caucasian, Sex 1 = M age = years

  
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  for_parse <- paste0("data.frame(data_frame$RACE, 
                      data_frame$sex_binarized, 
                      data_frame$PAT_AGE_AT_EXAM, 
                      data_frame$anxietyGroupVar,
                      data_frame$anxietydxANDbenzoGroupVar,
                      data_frame$Anxiety.And.Dep.GroupVar,
                      data_frame$Has.Anxietydx.Or.Antianxietymed,
                      data_frame$Has.Anxietydx.AND.Antianxietymed,
                      data_frame$Has.Anxietydx.AND.AntianxietymedGroupVar,
                      data_frame$oAnxiety.And.Dep.And.AnxietyMeds.TrueHealthy.GroupVar,
                      data_frame$oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed,
                      data_frame$Has.depdx, 
                      data_frame$Has.anxietydx, 
                      data_frame$On.Antidepressants,
                      data_frame$On.Anxiolytics_no_beta_blocker,
                      data_frame$On.Benzos,
                      data_frame$On.Anti_cd20,
                      data_frame$On.Interferon,
                      data_frame$On.Steroids,
                      data_frame$On.Dmt,
                      data_frame$PHQ.2, 
                      data_frame$PHQ.9, 
                      data_frame$number_of_dx, 
                      data_frame$number_of_dx_extended, 
                      data_frame$number_of_psychiatric_comorbidities_extended,
                      data_frame$number_of_anxiety_comorbidities_extended,
                      data_frame$number_of_medications, 
                      data_frame$On.Bcp, 
                      data_frame$volume_of_mimosa_lesions, 
                      data_frame$sum_fascicle_vol_lost, 
                      data_frame$proportion_volume_lost_per_total_network_size_sum,
                      data_frame$sum_fascicle_vol_lost_inanxietynet_all,
                      data_frame$proportion_volume_lost_anxiety_net_by_network_size_sum,
                      data_frame$sum_fascicle_vol_lost_inanxietynet_top_quartile, 
                      data_frame$proportion_volume_lost_anxiety_net_by_network_size_sum_top_25, 
                      data_frame$sum_fascicle_vol_lost_nonanxiety_net_bottom_3_quartiles, 
                      data_frame$proportion_volume_lost_nonanxiety_net_by_network_size_sum_bottom_75,
                      data_frame$proportion_volume_lost_depression_net_by_network_size_sum_top_25,
                      data_frame$proportion_volume_lost_nondepression_net_by_network_size_sum_bottom_75,
                      data_frame$proportion_fascicle_vol_lost_indep_and_anxiety_net_combined,
                      data_frame$proportion_fascicle_vol_lost_indep_and_anxiety_net_overlap,
                      data_frame$proportion_fascicle_vol_lost_indep_and_anxiety_net_overlap_no_CC,
                      data_frame$proportion_fascicle_vol_lost_indepressionnet_no_anxiety_overlap,
                      data_frame$proportion_fascicle_vol_lost_inanxietynet_no_depression_overlap)")
  

  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  # demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "AnxietyGroupVar", "AnxietydxANDbenzoGroupVar", "Has.Anxietydx.AND.AntianxietymedGroupVar", "oAnxiety.And.Dep.And.AnxietyMeds.TrueHealthy.GroupVar", "oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed", "Has.anxietydx","Anxiety.And.Dep.GroupVar", "Has.Anxietydx.Or.Antianxietymed", "On.Anxiolytics_no_beta_blocker","On.Benzos", "Has.depdx", "On.Antidepressants", "on_anti_cd20", "on_interferon", "on_steroids", "on_dmt", "on_bcp")
  title <- c(paste0("Demographics_Anxiety"))
  
  #create demographics table stratifying by anxiety Group Var
  demo_table_by_anxietyGroupVar <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("AnxietyGroupVar"))
  print(demo_table_by_anxietyGroupVar , showAllLevels = TRUE)
  
  #create demographics table stratifying by anxietydxANDbenzo Group Var
  demo_table_by_anxietyGroupVar <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("AnxietydxANDbenzoGroupVar"))
  print(demo_table_by_anxietyGroupVar , showAllLevels = TRUE)
  
  #create demographics table stratifying by having psych 
  demo_table_by_anxietydx <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Has.anxietydx"))
  print(demo_table_by_anxietydx , showAllLevels = TRUE)
  
  #create demographics table stratifying by being on anxiety meds
  demo_table_by_on_anxiolytics <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("On.Anxiolytics_no_beta_blocker"))
  print(demo_table_by_on_anxiolytics , showAllLevels = TRUE)
  
  #create demographics table stratifying by anxiety and dep Group var
  demo_table_by_on_depAndAnxiety <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Anxiety.And.Dep.GroupVar"))
  print(demo_table_by_on_depAndAnxiety , showAllLevels = TRUE)

  #create demographics table stratifying by on/off benzos
  demo_table_by_on_depAndAnxiety <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("On.Benzos"))
  print(demo_table_by_on_depAndAnxiety , showAllLevels = TRUE)
  
  #create demographics table stratifying by hasAnxietyDx and on Anxiety meds
  demo_table_by_on_depAndAnxiety <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Has.Anxietydx.AND.AntianxietymedGroupVar"))
  print(demo_table_by_on_depAndAnxiety , showAllLevels = TRUE)
  
  #create demographics table stratifying by hasAnxietyDx and on Anxiety meds, and true healthy rather than healthy_ish
  demo_table_by_on_depAndAnxiety_true_healthy <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("oAnxiety.And.Dep.And.AnxietyMeds.TrueHealthy.GroupVar"))
  print(demo_table_by_on_depAndAnxiety_true_healthy , showAllLevels = TRUE)
  
  #Groups - true healthy, anxiety OR anti anxiety meds, anxiety AND antianxiety meds
  demo_table_by_on_depAndAnxiety_true_healthy <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("oanxietydx_OR_meds_AND_Anxietydx.Meds.And.Dep.True.Healthy.Collapsed"))
  print(demo_table_by_on_depAndAnxiety_true_healthy , showAllLevels = TRUE)
  
  
}

#######Matched group all clusters ##############
make_demographics_table_ms_Hydra_k2<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", 
                "Sex",
                "Age",
                "Depression",
                "Hydra_k2",
                "PHQ2",
                "PHQ9",
                "Has.depdx", 
                "On.Antidepressants",
                "on_anti_cd20",
                "on_interferon",
                "on_steroids",
                "on_dmt",
                "number_of_dx",
                "number_of_dx_extended",
                "number_of_psychiatric_comorbidities_extended",
                "number_of_anxiety_comorbidities_extended",
                "number_of_medications",
                "on_bcp",
                "volume_of_mimosa_lesions", 
                "sum_fascicle_vol_lost",
                "proportion_volume_lost_per_total_network_size_sum",
                "sum_fascicle_vol_lost_indepnet_top_quartile", 
                "proportion_volume_lost_dep_net_by_network_size_sum",
                "sum_fascicle_vol_lost_nondep_bottom_3_quartiles", 
                "proportion_volume_lost_nondep_net_by_network_size_sum") #Race 1 = caucasian, Sex 1 = M age = years

  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  for_parse <- paste0("data.frame(data_frame$RACE, 
                      data_frame$sex_binarized, 
                      data_frame$PAT_AGE_AT_EXAM, 
                      data_frame$depGroupVar, 
                      data_frame$Hydra_k2, 
                      data_frame$PHQ.2, 
                      data_frame$PHQ.9, 
                      data_frame$Has.depdx, 
                      data_frame$On.Antidepressants,
                      data_frame$on_anti_cd20,
                      data_frame$on_interferon,
                      data_frame$on_steroids,
                      data_frame$on_dmt,
                      data_frame$number_of_dx, 
                      data_frame$number_of_dx_extended, 
                      data_frame$number_of_psychiatric_comorbidities_extended,
                      data_frame$number_of_anxiety_comorbidities_extended,
                      data_frame$number_of_medications, 
                      data_frame$on_bcp, 
                      data_frame$volume_of_mimosa_lesions, 
                      data_frame$sum_fascicle_vol_lost, 
                      data_frame$proportion_volume_lost_per_total_network_size_sum, 
                      data_frame$sum_fascicle_vol_lost_indepnet_top_quartile, 
                      data_frame$proportion_volume_lost_dep_net_by_network_size_sum, 
                      data_frame$sum_fascicle_vol_lost_nondep_bottom_3_quartiles, 
                      data_frame$proportion_volume_lost_nondep_net_by_network_size_sum)")
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  # demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "Depression", "Hydra_k2", "Has.depdx", "On.Antidepressants", "on_anti_cd20", "on_interferon", "on_steroids", "on_dmt", "on_bcp")
  title <- c(paste0("Hydra_k2 vs Clean Healthy Demographics"))
  
  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Hydra_k2"))
  print(demo_table, showAllLevels = TRUE)
  
}

make_demographics_table_ms_Hydra_k4<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", 
                "Sex",
                "Age",
                "Depression",
                "Hydra_k4",
                "PHQ2",
                "PHQ9",
                "Has.depdx", 
                "On.Antidepressants",
                "on_anti_cd20",
                "on_interferon",
                "on_steroids",
                "on_dmt",
                "number_of_dx",
                "number_of_dx_extended",
                "number_of_psychiatric_comorbidities_extended",
                "number_of_anxiety_comorbidities_extended",
                "number_of_medications",
                "on_bcp",
                "volume_of_mimosa_lesions", 
                "sum_fascicle_vol_lost",
                "proportion_volume_lost_per_total_network_size_sum",
                "sum_fascicle_vol_lost_indepnet_top_quartile", 
                "proportion_volume_lost_dep_net_by_network_size_sum",
                "sum_fascicle_vol_lost_nondep_bottom_3_quartiles", 
                "proportion_volume_lost_nondep_net_by_network_size_sum") #Race 1 = caucasian, Sex 1 = M age = years
  
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  for_parse <- paste0("data.frame(data_frame$RACE, 
                      data_frame$sex_binarized, 
                      data_frame$PAT_AGE_AT_EXAM, 
                      data_frame$depGroupVar, 
                      data_frame$Hydra_k4, 
                      data_frame$PHQ.2, 
                      data_frame$PHQ.9, 
                      data_frame$Has.depdx, 
                      data_frame$On.Antidepressants,
                      data_frame$on_anti_cd20,
                      data_frame$on_interferon,
                      data_frame$on_steroids,
                      data_frame$on_dmt,
                      data_frame$number_of_dx, 
                      data_frame$number_of_dx_extended, 
                      data_frame$number_of_psychiatric_comorbidities_extended,
                      data_frame$number_of_anxiety_comorbidities_extended,
                      data_frame$number_of_medications, 
                      data_frame$on_bcp, 
                      data_frame$volume_of_mimosa_lesions, 
                      data_frame$sum_fascicle_vol_lost, 
                      data_frame$proportion_volume_lost_per_total_network_size_sum, 
                      data_frame$sum_fascicle_vol_lost_indepnet_top_quartile, 
                      data_frame$proportion_volume_lost_dep_net_by_network_size_sum, 
                      data_frame$sum_fascicle_vol_lost_nondep_bottom_3_quartiles, 
                      data_frame$proportion_volume_lost_nondep_net_by_network_size_sum)")
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  # demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "Depression", "Hydra_k2", "Has.depdx", "On.Antidepressants", "on_anti_cd20", "on_interferon", "on_steroids", "on_dmt", "on_bcp")
  title <- c(paste0("Hydra_k4 vs Clean Healthy Demographics"))
  
  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Hydra_k4"))
  print(demo_table, showAllLevels = TRUE)
  
}

make_demographics_table_ms<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", "Sex", "Age", "Depression", "PHQ2", "PHQ9") #Race 1 = caucasian, Sex 1 = M age = years
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  for_parse <- paste0("data.frame(data_frame$RACE, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
 
  ### uncomment the line below if you want to binarize race
 # demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "Depression")
  title <- c(paste0("MS Depression vs Clean Healthy Demographics"))
  
  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Depression"))
  print(demo_table, showAllLevels = TRUE)
  
}


#make_demographics_table_ms<- function(data_frame) {
  #subset demographics
  
 # listVars <- c("Race", "Sex", "Age", "Depression", "PHQ2", "PHQ9") #Race 1 = caucasian, Sex 1 = M age = years
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
#  for_parse <- paste0("data.frame(data_frame$RACE, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
#  demo <- eval(parse(text = for_parse)) 
#  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  # demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
 # demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  #Define Categorical Variables
  #cat_variables <- c("Race", "Sex", "Depression")
  #title <- c(paste0("MS Depression vs Clean Healthy Demographics"))
  
  #create demographics table
#  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Depression"))
 # print(demo_table, showAllLevels = TRUE)
  
#}

make_demographics_table_ms_w_promis<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", "Sex", "Age", "Depression", "PHQ2", "PHQ9", "Quality_of_Life", "Physical_Health", "Mental_Health_and_Mood",
                "Social_Activities_Satisfaction", "Carrying_Out_Social_Activities", "Carrying_Out_Physical_Activities", "Emotional_Problems", 
                "Fatique_Average", "PostOp.PROMIS.Physical.Score", "PostOp.PROMIS.Mental.Score") #Race 1 = caucasian, Sex 1 = M age = years
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  for_parse <- paste0("data.frame(data_frame$RACE, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9,",
                      "data_frame$Quality_of_Life, data_frame$Physical_Health, data_frame$Mental_Health_and_Mood, data_frame$Social_Activities_Satisfaction,",
                      "data_frame$Carrying_Out_Social_Activities, data_frame$Carrying_Out_Physical_Activities, data_frame$Emotional_Problems, data_frame$Fatique_Average,",
                      "data_frame$PostOp.PROMIS.Physical.Score, data_frame$PostOp.PROMIS.Mental.Score)")
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  demo$Race <- ifelse(demo$Race == "WHITE", "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "Depression")
  title <- c(paste0("MS Depression vs Clean Healthy Demographics"))
  
  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Depression"))
  print(demo_table, showAllLevels = TRUE)
  

}



make_demographics_table_ms_w_promis_hydra_k2_simple<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", "Sex", "Age", "Hydra_k2", "PHQ2", "PHQ9", "Quality_of_Life", "Physical_Health", "Mental_Health_and_Mood",
                "Social_Activities_Satisfaction", "Carrying_Out_Social_Activities", "Carrying_Out_Physical_Activities", "Emotional_Problems", 
                "Fatique_Average", "PostOp.PROMIS.Physical.Score", "PostOp.PROMIS.Mental.Score") #Race 1 = caucasian, Sex 1 = M age = years
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  for_parse <- paste0("data.frame(data_frame$RACE, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$Hydra_k2, data_frame$PHQ.2, data_frame$PHQ.9,",
                      "data_frame$Quality_of_Life, data_frame$Physical_Health, data_frame$Mental_Health_and_Mood, data_frame$Social_Activities_Satisfaction,",
                      "data_frame$Carrying_Out_Social_Activities, data_frame$Carrying_Out_Physical_Activities, data_frame$Emotional_Problems, data_frame$Fatique_Average,",
                      "data_frame$PostOp.PROMIS.Physical.Score, data_frame$PostOp.PROMIS.Mental.Score)")
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  demo$Race <- ifelse(demo$Race == "WHITE", "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "Hydra_k2")
  title <- c(paste0("MS Depression vs Clean Healthy Demographics"))
  
  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Hydra_k2"))
  print(demo_table, showAllLevels = TRUE)
  
  
}


make_demographics_table_ms_w_promis_hydra_k4_simple<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", "Sex", "Age", "Hydra_k4", "PHQ2", "PHQ9", "Quality_of_Life", "Physical_Health", "Mental_Health_and_Mood",
                "Social_Activities_Satisfaction", "Carrying_Out_Social_Activities", "Carrying_Out_Physical_Activities", "Emotional_Problems", 
                "Fatique_Average", "PostOp.PROMIS.Physical.Score", "PostOp.PROMIS.Mental.Score") #Race 1 = caucasian, Sex 1 = M age = years
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  for_parse <- paste0("data.frame(data_frame$RACE, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$Hydra_k4, data_frame$PHQ.2, data_frame$PHQ.9,",
                      "data_frame$Quality_of_Life, data_frame$Physical_Health, data_frame$Mental_Health_and_Mood, data_frame$Social_Activities_Satisfaction,",
                      "data_frame$Carrying_Out_Social_Activities, data_frame$Carrying_Out_Physical_Activities, data_frame$Emotional_Problems, data_frame$Fatique_Average,",
                      "data_frame$PostOp.PROMIS.Physical.Score, data_frame$PostOp.PROMIS.Mental.Score)")
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  demo$Race <- ifelse(demo$Race == "WHITE", "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "Hydra_k4")
  title <- c(paste0("MS Depression vs Clean Healthy Demographics"))
  
  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Hydra_k4"))
  print(demo_table, showAllLevels = TRUE)
  
  
}


make_demographics_table_ms_w_promis_hydra_k2<- function(data_frame) {
  #subset demographics
  
  listVars <- c("Race", "Sex", "Age", "Depression", "Hydra_K2", "PHQ2", "PHQ9", "Quality_of_Life", "Physical_Health", "Mental_Health_and_Mood",
                "Social_Activities_Satisfaction", "Carrying_Out_Social_Activities", "Carrying_Out_Physical_Activities", "Emotional_Problems", 
                "Fatique_Average", "PostOp.PROMIS.Physical.Score", "PostOp.PROMIS.Mental.Score") #Race 1 = caucasian, Sex 1 = M age = years
  #for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$depGroupVar, data_frame$PHQ.2, data_frame$PHQ.9)")
  for_parse <- paste0("data.frame(data_frame$RACE, 
                      data_frame$sex_binarized, 
                      data_frame$PAT_AGE_AT_EXAM, 
                      data_frame$depGroupVar, 
                      data_frame$Hydra_k2, 
                      data_frame$PHQ.2, 
                      data_frame$PHQ.9, 
               #       data_frame$number_of_comorbidities, 
                #      data_frame$number_of_medications, 
                 #     data_frame$On.Antidepressants,
                  #    data_frame$on_bcp, 
                  #    data_frame$volume_of_mimosa_lesions, 
                   #   data_frame$sum_fascicle_vol_lost, 
                    #  data_frame$proportion_volume_lost_per_total_network_size_sum, 
                     # data_frame$sum_fascicle_vol_lost_indepnet_top_quartile, 
                      #data_frame$proportion_volume_lost_dep_net_by_network_size_sum, 
                      #data_frame$sum_fascicle_vol_lost_nondep_bottom_3_quartiles, 
                      #data_frame$proportion_volume_lost_nondep_net_by_network_size_sum,
                      data_frame$Quality_of_Life, data_frame$Physical_Health, 
                      data_frame$Mental_Health_and_Mood, data_frame$Social_Activities_Satisfaction,
                      data_frame$Carrying_Out_Social_Activities, 
                      data_frame$Carrying_Out_Physical_Activities, 
                      data_frame$Emotional_Problems, data_frame$Fatique_Average,
                      data_frame$PostOp.PROMIS.Physical.Score, 
                      data_frame$PostOp.PROMIS.Mental.Score)")
  demo <- eval(parse(text = for_parse)) 
  names(demo) <- c(listVars)
  
  #Change categorical values to have names
  
  ### uncomment the line below if you want to binarize race
  demo$Race <- ifelse(demo$Race == "WHITE", "Caucasian", "Non-caucasian")
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex", "Hydra_k2")
  title <- c(paste0("HYDRA_k2 vs Clean Healthy Demographics"))
  
  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Hydra_k2n"))
  print(demo_table, showAllLevels = TRUE)
  
}



make_demographics_table_all_subjs<- function(data_frame, binarized_race) {
  #makes demographics table but does NOT separate out by depression. Instead, separates out by sex
  listVars <- c("Race", "Sex", "Age", "PHQ2", "PHQ9") #Race 1 = caucasian, Sex 1 = M age = years
  
   #subset demographics
  if (binarized_race == 1) {
    #this means that we only want to do caucasion  vs non
    for_parse <- paste0("data.frame(data_frame$race_binarized, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$PHQ.2, data_frame$PHQ.9)")
    demo <- eval(parse(text = for_parse)) 
    names(demo) <- c(listVars)
    demo$Race <- ifelse(demo$Race == 1, "Caucasian", "Non-caucasian")
    } else {
    #list all races
    for_parse <- paste0("data.frame(data_frame$RACE, data_frame$sex_binarized, data_frame$PAT_AGE_AT_EXAM, data_frame$PHQ.2, data_frame$PHQ.9)")
    demo <- eval(parse(text = for_parse)) 
    names(demo) <- c(listVars)
  }
 
  
  #Change categorical values to have names
  demo$Sex <- ifelse(demo$Sex == 1, "Male", "Female")
  
  #Define Categorical Variables
  cat_variables <- c("Race", "Sex") 
  title <- c(paste0("MS Male versus Female Demographics"))
  
  #create demographics table
  demo_table <- CreateTableOne(vars = listVars, data = demo, factorVars = cat_variables, strata = c("Sex"))
  print(demo_table, showAllLevels = TRUE)
  
}

#######################
### Assign BCP Dose ###
#######################

assign_bcp_dose <- function(data_frame, bcp_data) {
  data_frame$est_dose <- -1
  for (subj in 1:dim(data_frame)[1]){ #loop through each subject
   
    bcp_found = FALSE #flag to identify whether bcp has been found in search data
    col = 58 #column where med list starts
    
    while (col <= 68 && !bcp_found) { #loop through columns with meds, 58:68
      
      bcp_cnt = 1 #set counter
   
      while (bcp_cnt <= dim(bcp_data)[1] && !bcp_found) { #loop through each BCP pill
      
  
        # if current med is a bcp, pull estrogen dose to be equivalent of est dose in table, and set flag to found - so there is no more search
        if (data_frame[subj,col] %in% bcp_data$bcp_name[bcp_cnt]) {
          data_frame$est_dose[subj] <- bcp_data$estrogen_dose_mcg[bcp_cnt]
          bcp_found = TRUE #set flag to true
        }
        bcp_cnt <- bcp_cnt + 1
      }
      col <- col + 1
    }
  }
  return(data_frame)
}

total_people_per_cluster <- function(data_frame, hydra_cluster)
{
  #returns vector with a number for each cluster
  if (hydra_cluster > 1 & hydra_cluster < 10) {
    length_controls <- eval(parse(text = paste0("length(which(data_frame$Hydra_k", hydra_cluster, " == -1))")))
    cluster_num_vector <- c(length_controls)
    for (cluster_counter in 1:hydra_cluster){
      length_cluster <- eval(parse(text = paste0("length(which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, "))")))
      cluster_num_vector <- c(cluster_num_vector, length_cluster)
    }
    return(cluster_num_vector)
  }
  else {
    print("Error: Cluster number must be between 2 and 10")
  }
}

total_people_per_cluster_by_group <- function(data_frame, variable, hydra_cluster, group_val)
{
  #returns vector with a number for each cluster per group
  #make sure variable is in quotes
  #group_val is the group you'd like. For exampke, if variable= sex and group_val = 1, it will send back number of men, 
       #if variable = race_binarized and group_val = 1, it will send back #caucasians
  if (hydra_cluster > 1 & hydra_cluster < 10) {
    length_controls <- eval(parse(text = paste0("length(which(data_frame$Hydra_k", hydra_cluster, " == -1 & data_frame$", variable, " == ", group_val, "))")))
    cluster_num_vector <- c(length_controls)
    for (cluster_counter in 1:hydra_cluster){
      length_cluster <- eval(parse(text = paste0("length(which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, " & data_frame$", variable, " == ", group_val, "))")))
      cluster_num_vector <- c(cluster_num_vector, length_cluster)
    }
    return(cluster_num_vector)
  }
  else {
    print("Error: Cluster number must be between 2 and 10")
  }
}

chi_sq <- function(data_frame, variable, hydra_cluster) {
  #make sure variable is in quotes
  chisq <- eval(parse(text = paste0("chisq.test(data_frame$", variable, ", data_frame$Hydra_k", hydra_cluster, ")")))
  return(chisq)
}

chi_sq_no_controls <- function(data_frame, variable, hydra_cluster)
{
  chisq <- eval(parse(text = paste0("chisq.test(data_frame$", variable, ", (data_frame$Hydra_k", hydra_cluster, " != -1))")))
  return(chisq)
}

chi_sq_p <- function(data_frame, variable, hydra_cluster) {
 #make sure variable is in quotes
  chisq <- eval(parse(text = paste0("chisq.test(data_frame$", variable, ", data_frame$Hydra_k", hydra_cluster, ")")))
  print(chisq)
  chisq_pvalue <- chisq$p.value
  return(chisq_pvalue)
}

get_cluster_titles <- function(hydra_cluster){
  # build vector of titles
  cluster_titles <- c("TD")
  for (cluster_counter in 1:hydra_cluster){
    title_to_add <- paste0("Subtype ", cluster_counter)
    cluster_titles <- c(cluster_titles, title_to_add)
  }
  return(cluster_titles)
}
#get_cluster_titles <- function(hydra_cluster){
  # build vector of titles
 # cluster_titles <- c("TD")
  #for (cluster_counter in 1:hydra_cluster){
   # title_to_add <- paste0("Cluster ", cluster_counter)
    #cluster_titles <- c(cluster_titles, title_to_add)
#  }
 # return(cluster_titles)
#}
get_cluster_titles_no_TD <- function(hydra_cluster){
  # build vector of titles
  cluster_titles <- NULL
  for (cluster_counter in 1:hydra_cluster){
    title_to_add <- paste0("Cluster ", cluster_counter)
    cluster_titles <- c(cluster_titles, title_to_add)
  }
  return(cluster_titles)
}

get_cluster_numerical_vector <- function(hydra_cluster){
  # build vector of titles
  cluster_vector <- c("-1")
  for (cluster_counter in 1:hydra_cluster){
    title_to_add <- paste0(cluster_counter)
    cluster_vector <- c(cluster_vector, title_to_add)
  }
  return(cluster_vector)
}
 
get_cluster_numerical_vector_no_TD <- function(hydra_cluster){
  # build vector of titles
  cluster_vector <- NULL
  for (cluster_counter in 1:hydra_cluster){
    title_to_add <- paste0(cluster_counter)
    cluster_vector <- c(cluster_vector, title_to_add)
  }
  return(cluster_vector)
}

get_variable_mean_vector <- function(data_frame, variable, hydra_cluster){
  #returns a vector of means, depends on # hydra clusters
  means <- eval(parse(text = paste0("c(mean(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == -1)]))")))
  for (cluster_counter in 1:hydra_cluster){
    mean_to_add <- eval(parse(text = paste0("c(mean(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")]))")))
    means <- c(means, mean_to_add)
  }
  return(means)
}

get_variable_mean_vector_no_TD <- function(data_frame, variable, hydra_cluster){
  #returns a vector of means, depends on # hydra clusters
  #means <- eval(parse(text = paste0("c(mean(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == -1)]))")))
  means <- NULL
  for (cluster_counter in 1:hydra_cluster){
    mean_to_add <- eval(parse(text = paste0("c(mean(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")]))")))
    means <- c(means, mean_to_add)
  }
  return(means)
}

get_variable_sd_vector <- function(data_frame, variable, hydra_cluster){
  #returns vector of sds, depends on # hydra clusters
  sds <- eval(parse(text = paste0("c(sd(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == -1)]))")))
  for (cluster_counter in 1:hydra_cluster){
    sd_to_add <- eval(parse(text = paste0("c(sd(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")]))")))
    sds <- c(sds, sd_to_add)
  }
  return(sds)
}

get_variable_sd_vector_no_TD <- function(data_frame, variable, hydra_cluster){
  #returns vector of sds, depends on # hydra clusters
  #sds <- eval(parse(text = paste0("c(sd(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == -1)]))")))
  sds <- NULL
  for (cluster_counter in 1:hydra_cluster){
    sd_to_add <- eval(parse(text = paste0("c(sd(data_frame$", variable, "[which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")]))")))
    sds <- c(sds, sd_to_add)
  }
  return(sds)
}

get_num_subj_per_cluster <- function(data_frame, hydra_cluster)
{
  nums <- eval(parse(text = paste0("c(length(which(data_frame$Hydra_k", hydra_cluster, " == -1)))")))
  for (cluster_counter in 1:hydra_cluster){
    num_to_add <- eval(parse(text = paste0("c(length(which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")))")))
    nums <- c(nums, num_to_add)
  }
  return(nums)
}

get_num_subj_per_cluster_no_TD <- function(data_frame, hydra_cluster)
{
 # nums <- eval(parse(text = paste0("c(length(which(data_frame$Hydra_k", hydra_cluster, " == -1)))")))
  nums <- NULL
  for (cluster_counter in 1:hydra_cluster){
    num_to_add <- eval(parse(text = paste0("c(length(which(data_frame$Hydra_k", hydra_cluster, " == ", cluster_counter, ")))")))
    nums <- c(nums, num_to_add)
  }
  return(nums)
}

data_frame_mean_sd_sem <- function(data_frame, variable, hydra_cluster){
  cluster_titles <- get_cluster_titles(hydra_cluster = hydra_cluster)
  variable_mean <- get_variable_mean_vector(data_frame = data_frame, variable = variable, hydra_cluster = hydra_cluster)
  variable_sd <- get_variable_sd_vector(data_frame = data_frame, variable = variable, hydra_cluster = hydra_cluster)
  num_per_cluster <- get_num_subj_per_cluster(data_frame = data_frame, hydra_cluster = hydra_cluster)
  variable_sem <- variable_sd/sqrt(num_per_cluster)
  
  #put all together in one data frame
  df_mean_sd_sem <- data.frame(cl = cluster_titles, mean = variable_mean, sd = variable_sd, sem = variable_sem)
  return(df_mean_sd_sem)
}

data_frame_mean_sd_sem_no_TD <- function(data_frame, variable, hydra_cluster){
  cluster_titles <- get_cluster_titles_no_TD(hydra_cluster = hydra_cluster)
  variable_mean <- get_variable_mean_vector_no_TD(data_frame = data_frame, variable = variable, hydra_cluster = hydra_cluster)
  variable_sd <- get_variable_sd_vector_no_TD(data_frame = data_frame, variable = variable, hydra_cluster = hydra_cluster)
  num_per_cluster <- get_num_subj_per_cluster_no_TD(data_frame = data_frame, hydra_cluster = hydra_cluster)
  variable_sem <- variable_sd/sqrt(num_per_cluster)
  
  #put all together in one data frame
  df_mean_sd_sem <- data.frame(cl = cluster_titles, mean = variable_mean, sd = variable_sd, sem = variable_sem)
  return(df_mean_sd_sem)
}



###### Plotting #####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot_continuous_variables <- function(data_frame, var1, var2, hydra_cluster, optional_variable_name_string){
#makes bar plots and line plot with error bars, specifically for plotting numerical things like age and medu. 
  #NOT for categorical variables like sex/race 
  
  num_total_groups <- hydra_cluster + 1
  cluster_titles <- get_cluster_titles(hydra_cluster = hydra_cluster)
  dat_var1_sd_sem <- data_frame_mean_sd_sem(data_frame = data_frame, variable = var1, hydra_cluster = hydra_cluster)
  dat_var2_sd_sem <- data_frame_mean_sd_sem(data_frame = data_frame, variable = var2, hydra_cluster = hydra_cluster)
  dat_var1_var2_sem_melted <- melt(c(dat_var1_sd_sem$sem,dat_var2_sd_sem$sem), id.vars = "cl")


  #get everything into the right format
  var1_and_var2 <- data.frame(cl=cluster_titles, var1 = dat_var1_sd_sem$mean, var2 = dat_var2_sd_sem$mean)
  var1_and_var2_for_plot <- melt(var1_and_var2, id.vars = "cl")
  var1_and_var2_for_plot$sem <- dat_var1_var2_sem_melted$value
  names(var1_and_var2_for_plot) <- c("cluster", "group", "years", "sem")
  
  #change names from var1 and var2 to their actual values
  if (!missing(optional_variable_name_string))
  {
    var1 = optional_variable_name_string[1]
    var2 = optional_variable_name_string[2]
  }
  
  replace_group_names <- c(rep(var1, num_total_groups), rep(var2, num_total_groups))
  #replace_group_names <- c(rep(var1, 4), rep(var2, 4))
  var1_and_var2_for_plot$group <- replace_group_names

  #plot
  title_of_plot <- paste0("Hydra_k", hydra_cluster, " ", var1, " and ", var2)
  p1 <- ggplot(data = var1_and_var2_for_plot, aes(x = group, y = years, group = cluster)) + 
    geom_line(aes(color=cluster)) +
    geom_point(aes(color=cluster)) + 
    geom_errorbar(aes(ymin=years-sem, ymax=years+sem), width=.1) +
    ggtitle(title_of_plot)

  p2 <- ggplot(dat_var1_sd_sem, aes(x = cl, y = mean, fill = cl)) + geom_col() + 
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2,position=position_dodge(.9)) + 
    scale_x_discrete(limits=cluster_titles) + ylim(0, 18) + xlab("Clusters") + ylab(paste0(var1, " in Years")) + 
    ggtitle(paste0(var1, " by Cluster")) + scale_fill_discrete(breaks=cluster_titles) +
    guides(fill=guide_legend(title=NULL)) 
  
  p3 <- ggplot(dat_var2_sd_sem, aes(x = cl, y = mean, fill = cl)) + geom_col() + 
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2,position=position_dodge(.9)) + 
    scale_x_discrete(limits=cluster_titles) + ylim(0, 18) + xlab("Clusters") + ylab(paste0(var2, " in Years")) + 
    ggtitle(paste0(var2, " by Cluster")) + scale_fill_discrete(breaks=cluster_titles) +
    guides(fill=guide_legend(title=NULL)) 
    
  #send back all three to the rmarkdown document that I called
  list_to_return <- list(p1, p2, p3)
   # multiplot(p1,p2,p3, cols=3)
  return(list_to_return)
  
}

plot_continuous_variables_no_ylim <- function(data_frame, var1, var2, hydra_cluster, optional_variable_name_string){
  #makes bar plots and line plot with error bars, specifically for plotting numerical things like age and medu. 
  #NOT for categorical variables like sex/race 
  #does NOT automatically set ylim
  
  num_total_groups <- hydra_cluster + 1
  cluster_titles <- get_cluster_titles(hydra_cluster = hydra_cluster)
  dat_var1_sd_sem <- data_frame_mean_sd_sem(data_frame = data_frame, variable = var1, hydra_cluster = hydra_cluster)
  dat_var2_sd_sem <- data_frame_mean_sd_sem(data_frame = data_frame, variable = var2, hydra_cluster = hydra_cluster)
  dat_var1_var2_sem_melted <- melt(c(dat_var1_sd_sem$sem,dat_var2_sd_sem$sem), id.vars = "cl")
  
  
  #get everything into the right format
  var1_and_var2 <- data.frame(cl=cluster_titles, var1 = dat_var1_sd_sem$mean, var2 = dat_var2_sd_sem$mean)
  var1_and_var2_for_plot <- melt(var1_and_var2, id.vars = "cl")
  var1_and_var2_for_plot$sem <- dat_var1_var2_sem_melted$value
  names(var1_and_var2_for_plot) <- c("cluster", "group", "years", "sem")
  
  #change names from var1 and var2 to their actual values
  if (!missing(optional_variable_name_string))
  {
    var1 = optional_variable_name_string[1]
    var2 = optional_variable_name_string[2]
  }
  
  replace_group_names <- c(rep(var1, num_total_groups), rep(var2, num_total_groups))
  #replace_group_names <- c(rep(var1, 4), rep(var2, 4))
  var1_and_var2_for_plot$group <- replace_group_names
  
  #plot
  title_of_plot <- paste0("Hydra_k", hydra_cluster, " ", var1, " and ", var2)
  p1 <- ggplot(data = var1_and_var2_for_plot, aes(x = group, y = years, group = cluster)) + 
    geom_line(aes(color=cluster)) +
    geom_point(aes(color=cluster)) + 
    geom_errorbar(aes(ymin=years-sem, ymax=years+sem), width=.1) +
    ggtitle(title_of_plot)
  
  p2 <- ggplot(dat_var1_sd_sem, aes(x = cl, y = mean, fill = cl)) + geom_col() + 
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2,position=position_dodge(.9)) + 
    scale_x_discrete(limits=cluster_titles) + xlab("Clusters") + ylab(paste0(var1, " in Years")) + 
    ggtitle(paste0(var1, " by Cluster")) + scale_fill_discrete(breaks=cluster_titles) +
    guides(fill=guide_legend(title=NULL)) 
  
  p3 <- ggplot(dat_var2_sd_sem, aes(x = cl, y = mean, fill = cl)) + geom_col() + 
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),width=.2,position=position_dodge(.9)) + 
    scale_x_discrete(limits=cluster_titles) + xlab("Clusters") + ylab(paste0(var2, " in Years")) + 
    ggtitle(paste0(var2, " by Cluster")) + scale_fill_discrete(breaks=cluster_titles) +
    guides(fill=guide_legend(title=NULL)) 
  
  #send back all three to the rmarkdown document that I called
  list_to_return <- list(p1, p2, p3)
  # multiplot(p1,p2,p3, cols=3)
  return(list_to_return)
  
}

better_levelplot <- function(adj, node_names, title) {
  adj_norm <- adj/max(abs(adj))
  limit = max(abs(adj_norm))
  #keycol=c('#FFFDE7','#FFFF00', '#F57F17', '#D50000',"#212121","#311B92","#2979FF","#B2EBF2")
  #keycol=c("d11225", "", "", "", "", "", "", "")
  plot<-levelplot(adj_norm, par.settings = RdBuTheme(region = brewer.pal(9, 'Reds')), #RdBuTheme(), 
                  colorkey=list(labels=list(cex=2,font=1,col="black")),
                  at = seq(limit,0,length.out = 12),xlab="",ylab = "", strip = F, contour = F, region= T,main=list(label=title,cex=3),
                  scales=list(x=list(at = 1:length(node_names), labels=node_names,rot=90, tck = 0, cex = 2),
                              y=list(at = 1:length(node_names),labels=node_names, tck = 0, cex = 2)))
  return(plot)
}

## better_level_plot ##
better_levelplot_edge <- function(adj, node_names_x, node_names_y, title) {
  adj_norm <- adj/max(abs(adj))
  limit = max(abs(adj_norm))
  keycol=c('#FFFDE7','#FFFF00', '#F57F17', '#D50000',"#212121","#311B92","#2979FF","#B2EBF2")
  plot<-levelplot(adj_norm, par.settings = BuRdTheme(), 
                  at = seq(limit,-limit,length.out = 12),xlab="",ylab = "", strip = F, contour = F, region= T,main=title,
                  scales=list(x=list(at = 1:length(node_names_x), labels=node_names_x,rot=90, tck = 0),
                              y=list(at = 1:length(node_names_y),labels=node_names_y, tck = 0)))
  return(plot)
}

better_chorDiagram_norm <- function(adj, node_names) {
  circos.clear()
  model_color=c('#FFFDE7','#FFFF00', '#F57F17', '#D50000',"#212121","#311B92","#2979FF","#B2EBF2")
  adj_norm <- adj/max(abs(adj))
  rownames(adj_norm) = node_names
  colnames(adj_norm) = node_names
  chordDiagram(adj_norm, directional = TRUE, transparency = 0.5, self.link = TRUE, grid.col = model_color) 
}

better_chorDiagram <- function(adj, node_names) {
  circos.clear()
  model_color=c('#CC2626','#CC7926', '#CCC326', '#26CCB8',"#2689CC","#3126CC","#A526CC","#CC2665")
  rownames(adj) = node_names
  colnames(adj) = node_names
  par(cex=2)
  chordDiagram(adj, directional = TRUE, transparency = 0.5, self.link = TRUE)#, grid.col = model_color) 
}

better_chorDiagram_13 <- function(adj, node_names) {
  circos.clear()
  model_color=c('#CC2626','#CC6D26', '#CCA526', '#C9CC26', '#8FCC26', '#36CC26', '#26CC8F', '#268CCC', '#262EEC', '#8426CC', '#CC26CC', '#CC2676', '#75696F')
  rownames(adj) = node_names
  colnames(adj) = node_names
  par(cex=1.5)
  chordDiagram(adj, directional = TRUE, transparency = 0.5, self.link = TRUE, grid.col = model_color) 
}

#data summary for violin plots
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}
#### Stats#####

### extract t, p, cohen f, effect size from gam
gam_summary_stats <- function(gam_model) {
  ### Extract T, P, Eff size, eff size size
  #t val
  t <- round(summary(gam_model)$p.t[2], 3)
  
  #
  p <- round(summary(gam_model)$p.p[2], 3)
  
  #f2 - R2/1-R2
  r2 <- summary(gam_model)$r.sq
  f2 <- round(r2/(1-r2), 3)
  
  #eff size summary
  eff_size = case_when(
    f2 >=0.35 ~ "large",
    f2 >= 0.15 & f2 < 0.35 ~ "medium",
    f2 >= 0.02 & f2 < 0.15 ~ "small",
    f2 >= 0 & f2 < 0.02 ~"minimal"
  )
  
  text = paste0("(T = ", t, ", P = ", p, ", Cohen's f2 =  ", f2, ", Effect size = ", eff_size, ")")
  return(text)
}

#for hydra k3
cohen_d_onepair <- function(data_frame, subtypeA, subtypeB, measure, hydra_cluster) {
  grpA_for_parse <- paste0("data_frame$", measure, "[which(data_frame$Hydra_k", hydra_cluster, " == ", subtypeA, ")]")
  grpA <- eval(parse(text = grpA_for_parse)) 
  grpB_for_parse <- paste0("data_frame$", measure, "[which(data_frame$Hydra_k", hydra_cluster, " == ", subtypeB, ")]")
  grpB <- eval(parse(text = grpB_for_parse)) 
  d <- c(grpA,grpB)
  grpA_name <- rep(c("grpA"), each=length(grpA))
  grpB_name <- rep(c("grpB"), each=length(grpB))
  f <- c(grpA_name, grpB_name)
  cohen_d <- cohen.d(grpA, grpB)
  ## data and factor
  #cohen.d(d,f)
  ## formula interface
  # cohen.d(d ~ f)
  ## compute Hedges' g
  hedges <- cohen.d(d,f,hedges.correction=TRUE)
  return(list(cohen_d, hedges))
}

cohen_d_allpairs <- function(data_frame, measure, hydra_cluster) {
  contrast_names <- get_cluster_numerical_vector(3)  
  contrast_pairs <- c("-1 - 1", "-1 - 2", "-1 - 3", "1 - 2", "1 - 3", "2 - 3") #just for Hydra k3 at this time
  eff_size_df <- data.frame(matrix("list", nrow = 8, ncol = 6))
  names(eff_size_df) <- contrast_pairs
  row.names(eff_size_df) <- c("cohen_d_est", 
                              "cohen_d_low", 
                              "cohen_d_upp",
                              "cohen_magn",
                              "hedges_est",
                              "hedges_low",
                              "hedges_upp",
                              "hedges_mag")
  contrast_count <- 1
  for (i in 1:(hydra_cluster + 1)) {
    for (j in 2:(hydra_cluster + 1)) {
        if (i < j) {
           results <- cohen_d_onepair(data_frame = data_frame, 
                                         subtypeA = contrast_names[i], 
                                         subtypeB = contrast_names[j],
                                         measure = measure, 
                                         hydra_cluster = hydra_cluster)
          eff_size_df[,contrast_count] <- c(results[[1]]$estimate, results[[1]]$conf.int[1], results[[1]]$conf.int[2], results[[1]]$magnitude,
                                            results[[2]]$estimate, results[[2]]$conf.int[1], results[[2]]$conf.int[2], results[[2]]$magnitude)
          contrast_count <- contrast_count + 1
        }
      }
  }
  return(eff_size_df)
  
}


fdr_anova <- function(data_frame, print_unc_df = FALSE) {
  models_anova <- lapply(data_frame, summary)
  
  #Pull p-values
  p_anova <- sapply(data_frame, function(v) v$"Pr(>F)"[2]) #$coef[,"Pr(>F)"][2]) #get the p value for dep binarized
  
  #Convert to data frame
  p_anova <- as.data.frame(p_anova)
  
  #print BEFORE FDR correction 
  #print("Anova scores, BEFORE FDR correction: i.e., uncorrected")
  if (print_unc_df == TRUE){
    print(p_anova)
  }
  
  #Print original p-values to three decimal places
  p_round_anova <- round(p_anova,3)
  
  #FDR correct p-values
  pfdr_anova <- p.adjust(p_anova[,1],method="fdr")
  
  #Convert to data frame
  pfdr_anova <- as.data.frame(pfdr_anova)
  row.names(pfdr_anova) <- names(data_frame)
  
  #To print fdr-corrected p-values to three decimal places
  pfdr_round_anova <- round(pfdr_anova,3)
  
  #List the components that survive FDR correction
  components_fdr_anova <- row.names(pfdr_anova)[pfdr_anova<0.05]
  
  #make a data frame with names and fdr values (rounded to 3 decimals)
  names_and_fdr_values_anova <- data.frame(cbind(components_fdr_anova, round(pfdr_anova[pfdr_anova<0.05],3)))
  
  #add titles to names_and_fdr tables
  names(names_and_fdr_values_anova) <- c("component", "p_FDR_corr")
  
 # print("Mean centered age that was then squared, FDR corrected")
  #print(names_and_fdr_values_anova)
  return(names_and_fdr_values_anova)
  
}

fdr_anova_generic <- function(data_frame, item_index, print_unc_df = FALSE) {
  models_anova <- lapply(data_frame, summary)
  
  #Pull p-values
  p_anova <- sapply(data_frame, function(v) v$"Pr(>F)"[item_index]) #$coef[,"Pr(>F)"][2]) #get the p value for dep binarized
  
  #Convert to data frame
  p_anova <- as.data.frame(p_anova)
  
  #print BEFORE FDR correction 
  #print("Anova scores, BEFORE FDR correction: i.e., uncorrected")
  if(print_unc_df == TRUE) {
    print(p_anova)
  }
  
  #Print original p-values to three decimal places
  p_round_anova <- round(p_anova,3)
  
  #FDR correct p-values
  pfdr_anova <- p.adjust(p_anova[,1],method="fdr")
  
  #Convert to data frame
  pfdr_anova <- as.data.frame(pfdr_anova)
  row.names(pfdr_anova) <- names(data_frame)
  
  #To print fdr-corrected p-values to three decimal places
  pfdr_round_anova <- round(pfdr_anova,3)
  
  #List the components that survive FDR correction
  components_fdr_anova <- row.names(pfdr_anova)[pfdr_anova<0.05]
  
  #make a data frame with names and fdr values (rounded to 3 decimals)
  names_and_fdr_values_anova <- data.frame(cbind(components_fdr_anova, round(pfdr_anova[pfdr_anova<0.05],3)))
  
  #add titles to names_and_fdr tables
  names(names_and_fdr_values_anova) <- c("component", "p_FDR_corr")
  
  # print("Mean centered age that was then squared, FDR corrected")
  #print(names_and_fdr_values_anova)
  return(names_and_fdr_values_anova)
  
}

fdr_anova_generic_p_already_extracted <- function(data_frame) {
#  models_anova <- lapply(data_frame, summary)
  
  #Pull p-values
  p_anova <- data_frame
  
  #Convert to data frame
  p_anova <- as.data.frame(p_anova)
  
  #print BEFORE FDR correction 
  #print("Anova scores, BEFORE FDR correction: i.e., uncorrected")
  print(p_anova)
  
  #Print original p-values to three decimal places
  p_round_anova <- round(p_anova,3)
  
  #FDR correct p-values
  pfdr_anova <- p.adjust(p_anova[,1],method="fdr")
  
  #Convert to data frame
  pfdr_anova <- as.data.frame(pfdr_anova)
  row.names(pfdr_anova) <- names(data_frame)
  
  #To print fdr-corrected p-values to three decimal places
  pfdr_round_anova <- round(pfdr_anova,3)
  
  #List the components that survive FDR correction
  components_fdr_anova <- row.names(pfdr_anova)[pfdr_anova<0.05]
  
  #make a data frame with names and fdr values (rounded to 3 decimals)
  names_and_fdr_values_anova <- data.frame(cbind(components_fdr_anova, round(pfdr_anova[pfdr_anova<0.05],3)))
  
  #add titles to names_and_fdr tables
  names(names_and_fdr_values_anova) <- c("component", "p_FDR_corr")
  
  # print("Mean centered age that was then squared, FDR corrected")
  #print(names_and_fdr_values_anova)
  return(names_and_fdr_values_anova)
  
}

pairwise_contrasts_3clusters <- function(data_frame_lm, fdr_anova) {
 #for 3 clusters
  print(fdr_anova)
  emmodel_df <- lapply(data_frame_lm, function(x) {as.list(ref_grid(x))})
  emgrid_df <- lapply(emmodel_df, function(x) {as.emmGrid(x)})
  
  #run emmeans
  emmeans_df <- lapply(emgrid_df, function(x) {emmeans(x, "Hydra_k3")})
  
  #run pairwise contrasts
  empairs_df <- lapply(emmeans_df, function(x) {pairs(x)})
  

  #Only include stuff that was fdr corrected (i.e., only keep parts of the model (or only display) ones that are corrected),this will be null if nothing was corrected
  empairs_FDR_corrected <- empairs_df[fdr_anova[,1]]
  
  print(empairs_FDR_corrected)
  #contrast names, -1 = controls, 1-3 are clusters
  contrast_names <- c("-1 - 1", "-1 - 2", "-1 - 3", "1 - 2", "1 - 3", "2 - 3")
  #go through each fdr corrected brain region, and extract p values
  #contrast_table <- lapply(fdr_anova[,1], function(x) {round(summary(x)$p.value,3)})
  contrast_table <- lapply(empairs_FDR_corrected, function(x) {round(summary(x)$p.value,3)})
  #get the names of the brain regions that were fdr corrected
  brain_regions <- names(contrast_table)
  #build table that will hold the name of the brain region and the p values
  pairwise_table <- data.frame(matrix(nrow = length(brain_regions), ncol = 6))
  #give the appropriate names
  rownames(pairwise_table) <- brain_regions
  colnames(pairwise_table) <- contrast_names
  
  #loop through each brain region, and manually assign the columns to be the p values
  for (region in brain_regions)
  {
    pair_pval <- contrast_table[[region]]
    pairwise_table[region,] <- pair_pval
  }
  pairwise_table_with_fdr <- pairwise_table
  pairwise_table_with_fdr$p_FDR_corr <- fdr_anova$p_FDR_corr
  
  pairwise <- list(empairs_df, empairs_FDR_corrected, pairwise_table, pairwise_table_with_fdr)
  return(pairwise)
  
}

make_pairwise_contrast_names <- function(num_clusters) {
  #get the numerical vector per number of clusters, will loop through them, and generate vector with all pairs
  clusters_vector1 <- get_cluster_numerical_vector(hydra_cluster = num_clusters)
  clusters_vector2 <- clusters_vector1
  pairs_vector <- NULL
  for(pair1 in clusters_vector1){
    for(pair2 in clusters_vector2)
      if (pair1 < pair2) {
        pair_text <- paste0(pair1, " - ", pair2)
        pairs_vector <- c(pairs_vector, pair_text)
      }
  }
  return(pairs_vector)
}


pairwise_contrasts_generic_num_clusters <- function(data_frame_lm, fdr_anova, num_clusters) {
  #for 3 clusters
  print(fdr_anova)
  emmodel_df <- lapply(data_frame_lm, function(x) {as.list(ref_grid(x))})
  emgrid_df <- lapply(emmodel_df, function(x) {as.emmGrid(x)})
  
  #run emmeans
  hydra_var <- paste0("Hydra_k", num_clusters)
  emmeans_df <- lapply(emgrid_df, function(x) {emmeans(x, hydra_var)})
  
  #run pairwise contrasts
  empairs_df <- lapply(emmeans_df, function(x) {pairs(x)})
  
  
  #Only include stuff that was fdr corrected (i.e., only keep parts of the model (or only display) ones that are corrected),this will be null if nothing was corrected
  empairs_FDR_corrected <- empairs_df[fdr_anova[,1]]
  
 # print(empairs_FDR_corrected)
  #contrast names, -1 = controls, other numbers are clusters
  contrast_names <- make_pairwise_contrast_names(num_clusters = num_clusters)
  #go through each fdr corrected brain region, and extract p values
  #contrast_table <- lapply(fdr_anova[,1], function(x) {round(summary(x)$p.value,3)})
  contrast_table <- lapply(empairs_FDR_corrected, function(x) {round(summary(x)$p.value,3)})
  #get the names of the brain regions that were fdr corrected
  brain_regions <- names(contrast_table)
  #build table that will hold the name of the brain region and the p values
  #number of columns (or number of unique pairs is)
  pairwise_table <- data.frame(matrix(nrow = length(brain_regions), ncol = length(contrast_names)))
  #give the appropriate names
  rownames(pairwise_table) <- brain_regions
  colnames(pairwise_table) <- contrast_names
  
  #loop through each brain region, and manually assign the columns to be the p values
  for (region in brain_regions)
  {
    pair_pval <- contrast_table[[region]]
    pairwise_table[region,] <- pair_pval
  }
  pairwise_table_with_fdr <- pairwise_table
  pairwise_table_with_fdr$p_FDR_corr <- fdr_anova$p_FDR_corr
  
  pairwise <- list(empairs_df, empairs_FDR_corrected, pairwise_table, pairwise_table_with_fdr)
  return(pairwise)
  
}



############## Get community names #############
get_community_names <- function(num_communities) {
  #takes a number of communities, returns a list of each community, within, and between network labels
  if (num_communities == 7) {
    com_names <- c("visual", "somatomotor", "dorsalAttention", "salienceVentralAttention", "limbic", "frontoparietalControl", "default")
    within <- c("visual_visual", "somatomotor_somatomotor", "dorsalAttention_dorsalAttention", "salienceVentralAttention_salienceVentralAttention", "limbic_limbic", "frontoparietalControl_frontoparietalControl", "default_default")
    between <- c("visual_somatomotor", "visual_dorsalAttention", "somatomotor_dorsalAttention", "visual_salienceVentralAttention", "somatomotor_salienceVentralAttention", "dorsalAttention_salienceVentralAttention", "visual_limbic", "somatomotor_limbic", "dorsalAttention_limbic",                       
    "salienceVentralAttention_limbic", "visual_frontoparietalControl",                  
    "somatomotor_frontoparietalControl", "dorsalAttention_frontoparietalControl",         
    "salienceVentralAttention_frontoparietalControl", "limbic_frontoparietalControl",                  
    "visual_default", "somatomotor_default",                          
    "dorsalAttention_default", "salienceVentralAttention_default",              
    "limbic_default", "frontoparietalControl_default")     
     } else if (num_communities == 17) {
       networks <- read.csv("/Users/eballer/BBL/from_chead/ballerDepHeterogen/results/csvs/community_names.csv")
       com_names <- networks[2]
       com_names <- c(sapply(com_names, as.character))
       within <- networks[2]
       within <- c(sapply(within, as.character))
      
      #make list of between
       num_between <- (num_communities * (num_communities -1))/2
       between <- array(data = "NA", dim = num_between)
       nrow <- 1
       num_netA <- 1
       num_netB <- 1
       for (network_a in within) {
         for (network_b in within) {
           #make sure you aren't adding betworks that are either WITHIN, or have already been added to the matrix
           if ((network_a != network_b) & (num_netA < num_netB)) {
              between[nrow] <- paste0(network_a, "_", network_b)
              nrow <- nrow + 1
           }
           num_netB <- num_netB + 1
         }
         num_netA <- num_netA + 1
         num_netB <- 1
       }
       between <- c(sapply(between, as.character))
       #make the character lists of within networks, aka visualCentral_visualCentral
       for (row in 1:length(within)) {
         within[row] <- paste0(within[row], "_", within[row])
       }
        
  } else {
    com_names <- c("error")
    within <- c("error")
    between <- c("error")
  }
  all_names<- list(com_names, within, between)
  return(all_names)
}
#-------------
get_fascicle_bundle_mapping <- function(){
  # returns vector with association (34 tracts) = 1; cerebellum (7 t) = 2
  # cranial nerves(10) = 3; projection (34) = 4; commissural (2) = 5
  dep_network_grouping <- read.csv("/Users/eballer/BBL/msdepression//templates/dti/HCP_YA1065_tractography/fascicle_names_dep_network_grouping.csv", header = T, sep = ",")
  yeo_7_network <- fascicle_volumes_yeo_7_binarized <- read.csv("/Users/eballer/BBL/msdepression/results/yeo_7_network_overlap_proportions.txt", header = T, sep = ",")
  yeo_7_network_binarized <- fascicle_volumes_yeo_7_binarized <- read.csv("/Users/eballer/BBL/msdepression//results/yeo_7_network_overlap_proportions_binarized.txt", header = T, sep = ",")
  numerical_vector <- c(rep(1,34), rep(2,7), rep(3,10), rep(4,34), rep(5,2))
  name_vector <- c(rep("association",34), 
                   rep("cerebellum",7), 
                   rep("cranial_nerve",10), 
                   rep("projection",34), 
                   rep("commissural",2))
  inDepNetwork_vector <- dep_network_grouping$inDepMask 
  fascicle_bundle_list <- list(numerical_vector, 
                               name_vector, 
                               inDepNetwork_vector,
                               yeo_7_network$prop_in_mask_VIS, 
                               yeo_7_network$prop_in_mask_MOT, 
                               yeo_7_network$prop_in_mask_DA, 
                               yeo_7_network$prop_in_mask_VA,
                               yeo_7_network$prop_in_mask_LIM,
                               yeo_7_network$prop_in_mask_FP,
                               yeo_7_network$prop_in_mask_DM,
                               yeo_7_network_binarized$VIS, 
                               yeo_7_network_binarized$MOT, 
                               yeo_7_network_binarized$DA, 
                               yeo_7_network_binarized$VA,
                               yeo_7_network_binarized$LIM,
                               yeo_7_network_binarized$FP,
                               yeo_7_network_binarized$DM)
  names(fascicle_bundle_list) <- c("numerical_vector", 
                                   "name_vector", 
                                   "inDepNetwork_vector", 
                                   "prop_VIS",
                                   "prop_MOT",
                                   "prop_DA",
                                   "prop_VA",
                                   "prop_LIM",
                                   "prop_FP",
                                   "prop_DM",
                                   "VIS",
                                   "MOT",
                                   "DA",
                                   "VA",
                                   "LIM",
                                   "FP",
                                   "DM")
  return(fascicle_bundle_list)
}

get_fascicle_bundle_mapping_generic_dep_mask <- function(dep_network_grouping_file){
  # returns vector with association (34 tracts) = 1; cerebellum (7 t) = 2
  # cranial nerves(10) = 3; projection (34) = 4; commissural (2) = 5
  # allows for generic depression mask to be added
  dep_network_grouping <- read.csv(file = dep_network_grouping_file, header = T, sep = ",")
  yeo_7_network <- fascicle_volumes_yeo_7_binarized <- read.csv(paste0(homedir, "results/yeo_7_network_overlap_proportions.txt"), header = T, sep = ",")
  yeo_7_network_binarized <- fascicle_volumes_yeo_7_binarized <- read.csv(paste0(homedir, "results/yeo_7_network_overlap_proportions_binarized.txt"), header = T, sep = ",")
  numerical_vector <- c(rep(1,34), rep(2,7), rep(3,10), rep(4,34), rep(5,2))
  name_vector <- c(rep("association",34), 
                   rep("cerebellum",7), 
                   rep("cranial_nerve",10), 
                   rep("projection",34), 
                   rep("commissural",2))
  inDepNetwork_vector <- dep_network_grouping$inDepMask 
  fascicle_bundle_list <- list(numerical_vector, 
                               name_vector, 
                               inDepNetwork_vector,
                               yeo_7_network$prop_in_mask_VIS, 
                               yeo_7_network$prop_in_mask_MOT, 
                               yeo_7_network$prop_in_mask_DA, 
                               yeo_7_network$prop_in_mask_VA,
                               yeo_7_network$prop_in_mask_LIM,
                               yeo_7_network$prop_in_mask_FP,
                               yeo_7_network$prop_in_mask_DM,
                               yeo_7_network_binarized$VIS, 
                               yeo_7_network_binarized$MOT, 
                               yeo_7_network_binarized$DA, 
                               yeo_7_network_binarized$VA,
                               yeo_7_network_binarized$LIM,
                               yeo_7_network_binarized$FP,
                               yeo_7_network_binarized$DM)
  names(fascicle_bundle_list) <- c("numerical_vector", 
                                   "name_vector", 
                                   "inDepNetwork_vector", 
                                   "prop_VIS",
                                   "prop_MOT",
                                   "prop_DA",
                                   "prop_VA",
                                   "prop_LIM",
                                   "prop_FP",
                                   "prop_DM",
                                   "VIS",
                                   "MOT",
                                   "DA",
                                   "VA",
                                   "LIM",
                                   "FP",
                                   "DM")
  return(fascicle_bundle_list)
}

#####
decrease_saturation <- function(hex_color, scale = 0.5){
  #takes a color, decreases the saturation, and returns a new hex color
  hsv <- rgb2hsv(col2rgb(hex_color))
  hsv[2] <- hsv[2] * scale
  new_color <- hsv2hex(hsv)
  return(new_color)
}

violin_plot_means <- function (homedir, network_names, data_frame){ #, max, subtype){
  #dataframe contains names of networks, mean injury per network, and subtypes, max is 
  #for storing statistics at the end
  #lh_spin_df <- data.frame(read.table(paste0(homedir, "/baller/results/coupling_accuracy//spin_test_results/lh_spin_test_mean_coupling.csv"), sep = ","))
  #rh_spin_df <- data.frame(read.table(paste0(homedir, "/baller/results/coupling_accuracy//spin_test_results/rh_spin_test_mean_coupling.csv"), sep = ","))
  
  #get yeo colors
  yeo_colors <- get_yeo7_colors()
  
  #remove the _net stuff from after the name of the network
  data_frame <- data_frame %>% 
    mutate(Network = gsub("(.*)_.*_net", "\\1", Network))

  #Make dfs for each subtype
  data_frame_s1 <- data_frame %>%
    filter(Hydra_k2 == 1)
  
  data_frame_s2 <- data_frame %>%
    filter(Hydra_k2 == 2)
  
  #take means of left and right
  actual_results_s1<- data.frame(mean(data_frame$prop_affected[(data_frame$Network == network_names[1]) & (data_frame$Hydra_k2==1)]),
                              mean(data_frame$prop_affected[(data_frame$Network == network_names[2]) & (data_frame$Hydra_k2==1)]),
                              mean(data_frame$prop_affected[(data_frame$Network == network_names[3]) & (data_frame$Hydra_k2==1)]),
                              mean(data_frame$prop_affected[(data_frame$Network == network_names[4]) & (data_frame$Hydra_k2==1)]),
                              mean(data_frame$prop_affected[(data_frame$Network == network_names[5]) & (data_frame$Hydra_k2==1)]),
                              mean(data_frame$prop_affected[(data_frame$Network == network_names[6]) & (data_frame$Hydra_k2==1)]),
                              mean(data_frame$prop_affected[(data_frame$Network == network_names[7]) & (data_frame$Hydra_k2==1)]))
  names(actual_results_s1) <- network_names

  actual_results_s2<- data.frame(mean(data_frame$prop_affected[(data_frame$Network == network_names[1]) & (data_frame$Hydra_k2==2)]),
                                 mean(data_frame$prop_affected[(data_frame$Network == network_names[2]) & (data_frame$Hydra_k2==2)]),
                                 mean(data_frame$prop_affected[(data_frame$Network == network_names[3]) & (data_frame$Hydra_k2==2)]),
                                 mean(data_frame$prop_affected[(data_frame$Network == network_names[4]) & (data_frame$Hydra_k2==2)]),
                                 mean(data_frame$prop_affected[(data_frame$Network == network_names[5]) & (data_frame$Hydra_k2==2)]),
                                 mean(data_frame$prop_affected[(data_frame$Network == network_names[6]) & (data_frame$Hydra_k2==2)]),
                                 mean(data_frame$prop_affected[(data_frame$Network == network_names[7]) & (data_frame$Hydra_k2==2)]))
  names(actual_results_s2) <- network_names
  
  #dataframes for all spins
  #spin_without_target_col <- cbind(lh_spin_df[,2:1001],rh_spin_df[,2:1001])
  #melted_df <- melt_df_for_violin_plot_yeo7(spin_without_target_col, network_names, num_spins)
  
  #with mean lines, different fonts
  #save images
  plot_violin_s1 <- ggplot(data_frame_s1, aes(x = factor(Network, level = network_names), y = prop_affected, fill = Network)) +  
    scale_fill_manual(values=yeo_colors) + 
    geom_violin(trim = TRUE) + 
    xlab("Yeo 7 Network") + ylab(paste0("Mean")) +
    ylim(0,1) + 
    theme_classic() + 
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(size = 10, colour = "black"),
          axis.text.y = element_text(size = 10, colour = "black"),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 10)) +
    stat_summary(fun.y = mean, geom = "errorbar", 
                 aes(ymax = ..y.., ymin = ..y.., group = factor(Network)),
                 width = 0.5, linetype = "dashed", position = position_dodge(0.9)) + 
    #geom_boxplot(width = 0.15, position = position_dodge(0.9)) + 
   # geom_segment(aes(x = 0.6, y = actual_results_s1[1], xend = 1.4, yend = actual_results[1])) + 
  #  geom_segment(aes(x = 1.6, y = actual_results_s1[2], xend = 2.4, yend = actual_results[2])) +
  #  geom_segment(aes(x = 2.6, y = actual_results_s1[3], xend = 3.4, yend = actual_results[3])) +
  #  geom_segment(aes(x = 3.6, y = actual_results_s1[4], xend = 4.4, yend = actual_results[4])) +
  #  geom_segment(aes(x = 4.6, y = actual_results_s1[5], xend = 5.4, yend = actual_results[5])) +
  #  geom_segment(aes(x = 5.6, y = actual_results_s1[6], xend = 6.4, yend = actual_results[6])) +
  #  geom_segment(aes(x = 6.6, y = actual_results_s1[7], xend = 7.4, yend = actual_results[7]))
    ggtitle(paste0("Mean burden by network S1"))
#  ggsave(plot=plot_violin, filename = paste0(homedir, "/baller/results/images/spin_mean_coupling_unc.png"), width = 4.81, height = 4.81)
 # ggsave(plot=plot_violin, filename = paste0(homedir, "/baller/results/images/spin_mean_coupling_unc.pdf"), width = 4.81, height = 4.81)

  
  plot_violin_s2 <- ggplot(data_frame_s2, aes(x = factor(Network, level = network_names), y = prop_affected, fill = Network)) +  
    scale_fill_manual(values=yeo_colors) + 
    geom_violin(trim = TRUE) + 
    xlab("Yeo 7 Network") + ylab(paste0("Mean")) +
    ylim(0,1) + 
    theme_classic() + 
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(size = 10, colour = "black"),
          axis.text.y = element_text(size = 10, colour = "black"),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 10)) +
    stat_summary(fun.y = mean, geom = "errorbar", 
                 aes(ymax = ..y.., ymin = ..y.., group = factor(Network)),
                 width = 0.5, linetype = "dashed", position = position_dodge(0.9)) + 
    #geom_boxplot(width = 0.15, position = position_dodge(0.9)) + 
    # geom_segment(aes(x = 0.6, y = actual_results_s1[1], xend = 1.4, yend = actual_results[1])) + 
    #  geom_segment(aes(x = 1.6, y = actual_results_s1[2], xend = 2.4, yend = actual_results[2])) +
    #  geom_segment(aes(x = 2.6, y = actual_results_s1[3], xend = 3.4, yend = actual_results[3])) +
    #  geom_segment(aes(x = 3.6, y = actual_results_s1[4], xend = 4.4, yend = actual_results[4])) +
    #  geom_segment(aes(x = 4.6, y = actual_results_s1[5], xend = 5.4, yend = actual_results[5])) +
    #  geom_segment(aes(x = 5.6, y = actual_results_s1[6], xend = 6.4, yend = actual_results[6])) +
    #  geom_segment(aes(x = 6.6, y = actual_results_s1[7], xend = 7.4, yend = actual_results[7]))
    ggtitle(paste0("Mean burden by network S2"))  
  violin_plot_list <- list(plot_violin_s1, plot_violin_s2)
  return(violin_plot_list) 
  
}


get_yeo7_colors <- function() {
  yeo_colors <- c(
    `VIS` = "#781286",
    `MOT` = "#4682b4",
    `DA` = "#00760e",
    `VA` = "#c43afa",
    `LIM` = "#dcf8a4",
    `FP` = "#e69422",
    `DM` = "#cd3e56")
  return(yeo_colors)
}

get_yeo7_network_names <- function() {
  network_names <- c("VIS",
                     "MOT",
                     "DA",
                     "VA",
                     "LIM",
                     "FP",
                     "DM" )
  return(network_names)
}
