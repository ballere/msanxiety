<br>
<br>



### Project Lead
Erica B. Baller

### Faculty Leads
Theodore D. Satterthwaite and Russell T. Shinohara

### Brief Project Description:
371 participants with MS were included(MS+noA=99; MS+mildA=249; MS+severeA=23). Lesions from research-grade clinical scans were segmented with MIMoSA and normalized to the HCP template. Streamline filtering was performed in DSI studio to generate measures of the degree of fascicle impact by lesions. Disease burden between MS+noA and MS+severeA in uncinate were compared, as well as parametric load of anxiety burden with uncinate burden.

### Analytic Replicator:
Audrey Luo

### Collaborators:
Erica B. Baller, M.D., M.S., Audrey Luo, B.A., Matthew C. Cieslak, Ph.D., Elena C. Cooper, B.A., Matthew K. Schindler, M.D., Ph.D., Amit Bar-Or, M.D., Clyde E. Markowitz, Melissa Martin, B.A., Victoria Rautman, Donovan Reid, Russell T. Shinohara, Ph.D.^, Theodore D. Satterthwaite, M.D., M.A.^
^shared last author

### Project Start Date:
7/2024

### Current Project Status:
In progress

### Dataset:
Multiple Sclerosis Cohort (Melissa Martin's Mimosa, EMR data pull 10/28/2023)

### Github repo:
[https://github.com/ballere/msanxiety/](https://github.com/ballere/msanxiety/tree/main/scripts)

### Website
TBD, update below
[https://pennlinc.github.io/msdepression/](https://pennlinc.github.io/msdepression/)

### Slack Channel:
#msdepression

### Zotero library:
MS Anxiety

### Current work products:
ACNP 2024 post abstract - "Anxiety as a disease of white matter disruption: association between anxiety severity and multiple sclerosis lesion burden in the uncinate fasciculus."

### Path to Data on Filesystem **PMACS**

*** will need to update!
All clinical data was drawn from the electronic medical record via the Data Acquisition Center (DAC). All images were obtained from the PACS system from the Department of Radiology.

DAC Pull: 

     /project/msdepression/data/erica_dac_pull/investigatingdepressioninmspatients_dates_right_format.csv
 
Psychiatry medication information: 

     /project/msdepression/drugs_data/nami_psych_meds_antidepressants.csv

Patients with parsable depression diagnosis (after incorporating medications):

     /project/msdepression/drugs_data/parsable_msdepression.csv *fed into R analysis*

Subject imaging data: 

     /project/msdepression/data/subj_directories

Cubids : 

     /project/msdepression/cubids/v1_validation.csv
     /project/msdepression/CuBIDS_outputs/*
     
MIMoSA QA info:

     /project/msdepression/data/melissa_martin_files/csv/mimosa_dataframe


Fascicle proportions (for each subject (one row), % of injured fascicle (each column is a fascicle)): 

     /project/msdepression/results/fascicle_volumes_all_subjects_roi_n2336.csv

Overlap of each fascicle (volume and proportion) with depression network: 

     /project/msdepression/results/streamline_volume_within_dep_network_3_09.csv

Volume of all lesions (NOT fascicles) for each subject: 

     /project/msdepression/results/mimosa_binary_masks_hcp_space_20211026_n2336_volumes.csv

Volume of each healthy (full volume) fascicle: 

     /project/msdepression/templates/dti/HCP_YA1065_tractography/fiber_volume_values.csv
     
Individual Healthy Fascicle Maps (.nii)

     /project/msdepression/templates/dti/HCP_YA1065_tractography<association,cerebellum,commissural,projection>

Harvard Depression Mask (thresholded 3.09, binarized)

     /project/msdepression/templates/harvard_depression/Dep_clust_T_3_09_binarized.nii

HCP template:

    /project/msdepression/templates/mni_icbm152_t1_tal_nlin_asym_09a.nii 
    /project/msdepression/templates/mni_icbm152_t1_tal_nlin_asym_09axbrainmask.nii

<br>
<br>

# CODE DOCUMENTATION

**The analytic workflow implemented in this project is described in detail in the following sections. Analysis steps are described in the order they were implemented; the script(s) used for each step are identified and links to the code on github are provided.** 
<br>

### * Functions for project *
[ms_functions.R](https://github.com/ballere/msanxiety/tree/main/scripts/ms_functions.R)

### Sample Construction

We first constructed our sample from n=890 individuals who were diagnosed with multiple sclerosis by a Multiple Sclerosis provider and received their clinical scans at the University of Pennsylvania. 

The following code takes the n=890 sample, and goes through a variety of exclusions to get the final n. Specifically, after excluding xxx participants with poor image, xxx participants were eligible for anxiety classification.  Participants with MS were identified from the electronic medical record and stratified into three age- and sex-matched groups: 1) MS without anxiety (MS+noA); 2) MS with mild anxiety (MS+mildA), 3) MS with severe anxiety (MS+severeA). MS+noA included persons who had no psychiatric diagnosis, took no psychiatric medications, and were asymptomatic on PHQ 2/9 (n = 99, age (SD) = 49.4 (11.7), % female = 75). MS+mildA included persons with either a diagnosis of an anxiety disorder (F40*) or a prescription for an anti-anxiety medication (n = 249, age (SD) = 47.1 (11.1), % female = 82). MS+severeA included persons who had both an anxiety disorder and were taking an anti-anxiety medication (n = 23, age (SD) = 47.1 (12.4), % female = 78).

[clean_dac_pull_icd10_codes_and_make_clean_df.R](https://github.com/ballere/msanxiety/tree/main/scripts/clean_dac_pull_icd10_codes_and_make_clean_df.R)

### Automated white matter lesion segmentation

After we obtained our sample, we used the Method for Intermodal Segmentation Analysis (MIMoSA) to extract white matter lesions for each subject. MIMoSA has been previously described: 

Valcarcel AM, Linn KA, Vandekar SN, Satterthwaite TD, Muschelli J, Calabresi PA, Pham DL, Martin ML, Shinohara RT. MIMoSA: An Automated Method for Intermodal Segmentation Analysis of Multiple Sclerosis Brain Lesions. J Neuroimaging. 2018 Jul;28(4):389-398. [doi: 10.1111/jon.12506](https://pubmed.ncbi.nlm.nih.gov/29516669/). Epub 2018 Mar 8. PMID: 29516669; PMCID: PMC6030441.

### Streamline Filtering
Streamline filtering is an interative process performed in DSI studio. HCP template fib file can be found here [dsistudio Download: HCP1065 1-mm FIB file](https://brain.labsolver.org/hcp_template.html). Template is based on [HCP 2009a asymmetric](https://www.bic.mni.mcgill.ca/~vfonov/icbm/2009/).

For each individual, the MIMoSA binary map was considered a region of interest. For each of the 77 fascicles, streamlines that ran through the lesion were "filtered" or kept, whereas the fascicles that avoided the MIMoSA mask were eliminated. Streamlines that passed through the MIMoSA map were then saved binary .nii files, where 1 indicated that disease was present in that voxel, and 0 indicated either 1) that fascicle did not cross through that voxel or 2) there was no disease in it. 
  
I was then able to calculate the "volume" of the disease in a fascicle (i.e. volume of the streamlines that were affected) by summing the # of 1s in the map. At the end, each individual had 77 single values that represented the volume of affected streamlines within each fascicle.
  
Full fascicle volumes were also calculated and saved as .niis. 

#### Step 1: Registering/normalizing MIMoSA binary maps to HCP template

[ants_registration_code.sh](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/ants_registration_code.sh)

#### Step 2: Take a subject's MIMoSA lesions and generate the fiber tracts (individual fascicles) that run through it

*Script that cycles through all subjects to do streamline filtering*

[dsi_studio_bash.sh](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/dsi_studio_bash.sh)

*Individual subject streamline filtering, called from dsi_studio_bash*

[indiv_mimosa_lesion_dsi_studio_script.sh](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/indiv_mimosa_lesion_dsi_studio_script.sh)

#### Step 3: Calculate the volume of each fascicle in a template (healthy) brain

*Make the volume of each of the healthy fascicles*

[make_streamline_volumes_for_template.sh](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/make_streamline_volumes_for_template.sh)

*Calculate the volume within each healthy fascicle*

[get_volume_of_mimosa_lesions.sh](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/get_volume_of_mimosa_lesions.sh)

#### Step 4: Calculate the volume of the fiber tracts that are impaired

*Make streamline volumes for all subjects*

[streamline_volumes_all_subjs.sh](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/streamline_volumes_all_subjs.sh)

*Make streamline volumes for a single subject, called from streamline_volumes_all_subjs.sh*

[make_streamline_volumes_single_subj_pmacs.sh](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/make_streamline_volumes_single_subj_pmacs.sh)

*If you want to calculate the volume of the MIMoSA lesions irrespective of the streamlines they affect*

[get_volume_of_mimosa_lesions.sh](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/get_volume_of_mimosa_lesions.sh)

#### Step 5: Generate summary fascicle measures

This specifically makes the fascicle injury ratio measure, calculated by taking the volume of injured fascicle and dividing by the overall volume of the healthy fascicle. 

[roi_ratio_regressions.R](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/roi_ratio_regressions.R)

#### Disease burden summary measures
  Having computed disease measures at the individual fascicle, I wanted to look specifically at uncinate fasiculus, given previous literature suggesting that the uncinate is the main white matter fiber connecting mPFC and amygdala, core brain areas associated with anxiety disorders. 
    
  As a comparison, I also looked specifically at fornix, which is another subcortical fiber primarily involved in cognition/memory. 
  
#### Main effect of Diagnosis

A gam with main effect of anxiety diagnosis (MS+noA vs MS+severeA), with sex and spline of age as covariates.

#### Parametric effect of anxiety "dose"

A gam with main effect of anxiety dose (dose = 0 (MS+nA), 1 (MS+mildA), or 2 (MS+severeA)), with sex and spline of age as covariates.


#### Coloring scripts for fascicle visualizations (to be fed into DSI studio)

*Sample script for making RGB scales in the red to yellow range*

[make_red_to_yellow_RGB_color_scheme.R](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/make_red_to_yellow_RGB_color_scheme.R)

*Sample script for making binary color schemes, simple*

[make_binary_colored_depression_net_maps.R](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/make_binary_colored_depression_net_maps.R)

*Sample script for making binary color schemes, coloring by whether fascicle in vs. outside dep network*

[make_binary_colored_depression_net_maps_by_dx.R](https://github.com/PennLINC/msdepression/blob/gh-pages/scripts/make_binary_colored_depression_net_maps_by_dx.R)


### Final group level analysis

This script is run locally, on R. It does all second level/group data analysis. 

[ACNP2024_MSAnxiety.Rmd](https://github.com/ballere/msanxiety/tree/main/scripts/ACNP2024_MSAnxiety.Rmd)



