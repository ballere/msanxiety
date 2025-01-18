#!/bin/bash
##########################
### Total brain volume ###
##########################

##Pre: list of full paths to mimosa files, which are in same directory as their n4 skull-stripped t1
##Post: in /results/total_brain_volume.csv, each EMPI and and total brain volumewill be listed
##Uses: To address JAMA, will go through each person, calculate tbv with fslstats <.nii.gz> -V | cut -f 1 -d ' ', and store in output file
## Dependencies: need fsl to be installed

set -euf -o pipefail

export ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=$LSB_DJOB_NUMPROC
num_cores=1

#set default paths 
#directory='/project/msdepression/'
#default="${directory}/data/melissa_martin_files/csv/mimosa_binary_masks_hcp_space_20211026_n2336"
output_csv="total_fast_brain_volume_values.csv"
#default='/project/msdepression/data/melissa_martin_files/csv/erica_mini_mimosa_paths_n3'
output_prefix="fast"
#fascicle_directory='/project/msdepression/templates/dti/HCP_YA1065_tractography/'


if [ $# == 0 ]
then
    echo "no file, something is wrong"
else  
    lesion=$1
fi
echo "File being read is "$lesion
echo "... Starting to calculate total brain volume ..."

	echo "working on ${lesion} ..."  
	skull_stripped_file=`echo ${lesion} | perl -pe 's/mimosa_binary_mask_0.25_mni_hcp.nii.gz/t1_n4_reg_brain_ws.nii.gz/'`
	echo "skullstripped file ${skull_stripped_file}"

	directory=$(echo ${lesion} | perl -pe 's/(.*run-001).*/$1/')
	echo $directory

	#initialize output file
	output_csv="${directory}/total_fast_brain_volume_values.csv"
	
	#remove it if it exists
	rm -f $output_csv
	touch $output_csv
	echo "EMPI,EXAM_DATE,gm_volume,wm_volume,total_volume" >> $output_csv

	#run fast
	fast -t 1 -n 3 -H 0.1 -I 4 -l 20.0 -o ${directory}/${output_prefix} output_prefix ${skull_stripped_file}

	#pull out metrics
	gm_vol=`fslstats ${directory}/${output_prefix}_pve_1.nii.gz -V| cut -f 2 -d ' '`
	wm_vol=`fslstats ${directory}/${output_prefix}_pve_2.nii.gz -V| cut -f 2 -d ' '`
	total_volume=$(echo "${gm_vol} + ${wm_vol}" | bc)

	echo $gm_vol $wm_vol $total_volume

#	total_brain_volume=`fslstats ${skull_stripped_file} -V | cut -f 1 -d ' '


#	echo "total_brain_volume ${total_brain_volume}"

	empi_examdate=$(echo ${skull_stripped_file} | perl -pe 's/.*sub-(.*)\/ses-(.*)\/r.*/$1,$2/g')

	echo "empi examdate" $empi_examdate

	#save in file
	echo "${empi_examdate},${gm_vol},${wm_vol},${total_volume}" >> $output_csv

