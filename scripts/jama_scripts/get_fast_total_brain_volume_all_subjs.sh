#!/bin/bash

#### Welcome to the fast party #########
### Pre: Must have a file with full paths to the lesioned data 
### Post: Within each directory specified by the lesioned data, we will have a bunch of fast files that we can use to calculate total volumes
### Uses: For use in MS depression - iterates through each subject and session
#dependencies: Using fsl

default='/project/msdepression/data/melissa_martin_files/csv/mimosa_binary_masks_hcp_space_20211026_n2336'
num_cores=1

if [ $# == 0 ]
then
    echo "We will use the default mimosa path file" $default
    lesion_file=$default
else  
    lesion_file=$1
fi
echo "File being read is "$lesion_file
echo "... Starting to fast ..."

lesion_paths=$(cat $lesion_file)

job_count=1
for lesion in ${lesion_paths}; do
    echo "Working on file" $lesion
    bsub -J "job+${job_count}" -n ${num_cores} -o /project/msdepression/scripts/logfiles/fast_${job_count}.out /project/msdepression/scripts/make_fast_files_single_subj_pmacs.sh $lesion
    ((job_count+=1))
done

