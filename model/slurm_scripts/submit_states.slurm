#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=10:00
#SBATCH --job-name=submit_states
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

source /etc/profile.d/http_proxy.sh

module load Rtidyverse/4.2.0

cd $code_repo/model/slurm_scripts
states=$(Rscript $code_repo/model/r_scripts/get_all_states.R )
state_jobs=""
for i in $states; do
	export state=$i
	state1=$(sbatch --job-name=$i state_forecast.slurm | awk '{print $NF}')
	if [[ $state_jobs != "" ]]; then
		state_jobs=$state_jobs:$state1
	else
		state_jobs=$state1
	fi
done
states="tt"
#states="$states tt"
Run seirfansy states which includes tt for India
export bottom=FALSE
for i in $states; do
	export state=$i
	state2=$(sbatch --job-name=$i state_gen_seirfansy.slurm | awk '{print $NF}')
	if [[ $state_jobs != "" ]]; then
		state_jobs=$state_jobs:$state2
	else
		state_jobs=$state2
	fi
done
echo $state_jobs > $data_repo/$today/state_jobids.txt
