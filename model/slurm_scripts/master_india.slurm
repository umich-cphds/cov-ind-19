#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=25:00:00
#SBATCH --job-name=master_india
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

export http_proxy=http://proxy.arc-ts.umich.edu:3128/
export FTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export ftp_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTPS_PROXY=http://proxy.arc-ts.umich.edu:3128/
export https_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export today=$(date +%Y-%m-%d)
export production=FALSE

# Set variables based on testing or production
if [[ $production == "TRUE" ]]; then
  export code_repo="$HOME/cov-ind-19"
  export data_repo="$HOME/cov-ind-19-data"
else
  export code_repo="$HOME/cov-ind-19-iris"
  export data_repo="$HOME/cov-ind-19-test"
fi

module load Rgeospatial/3.6.1-2019-09-29

cd ~/COVID-19/
remote_sha=$(git ls-remote https://github.com/CSSEGISandData/COVID-19.git | grep HEAD | cut -f 1 )
local_sha=$(git show-ref --head refs/heads/master | grep HEAD | cut -f 1 -d ' ')
if [[ "$remote_sha" != "$local_sha" ]]; then
	echo "JHU data has been updated. Running git pull and forecasts"
	gitpull=$(sbatch $code_repo/model/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')
	preprocess=$(sbatch --dependency=afterok:$gitpull $code_repo/model/slurm_scripts/preprocess_data.slurm | awk '{print $NF}')
	spatialmap=$(sbatch --dependency=afterok:$preprocess $code_repo/model/slurm_scripts/spatialmap.slurm | awk '{print $NF}')
	national=$(sbatch --dependency=afterok:$preprocess $code_repo/model/slurm_scripts/national_forecast.slurm | awk '{print $NF}')
	#submit_states
	sleep 2m #This is to give preprocess data a chance to run before get_states.R which requires the data from preprocess data
	cd $code_repo/model/slurm_scripts
	states=$(Rscript $code_repo/model/r_scripts/get_states.R)
	state_jobs=""
	for i in $states; do
		export state=$i
		state1=$(sbatch --dependency=afterok:$preprocess --job-name=$i state_forecast.slurm | awk '{print $NF}')
		if [[ $state_jobs != "" ]]; then
			state_jobs=$state_jobs:$state1
		else
			state_jobs=$state1
		fi
	done
	states="$states tt"
	for i in $states; do
		export state=$i
                state2=$(sbatch --dependency=afterok:$preprocess --job-name=$i state_gen_seirfansy.slurm | awk '{print $NF}')
		if [[ $state_jobs != "" ]]; then
			state_jobs=$state_jobs:$state2
		else
			state_jobs=$state2
		fi
	done
	#end of submit_states
	generateplotdata=$(sbatch --dependency=afterok:$national:$state_jobs $code_repo/model/slurm_scripts/generate_plot_data.slurm | awk '{print $NF}')
	generateplots=$(sbatch --dependency=afterok:$generateplotdata $code_repo/model/slurm_scripts/generate_plots.slurm | awk '{print $NF}')
	generateplotsseirfansy=$(sbatch --dependency=afterok:$generateplotdata $code_repo/model/slurm_scripts/plot_state_seirfansy.slurm | awk '{print $NF}')
	sbatch --dependency=afterok:$generateplots:$generateplotsseirfansy:$spatialmap $code_repo/model/slurm_scripts/gitpush.slurm
else
	echo "JHU data has NOT been updated since last run. Taking a nap for 5.5 hours and checking again when I wake up."
fi

sbatch --begin=now+24hour $code_repo/model/slurm_scripts/master_india.slurm
