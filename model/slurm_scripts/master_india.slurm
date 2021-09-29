#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=1:00:00
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

echo $http_proxy
# Set variables based on testing or production
if [[ $production == "TRUE" ]]; then
  export code_repo="$HOME/cov-ind-19"
  export data_repo="$HOME/cov-ind-19-data"
else
  export code_repo="$HOME/cov-ind-19-iris"
  export data_repo="$HOME/cov-ind-19-test"
fi

module load Rtidyverse/4.1.0

cd ~/COVID-19/
remote_sha=$(git ls-remote https://github.com/CSSEGISandData/COVID-19.git | grep HEAD | cut -f 1 )
local_sha=$(git show-ref --head refs/heads/master | grep HEAD | cut -f 1 -d ' ')
if [[ "$remote_sha" != "$local_sha" ]]; then
	echo "JHU data has been updated. Running git pull and forecasts"
	scraper=$(sbatch $code_repo/model/slurm_scripts/scraper.slurm | awk '{print $NF}')
	gitpull=$(sbatch --dependency=afterok:$scraper $code_repo/model/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')
	preprocess=$(sbatch --dependency=afterok:$gitpull $code_repo/model/slurm_scripts/preprocess_data.slurm | awk '{print $NF}')
	spatialmap=$(sbatch --dependency=afterok:$preprocess $code_repo/model/slurm_scripts/spatialmap.slurm | awk '{print $NF}')
	national=$(sbatch --dependency=afterok:$preprocess $code_repo/model/slurm_scripts/national_forecast.slurm | awk '{print $NF}')
	submitstates=$(sbatch --dependency=afterok:$preprocess $code_repo/model/slurm_scripts/submit_states.slurm | awk '{print $NF}')
	while [ ! -f $data_repo/$today/state_jobids.txt ]
        do
          sleep 10
        done
	state_jobs=$(cat $data_repo/$today/state_jobids.txt)
	generateplotdata=$(sbatch --dependency=afterok:$national:$state_jobs $code_repo/model/slurm_scripts/generate_plot_data.slurm | awk '{print $NF}')
	generateplots=$(sbatch --dependency=afterok:$generateplotdata $code_repo/model/slurm_scripts/generate_plots.slurm | awk '{print $NF}')
	sbatch --dependency=afterok:$generateplots:$spatialmap $code_repo/model/slurm_scripts/gitpush.slurm
else
	echo "JHU data has NOT been updated since last run. Taking a nap for 5.5 hours and checking again when I wake up."
fi

#sbatch --begin=now+24hour $code_repo/model/slurm_scripts/master_india.slurm
