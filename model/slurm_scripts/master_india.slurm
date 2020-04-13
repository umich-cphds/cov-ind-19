#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=7:00:00
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
export production=TRUE

cd ~/COVID-19/
remote_sha=$(git ls-remote https://github.com/CSSEGISandData/COVID-19.git | grep HEAD | cut -f 1 )
local_sha=$(git show-ref --head refs/heads/master | grep HEAD | cut -f 1 -d ' ')
if [[ "$remote_sha" != "$local_sha" ]]; then
	echo "JHU data has been updated. Running git pull and forecasts"
	spatialmap=$(sbatch ~/cov-ind-19/model/slurm_scripts/spatialmap.slurm | awk '{print $NF}')
	gitpull=$(sbatch ~/cov-ind-19/model/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')
	preprocess=$(sbatch --dependency=afterok:$gitpull ~/cov-ind-19/model/slurm_scripts/preprocess_data.slurm | awk '{print $NF}')
	sensitivity=$(sbatch --dependency=afterok:$gitpull:$preprocess ~/cov-ind-19/model/slurm_scripts/forecasting_sensitivity.slurm | awk '{print $NF}')
	states=$(sbatch --dependency=afterok:$gitpull:$preprocess ~/cov-ind-19/model/slurm_scripts/states/submit_states.slurm | awk '{print $NF}')
	forecast1wk=$(sbatch --dependency=afterok:$gitpull:$preprocess ~/cov-ind-19/model/slurm_scripts/forecasting_1wk_models.slurm | awk '{print $NF}')
	forecast2wk=$(sbatch --dependency=afterok:$gitpull:$preprocess ~/cov-ind-19/model/slurm_scripts/forecasting_2wk_models.slurm | awk '{print $NF}')
	generateplotdata=$(sbatch --dependency=afterok:$forecast1wk:$forecast2wk ~/cov-ind-19/model/slurm_scripts/generate_plot_data.slurm | awk '{print $NF}')
	generateplots=$(sbatch --dependency=afterok:$generateplotdata ~/cov-ind-19/model/slurm_scripts/generate_plots.slurm | awk '{print $NF}')
	sbatch --dependency=afterok:$generateplots:$spatialmap:$states:$sensitivity ~/cov-ind-19/model/slurm_scripts/gitpush.slurm
else
	echo "JHU data has NOT been updated since last run. Taking a nap for 5.5 hours and checking again when I wake up."
fi

sleep 5.5h
sbatch ~/cov-ind-19/model/slurm_scripts/master_india.slurm
