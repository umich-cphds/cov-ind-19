#!/bin/bash
#SBATCH --mail-type=FAIL,ARRAY_TASKS
#SBATCH --time=1-0
#SBATCH --job-name=state_forecast
#SBATCH --mem-per-cpu=12G
#SBATCH --array=1
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

module load Rtidyverse/4.2.0 jags

source /etc/profile.d/http_proxy.sh

Rscript --vanilla $code_repo/model/r_scripts/state_forecast.R
