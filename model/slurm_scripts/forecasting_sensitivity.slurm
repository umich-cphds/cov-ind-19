#!/bin/bash
#SBATCH --mail-type=FAIL,ARRAY_TASKS
#SBATCH --time=0-12:00:00
#SBATCH --job-name=sensitivity
#SBATCH --mem-per-cpu=12G
#SBATCH --array=1-3
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

module load Rtidyverse/4.2.0 jags

source /etc/profile.d/http_proxy.sh

Rscript --vanilla $code_repo/model/r_scripts/forecasting_sensitivity.R
