#!/bin/bash
#SBATCH --mail-type=FAIL
#SBATCH --time=1:00:00
#SBATCH --job-name=pull_cowin
#SBATCH --mem-per-cpu=1g
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

module load Rtidyverse/4.2.0

source /etc/profile.d/http_proxy.sh

Rscript --vanilla $code_repo/model/r_scripts/pull_cowin_data.R
