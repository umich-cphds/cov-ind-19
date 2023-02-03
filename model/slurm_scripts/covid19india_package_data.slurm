#!/bin/bash
#SBATCH --mail-type=FAIL
#SBATCH --time=1:00:00
#SBATCH --job-name=covid19india_package_data
#SBATCH --mem-per-cpu=1g
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

source /etc/profile.d/http_proxy.sh

module load Rtidyverse/4.2.0
Rscript --vanilla $code_repo/app/covid19india_package_data.R
