#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=0-12:00:00
#SBATCH --job-name=sensitivity
#SBATCH --mem-per-cpu=8G
#SBATCH --array=1-3
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

module load Rgeospatial/3.6.1-2019-09-29
module load jags

export http_proxy=http://proxy.arc-ts.umich.edu:3128/
export FTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export ftp_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTPS_PROXY=http://proxy.arc-ts.umich.edu:3128/
export https_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTP_PROXY=http://proxy.arc-ts.umich.edu:3128/

Rscript --vanilla ~/cov-ind-19/model/r_scripts/forecasting_sensitivity.R
