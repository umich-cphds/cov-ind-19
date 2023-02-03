#!/bin/bash
#SBATCH --mail-type=FAIL
#SBATCH --time=10:00
#SBATCH --job-name=generate_plot_data
#SBATCH --mem-per-cpu=1g
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

module load Rtidyverse/4.2.0 jags

source /etc/profile.d/http_proxy.sh

Rscript --vanilla $code_repo/model/r_scripts/generate_plot_data.R
