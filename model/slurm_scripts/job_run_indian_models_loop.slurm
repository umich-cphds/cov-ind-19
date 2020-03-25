#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=0-12:00:00
#SBATCH --job-name=cov-ind-19
#SBATCH --mem-per-cpu=8G
#SBATCH --array=1-10
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

module load R/3.6.1-gcc8.3.0

Rscript --vanilla ~/cov-ind-19/r_scripts/covid19_prediction_india.R
