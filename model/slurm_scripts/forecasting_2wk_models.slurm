#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=0-12:00:00
#SBATCH --job-name=forecasting_models
#SBATCH --mem-per-cpu=8G
#SBATCH --array=1-6
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

module load R/3.6.1-gcc8.3.0

Rscript --vanilla ~/cov-ind-19/model/r_scripts/forecasting_2wk_models.R
