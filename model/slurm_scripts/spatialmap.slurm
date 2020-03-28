#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=0-12:00:00
#SBATCH --job-name=spatialmap
#SBATCH --mem-per-cpu=7g
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

module load R/3.6.1-gcc8.3.0

Rscript --vanilla ~/cov-ind-19/model/r_scripts/spatialmap.R
