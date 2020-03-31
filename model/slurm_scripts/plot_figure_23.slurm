#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=10:00
#SBATCH --job-name=plot_figure_23
#SBATCH --mem-per-cpu=1g
#SBATCH --array=1-2
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

module load R/3.6.1-gcc8.3.0

Rscript --vanilla ~/cov-ind-19/model/r_scripts/plot_figure_23.R
