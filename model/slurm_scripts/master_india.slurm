#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=25:00:00
#SBATCH --job-name=master_india
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

depend=$(sbatch ~/cov-ind-19/model/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')
depend1=$(sbatch --dependency=afterok:$depend ~/cov-ind-19/model/slurm_scripts/forecasting_models.slurm | awk '{print $NF}')
sbatch --dependency=afterok:$depend1 ~/cov-ind-19/model/slurm_scripts/gitpush.slurm
sleep 24h
sbatch ~/cov-ind-19/model/slurm_scripts/master_india.slurm
