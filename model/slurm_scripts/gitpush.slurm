#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=10:00
#SBATCH --job-name=git_commit_and_push
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

cd /mnt/biostat/ftp/ncov2019/India
Rscript PATH/model/r_scripts/app_data_preprocess.R
git add *
git commit -m "Automated push from slurm job"
git push
