#!/bin/bash

#SBATCH --mail-type=FAIL,END
#SBATCH --time=10:00
#SBATCH --job-name=git_commit_and_push_prediction_data
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

today=$(date +%Y-%m-%d)
cd ~/cov-ind-19-data/$today
git add *
git commit -m "Automated push from slurm job"
git push
