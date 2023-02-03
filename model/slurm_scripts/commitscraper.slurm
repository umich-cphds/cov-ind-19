#!/bin/bash

#SBATCH --mail-type=FAIL,ARRAY_TASKS
#SBATCH --time=10:00
#SBATCH --job-name=commit_scraped_data
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

source /etc/profile.d/http_proxy.sh

cd $data_repo/source_data/
git add *
git commit -m "Automated push from slurm job"
git push
