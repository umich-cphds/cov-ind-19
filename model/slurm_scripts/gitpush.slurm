#!/bin/bash
#SBATCH --mail-type=FAIL,ARRAY_TASKS
#SBATCH --time=10:00
#SBATCH --job-name=git_commit_and_push_prediction_data
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

source /etc/profile.d/http_proxy.sh

cd $data_repo/$today/
git add *
git commit -m "Automated push from slurm job"
git push
