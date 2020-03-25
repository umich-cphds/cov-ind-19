#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=0-36:00:05
#SBATCH --job-name=master_india
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

for i in $(seq 1 3); do
  sbatch ~/cov-ind-19/slurm_scripts/job_run_indian_models_loop.slurm
  sleep 24h
done
sbatch ~/cov-ind-19/slurm_scripts/master_india.slurm
