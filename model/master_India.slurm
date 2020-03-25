#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --mail-user=lilywang@umich.edu
#SBATCH --time=0-36:00:05
#SBATCH --job-name=master
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/lilywang/slurm_output/slurm-%A_%a.out

for i in $(seq 1 3); do
  sbatch /home/lilywang/ncov2019/report/International/job_run_Indian_Models_loop.txt
  sleep 24h
done
sbatch /home/lilywang/continuous_submission/master.slurm
