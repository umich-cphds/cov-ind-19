#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=10:00
#SBATCH --job-name=git_pull_JHU
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

cd ~/COVID-19
git pull
