#!/bin/bash
#SBATCH --mail-type=FAIL
#SBATCH --time=10:00
#SBATCH --job-name=git_pull_JHU
#SBATCH --mem-per-cpu=2G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

source /etc/profile.d/http_proxy.sh

cd ~/COVID-19
git pull
