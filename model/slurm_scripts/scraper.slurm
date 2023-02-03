#!/bin/bash
#SBATCH --mail-type=FAIL
#SBATCH --time=1:00:00
#SBATCH --job-name=scraper
#SBATCH --mem-per-cpu=1g
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

module load use.own
module load geckodriver
module load python3.9-anaconda

source /etc/profile.d/http_proxy.sh
source ~/.bashrc
conda activate cov-ind-19

python $code_repo/model/r_scripts/scraper.py
