#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=5:00:00
#SBATCH --job-name=submit_states
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

cd ~/cov-ind-19/model/slurm_scripts/states
sbatch forecasting_1wk_delhi.slurm
sbatch forecasting_1wk_kerala.slurm
sbatch forecasting_1wk_maharashtra.slurm
sbatch forecasting_2wk_delhi.slurm
sbatch forecasting_2wk_kerala.slurm
sbatch forecasting_2wk_maharashtra.slurm
