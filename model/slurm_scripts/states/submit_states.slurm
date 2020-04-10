#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=5:00:00
#SBATCH --job-name=submit_states
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

cd ~/cov-ind-19/model/slurm_scripts/states
cities="ap br dl gj ka mh mp kl rj tn up wb"
for i in $cities; do
	export city=$i
	sbatch --job-name=1wk_$i forecasting_1wk_city.slurm
	sbatch --job-name=2wk_$i forecasting_2wk_city.slurm
done
