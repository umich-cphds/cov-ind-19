#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=5:00:00
#SBATCH --job-name=submit_states
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

cd ~/cov-ind-19/model/slurm_scripts/states
states="India an ap ar as br ch cg dh dd dl ga gj hr hp jk jh ka kl mp mh mn ml mz nl or py pb rj sk tn tr up uk wb"
for i in $states; do
	export state=$i
	sbatch --job-name=1wk_$i forecasting_1wk_city.slurm
	sbatch --job-name=2wk_$i forecasting_2wk_city.slurm
done
