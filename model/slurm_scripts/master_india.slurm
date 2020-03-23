#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=5:00:00
#SBATCH --job-name=master_india
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

depend=$(sbatch ~/cov-ind-19/model/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')
depend1=$(sbatch --dependency=afterok:$depend ~/cov-ind-19/model/slurm_scripts/forecasting_1wk_models.slurm | awk '{print $NF}')
depend2=$(sbatch --dependency=afterok:$depend ~/cov-ind-19/model/slurm_scripts/forecasting_2wk_models.slurm | awk '{print $NF}')
depend3=$(sbatch --dependency=afterok:$depend1:$depend2 ~/cov-ind-19/model/slurm_scripts/plot_figure_4.slurm | awk '{print $NF}')
depend4=$(sbatch --dependency=afterok:$depend1:$depend2 ~/cov-ind-19/model/slurm_scripts/plot_figure_5.slurm | awk '{print $NF}')
depend5=$(sbatch --dependency=afterok:$depend1:$depend2 ~/cov-ind-19/model/slurm_scripts/plot_figure_5_inc.slurm | awk '{print $NF}')
depend6=$(sbatch ~/cov-ind-19/model/slurm_scripts/spatialmap.slurm | awk '{print $NF}')
sbatch --dependency=afterok:$depend3:$depend4:$depend5:$depend6 ~/cov-ind-19/model/slurm_scripts/gitpush.slurm
sleep 4h
sbatch ~/cov-ind-19/model/slurm_scripts/master_india.slurm
