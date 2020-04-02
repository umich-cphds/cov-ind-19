#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=5:00:00
#SBATCH --job-name=master_india
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

gitpull=$(sbatch ~/cov-ind-19/model/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')

dataframe=$(sbatch --dependency=afterok:$gitpull ~/cov-ind-19/model/slurm_scripts/data.slurm | awk '{print $NF}')
forecast1wk=$(sbatch --dependency=afterok:$gitpull ~/cov-ind-19/model/slurm_scripts/forecasting_1wk_models.slurm | awk '{print $NF}')
forecast2wk=$(sbatch --dependency=afterok:$gitpull ~/cov-ind-19/model/slurm_scripts/forecasting_2wk_models.slurm | awk '{print $NF}')
fig4=$(sbatch --dependency=afterok:$forecast1wk:$forecast2wk ~/cov-ind-19/model/slurm_scripts/plot_figure_4.slurm | awk '{print $NF}')
fig5=$(sbatch --dependency=afterok:$forecast1wk:$forecast2wk ~/cov-ind-19/model/slurm_scripts/plot_figure_5.slurm | awk '{print $NF}')
fig5_inc=$(sbatch --dependency=afterok:$forecast1wk:$forecast2wk ~/cov-ind-19/model/slurm_scripts/plot_figure_5_inc.slurm | awk '{print $NF}')
generateplots=$(sbatch --dependency=afterok:$fig4:$fig5:$fig5_inc ~/cov-ind-19/model/slurm_scripts/generate_plots.slurm | awk '{print $NF}')

spatialmap=$(sbatch ~/cov-ind-19/model/slurm_scripts/spatialmap.slurm | awk '{print $NF}')

sbatch --dependency=afterok:$generateplots:$spatialmap ~/cov-ind-19/model/slurm_scripts/gitpush.slurm

sleep 4.5h
sbatch ~/cov-ind-19/model/slurm_scripts/master_india.slurm
