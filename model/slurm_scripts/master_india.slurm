#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=5:00:00
#SBATCH --job-name=master_india
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out


dataframe=$(sbatch ~/cov-ind-19/model/slurm_scripts/data.slurm | awk '{print $NF}')
fig1=$(sbatch --dependency=afterok:$dataframe ~/cov-ind-19/model/slurm_scripts/plot_fig_1.slurm | awk '{print $NF}')
fig23=$(sbatch --dependency=afterok:$dataframe ~/cov-ind-19/model/slurm_scripts/plot_fig_23.slurm | awk '{print $NF}')

gitpull=$(sbatch ~/cov-ind-19/model/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')
forecast1wk=$(sbatch --dependency=afterok:$gitpull ~/cov-ind-19/model/slurm_scripts/forecasting_1wk_models.slurm | awk '{print $NF}')
forecast2wk=$(sbatch --dependency=afterok:$gitpull ~/cov-ind-19/model/slurm_scripts/forecasting_2wk_models.slurm | awk '{print $NF}')

fig4=$(sbatch --dependency=afterok:$forecast1wk:$forecast2wk ~/cov-ind-19/model/slurm_scripts/plot_figure_4.slurm | awk '{print $NF}')
fig4a=$(sbatch --dependency=afterok:$fig4 ~/cov-ind-19/model/slurm_scripts/plot_fig_4a.slurm | awk '{print $NF}')
fig4b=$(sbatch --dependency=afterok:$fig4 ~/cov-ind-19/model/slurm_scripts/plot_fig_4b.slurm | awk '{print $NF}')

fig5=$(sbatch --dependency=afterok:$forecast1wk:$forecast2wk ~/cov-ind-19/model/slurm_scripts/plot_figure_5.slurm | awk '{print $NF}')
fig5_inc=$(sbatch --dependency=afterok:$forecast1wk:$forecast2wk ~/cov-ind-19/model/slurm_scripts/plot_figure_5_inc.slurm | awk '{print $NF}')
fig5a=$(sbatch --dependency=afterok:$fig5 ~/cov-ind-19/model/slurm_scripts/plot_fig_5a.slurm | awk '{print $NF}')
fig5b=$(sbatch --dependency=afterok:$fig5_inc ~/cov-ind-19/model/slurm_scripts/plot_fig_5b.slurm | awk '{print $NF}')
fig6a=$(sbatch --dependency=afterok:$fig5 ~/cov-ind-19/model/slurm_scripts/plot_fig_6.slurm | awk '{print $NF}')
fig6b=$(sbatch --dependency=afterok:$fig5_inc ~/cov-ind-19/model/slurm_scripts/plot_fig_6b.slurm | awk '{print $NF}')

spatialmap=$(sbatch ~/cov-ind-19/model/slurm_scripts/spatialmap.slurm | awk '{print $NF}')

sbatch --dependency=afterok:$fig1:$fig23:$fig4a:$fig4b:$fig5a:$fig5b:$fig6:$fig6b:$spatialmap ~/cov-ind-19/model/slurm_scripts/gitpush.slurm
sleep 4h
sbatch ~/cov-ind-19/model/slurm_scripts/master_india.slurm
