#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=5:00:00
#SBATCH --job-name=master_india
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out

depend=$(sbatch ~/cov-ind-19/model/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')
depend6=$(sbatch ~/cov-ind-19/model/slurm_scripts/spatialmap.slurm | awk '{print $NF}')
depend7=$(sbatch ~/cov-ind-19/model/slurm_scripts/data.slurm | awk '{print $NF}')
depend1=$(sbatch --dependency=afterok:$depend ~/cov-ind-19/model/slurm_scripts/forecasting_1wk_models.slurm | awk '{print $NF}')
depend2=$(sbatch --dependency=afterok:$depend ~/cov-ind-19/model/slurm_scripts/forecasting_2wk_models.slurm | awk '{print $NF}')
depend3=$(sbatch --dependency=afterok:$depend1:$depend2 ~/cov-ind-19/model/slurm_scripts/plot_figure_4.slurm | awk '{print $NF}')
depend4=$(sbatch --dependency=afterok:$depend1:$depend2 ~/cov-ind-19/model/slurm_scripts/plot_figure_5.slurm | awk '{print $NF}')
depend5=$(sbatch --dependency=afterok:$depend1:$depend2 ~/cov-ind-19/model/slurm_scripts/plot_figure_5_inc.slurm | awk '{print $NF}')
depend8=$(sbatch --dependency=afterok:$depend3:$depend4:$depend5 ~/cov-ind-19/model/slurm_scripts/plot_figure_1.slurm | awk '{print $NF}')
depend9=$(sbatch --dependency=afterok:$depend3:$depend4:$depend5 ~/cov-ind-19/model/slurm_scripts/plot_figure_23.slurm | awk '{print $NF}')
depend10=$(sbatch --dependency=afterok:$depend3:$depend4:$depend5 ~/cov-ind-19/model/slurm_scripts/plot_figure_4a.slurm | awk '{print $NF}')
depend11=$(sbatch --dependency=afterok:$depend3:$depend4:$depend5 ~/cov-ind-19/model/slurm_scripts/plot_figure_4b.slurm | awk '{print $NF}')
depend12=$(sbatch --dependency=afterok:$depend3:$depend4:$depend5 ~/cov-ind-19/model/slurm_scripts/plot_figure_4b.slurm | awk '{print $NF}')
depend13=$(sbatch --dependency=afterok:$depend3:$depend4:$depend5 ~/cov-ind-19/model/slurm_scripts/plot_figure_4b.slurm | awk '{print $NF}')
depend14=$(sbatch --dependency=afterok:$depend3:$depend4:$depend5 ~/cov-ind-19/model/slurm_scripts/plot_figure_4b.slurm | awk '{print $NF}')
depend15=$(sbatch --dependency=afterok:$depend3:$depend4:$depend5 ~/cov-ind-19/model/slurm_scripts/plot_figure_4b.slurm | awk '{print $NF}')
sbatch --dependency=afterok:$depend8:$depend9:$depend10:$depend11:$depend12:$depend13:$depend14:$depend15 ~/cov-ind-19/model/slurm_scripts/gitpush.slurm
sleep 4h
sbatch ~/cov-ind-19/model/slurm_scripts/master_india.slurm
