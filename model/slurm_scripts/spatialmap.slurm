#!/bin/bash
#SBATCH --mail-type=FAIL
#SBATCH --time=0-12:00:00
#SBATCH --job-name=spatialmap
#SBATCH --mem-per-cpu=7g
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

module load Rgeospatial/3.6.1-2019-09-29 jags
#module load Rtidyverse/4.1.0 hdf5/1.8.21 netcdf-c/4.6.2 geos/3.7.2 netcdf-fortran/4.4.5 gdal/3.0.1 proj/6.1.1 szip/2.1.1 jags

export http_proxy=http://proxy.arc-ts.umich.edu:3128/
export FTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export ftp_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTPS_PROXY=http://proxy.arc-ts.umich.edu:3128/
export https_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTP_PROXY=http://proxy.arc-ts.umich.edu:3128/

Rscript --vanilla $code_repo/model/r_scripts/spatialmap.R
