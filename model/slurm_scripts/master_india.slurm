#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=1:00:00
#SBATCH --job-name=master_india
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

source /etc/profile.d/http_proxy.sh
export today=$(date +%Y-%m-%d)
export production=FALSE

# Set variables based on testing or production
if [[ $production == "TRUE" ]]; then
  export code_repo="$HOME/cov-ind-19"
  export data_repo="$HOME/cov-ind-19-data"
else
  export code_repo="$HOME/cov-ind-19-iris"
  export data_repo="$HOME/cov-ind-19-test"
fi

module load Rtidyverse/4.2.0

scraper=$(sbatch $code_repo/model/slurm_scripts/scraper.slurm | awk '{print $NF}')
cowin=$(sbatch $code_repo/model/slurm_scripts/pull_cowin_data.slurm | awk '{print $NF}')
bharat=$(sbatch $code_repo/model/slurm_scripts/covid19bharat_data_scraper.slurm | awk '{print $NF}')
commit=$(sbatch --dependency=afterok:$scraper:$cowin:$bharat $code_repo/model/slurm_scripts/commitscraper.slurm | awk '{print $NF}')
nat_seirfansy=$(sbatch --dependency=afterok:$scraper:$cowin:$bharat $code_repo/model/slurm_scripts/nat_seirfansy.slurm | awk '{print $NF}')
covid19india_package_data=$(sbatch --dependency=afterok:$nat_seirfansy $code_repo/model/slurm_scripts/covid19india_package_data.slurm | awk '{print $NF}')
bharat_package_data=$(sbatch --dependency=afterok:covid19india_package_data $code_repo/model/slurm_scripts/bharat_package_data.slurm | awk '{print $NF}')
commit=$(sbatch --dependency=afterok:$covid19india_package_data:$bharat_package_data $code_repo/model/slurm_scripts/commitscraper.slurm | awk '{print $NF}')

covid19india_package_data6=$(sbatch --being=now+6hour $code_repo/model/slurm_scripts/covid19india_package_data.slurm | awk '{print $NF}')
commit=$(sbatch --dependency=afterok:$covid19india_package_data6 $code_repo/model/slurm_scripts/commitscraper.slurm | awk '{print $NF}')

covid19india_package_data12=$(sbatch --being=now+12hour $code_repo/model/slurm_scripts/covid19india_package_data.slurm | awk '{print $NF}')
commit=$(sbatch --dependency=afterok:$covid19india_package_data12 $code_repo/model/slurm_scripts/commitscraper.slurm | awk '{print $NF}')

covid19india_package_data18=$(sbatch --being=now+18hour $code_repo/model/slurm_scripts/covid19india_package_data.slurm | awk '{print $NF}')
commit=$(sbatch --dependency=afterok:$covid19india_package_data18 $code_repo/model/slurm_scripts/commitscraper.slurm | awk '{print $NF}')

sbatch --begin=now+24hour $code_repo/model/slurm_scripts/master_india.slurm
