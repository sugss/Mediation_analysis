#PBS -l walltime=35:00:00
#PBS -l select=1:ncpus=1:mem=50gb
#PBS -N clust

cd /rds/general/user/is2020/home/THESIS/analysis/3-clustering
module load anaconda3/personal
source activate thesis


data_path=/rds/general/user/is2020/home/THESIS/analysis/outputs/t2_scaled.rds


Rscript 3b-job_script.R $data_path 

# EDIT line 20 and 26
