#!/bin/bash -l

#SBATCH --partition=atlas
#SBATCH --qos=normal
#SBATCH --job-name=landopt
#SBATCH --output=/90daydata/geoecoservices/optimization/landcomplex-%j.out
#SBATCH --error=/90daydata/geoecoservices/optimization/landcomplex-%j.err
#SBATCH --account=geoecoservices
#SBATCH --mail-user=likevin@psu.edu
#SBATCH --mail-type=NONE
#SBATCH --time=24:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --get-user-env              ### Import your user environment setup
#SBATCH --array=1-19                ### Array index

module purge

module load miniconda3
source activate
conda activate r-invest2

srun Rscript --vanilla /project/geoecoservices/optimization/landscape-complexity/scripts/atlas_max_one.R ${SLURM_ARRAY_TASK_ID}