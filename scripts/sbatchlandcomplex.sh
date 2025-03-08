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
#SBATCH --ntasks=48

module purge

module load miniconda3
source activate
conda activate r-invest2

srun Rscript --vanilla 