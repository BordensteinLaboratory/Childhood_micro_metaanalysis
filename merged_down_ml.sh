#!/bin/bash

#SBATCH --job-name=merged_rf
#SBATCH --mail-type=ALL
#SBATCH --mail-user=elizabeth.mallott@vanderbilt.edu
#SBATCH --nodes=1
#SBATCH --ntasks=12
#SBATCH	--cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=96:00:00
#SBATCH --output=/home/mallote/merged_ml.out
#SBATCH --error=/home/mallote/merged_ml.err

module purge all

module load GCC/10.2.0  OpenMPI/4.0.5 R/4.0.5 R-bundle-Bioconductor

Rscript --no-save merged_rf.R
