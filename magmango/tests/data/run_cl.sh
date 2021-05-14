#!/bin/bash

#SBATCH --job-name=scfcb
#SBATCH --qos=regular
#SBATCH --nodes=1
#SBATCH --constraint=knl

#SBATCH --time=24:00:00

module load vasp/20181030-knl 

export OMP_NUM_THREADS=4

EXE="vasp_std"

time srun -n16 -c16 --cpu_bind=cores $EXE

