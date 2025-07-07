#!/bin/bash

#SBATCH --job-name=GLMs_GAMs_job.sh
#SBATCH --mail-type=ALL
#SBATCH --mail-user=kuznetsov1@uni-potsdam.de
#SBATCH --output=GLMs_GAMs_output.log  # Standard output
#SBATCH --error=rGLMs_GAMs_error.log    # Standard error
#SBATCH --nodelist=ecoc9             # name of node
#SBATCH --nodes=1                    # on a single node
#SBATCH --ntasks=1                   # with a single task (this should always be 1, apart from special cases)
#SBATCH --cpus-per-task=40           # with that many cpu cores
#SBATCH --time=10:00:00              # maximum runtime of the job as "d-hh:mm" maximal runtime is 5 days!  
#SBATCH --mem=30gb                   # will require that amount of RAM at maximum (if the process takes more it gets killed)
   



# Change to the directory where the job was submitted
cd ${SLURM_SUBMIT_DIR}


# Run the R script
Rscript /import/ecoc9/data-zurell/kuznetsova/pseudoabsences/scripts/6_GLMs_GAMs.R