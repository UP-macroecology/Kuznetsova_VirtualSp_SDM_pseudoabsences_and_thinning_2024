#!/bin/bash

#SBATCH --job-name=select_07_cv_job.sh.sh
#SBATCH --mail-type=ALL
#SBATCH --mail-user=kuznetsov1@uni-potsdam.de
#SBATCH --output=select_07_cv_output.log  # Standard output
#SBATCH --error=select_07_cv_error.log    # Standard error
#SBATCH --nodelist=ecoc9             # name of node
#SBATCH --nodes=1                    # on a single node
#SBATCH --ntasks=1                   # with a single task (this should always be 1, apart from special cases)
#SBATCH --cpus-per-task=1           # with that many cpu cores
#SBATCH --time=10:00:00              # maximum runtime of the job as "d-hh:mm" maximal runtime is 5 days!  
#SBATCH --mem=15gb                   # will require that amount of RAM at maximum (if the process takes more it gets killed)
   



# Change to the directory where the job was submitted
cd ${SLURM_SUBMIT_DIR}

# Run the R script with arguments
Rscript /import/ecoc9/data-zurell/kuznetsova/pseudoabsences/scripts/select_07_cv.R