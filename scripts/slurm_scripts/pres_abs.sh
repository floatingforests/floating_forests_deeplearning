#!/bin/bash

# Sample slurm submission script for the Gibbs compute cluster
# Lines beginning with # are comments, and will be ignored by
# the interpreter.  Lines beginning with #SBATCH are directives
# to the scheduler.  These in turn can be commented out by
# adding a second # (e.g. ##SBATCH lines will not be processed
# by the scheduler).
# to run, sbatch script.sh
#
#
# set name of job
#SBATCH --job-name=pres_abs_resnet50_ff_image_mod
#

# set the number of nodes
##SBATCH -N1

# set the number of processes per node
#SBATCH -n 10

# set the memory
#SBATCH --mem=64G   

#set an account to use
#if not used then default will be used
##SBATCH --account=scavenger

# set the number of GPU cards per node
# --gres=gpu[[:type]:count]
##SBATCH --gres=gpu:GTX980:4

#Or can use this
#SBATCH --gres=gpu:4


# set max wallclock time  DD-HH:MM:SS
#SBATCH --time=14-10:00:00


#To get error and output
#SBATCH --error=pres_abs_resnet50_ff_image_mod.err
#SBATCH --output=pres_abs_resnet50_ff_image_mod.out
#


#Optional
# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# send mail to this address
#SBATCH --mail-user=jarrett.byrnes@umb.edu

# Put your job commands here, including loading any needed
# modules.

# module load
module load proj-7.1.0-gcc-8.4.0-sjt4ita
module load R/4.0.3
module load python/3.5.1
module load cuda/10.1-update2
module load gdal-3.2.0-gcc-8.4.0-fpys6w7
module load geos-3.8.1-gcc-8.4.0-awcmh22

#cd to where the script lives
cd /home/jarrett.byrnes/floating-forests/floating_forests_deeplearning/scripts/presence_abscence_model

#execute
Rscript ff_resnet_tiff_generator.R

