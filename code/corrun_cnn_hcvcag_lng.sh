#!/usr/bin/env bash

## Wrapper skin to run on different computers
## CNN ensemble models unto UKBB subjects
## Longitudinal version
## Uses *shuffled* list to allow parallel run

# Needed environment
#conda_env=pytorch-1.10.1
conda_env=hvr_ukbb

# Script
script=/ipl/ipl27/sfernandez/hvr_ukbb/code/cnn_hcvcag_lng.sh

# Check that conda is loaded; if not, source it.
mamba_path=/export01/data/sfernand/mamba/etc/profile.d/conda.sh
if [ -e $mamba_path ]
then
	[ -z $(which conda) ] && . $mamba_path && echo "Loaded mamba"
	[ "$CONDA_DEFAULT_ENV" = "$conda_env" ] || conda activate $conda_env
else
	conda_path=/ipl/quarantine/conda/etc/profile.d/conda.sh
	[ -z $(which conda) ] && . $conda_path && echo "Loaded conda"
	[ "$CONDA_DEFAULT_ENV" = "$conda_env" ] || conda activate $conda_env
fi

[ -f $script ] && source $script
