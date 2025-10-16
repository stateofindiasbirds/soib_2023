#!/bin/bash
#
# Job setup script EXAMPLE
# Please customize this for your own environment
# This file is provided as a simple, fast to run example

# Required: Start from scratch
rm -rf shared

# Run the Country mask using 4 nodes. All the nodes will
# run 96 threads.
#
# Assignment range of 1:1000 will be split between these
# nodes (default)
#
# Job config will be stored in the config.R (default)
# use aarch64 as architecture depending on
# compute nodes
../py/bin/python3 setup-species-run.py \
	--mask none \
	--nodes 4 \
	--threads 96 \
	--assignment 1:1000 \
	--arch aarch64

# It's possible to have more than 1 species run here, but that will be
# considered "advanced" usage
