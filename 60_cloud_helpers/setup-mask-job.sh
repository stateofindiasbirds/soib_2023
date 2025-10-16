#!/bin/bash

# Required: Start from scratch
MASK="$1"
TGT="job_$MASK"
rm -rf $TGT

../py/bin/python3 setup-species-run.py \
	--mask "$MASK" \
	--nodes 1 \
	--threads 8 \
	--assignment 1:1000 \
	--arch x86_64 \
        --package "$TGT" \
	--slim
