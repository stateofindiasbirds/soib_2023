#!/bin/bash
remote=azureuser@`terraform output -raw head_node_public_ip`
ssh -A $remote /shared/scripts/launch-job.py
