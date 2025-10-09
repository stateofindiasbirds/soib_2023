#!/bin/bash
remote=azureuser@`terraform output -raw head_node_public_ip`
echo "Syncing shared directory to $remote"
rsync --progress -avz shared/* $remote:/shared/
