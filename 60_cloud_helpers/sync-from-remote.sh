#!/bin/bash
remote=azureuser@`terraform output -raw head_node_public_ip`
echo "Syncing $remote to cluster_results directory"
mkdir cluster_results
echo "Sync: outputs"
rsync -avz $remote:/shared/output/ cluster_results/output
echo "Sync: logs"
rsync -avz $remote:/shared/logs/ cluster_results/logs
echo "Sync: config (for reference)"
rsync -avz $remote:/shared/config/ cluster_results/config
