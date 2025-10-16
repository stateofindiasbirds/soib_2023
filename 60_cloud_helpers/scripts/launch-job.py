#!/usr/bin/env python3
import subprocess
import time
import glob
import os

cmd = "nohup /bin/sh /shared/scripts/node-run-jobs.sh 2>&1 > /dev/null &"
print("Starting jobs on:")
os.chdir("/shared/config")
nodes = glob.glob('*')
for node in nodes:
    print(f'  {node}')
    proc = subprocess.Popen(["ssh", node, cmd], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
time.sleep(1)
print("\nCompute nodes will automatically shutdown once done.")
print("Please monitor outputs to track progress.")

