#!../py/bin/python3
import subprocess
import json
from pprint import pprint

# we have as many nodes as private IPs listed in the TF state
tfinfo = json.load(open('terraform.tfstate','r'))
expected_nodes = len(tfinfo['outputs']['compute_node_private_ips']['value'])

print(f"Waiting for {expected_nodes} compute nodes to mount NFS directory from head node")
while True:
    proc = subprocess.Popen(["./ssh-head-node.sh", "sudo", "ls", "/proc/fs/nfsd/clients"],
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stdout, stderr = proc.communicate()
    proc.wait()
    mounted = len(stdout.splitlines())
    if expected_nodes == mounted:
        print(f"All {mounted} nodes are ready.")
        break
    else:
        print(f"  {mounted} nodes are ready. {expected_nodes-mounted} are pending...")

