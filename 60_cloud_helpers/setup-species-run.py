# Sets up data and configuration file
#
from argparse import ArgumentParser
from pathlib import Path
import shutil
import os
import pyreadr
from pprint import pprint
import sys
import subprocess
import os
import stat

# ssh-keyscan hostname >> ~/.ssh/known_hosts

parser = ArgumentParser()
parser.add_argument('-n','--nodes', type=int, required=True)
parser.add_argument('-t','--threads', type=int, required=True)
parser.add_argument('-m','--mask', type=str, required=True)
parser.add_argument('-s','--species', type=str, action='append', default=[])
parser.add_argument('-a','--assignment', type=str, default="1:1000")
parser.add_argument('-p','--package', type=str, default="shared")
parser.add_argument('-c','--config_R', type=str, default="config.R") # name of config file
parser.add_argument('--arch', type=str, default="x86_64")
parser.add_argument('--slim', action='store_true')
args = parser.parse_args()

# add quoted species
species_to_process = ",".join(list(map(lambda x: '"'+x+'"', args.species)))
compute_nodes = args.nodes
threads = args.threads
mask = args.mask
arch = args.arch

trends_list = []

assignment_limit = 1000

assignment_min, assignment_max = map(lambda x:int(x), args.assignment.split(':'))
if ((assignment_min<1) or (assignment_min>assignment_limit) or
   (assignment_max<1) or (assignment_max>assignment_limit) or
   (assignment_min>assignment_max)):
    raise ValueError("Assignment value out of range")

num_assignments = (assignment_max-assignment_min+1)
if (num_assignments < args.nodes):
    raise ValueError("Can't have less assignments than nodes")

node_names = []
for i in range(compute_nodes):
    per_node = num_assignments//compute_nodes
    assignment_start = ((i)*per_node)
    assignment_end = (i+1)*per_node
    assignment_start += (assignment_min)
    assignment_end += (assignment_min-1)
    if i==(compute_nodes-1):
        assignment_end = assignment_max
    if compute_nodes == 1:
        node = 'localhost'
    else:
        node = f'vm-compute-node-{i+1}'
    print(f"node:{node} {assignment_start}:{assignment_end}")
    node_names.append(node)
    cfg_path = Path(f'{args.package}/config/{node}')
    cfg_file = os.path.join(cfg_path, args.config_R)
    cfg_text = f"""threads <- {threads}
container <- TRUE
cur_mask <- "{mask}"
my_assignment <- {assignment_start}:{assignment_end}
species_to_process <- c(
{species_to_process})
force_trends_computation <- FALSE
reproducible_run <- TRUE
"""
    cfg_path.mkdir(parents=True, exist_ok=True)
    open(cfg_file, 'w').write(cfg_text)
    print(cfg_file)

    # Add trends
    for idx in range(assignment_start,assignment_end+1):
        # Paths are relative to the mask directory, which is where
        # we'll store the done list
        trends_list.append(f'{node}/{idx}/trends_{idx}.csv')

script_dir = Path(f"{args.package}/scripts")
script_dir.mkdir(parents=True, exist_ok=True)
headnode_script = f'{args.package}/scripts/cluster-setup.sh'
hf = open(headnode_script, "w")
hf.write('#!/bin/sh\n')
hf.write('set -e\n') # exit on error
for i in range(compute_nodes):
    hf.write(f'ssh-keyscan vm-compute-node-{i+1} >> ~/.ssh/known_hosts\n')
for i in range(compute_nodes):
    hf.write(f'ssh vm-compute-node-{i+1} /bin/sh /{args.package}/scripts/compute-node-setup.sh\n')
hf.write(f'echo "Cluster setup OK"\n')
hf.close()
os.chmod(headnode_script, stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR)

shutil.copy('scripts/compute-node-setup.sh', f'{args.package}/scripts/compute-node-setup.sh')
shutil.copy('scripts/head-node-setup.sh', f'{args.package}/scripts/head-node-setup.sh')
shutil.copy('scripts/node-run-jobs.sh', f'{args.package}/scripts/node-run-jobs.sh')
shutil.copy('scripts/launch-job.py', f'{args.package}/scripts/launch-job.py')
shutil.copy('scripts/job-status.py', f'{args.package}/scripts/job-status.py')
if compute_nodes == 1:
    shutil.copy('scripts/run_container.sh', f'{args.package}/run_container.sh')
    if not args.slim:
        shutil.copy('scripts/install-x86_64-container.sh', f'{args.package}/install-x86_64-container.sh')
        shutil.copy('scripts/install-aarch64-container.sh', f'{args.package}/install-aarch64-container.sh')

# Add generic data files
data_dir = Path(f"{args.package}/data")

# add all the data files required for mask
md = pyreadr.read_r("../00_data/analyses_metadata.RData")
metadata = md['analyses_metadata']
#pprint(metadata['MASK'])
mask_dir = metadata[metadata['MASK']==mask]['DATA.PATH'].values[0]
mask_dir, unused_fname = os.path.split(mask_dir)

tgt_mask_dir = Path(f"{args.package}/data/{mask_dir}")
tgt_mask_dir.mkdir(parents=True, exist_ok=True)
for fname in ['dataforanalyses.RData-data_opt',
              'dataforanalyses.RData-metadata',
              'species_names.RData',
              'specieslists.RData',
              'timegroups.RData']:
    src_fname = os.path.join('..',mask_dir,fname)
    tgt_fname = os.path.join(tgt_mask_dir,fname)
    print(f"{src_fname} -> {tgt_fname}")
    shutil.copy(src_fname, tgt_fname)

print(f'Copying {num_assignments}({assignment_min}:{assignment_max}) random group ids from ../{mask_dir} to {tgt_mask_dir}')
for i in range(assignment_min,assignment_max+1):
    rgid = f"rgids-{i}.RData"
    src_fname = os.path.join('..',mask_dir, rgid)
    tgt_fname = os.path.join(tgt_mask_dir,rgid)
    shutil.copy(src_fname, tgt_fname)

print("Copying common data files")
data2_dir = Path(f"{args.package}/data/00_data")
data2_dir.mkdir(parents=True, exist_ok=True)
shutil.copy('../00_data/analyses_metadata.RData', f'{args.package}/data/00_data/analyses_metadata.RData')
shutil.copy('../00_data/current_soib_migyears.RData', f'{args.package}/data/00_data/current_soib_migyears.RData')

print("Generating done list")
output_dir = Path(f"{args.package}/output/{mask}")
output_dir.mkdir(parents=True, exist_ok=True)
done_file = open(os.path.join(output_dir, "done.list"),"w")
for trends_fname in trends_list:
    done_file.write(f"{trends_fname}\n")

print("Creating log directories...")
for node in node_names:
    log_dir = Path(f"{args.package}/logs/{node}")
    log_dir.mkdir(parents=True, exist_ok=True)

if not args.slim:
    for this_arch in ['x86_64', 'aarch64']:
        print(f"Copying container for {this_arch}")
        container_dir = Path(f"{args.package}/container/{this_arch}")
        container_dir.mkdir(parents=True, exist_ok=True)
        src_container = f'../50_container/soib-container/{this_arch}/soib.tar'
        tgt_container = os.path.join(container_dir,'soib.tar')
        shutil.copy(src_container, tgt_container)
        prev_cwd = os.getcwd()
        os.chdir(container_dir)
        print(f"Compressing container...")
        subprocess.run(["xz","-T0","soib.tar"])
        os.chdir(prev_cwd)
