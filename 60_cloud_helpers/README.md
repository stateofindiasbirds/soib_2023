# Cloud HOWTO

This directory shows howto use the cloud to accelerate SoIB compute.

Terraform is used to setup cloud resources. A head node and compute
nodes are allocated in the cloud. Microsoft Azure is used as the
cloud provider. This document refers to Virtual Machines as "nodes".

## Cloud Infrastructure

The head node is accessible over SSH.  It acts as the coordinator,
and external access point with a public IP address.

Compute nodes are the ones that do the compute. These must be
appropriately configured in main.tf, specifically as "size":

    resource "azurerm_linux_virtual_machine" "compute_nodes" {
      count               = var.compute_node_count
      name                = "vm-compute-node-${count.index + 1}"
      resource_group_name = azurerm_resource_group.cluster_rg.name
      location            = azurerm_resource_group.cluster_rg.location
      size                = "Standard_F4as_v6"
      admin_username      = "azureuser"

We're using the Ubuntu 24.04 LTS images in the compute nodes,
specifically the minimal variant:

    source_image_reference {
      publisher = "Canonical"
      offer     = "ubuntu-24_04-lts"
      sku       = "minimal"
      version   = "latest"
    }

The head node is configured slightly differently.  It runs a full blown
server installation:

    source_image_reference {
      publisher = "Canonical"
      offer     = "ubuntu-24_04-lts"
      sku       = "server"
      version   = "latest"
    }

The head node also runs a different VM size, as it's not going to be
actually doing the calculations:

    resource "azurerm_linux_virtual_machine" "head_node" {
      name                = "vm-head-node"
      resource_group_name = azurerm_resource_group.cluster_rg.name
      location            = azurerm_resource_group.cluster_rg.location
      size                = "Standard_D4s_v3"
      admin_username      = "azureuser"


## Resource Sizing for the Cloud

First, you need to determine the "VM size" and the number of VMs you want
to run your cloud job on.

The "VM size" has to be ideally chosen to maximize cost-performance. For
the SoIB workload, ARM64 based VMs seem well suited, outperforming x86_64
machines in price-performance. e.g. a D16ps_v6 instance takes takes
roughly 1431 seconds for a single assignment iteration for the whole
country mask. An FS16as_v6 instance takes 1699 seconds. Both are 16 core
VMs with 64 GB of RAM, attached to premium SSD disks.

The number of VMs you need depends on the amount of time you have to
do the run (e.g. how many hours/days), and the/or the number and type
of VMs you are looking to deploy.  VMs are typically billed per minute,
and the other resources are also billed pro-rata (e.g. storage is 
billed per day).

As an example, we need 1000 iterations to run to complete the full country
species trends calculations. If each run takes say 1431 seconds (24 minutes)
on a VM, then we need roughly 400 hours of VM compute time to finish
the 1000 iterations. If 10 VMs are deployed, that's 40 hours to finish.
If 20 VMs are deployed, the job may finish in 20 hours. With 40 VMs, you'll
be done in 10 hours.

You could opt to deploy beefier VMs to tackle the job, especially if the
performance is found to scale well.  Let's say a D96ps_v6 is deployed.
It's 6x the cores of D16ps_v6. If we make the simplifying assumption
(this needs to be backed by benchmarking) that this VM runs 6x faster.
Then we're talking under 67 hours to finish with a single VM. With 10
VMs we can finish the job in under 8 hours, possibly accounting for
effects of multi-tenancy in the cloud.

## Installing Python

The scripts here depend on a python installation in the "py" subdirectory
in the code directory.  You first need to set it up, as follows:

    $ python3 -m venv py
    $ ./py/bin/pip3 install pyreadr

## Setting up the Cloud Job on your Local System

Edit the setup-cluster-job.sh script.  The commands there are self
explanatory. The existing example does a small cloud job run for
Uttarakhand.

    ../py/bin/python3 setup-species-run.py \
	    --mask Uttarakhand \
        --nodes 2 \
        --threads 4 \
        --assignment 6:15 \
        --arch x86_64

Let's say you want to run it for full country, on 8 nodes of Azure
VM size D96ps-v6 (96 core), you would change it to:

    ../py/bin/python3 setup-species-run.py \
	    --mask none \
        --nodes 8 \
        --threads 96 \
        --assignment 1:1000 \
        --arch arm64

Next, run the setup-cluster-job.sh script.  This sets up the
content of the "shared" subdirectory - which has packages
everything that goes to the cloud.

    $ ./setup-cluster-job.sh 
    node:vm-compute-node-1 6:10
    shared/config/vm-compute-node-1/config.R
    node:vm-compute-node-2 11:15
    shared/config/vm-compute-node-2/config.R
    ../01_analyses_states/Uttarakhand/dataforanalyses.RData-data_opt -> shared/data/01_analyses_states/Uttarakhand/dataforanalyses.RData-data_opt
    ../01_analyses_states/Uttarakhand/dataforanalyses.RData-metadata -> shared/data/01_analyses_states/Uttarakhand/dataforanalyses.RData-metadata
    ../01_analyses_states/Uttarakhand/species_names.RData -> shared/data/01_analyses_states/Uttarakhand/species_names.RData
    ../01_analyses_states/Uttarakhand/specieslists.RData -> shared/data/01_analyses_states/Uttarakhand/specieslists.RData
    ../01_analyses_states/Uttarakhand/timegroups.RData -> shared/data/01_analyses_states/Uttarakhand/timegroups.RData
    Copying 10(6:15) random group ids from ../01_analyses_states/Uttarakhand to shared/data/01_analyses_states/Uttarakhand
    Copying common data files
    Generating done list
    Creating log directories...
    Copying container for x86_64
    Compressing container...

At this point the following directories will be setup

- shared/config : Inside this directory, there will be a subdirectory for every
  compute node, with a single configuration file determining what will computations
  happen on that node
- shared/scripts : Scripts for usage
- shared/container : Compressed container package
- shared/data : Packages all the data
- shared/logs : This directory will hold the logs that compute nodes generate
- shared/output : All the trends files and intermediate results from compute nodes
  are generated in this directory

## Creating Job packages

An easy way to create packages to run full masks involves the use of the
setup-mask-job.sh script.  E.g. to build a package for the entire country,
run:

    $ ./setup-mask-job.sh ONEland

This will generate a directory "job_ONEland". This can be compressed and sent
to someone who will run the 1000 runs for ONEland.

## Running on the Cloud

### Prerequisites

You need to install "az" the Azure CLI.  Also install terraform.
Note that the steps given here are for linux hosts.

### Setup

First, edit main.tf with the right VM size as determined by you.

Then login to your account

    $ az login

Initialize terraform

    $ terraform init

### Head Node Setup

Check the resources that we will setup with terraform first:

    $ terraform plan

This will end with output like:

    Plan: 8 to add, 0 to change, 0 to destroy.

    Changes to Outputs:
      + compute_node_private_ips  = []
      + deployment_phase          = "HEAD NODE ONLY - Ready for manual setup"
      + head_node_private_ip      = (known after apply)
      + head_node_public_ip       = (known after apply)
      + next_steps                = "Head Node Only"
      + nfs_shared_directory_info = <<-EOT
            NFS Shared Directory Setup:
            ==========================

            Shared directory: /shared
            - Location: Available at /shared (head node always has NFS running)
            - Server: Head node (10.0.1.4:/shared)
            - Status: NFS server is always running and ready

            When compute nodes are deployed, they will automatically:
            - Mount /shared from head node
            - Have read/write access to shared directory
            - Auto-mount on boot via /etc/fstab

            Manual mount command for testing:
            mount -t nfs4 10.0.1.4:/shared /mnt/test
        EOT
      + ssh_connection_command    = (known after apply)
      + ssh_setup_instructions    = (known after apply)

Start the head node

    $ terraform apply

This will show the resources that will be allocated again. It will ask you
for confirmation:

    Do you want to perform these actions?
      Terraform will perform the actions described above.
      Only 'yes' will be accepted to approve.

      Enter a value:

Enter "yes" to allocate the head node.

    azurerm_resource_group.cluster_rg: Creating...
    azurerm_resource_group.cluster_rg: Still creating... [00m10s elapsed]
    azurerm_resource_group.cluster_rg: Creation complete after 15s [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster]
    azurerm_virtual_network.cluster_vnet: Creating...
    azurerm_public_ip.head_node_public_ip: Creating...
    azurerm_network_security_group.cluster_nsg: Creating...
    azurerm_network_security_group.cluster_nsg: Creation complete after 7s [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/networkSecurityGroups/nsg-cluster]
    azurerm_public_ip.head_node_public_ip: Creation complete after 8s [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/publicIPAddresses/pip-head-node]
    azurerm_virtual_network.cluster_vnet: Creation complete after 9s [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/virtualNetworks/vnet-cluster]
    azurerm_subnet.cluster_subnet: Creating...
    azurerm_subnet.cluster_subnet: Creation complete after 8s [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/virtualNetworks/vnet-cluster/subnets/subnet-cluster]
    azurerm_network_interface.head_node_nic: Creating...
    azurerm_network_interface.head_node_nic: Creation complete after 7s [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/networkInterfaces/nic-head-node]
    azurerm_network_interface_security_group_association.head_node_nsg_association: Creating...
    azurerm_linux_virtual_machine.head_node: Creating...
    azurerm_network_interface_security_group_association.head_node_nsg_association: Creation complete after 7s [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/networkInterfaces/nic-head-node|/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/networkSecurityGroups/nsg-cluster]
    azurerm_linux_virtual_machine.head_node: Still creating... [00m10s elapsed]
    azurerm_linux_virtual_machine.head_node: Still creating... [00m20s elapsed]
    azurerm_linux_virtual_machine.head_node: Still creating... [00m30s elapsed]
    azurerm_linux_virtual_machine.head_node: Still creating... [00m40s elapsed]
    azurerm_linux_virtual_machine.head_node: Still creating... [00m50s elapsed]
    azurerm_linux_virtual_machine.head_node: Creation complete after 54s [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Compute/virtualMachines/vm-head-node]

    Apply complete! Resources: 8 added, 0 changed, 0 destroyed.

And you will get some usage instructions as well:

    Outputs:

    compute_node_private_ips = []
    deployment_phase = "HEAD NODE ONLY - Ready for manual setup"
    head_node_private_ip = "10.0.1.4"
    head_node_public_ip = "52.190.20.156"
    next_steps = "Head Node Only"
    nfs_shared_directory_info = <<EOT
    NFS Shared Directory Setup:
    ==========================

    Shared directory: /shared
    - Location: Available at /shared (head node always has NFS running)
    - Server: Head node (10.0.1.4:/shared)
    - Status: NFS server is always running and ready

    When compute nodes are deployed, they will automatically:
    - Mount /shared from head node
    - Have read/write access to shared directory
    - Auto-mount on boot via /etc/fstab

    Manual mount command for testing:
    mount -t nfs4 10.0.1.4:/shared /mnt/test

    EOT
    ssh_connection_command = "ssh azureuser@52.190.20.156"
    ssh_setup_instructions = <<EOT
    SSH Access Instructions:
    =======================

    1. SSH to head node with agent forwarding:
       ssh -A azureuser@52.190.20.156

    2. From head node, SSH to compute nodes:
       ssh azureuser@10.0.1.5
       ssh azureuser@10.0.1.6

    Alternative methods:
    A) Use SSH agent forwarding (recommended):
       - Add key to agent: ssh-add ~/.ssh/id_rsa
       - SSH with -A flag: ssh -A azureuser@<head-node-ip>

    B) Copy private key to head node:
       scp ~/.ssh/id_rsa azureuser@52.190.20.156:~/.ssh/

    C) Use ProxyJump to connect directly:
       ssh -J azureuser@52.190.20.156 azureuser@10.0.1.5

    EOT

You can use the IP address to login, as described in the instructions:

    $ ssh -A azureuser@<head_node_ip>

Check the NFS mount

    azureuser@vm-head-node:~$ ls -l /shared/
    total 4
    -rw-r--r-- 1 root root 114 Aug 31 07:03 head_node_ready.txt
    azureuser@vm-head-node:~$ 

At this point, you can copy over the data files, using the following command.
This command copies potentially multiple GBs of data to the cloud head node.
So expect it to take some time:

    $ ./sync-to-remote.sh
    Syncing shared directory to azureuser@20.42.21.40
    The authenticity of host '20.42.21.40 (20.42.21.40)' can't be established.
    ED25519 key fingerprint is SHA256:2BBqNfVHnydvZ7aY05RTzgWUxs5BaZuvngEjJzRkzMA.
    This key is not known by any other names.
    Are you sure you want to continue connecting (yes/no/[fingerprint])? yes
    Warning: Permanently added '20.42.21.40' (ED25519) to the list of known hosts.
    sending incremental file list
    rsync: [generator] chgrp "/shared/." failed: Operation not permitted (1)
    ./
    config/
    config/vm-compute-node-1/
    config/vm-compute-node-1/config.R
    config/vm-compute-node-2/
    config/vm-compute-node-2/config.R
    container/
    container/x86_64/
    container/x86_64/soib.tar.xz
    data/
    data/00_data/
    data/00_data/analyses_metadata.RData
    data/00_data/current_soib_migyears.RData
    data/01_analyses_states/
    data/01_analyses_states/Uttarakhand/
    data/01_analyses_states/Uttarakhand/dataforanalyses.RData-data_opt
    data/01_analyses_states/Uttarakhand/dataforanalyses.RData-metadata
    data/01_analyses_states/Uttarakhand/rgids-10.RData
    data/01_analyses_states/Uttarakhand/rgids-11.RData
    data/01_analyses_states/Uttarakhand/rgids-12.RData
    data/01_analyses_states/Uttarakhand/rgids-13.RData
    data/01_analyses_states/Uttarakhand/rgids-14.RData
    data/01_analyses_states/Uttarakhand/rgids-15.RData
    data/01_analyses_states/Uttarakhand/rgids-16.RData
    data/01_analyses_states/Uttarakhand/rgids-7.RData
    data/01_analyses_states/Uttarakhand/rgids-8.RData
    data/01_analyses_states/Uttarakhand/rgids-9.RData
    data/01_analyses_states/Uttarakhand/species_names.RData
    data/01_analyses_states/Uttarakhand/specieslists.RData
    data/01_analyses_states/Uttarakhand/timegroups.RData
    logs/
    logs/vm-compute-node-1/
    logs/vm-compute-node-2/
    output/
    output/done.list
    scripts/
    scripts/cluster-setup.sh
    scripts/compute-node-setup.sh
    scripts/launch-job.py
    scripts/node-run-jobs.sh

    sent 474,131,240 bytes  received 566 bytes  1,643,437.80 bytes/sec
    total size is 474,025,505  speedup is 1.00
    rsync error: some files/attrs were not transferred (see previous errors) (code 23) at main.c(1338) [sender=3.2.7]

Install required packages on the head node (from local machine):

    $ ./do-head-node-setup.sh


# Allocating compute nodes

    $ terraform apply -var="compute_node_count=2"

Again, you will be asked for confirmation. Once you type "yes", you'll get two nodes added.

At this point, you may SSH from the head node to vm-compute-node-1 (10.0.1.5)
and vm-compute-node-2 (10.0.1.6). Note that the head node is vm-head-node
(10.0.1.4).

You'll also see that NFS mounts are active:

    $ ss -a | grep nfs
    tcp   LISTEN    0      64                                                   0.0.0.0:nfs                        0.0.0.0:*
    tcp   ESTAB     0      0                                                   10.0.1.4:nfs                       10.0.1.6:922
    tcp   ESTAB     0      0                                                   10.0.1.4:nfs                       10.0.1.5:721
    tcp   LISTEN    0      64                                                      [::]:nfs                           [::]:*


# Setting up the head node and compute nodes

You need to ensure that the compute nodes can deallocate themselves at the end of the job.
To do that:

    $ ./setup-node-rights.py

# Setting up the Cluster for Job Execution

    $  ./do-cluster-setup.sh

This script will take a few minutes.  It will install the dependencies required in
every compute node, including the azure CLI and the SoIB container.

# Starting the Compute Job

You can now start the job:

    $ ./do-cluster-job.sh
    Starting jobs on:
      vm-compute-node-1
      vm-compute-node-2
    Nodes will shutdown once done. Monitor outputs for progress.

# Checking job status

From your local node, run
    $ ./check-job-status.sh
    Monitoring: /shared/output/Uttarakhand
    2025-09-02 00:50:28 Total: 10, Pending: 10
    Done: /shared/output/Uttarakhand/vm-compute-node-1/6/trends_6.csv
    2025-09-02 00:51:36 Total: 10, Pending: 9
    Done: /shared/output/Uttarakhand/vm-compute-node-2/11/trends_11.csv
    2025-09-02 00:51:53 Total: 10, Pending: 8
    Done: /shared/output/Uttarakhand/vm-compute-node-1/7/trends_7.csv
    2025-09-02 00:52:39 Total: 10, Pending: 7
    Done: /shared/output/Uttarakhand/vm-compute-node-2/12/trends_12.csv
    2025-09-02 00:52:58 Total: 10, Pending: 6
    Done: /shared/output/Uttarakhand/vm-compute-node-1/8/trends_8.csv
    2025-09-02 00:53:42 Total: 10, Pending: 5
    Done: /shared/output/Uttarakhand/vm-compute-node-2/13/trends_13.csv
    2025-09-02 00:54:00 Total: 10, Pending: 4
    Done: /shared/output/Uttarakhand/vm-compute-node-1/9/trends_9.csv
    2025-09-02 00:54:43 Total: 10, Pending: 3
    Done: /shared/output/Uttarakhand/vm-compute-node-2/14/trends_14.csv
    2025-09-02 00:55:01 Total: 10, Pending: 2
    Done: /shared/output/Uttarakhand/vm-compute-node-1/10/trends_10.csv
    2025-09-02 00:55:44 Total: 10, Pending: 1
    Done: /shared/output/Uttarakhand/vm-compute-node-2/15/trends_15.csv
    2025-09-02 00:56:01 Total: 10, Pending: 0

At the end of the job, you will find that the compute nodes
have been deallocated.  This stops billing.  Run this command
locally

    $ az vm list -d -o table
    Name               ResourceGroup    PowerState      PublicIps    Fqdns    Location
    -----------------  ---------------  --------------  -----------  -------  ----------
    vm-compute-node-1  SOIB-CLUSTER     VM deallocated                        eastus
    vm-compute-node-2  SOIB-CLUSTER     VM deallocated                        eastus
    vm-head-node       SOIB-CLUSTER     VM running      20.42.21.40           eastus

# Retrieving Job Results

Run this command. It copies the results to subdirectory "cluster_results".

    $ ./sync-from-remote.sh

cluster_results has a few subdirectories:

- output contains the trends CSV files in a directory structure.
- config is a copy of the actual node configuration files used during the run
- logs are the log files generated

# Merge Job Results back into the tree

The results generated by the job need to be merged back to continue
with the rest of the local calculation.

Use the collect-trends-results.py script in the root directory to do
this:

    $ ./collect-trends-results.py -v cloud_helpers/cluster_results/output/
    mask= Uttarakhand
    Copying 10 files...
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-2/12/trends_12.csv -> 01_analyses_states/Uttarakhand/trends_12.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-2/14/trends_14.csv -> 01_analyses_states/Uttarakhand/trends_14.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-2/11/trends_11.csv -> 01_analyses_states/Uttarakhand/trends_11.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-2/15/trends_15.csv -> 01_analyses_states/Uttarakhand/trends_15.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-2/13/trends_13.csv -> 01_analyses_states/Uttarakhand/trends_13.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-1/7/trends_7.csv -> 01_analyses_states/Uttarakhand/trends_7.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-1/6/trends_6.csv -> 01_analyses_states/Uttarakhand/trends_6.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-1/8/trends_8.csv -> 01_analyses_states/Uttarakhand/trends_8.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-1/9/trends_9.csv -> 01_analyses_states/Uttarakhand/trends_9.csv
    cloud_helpers/cluster_results/output/Uttarakhand/vm-compute-node-1/10/trends_10.csv -> 01_analyses_states/Uttarakhand/trends_10.csv


# Freeing Up Resources

At any time, you can free up resources by running

    $ ./set-compute-node-state.py

Followed by

    $ terraform destroy

Note this you need to pull any data from the cloud before going ahead
and doing this. Else you will lose any data/setup done in the cloud
nodes.

You'll get the list of changes that will be made, followed by:

    Do you really want to destroy all resources?
      Terraform will destroy all your managed infrastructure, as shown above.
      There is no undo. Only 'yes' will be accepted to confirm.

      Enter a value:

Type "yes" if you want to destroy all the resources.

You'll see the resources being destroyed. In this particular case
the text below only shows the head node, but you will see the compute
nodes as well if they are allocated:

    azurerm_network_interface_security_group_association.head_node_nsg_association: Destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/networkInterfaces/nic-head-node|/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/networkSecurityGroups/nsg-cluster]
    azurerm_linux_virtual_machine.head_node: Destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Compute/virtualMachines/vm-head-node]
    azurerm_network_interface_security_group_association.head_node_nsg_association: Destruction complete after 5s
    azurerm_network_security_group.cluster_nsg: Destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/networkSecurityGroups/nsg-cluster]
    azurerm_linux_virtual_machine.head_node: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-...t.Compute/virtualMachines/vm-head-node, 00m10s elapsed]
    azurerm_network_security_group.cluster_nsg: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-...work/networkSecurityGroups/nsg-cluster, 00m10s elapsed]
    azurerm_network_security_group.cluster_nsg: Destruction complete after 12s
    azurerm_linux_virtual_machine.head_node: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-...t.Compute/virtualMachines/vm-head-node, 00m20s elapsed]
    azurerm_linux_virtual_machine.head_node: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-...t.Compute/virtualMachines/vm-head-node, 00m30s elapsed]
    azurerm_linux_virtual_machine.head_node: Destruction complete after 31s
    azurerm_network_interface.head_node_nic: Destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/networkInterfaces/nic-head-node]
    azurerm_network_interface.head_node_nic: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-...etwork/networkInterfaces/nic-head-node, 00m10s elapsed]
    azurerm_network_interface.head_node_nic: Destruction complete after 13s
    azurerm_subnet.cluster_subnet: Destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/virtualNetworks/vnet-cluster/subnets/subnet-cluster]
    azurerm_public_ip.head_node_public_ip: Destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/publicIPAddresses/pip-head-node]
    azurerm_subnet.cluster_subnet: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-...ks/vnet-cluster/subnets/subnet-cluster, 00m10s elapsed]
    azurerm_public_ip.head_node_public_ip: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-...etwork/publicIPAddresses/pip-head-node, 00m10s elapsed]
    azurerm_public_ip.head_node_public_ip: Destruction complete after 12s
    azurerm_subnet.cluster_subnet: Destruction complete after 12s
    azurerm_virtual_network.cluster_vnet: Destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster/providers/Microsoft.Network/virtualNetworks/vnet-cluster]
    azurerm_virtual_network.cluster_vnet: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-...t.Network/virtualNetworks/vnet-cluster, 00m10s elapsed]
    azurerm_virtual_network.cluster_vnet: Destruction complete after 13s
    azurerm_resource_group.cluster_rg: Destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster]
    azurerm_resource_group.cluster_rg: Still destroying... [id=/subscriptions/c8be4a7a-da50-4ea0-aa2b-9543e35494ef/resourceGroups/rg-cluster, 00m10s elapsed]
    azurerm_resource_group.cluster_rg: Destruction complete after 17s

    Destroy complete! Resources: 8 destroyed.

As you can see from the messages, all the resources are deallocated, VMs
are stopped and so on once you say "yes".  Destroying resources ensures
that they are not billed into your account after this.

## Node Management Commands

Resources may be managed using the Azure web interface, or by using the CLI.
CLI is a convenient 

### Query overall status

Sometimes, you'll be worried about what's going on in the cluster ? Which
nodes are running, etc.  Use this handy command to see the status at a
glance:

    $ az vm list -d -o table
    Name               ResourceGroup    PowerState      PublicIps       Fqdns    Location
    -----------------  ---------------  --------------  --------------  -------  ----------
    vm-compute-node-1  SOIB-CLUSTER     VM deallocated                           eastus
    vm-compute-node-2  SOIB-CLUSTER     VM deallocated                           eastus
    vm-head-node       SOIB-CLUSTER     VM running      172.191.13.171           eastus

### Query Status of a single VM

    $ az vm show --resource-group SoIBAnalysis --name vm-compute-node-1 -d --query powerState

Status could be "VM running", "VM stopped" or "VM deallocated"

### Start VM

    $ az vm start --resource-group SoIBAnalysis --name vm-compute-node-1

### Stop VM

    $ az vm stop --resource-group SoIBAnalysis --name vm-compute-node-1

    About to power off the specified VM...
    It will continue to be billed. To deallocate a VM, run: az vm deallocate.

###

$ az vm show --resource-group SoIBAnalysis --name vm-compute-node-1 --query hardwareProfile.vmSize

# Quick Reference Commands

Once you get used to running the commands, you might find the following list and sequence
a better way to remember the order:

    $ ./setup-cluster-job.sh
    $ terraform apply
    $ ./sync-to-remote.sh
    $ ./do-head-node-setup.sh
    $ terraform apply -var="compute_node_count=2"
    $ ./setup-node-rights.py
    $ ./compute-nodes-nfs-ready.py
    $ ./do-cluster-setup.sh
    $ ./start-cluster-job.sh
    $ ./check-job-status.sh
    $ ./sync-from-remote.sh
    $ ./set-compute-node-state.py
    $ terraform destroy
    $ cd ..
    $ ./collect-trends-results.py -v cloud_helpers/cluster_results/output

At any time, to check cluster status, run:
    $ ./show-cluster-status.sh
