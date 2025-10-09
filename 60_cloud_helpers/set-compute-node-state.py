#!../py/bin/python3
# Takes all the compute nodes and gives them azure rights to
# manage themselves.  This ensures that each node can deallocate
# itself, stopping billing
import subprocess
import json
from pprint import pprint

def run_az_command(command):
    try:
        result = subprocess.run(command, capture_output=True, text=True, check=True)
        result = json.loads(result.stdout)
        return result
    except subprocess.CalledProcessError as e:
        print(f"Error executing Azure CLI command: {e}")
        print(f"Stderr: {e.stderr}")
        raise RuntimeError("Failed to run AZ command")
    except json.JSONDecodeError:
        print("Error decoding JSON output from Azure CLI.")
        raise RuntimeError("Bad JSON from AZ command")

#print("All VMs are:")
#node_list = run_az_command(["az", "vm", "list", "--resource-group", "SoIBAnalysis", "-d"])
#vm_list = []
#for node in node_list:
#    print(node['name'])
#    print(f"  id={node['id']}")
#    vm_list.append(node['id'])

def check_node_status():
    vm_list = []
    needs_power_on = False
    print("State of VMs now is:")
    node_list = run_az_command(["az", "vm", "list", "--resource-group", "SoIBAnalysis", "-d"])
    #print(json.dumps(result, indent=4))
    for node in node_list:
        print(node['name'])
        print(f"  id={node['id']}")
        vm_list.append(node['id'])
        powerState = node['powerState']
        if powerState != "VM running":
            needs_power_on = True
        print(f"  PowerState={node['powerState']}")
        print(f"  VM Size={node['hardwareProfile']['vmSize']}")
    return needs_power_on, vm_list

all_not_on, vm_list = check_node_status()

if all_not_on:
    print("Ensuring ALL are RUNNING...")
    result = run_az_command(["az", "vm", "start", "--ids"]+vm_list)
    all_not_on, vm_list = check_node_status()
    if all_not_on:
        print("Unable to power on. Retry.")
        sys.exit(-1)

print("All nodes are running, you may use them, or do terraform destroy to deallocate them NOW")
