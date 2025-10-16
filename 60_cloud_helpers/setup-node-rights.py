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


node_list = run_az_command(["az", "vm", "list", "--resource-group", "SoIBAnalysis", "-d"])
for node in node_list:
    if not node['name'].startswith('vm-compute-node'):
        continue
    print(node['name'])
    print(f"  id={node['id']}")
    print("  Setting identity...")
    sid_result = run_az_command([
                   "az", "vm", "identity", "assign",
                   "--resource-group", node["resourceGroup"],
                   "--name", node["name"]])
    sid = sid_result["systemAssignedIdentity"]
    #pprint(sid_result)
    print("  Allowing self management for deallocate rights...")
    current_role = None
    max_retry = 10
    for retry in range(max_retry):
        assign_role = run_az_command([
                   "az", "role", "assignment", "create",
                   "--assignee", sid,
                   "--role", 'Virtual Machine Contributor',
                   "--scope", node["id"]])
        try:
            current_role = assign_role["roleDefinitionName"]
            print("  Setup role:", current_role)
            break
        except:
            print("  Retry ", (retry+1))
            if retry==(max_retry-1):
                pprint(assign_role)
    if not current_role:
        print(f"Failed to setup management rights for {node['name']}")
        sys.exit(-1)
#pprint(result)
