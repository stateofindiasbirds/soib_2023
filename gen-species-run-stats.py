#
# Generates stats for species trend runs.
#
# Species name, time to process species trends, peak ram
# are captured. May be used to "schedule" tasks in a memory
# aware manner
#
import pyreadr
from glob import glob
from pprint import pprint
import pandas as pd
import argparse
from copy import deepcopy

parser = argparse.ArgumentParser()
parser.add_argument("stats_path", nargs='?', default='output/localhost/none/stats')
parser.add_argument("out_path", nargs='?', default='01_analyses_full')

args = parser.parse_args()
print(f'Using masked path: {args.stats_path}')
results = []
overall_time = 0
for filename in glob(f'{args.stats_path}/*.RData'):
    rdata = pyreadr.read_r(filename)
    run_stats = rdata['run_stats'].to_numpy()
    data_rows = int(run_stats[0,0])
    time = run_stats[0,1]
    max_ram = run_stats[0,2]
    pid = run_stats[0,3]
    results.append([time, max_ram, data_rows, pid, filename])
    overall_time += time

# sort in order of time, high to low
results.sort(key=lambda x:x[0], reverse=True)

for i in range(len(results)):
    print(i+1, results[i])
print("Number of species =",len(results))
print(f"Total time = {overall_time} avg = {overall_time/16}")

# Simulate run
cores = 16
running = deepcopy(results[:cores]) # start of all threads first
cur_t = 0
max_ram = 0
next_species = cores

while len(running)>0:
    times = list(map(lambda x: x[0], running))
    max_ram = max(max_ram,sum(map(lambda x: x[1], running)))
    # shortest time remaining job will finish first
    min_t = min(times)
    # remove the shortest job (thread)
    del running[times.index(min_t)]
    # trim job running time by this
    for species in running:
        species[0] -= min_t
    # advance our time by this much
    cur_t += min_t
    #print(cur_t)
    # If we have more, we can add, else we'll be in drain phase
    if next_species<len(results):
        running.append(deepcopy(results[next_species]))
        next_species += 1

print(f'Time at end = {cur_t}')
print(f'Total Peak RAM = {max_ram/1000} GB')

def extract_species(path):
    # path is directory followed by species.RData
    # we'll just return between last / and .
    return path[path.rfind('/')+1:path.rfind('.')]

species_order = list(map(lambda x: extract_species(x[4]), results))
time_order = list(map(lambda x: x[0], results))
ram_order = list(map(lambda x: x[1], results))
df_data = {
  'species_name' : species_order,
  'time' : time_order,
  'peakRAM' : ram_order
}

df = pd.DataFrame(df_data)
# This is hacky - assumes we have only one. Ideally we have one
# "profile" per mask, if the files are large enough
outfile = f"{args.out_path}/species_run_stats.RData"
pyreadr.write_rdata(outfile, df, df_name="species_run_stats")
print("Generated:", outfile)
