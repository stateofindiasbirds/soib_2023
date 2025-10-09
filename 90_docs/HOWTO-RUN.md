# HOWTO run SoIB

Performance of some steps in the SoIB pipeline has been improved.
Specifically, part 3 step 1. This is a RAM and compute intensive step.
The detailed path to achieving the improvements are documented in the
branch
[optimize_ram of this fork](https://github.com/shreekumar3d/soib_2023/tree/optimize_ram) .
The implementation in this repository is a mirror of that, but
squashed in terms of learning/commits.

The instructions in this file only run everything till Part 3, Step 1
i.e. computing species trends.  The rest of the steps have to be run
according to the compute pipeline.

This file basically jots down the exact steps I followed to run things
at this point in time. Note all the runtimes are with reference to
Shree's desktop - a 16 core AMD Ryzen 9 7950X with 64 GB RAM. Most
of the steps use 16 threads max. For XZ compression in one of the
steps, all cores are used.  GLM performance suffers with all cores
active, so this is the best configuraion for this particular machine.
Systems with higher memory bandwidth can run more threads.

## Start

After git clone, you need to copy two data files to 00_data:

- ebd_IN_unv_smp_relAug-2025.txt
- 00_data/ebd_sensitive_relAug-2025_IN.txt

The exact names may differ, and are in p1s1.R.
This command copies the two txt files to the right place:

    $ cp ~/biz/soib/data-packages/jul-30-ashwin/ebd_* 00_data/

For the next step to *not* fail, you need to up your stack limit:

    $ ulimit -s unlimited

## Run Part 1, Step 1

    $ time Rscript p1s1.R

    Reading and cleaning raw data: 1718.835 sec elapsed

    real	28m43.566s
    user	22m31.544s
    sys	    6m6.424s

Running this also overwrites these files which are under source control:

- 00_data/analyses_metadata.RData
- 00_data/current_soib_migyears.RData
- 00_data/eBird_location_data.csv
- 00_data/indiaspecieslist.csv

Next copy sensitive species map - maps_pa_sf.RData. I do it as follows:

    $ cp ~/biz/soib/data-packages/jul-30-ashwin/Spatial\ files/maps_pa_sf.RData 00_data/

## Run Part 1, Step 2

Run p1s2.R

    $ time Rscript p1s2.R

    Adding map and grid variables to dataset: 496.597 sec elapsed

    real	8m19.929s
    user	8m4.873s
    sys	    0m14.636s

Note thas step Does not change any tracked files.

## Run Part 1, Step 3

Run p1s3.R

    $ time Rscript p1s3.R

    Processing and filtering data for analyses: 999.386 sec elapsed

    real	16m42.386s
    user	15m25.444s
    sys	    1m16.115s

Running p1s3.R changes these files, which are all tracked under git:

- 01_analyses_full/fullspecieslist.csv
- 01_analyses_full/specieslists.RData
- 01_analyses_mask-ONEland/specieslists.RData
- 01_analyses_mask-PA/specieslists.RData
- 01_analyses_mask-cropland/specieslists.RData
- 01_analyses_mask-woodland/specieslists.RData
- 01_analyses_states/Andaman and Nicobar Islands/specieslists.RData
- 01_analyses_states/Andhra Pradesh/specieslists.RData
- 01_analyses_states/Arunachal Pradesh/specieslists.RData
- 01_analyses_states/Assam/specieslists.RData
- 01_analyses_states/Bihar/specieslists.RData
- 01_analyses_states/Chandigarh/specieslists.RData
- 01_analyses_states/Chhattisgarh/specieslists.RData
- 01_analyses_states/Dadra and Nagar Haveli/specieslists.RData
- 01_analyses_states/Daman and Diu/specieslists.RData
- 01_analyses_states/Delhi/specieslists.RData
- 01_analyses_states/Goa/specieslists.RData
- 01_analyses_states/Gujarat/specieslists.RData
- 01_analyses_states/Haryana/specieslists.RData
- 01_analyses_states/Himachal Pradesh/specieslists.RData
- 01_analyses_states/Jammu and Kashmir/specieslists.RData
- 01_analyses_states/Jharkhand/specieslists.RData
- 01_analyses_states/Karnataka/specieslists.RData
- 01_analyses_states/Kerala/specieslists.RData
- 01_analyses_states/Ladakh/specieslists.RData
- 01_analyses_states/Lakshadweep/specieslists.RData
- 01_analyses_states/Madhya Pradesh/specieslists.RData
- 01_analyses_states/Maharashtra/specieslists.RData
- 01_analyses_states/Manipur/specieslists.RData
- 01_analyses_states/Meghalaya/specieslists.RData
- 01_analyses_states/Mizoram/specieslists.RData
- 01_analyses_states/Nagaland/specieslists.RData
- 01_analyses_states/Odisha/specieslists.RData
- 01_analyses_states/Puducherry/specieslists.RData
- 01_analyses_states/Punjab/specieslists.RData
- 01_analyses_states/Rajasthan/specieslists.RData
- 01_analyses_states/Sikkim/specieslists.RData
- 01_analyses_states/Tamil Nadu/specieslists.RData
- 01_analyses_states/Telangana/specieslists.RData
- 01_analyses_states/Tripura/specieslists.RData
- 01_analyses_states/Uttar Pradesh/specieslists.RData
- 01_analyses_states/Uttarakhand/specieslists.RData
- 01_analyses_states/West Bengal/specieslists.RData

## Run Part 2, Step 1a

This is a variant of the original Part 2, Step 1.

Everything is ready to run the next step

    $ time Rscript p2s1.R

    ...
    generated random group IDs for all states: 175.028 sec elapsed

    real	8m48.239s
    user	8m25.126s
    sys  	0m22.606s


## Run Part 2, Step 2a

This is a variant of the original Part 2, Step 2.

Everything is ready to run the next step. Unlike the original
code, this does not generate remapped data. It merely optimizes
the data for each mask. Runtime uses the optimized data and
randomgroupids which are stored separately to get to the actual
data:

    $ time Rscript p2s2.R

    ...
    Generated subsampled data for Puducherry state: 1.736 sec elapsed
    Generated subsampled data for all states: 260.871 sec elapsed

    real	9m15.443s
    user	53m27.440s
    sys 	0m29.929s

## Run Part 3, Step 1

This computes the species trends. It is a compute intensive step.

The configuration for this step is in config/localhost/config.R

By default, the entire country is run:

    $ export OMP_NUM_THREADS=1
    $ time Rscript p3s1.R
    ....
    Species trends for none: 1/1: 3247.166 sec elapsed
    Species trends for mask none (sims 1:1): 3259.361 sec elapsed

    real	54m21.398s
    user	804m45.533s
    sys	    61m58.485s


First time runs don't dump info about script runtime estimates,
as it is not available. To fix it, generate the stats using:

    $ ~/tools/py/bin/python3 gen-species-run-stats.py 01_analyses_full/trends/species_1/stats/ 01_analyses_full/

Note this has to be done for all regions separately.

Note different masks take different time to run:

e.g. Woodland

    Species trends for woodland: 1/1: 421.973 sec elapsed
    Species trends for mask woodland (sims 1:1): 426.586 sec elapsed

    real	7m8.597s
    user	104m11.083s
    sys  	7m33.916s

e.g. Kerala

    Species trends for Kerala: 1/1: 192.011 sec elapsed
    Species trends for mask Kerala (sims 1:1): 193.994 sec elapsed

    real	3m15.934s
    user	47m52.214s
    sys  	2m7.714s

e.g. Cropland

    T=53.997 Threads:1 Done:78 Pending:0 Failed:0
    Finished: Paddyfield Pipit Time taken:13.919 secs (100.00 %)
    Species trends for cropland: 1/1: 50.274 sec elapsed
    Species trends for mask cropland (sims 1:1): 52.918 sec elapsed

    real	0m54.833s
    user	11m19.864s
    sys	    1m0.883s

e.g. PA

    T=140.246 Threads:1 Done:523 Pending:0 Failed:0
    Finished: Nilgiri Pipit Time taken:1.28700000000001 secs (100.00 %)
    Species trends for PA: 1/1: 138.056 sec elapsed
    Species trends for mask PA (sims 1:1): 139.664 sec elapsed

    real	2m21.588s
    user	32m27.248s
    sys  	4m32.788s

e.g. ONEland

    Finished: Green Avadavat Time taken:1.191 secs (100.00 %)
    Species trends for ONEland: 1/1: 27.094 sec elapsed
    Species trends for mask ONEland (sims 1:1): 28.597 sec elapsed

    real	0m30.505s
    user	6m9.547s
    sys	    0m50.357s

e.g. Karnataka

    T=143.227 Threads:1 Done:238 Pending:0 Failed:0
    Finished: Indian Vulture Time taken:8.125 secs (100.00 %)
    Species trends for Karnataka: 1/1: 139.848 sec elapsed
    Species trends for mask Karnataka (sims 1:1): 142.005 sec elapsed

    real	2m23.938s
    user	34m20.512s
    sys 	2m46.765s

e.g. Maharashtra

    T=146.311 Threads:1 Done:280 Pending:0 Failed:0
    Finished: White-bellied Blue Flycatcher Time taken:3.77800000000002 secs (100.00 %)
    Species trends for Maharashtra: 1/1: 144.059 sec elapsed
    Warning message:
    There were 2 warnings in `mutate()`.
    The first warning was:
    ℹ In argument: `across(c("freq", "se"), ~as.numeric(.))`.
    Caused by warning:
    ! NAs introduced by coercion
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning. 
    Species trends for mask Maharashtra (sims 1:1): 145.671 sec elapsed

    real	2m27.599s
    user	35m9.549s
    sys 	3m5.216s

e.g. Gujarat

    T=62.528 Threads:1 Done:223 Pending:0 Failed:0
    Finished: Striolated Bunting Time taken:1.642 secs (100.00 %)
    Species trends for Gujarat: 1/1: 60.375 sec elapsed
    Species trends for mask Gujarat (sims 1:1): 61.216 sec elapsed

    real	1m3.140s
    user	14m16.985s
    sys 	1m41.948s

Telangana

    T=5.529 Threads:1 Done:19 Pending:0 Failed:0
    Finished: Tricolored Munia Time taken:1.32 secs (100.00 %)
    Species trends for Telangana: 1/1: 3.403 sec elapsed
    Species trends for mask Telangana (sims 1:1): 3.806 sec elapsed

    real	0m5.703s
    user	0m33.288s
    sys 	0m5.890s

Uttarakhand

    T=23.046 Threads:1 Done:103 Pending:0 Failed:0
    Finished: Red-headed Bullfinch Time taken:2.059 secs (100.00 %)
    Species trends for Uttarakhand: 1/1: 20.642 sec elapsed
    Species trends for mask Uttarakhand (sims 1:1): 21.641 sec elapsed

    real	0m23.540s
    user	4m41.073s
    sys	    0m38.752s

## Dataset sizes for various region masks

Sizes of datasets vary quite a bit. They are captured here for reference:

350M Oct  8 17:33  ./01_analyses_full/dataforanalyses.RData-data_opt
148M Oct  8 17:34  ./01_analyses_mask-woodland/dataforanalyses.RData-data_opt
 74M Oct  8 17:35  ./01_analyses_mask-cropland/dataforanalyses.RData-data_opt
 47M Oct  8 17:36  ./01_analyses_states/Karnataka/dataforanalyses.RData-data_opt
 42M Oct  8 17:35  ./01_analyses_mask-PA/dataforanalyses.RData-data_opt
 39M Oct  8 17:39  ./01_analyses_states/Kerala/dataforanalyses.RData-data_opt
 36M Oct  8 17:35  ./01_analyses_mask-ONEland/dataforanalyses.RData-data_opt
 36M Oct  8 17:40 './01_analyses_states/Tamil Nadu/dataforanalyses.RData-data_opt'
 35M Oct  8 17:39  ./01_analyses_states/Maharashtra/dataforanalyses.RData-data_opt
 17M Oct  8 17:37  ./01_analyses_states/Gujarat/dataforanalyses.RData-data_opt
 16M Oct  8 17:37  ./01_analyses_states/Uttarakhand/dataforanalyses.RData-data_opt
 14M Oct  8 17:38 './01_analyses_states/Madhya Pradesh/dataforanalyses.RData-data_opt'
 13M Oct  8 17:36  ./01_analyses_states/Rajasthan/dataforanalyses.RData-data_opt
 12M Oct  8 17:38 './01_analyses_states/West Bengal/dataforanalyses.RData-data_opt'
8.7M Oct  8 17:38  ./01_analyses_states/Assam/dataforanalyses.RData-data_opt
6.3M Oct  8 17:37 './01_analyses_states/Uttar Pradesh/dataforanalyses.RData-data_opt'
5.2M Oct  8 17:38  ./01_analyses_states/Telangana/dataforanalyses.RData-data_opt
4.9M Oct  8 17:37  ./01_analyses_states/Haryana/dataforanalyses.RData-data_opt
4.8M Oct  8 17:35  ./01_analyses_states/Goa/dataforanalyses.RData-data_opt
4.6M Oct  8 17:40 './01_analyses_states/Andhra Pradesh/dataforanalyses.RData-data_opt'
4.2M Oct  8 17:38  ./01_analyses_states/Chhattisgarh/dataforanalyses.RData-data_opt
3.8M Oct  8 17:36 './01_analyses_states/Arunachal Pradesh/dataforanalyses.RData-data_opt'
3.5M Oct  8 17:37  ./01_analyses_states/Delhi/dataforanalyses.RData-data_opt
3.3M Oct  8 17:36  ./01_analyses_states/Odisha/dataforanalyses.RData-data_opt
3.2M Oct  8 17:37 './01_analyses_states/Himachal Pradesh/dataforanalyses.RData-data_opt'
2.3M Oct  8 17:36 './01_analyses_states/Andaman and Nicobar Islands/dataforanalyses.RData-data_opt'
1.9M Oct  8 17:38  ./01_analyses_states/Punjab/dataforanalyses.RData-data_opt
1.4M Oct  8 17:38 './01_analyses_states/Jammu and Kashmir/dataforanalyses.RData-data_opt'
1.2M Oct  8 17:37  ./01_analyses_states/Sikkim/dataforanalyses.RData-data_opt
997K Oct  8 17:39  ./01_analyses_states/Ladakh/dataforanalyses.RData-data_opt
973K Oct  8 17:37  ./01_analyses_states/Tripura/dataforanalyses.RData-data_opt
693K Oct  8 17:40  ./01_analyses_states/Puducherry/dataforanalyses.RData-data_opt
641K Oct  8 17:39  ./01_analyses_states/Meghalaya/dataforanalyses.RData-data_opt
364K Oct  8 17:38  ./01_analyses_states/Nagaland/dataforanalyses.RData-data_opt
274K Oct  8 17:36  ./01_analyses_states/Chandigarh/dataforanalyses.RData-data_opt
