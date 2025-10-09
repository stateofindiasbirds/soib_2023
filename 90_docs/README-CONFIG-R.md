# config.R

config.R provides the mechanism to pass parameters related to computing species
trends (part 3 step 1).  R is chosen as the language as the rest of the package
is written with it, thus the developers are familiar with it.

Basically, users set variables in the config.R file to setup parameters to
decide what is computed.

config/localhost/config.R file is sourced if available. If not,
config/<hostname>/config.R is used.

## Running in container/git source tree

Parameter "container" impacts the paths that are used:

  * container <- FALSE
    Uses unaltered paths from the source tree.
    Output is also generated in the source tree paths.
  * container <- TRUE
    Uses "data" as the main directory for data files.
    Ouput is generated in the "output" directory

Aggregating data and output files in separate directories
makes it easy to pass around data and collect results.

## Threads

Parameter "threads" setups up number of worker threads that are
used to compute trends.  If not set, this number defaults to
(number of cores in system)/2, which is mostly good for x86
servers.

It is desirable to setup this number explcitly to ensure best
performance. On ARM systems,

  * On Mac M4, set it to the number of performance cores
    (e.g. 4 on Mac Mini M4)
  * On cloud VMs, set it to number of cores
    (e.g 96 on D96ps_v6)

## Mask

"cur_mask" parameter selects the region. e.g. "none" runs
for the entire country.

## Assignment

Selects which of the 1000 species trends computations to run.
Default is my_assignment <- 1:1

## Species to Process

You may run species trends for a subset of species by setting
"species_to_process". If unset, all the species for the mask
are run.  Set it a list of one/more species if checking for
anything specific/for quick runs.

## Forcing Trends Computations

By default, once a trends file is generated/available, subsequent
computations for it are skipped. Setting

force_trends_computation <- FALSE

recomputes trends every time.

## Reproducible Runs

Set reproducible_run <- TRUE to lock the random seed to 0 for
every species trend computation.  With the same data and random
files, this ensures 100% repeatable results on the same platform
(tested on x86, running non-containerized)

Across platforms (e.g. x86 compared to ARM, or across containerized
v/s native exectution), minor but negligible variations in numerical
results is seen (less than 1e-3 across all species for country mask,
assignment = 1)

## Miscellaneous experimental settings

config.R supports few other settings. Look at config.R for details. 
