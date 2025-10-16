# Running in source directory or container?
# Running without container will assume
#  - data files aren't stored hierarchically
#  - output paths same as earlier
# Running inside container will assign top
# level paths such as "data" and "output"
container <- TRUE

# Setup a specific number of max threads to run
# by defining them here.
# Default value is cores/2
# Note: these many threads may not be started if there
# is not enough RAM. This may happen if running under
# WSL, or a laptop with limited memory, etc
# On a Mac, set this to the number of performance cores
#threads <- 4

# Mask
# Defaults to whole country, i.e. "none"
cur_mask <- "none"

# my_assignment : Which subset to process
# if undefined, defaults to 1
#my_assignment <- 1:1

# You may choose to process a subset of species by
# defining those names here. An empty/absent list
# means "process all species"
species_to_process <- c(
#  "Coppersmith Barbet",
#  "Oriental Magpie-Robin",
#  "Rufous-throated Fulvetta" # Short runtime
)

# Trends computation results in a trends_N.csv file
# Force trends computation. Defaults to TRUE for
# developer use.
# If this is set to FALSE, then we'll check if the
# result file exists, and skip computation. Useful
# option for running on cloud and recovering from
# errors in extreme cases
#force_trends_computation <- FALSE

# Runs RAM hungry jobs in interleaved fashion. This is
# the default.  Helps reduce peak memory pressure, making
# it easier to use on lower memory devices like laptops
# and Macs.
# If you set this to FALSE, peak memory usage will more or
# less happen early on, and then the memory usage will
# keep dropping.
#ram_interleave <- TRUE

# 3 GB margin to account for no process kills on Mac. Higher
# need for this if you set ram_interleave to FALSE
#ram_safety_margin <- 3000

# Set "Reproducible run" to TRUE to help compare results
# between different software/hardware environments.
# This locks the random seed at the start of every singlespeciesrun
# Idea is that if you take care to use the same randomgroupids,
# you must get the same results every time
# default value is FALSE
#reproducible_run <- TRUE
