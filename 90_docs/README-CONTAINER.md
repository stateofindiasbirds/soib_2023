# SoIB Container

SoIB container is a concept/package that allows you to run SoIB on any platform
without going through the process of installing any dependencies, and with a
minimum of OS privileges.  You'll typically get this package as as a ZIP
file : soib-container.zip

This file includes everything required to run the compute intensive part of
the SoIB computation : Part 3 Step 1. This is typically run across multiple machines.

The container is supplied as a package that includes some of modified (RAM optimized)
SoIB code, an R installation with all dependent packages. You first need to install
the container itself (the steps are detailed in this document for Linux, MacOS and
Windows)

To run the container (generate trends files), you need to get the data files separately.
These are packaged as "job" files, along with scripts to run the job. Each "job" typically
includes data for multiple assignments(in the range 1:1000) for a specific "mask"
(country, states, etc).

# Operating Systems and Hardware

The container may be used under Linux, Windows - using WSL(Windows
Subsystem for Linux), or Mac

The containers provided here are tested on:

- Linux on 16C/32T AMD Ryzen 9 7950X desktop with 64 GB RAM. Time to finish
  computation for one assignment: 54 minutes
- Mac Mini M4 (base model with 16 GB RAM). Time to finish: 3860 seconds (1 hr
  5 minutes). Also a Mac Pro M4
- Lenovo Legion laptop with 16 core AMD processor (Ryzen 7 5800H), 16 GB memory,
  (5 year old laptop). Running Linux and Windows. Time to finish: 7256 seconds
  (2 hrs)
- 96 core ARM VM on Azure : D96ps_v6a - time to run approximately 7.5 - 8 minutes
- 128 core x86_64 AMD EPYC server - time to run approximately 15.5 minutes

On Windows and Mac, the Linux VM(virtual machine) that runs the container
is allocated a separate memory block.  SoIB species trends computations are memory
intensive. These OSes typically allocate half the system RAM to the VM, which
may be less. With less RAM, less threads will be allocated for processing, slowing
it down. A minimum of 12 GB (14 on M4 mini) is desirable to be allocated to the VM to run
4-6 threads simultaneously. For best performance, please follow the appropriate steps
given for specific operating systems, before you start a run.

# Getting Started and Basic Usage

Extract the ZIP package.  It includes a directory "soib-container". Change to
that directory on your terminal.  Then follow the OS specific steps. Every
command mentioned in this document needs to be run from inside this directory.

## Linux

### Install Podman

Recommended tool to run the container is podman.  This may be installed
using your regular package manager.  E.g., on ubuntu, use

    $ sudo apt update && sudo apt install -y podman

### Install the container

Run this command, after changing to the unzipped directory

    $ sh install-x86_64-container.sh

You should see output such as:

    Getting image source signatures
    Copying blob 018061dfc73b skipped: already exists
    Copying blob aa8ee6a3b6ad skipped: already exists
    Copying blob d0112744eae5 skipped: already exists
    Copying blob 6e60de00e158 skipped: already exists
    Copying blob c5f08d14b7e1 skipped: already exists
    Copying blob 6f8f9d6a2dff skipped: already exists
    Copying blob e9c1249fc42c skipped: already exists
    Copying blob 4833e1223c04 skipped: already exists
    Copying blob 40411a808299 skipped: already exists
    Copying blob 418dccb7d85a skipped: already exists
    Copying blob be302c00df36 done   |
    Copying blob 7dd24ce0fe57 done   |
    Copying blob 4573d540eb85 done   |
    Copying blob 6124306be98e done   |
    Copying config adeeaae89d done   |
    Writing manifest to image destination
    Loaded image: localhost/soib:latest

The container is now installed.

### Running the container

Extract the job file. Inside the extracted directory you'll find run_container.sh,
which can be executed from the shell as below

    $ sh run_container.sh

You'll see familiar output of execution of the script. You may press Control-C
to terminate the execution of the container at any time.

The messages also help you track how much of the compute job is running, finished,
and pending. Some of these numbers (peak RAM, runtime) are indicative, not exact.
They are measured on a reference machine, not the one where you might actually
run the container. These are used as heurestics to schedule execution order of
species in a RAM aware manner. You may see messages like the following:

    T=3344.302 Estmated Peak Free RAM:116 MB Threads:5 Done:105 Pending:627
    Generated :  output/stats/Black-winged Kite.RData
    Threads finished: 1
    Starting: Dunlin, estimated peakRAM: 738 MB, runtime: 10.7130000000001 seconds
    Won't start 3 threads due to insufficient RAM (needed for 1 more: 2280 MB free:2218 MB)
    Threads started: 1
    T=3346.034 Estmated Peak Free RAM:2218 MB Threads:5 Done:106 Pending:626

### Output

After the container finishes running successfully, the species trends output will be
in a subdirectory of output. The find command can help locate the output

    $ find output -name "*.csv"

## Windows

To use the container, you need to have WSL(Windows Subsystem for Linux) installed.
Installation and usage of WSL is out of the scope of this document.

With WSL installed, the usage steps are exactly the same as Linux.  In Linux,
you would use the terminal to execute the commands. In WSL, you first run "wsl"
to start the WSL shell.

### Memory Limits on WSL

By default, Windows allocates about 50% of your system's memory (RAM) to WSL.
SoIB needs a lot of memory to run. Without changing the amount of memory
allocated to WSL, you may find that the container takes a long time to finish,
as it won't start many parallel jobs to ensure the RAM limit is not hit.

You can setup a specific memory limit by editing the file .wslconfig in your
user profile directory. e.g. to setup a limit of 12 GB, the contents of the
file may look like below:

    [wsl2]
    memory=12GB

Note that WSL is like an entire OS running together with your windows OS, which
itself needs a good amount of memory to work satisfactorily. So do not try to
allocated the entire the entire memory available to WSL.

For the change to take effect, you will need to shutdown any running WSL instance,
using:

    wsl --shutdown

### Install Podman

Inside the wsl shell, execute

    $ sudo apt update && sudo apt install -y podman

### Install the Container

Run this command inside the wsl shell, inside the unzipped directory:

    $ sh install-x86_64-container.sh

### Running the container

Extract the job file. Inside the extracted directory you'll find run_container.sh,
which can be executed from the wsl shell as below

    $ sh run_container.sh

You'll see familiar output of execution of the script. You may press Control-C
to terminate the execution of the container at any time.

### Output

After the container finishes running successfully, the species trends output will be
in a subdirectory of output. The find command can help locate the output

    $ find output -name "*.csv"

## Mac

### Install Podman

Install podman from podman.io for the Mac.  The exact steps you use may vary.

After installing podman, you typically want to setup a Linux virtual machine
that can then run the SoIB container.  You can do that by running this on
the CLI:

    $ podman machine init

This will take a while to download things.  Then run:

    $ podman machine start

### Install the container

Use the terminal, change to the directory where the ZIP package is extracted,
and run:

    $ sh install-arm64-container.sh

### Memory Limits on Mac

Mac machines have lower RAM, like laptops.  Also, like windows the default
allocation to the podman linux virtual machine is half the RAM, which is
low.  You typically want to bump this up to at-least 14 GB for a 4 core
system. Set the number of CPUs to the number of performance cores on your
Mac (e.g. 4 on M4)

    $ podman machine stop
    $ podman machine set --cpus 4 --memory 14000
    $ podman machine start

### Running the container

Extract the job file. Inside the extracted directory you'll find run_container.sh,
which can be executed from the command shell as below

    $ sh run_container.sh

You'll see familiar output of execution of the script. You may press Control-C
to terminate the execution of the container at any time.

### Output

After the container finishes running successfully, the species trends output will be
in a subdirectory of output. The find command can help locate the output

    $ find output -name "*.csv"

# Additional Usage Notes

The config directory has a file config.R, using which you can configure the
number of threads, as well as which species to run trends calculation for.

You may modify config/localhost/config.R, or create a separate configuration
file in config/localhost. To use a different configuration file, run

    $ sh run_container.sh config-mod.R

This will look for config file config/localhost/config-mod.R (first) and then
config/hostname/config-mod.R (in that order).

To get the version of the software in the container, run:

    $ sh run_container.sh -version
    Version: 20251015

To dump the versions of all the dependent software in the container, run:

    $ sh run_container.sh -dep-versions

Dependency report will include version of R, all R packages installed (with
their version), and base linux (Alpine linux) version information, like:

    R version: x86_64-pc-linux-muslx86_64linux-muslx86_64, linux-musl45.02025041188135RR version 4.5.0 (2025-04-11)How About a Twenty-Six
    Installed packages are:
           Package   Version
             abind     1.4-8
               arm    1.14-4
           askpass     1.2.1
         backports     1.5.0
         base64enc     0.1-3
               bit     4.6.0
             bit64   4.6.0-1
              blme     1.0-6
              blob     1.2.4
             broom     1.0.9
       broom.mixed   0.2.9.6
             bslib     0.9.0
            cachem     1.1.0
             callr     3.7.6
        cellranger     1.1.0
          classInt    0.4-11
               cli     3.6.5
             clipr     0.8.0
              coda  0.19-4.1
        commonmark     2.0.0
        conflicted     1.2.0
             cpp11     0.5.2
            crayon     1.5.3
              curl     7.0.0
        data.table    1.17.8
               DBI     1.2.3
            dbplyr     2.5.0
            digest    0.6.37
             dplyr     1.1.4
            dtplyr     1.3.1
             e1071    1.7-16
          evaluate     1.0.4
            farver     2.1.2
           fastmap     1.2.0
       fontawesome     0.5.3
           forcats     1.0.0
           foreach     1.5.2
                fs     1.6.6
             furrr     0.3.1
            future    1.67.0
            gargle     1.5.2
          generics     0.1.4
           ggplot2     3.5.2
           globals    0.18.0
              glue     1.8.0
       googledrive     2.1.1
     googlesheets4     1.1.1
            gtable     0.3.6
             haven     2.5.5
             highr      0.11
               hms     1.1.3
         htmltools   0.5.8.1
            httpuv    1.6.16
              httr     1.4.7
               ids     1.0.1
           isoband     0.2.7
         iterators    1.0.14
         jquerylib     0.1.4
          jsonlite     2.0.0
             knitr      1.50
          labeling     0.4.3
             later     1.4.3
         lifecycle     1.0.4
           listenv     0.9.1
              lme4    1.1-37
         lubridate     1.9.4
          magrittr     2.0.3
           memoise     2.0.1
          merTools     0.6.2
              mime      0.13
             minqa     1.2.8
            modelr    0.1.11
           mvtnorm     1.3-3
            nloptr     2.2.1
           openssl     2.3.3
               pak     0.9.0
        parallelly    1.45.1
           peakRAM     1.0.2
            pillar    1.11.0
         pkgconfig     2.0.3
              plyr     1.8.9
       prettyunits     1.2.0
          processx     3.8.6
          progress     1.2.3
          promises     1.3.3
             proxy    0.4-27
                ps     1.9.1
             purrr     1.1.0
                R6     2.6.1
              ragg     1.4.0
          rappdirs     0.3.3
         rbibutils       2.3
      RColorBrewer     1.1-3
              Rcpp     1.1.0
     RcppArmadillo  14.6.3-1
         RcppEigen 0.3.4.0.2
            Rdpack     2.6.4
             readr     2.1.5
            readxl     1.4.5
        reformulas     0.4.1
           rematch     2.0.0
          rematch2     2.1.2
            reprex     2.1.1
          reshape2     1.4.4
             rlang     1.1.6
         rmarkdown      2.29
        rstudioapi    0.17.1
             rvest     1.0.4
                s2     1.1.9
              sass    0.4.10
            scales     1.4.0
           selectr     0.4-2
                sf    1.0-21
             shiny    1.11.1
       sourcetools   0.1.7-1
           stringi     1.8.7
           stringr     1.5.1
               sys     3.4.3
       systemfonts     1.2.3
       textshaping     1.0.1
            tibble     3.3.0
            tictoc     1.2.1
             tidyr     1.3.1
        tidyselect     1.2.1
         tidyverse     2.0.0
        timechange     0.3.0
           tinytex      0.57
               TMB    1.9.17
              tzdb     0.5.0
             units     0.8-7
          unmarked     1.5.0
              utf8     1.2.6
              uuid     1.2-1
             vctrs     0.6.5
              VGAM    1.1-13
       viridisLite     0.4.2
             vroom     1.6.5
             withr     3.0.2
                wk     0.9.4
              xfun      0.53
              xml2     1.4.0
            xtable     1.8-4
              yaml    2.3.10
    Alpine linux (container) info:
    NAME="Alpine Linux"
    ID=alpine
    VERSION_ID=3.22.1
    PRETTY_NAME="Alpine Linux v3.22"
    HOME_URL="https://alpinelinux.org/"
    BUG_REPORT_URL="https://gitlab.alpinelinux.org/alpine/aports/-/issues"

## Threads

By default, the container will try to run multiple threads - as many as half
the number of cores, subject to RAM limits.  This is a good default choice.

But you can bump this number up or down by setting the "threads" variable
in output/config.R . Bumping up the threads to match cores may make sense
on computers with a lot of memory bandwidth, eg

    threads <- 32

Reducing the number of threads is a useful thing for testing or observing
RAM and CPU consumption patterns.

    threads <- 1

If you are on a Mac, then set the number of threads to the number of
performance cores. E.g. on a Mac Mini M4

    threads <- 4

## Species List

By default, trends calculations are done for all species.  You may choose
a subset of species by naming them "species_to_process" variable inside
output/config.R . This is useful to quickly check something for a subset
of species without completing an entire run.

E.g., to run for just two species:

    species_to_process <- c(
      "Coppersmith Barbet",
      "Oriental Magpie-Robin"
    )
 
Leaving the list empty or not defining species_to_process in config.R will
run the trends calculations for all species.  Species names are not validated,
so ensure they are in the list. Else the script may fail.
