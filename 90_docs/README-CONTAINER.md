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

The output directory has a file config.R, using which you can configure the
number of threads, as well as which species to run trends calculation for.

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
