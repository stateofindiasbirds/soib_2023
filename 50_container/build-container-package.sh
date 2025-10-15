#
# Test script that creates a ZIP package with
# container + data for developer TESTING ONLY.
#
# Data includes entire country run data
#
# NOTE: depends on pre-generated container
# from one of 50_container/build-container-{arch}.sh
#
echo "Assembling test container package contents..."
set -e
rm -rf soib-container
mkdir soib-container
mkdir -p soib-container/config/localhost
mkdir -p soib-container/output
mkdir -p soib-container/data/00_data
mkdir -p soib-container/data/01_analyses_full

cp 90_docs/README-CONTAINER.md soib-container/
cp config/localhost/config-container.R soib-container/config/localhost/config.R
cp 50_container/run_container_package.sh soib-container/run_container.sh
cp 00_data/analyses_metadata.RData soib-container/data/00_data
cp 01_analyses_full/specieslists.RData soib-container/data/01_analyses_full
cp 00_data/current_soib_migyears.RData soib-container/data/00_data

# The full data, metadata
cp 01_analyses_full/dataforanalyses.RData-data_opt soib-container/data/01_analyses_full
cp 01_analyses_full/dataforanalyses.RData-metadata soib-container/data/01_analyses_full
cp 01_analyses_full/species_names.RData soib-container/data/01_analyses_full
cp 01_analyses_full/timegroups.RData soib-container/data/01_analyses_full
# All random groupids
cp 01_analyses_full/rgids-*.RData soib-container/data/01_analyses_full/
# Stats to guide scheduling
cp 01_analyses_full/species_run_stats.RData soib-container/data/01_analyses_full

# Copy container installation scripts
cp 50_container/soib-container/install-arm64-container.sh soib-container
cp 50_container/soib-container/install-x86_64-container.sh soib-container

# copy container from appropriate arch dir
#
mkdir soib-container/`uname -m`
cp 50_container/soib-container/`uname -m`/soib.tar soib-container/`uname -m`
rm -f soib-container.zip
echo "soib-container directory has contiainer package contents, with data"
echo "Putting together soib-container into a single ZIP file that can be passed around ONLY for testing"
zip -q -r soib-container.zip soib-container
echo "Generated soib-container.zip"
