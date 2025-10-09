
cp README-CONTAINER.md soib-container/
cp config/localhost/config-container.R soib-container/config/localhost/config.R
cp run_container_package.sh soib-container/run_container.sh
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

cp install-x86_64-container.sh soib-container/
cp install-arm64-container.sh soib-container/

# copy container from appropriate arch dir
#
cp `uname -m`/soib.tar soib-container/`uname -m`
rm soib-container.zip
zip -r soib-container.zip soib-container
