# State of India's Birds 2023 

[![Website](https://img.shields.io/website?label=Website&up_color=teal&up_message=Online&url=https%3A%2F%2Fstateofindiasbirds.in%2F)](https://stateofindiasbirds.in/)
[![Twitter](https://img.shields.io/twitter/follow/SoIB_India?style=social)](https://twitter.com/SoIB_India)

Citable archive of this repository (v2023): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12698375.svg)](https://doi.org/10.5281/zenodo.12698375)

SoIB 2023 report: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11124589.svg)](https://doi.org/10.5281/zenodo.11124589)

***

This repository contains the source code associated with the SoIB 2023 analysis, as well as all outputs that fed into the SoIB 2023 report[^1]. All analyses were performed in the R environment in RStudio, and with the addition of a couple of large (publicly available) data files that need to be procured externally, the source code and files in this repo were designed to make SoIB 2023 fully reproducible.

The broad-level structure and workflow for the analysis is outlined below. However, full details about the entire methodology/workflow and the SoIB framework can be found in Viswanathan et al. 2024[^2], which we recommend examining.

*To quickly jump to relevant sections of this README, open the document outline (icon on the top right of this document) and follow the links.*

## Pipeline

The control centre for the main SoIB 2023 analysis (once necessary files have been procured and put in place) is `00_pipeline.R` located in the main level of the repo. This lists all the steps in the pipeline in order, calling on corresponding scripts and functions, and covers the entire process from importing the raw EBD `.txt` data file till running the SoIB classification algorithm and summarising the results. 

The script is structured into parts and steps, with ample annotation at each step. Although the pipeline is comprehensive and steps are ordered, due to the complexity and associated overheads, each step is best run individually; this has been enabled: aside from the [lines outside section headers](https://github.com/stateofindiasbirds/soib_2023/blob/master/00_pipeline.R#L1-L8), one need only run the lines specified at each specific step. 

Certain steps were required to be run on multiple machines. For this, the code pertaining to the relevant steps alone is copied in `00_pipeline_multimachine.R`. Similar to the main pipeline, loading of dependencies happens in a few lines of code at the start, and the steps can be run in order or individually by commenting out whatever is not of interest.

## Data (`00_data/`)

All input and many intermediary data are located in `00_data/`. Aside from the main eBird dataset (not committed, can be downloaded from [here](https://ebird.org/data/download/ebd)) and other eBird data inputs, this folder also contains various spatial data (shapefiles and rasters), intermediary data, and other data produced for auxiliary outputs such as the Systematic Monitoring chapter in the report[^1], SoIB State Posters, SoIB website, etc. The font files used for most outputs in the report[^1] are in `Gandhi Sans/`, while Citation Style Language (CSL) used in the report[^1] lies in the main level of the repo (`soib.csl`). `SoIB_mapping_2022.csv` is a crucial species-level dataset that forms the backbone for much of the analysis, and `analyses_metadata.RData` forms the backbone for the pipeline.

## Scripts (`00_scripts/`)

All individual scripts aside from the high-level script (`00_pipeline.R` and `00_pipeline_multimachine.R`) are located in `00_scripts/`. They include several functions relevant for the main analytical workflow but also many helper functions for both inside and outside this main pipeline. These are all outlined in [the corresponding README](00_scripts/README.md).

## Main outputs

### Analysis results (`01_analyses_`)

SoIB 2023 analysed abundance trends and range size of Indian bird species at multiple spatial levels. The analysis first looked at nationwide patterns (`01_analyses_full/`), then explored patterns using four habitat masks (`01_analyses_mask-ABC/`), and finally analysed each state/UT individually (`01_analyses_states/`). Throughout these, the analytical framework remained the same. Each of these folders contains intermediary data files as well as a `results/` folder containing relevant outputs of the analysis. 

`SoIB_main.csv` is the primary result of interest for each spatial level, which is a species-level data sheet with SoIB 2023 results appended as columns (model-predicted trends are generated in `trends.csv`). `SoIB_summaries.xlsx` contains tabular summaries of the SoIB classification and prioritisation effort. `redlist.csv` is related to the IUCN Red List comparison that is part of the report[^1], and `key_state_species_` files relate to the "Highest Priority Species" analysis also done in the report[^1] (p. 23)---both were done only at the nationwide level. 

All the different SoIB results for all spatial levels were aggregated into a single SoIB 2023 results file (`20_website/SoIB_2023_main.xlsx`), downloadable from the website. This `.xlsx` file contains a README in the first sheet, detailing metadata for the dataset. It is also available as a citable Zenodo archive record[^1] along with the report itself.

### Plots (`02_graphs`)

`02_graphs/00_methods/` contains figures related to the methods section, while all single-species trends graphs (both LTT and CAT) for all spatial levels are generated in `02_graphs/01_single/`. Trend graphs of multiple species with common groupings are located in `02_graphs/02_multispecies/`, whereas composite graphs showing trends averaged across entire groups are in `02_graphs/03_composite/`. Distribution range maps are in `02_graphs/10_rangemaps/`. Additional plots are located in appropriate folders. 

*Note that the actual plots (image files) themselves are not committed, so these aforementioned subfolders are technically empty on the repo and therefore do not appear.*

## Auxiliary outputs

Input data and outputs for the systematic monitoring chapter of the report[^1] are located in `10_sys-mon/`. The browsable results directory on the SoIB website is controlled by a core database, `20_website/website_database.csv`, and the folder also contains other website-specific outputs. 

## References

[^1]: SoIB 2023. State of India's Birds, 2023: Range, trends, and conservation status. The SoIB Partnership. Pp. 119. Available online: https://stateofindiasbirds.in/#soib_report.
[^2]: Viswanathan et al. 2024.
