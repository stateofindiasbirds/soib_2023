# Codebase documentation


The SoIB codebase contains several functions relevant for the main
analytical workflow but also many helper functions for both inside and
outside this main pipeline. All these are outlined below.

All individual scripts are housed in `00_scripts/`. Numerical prefixes
in filenames indicate where in the pipeline the scripts fall:

-   `00_*.R` Setup & helper functions
-   `01_*.R` Main analysis
-   `02_*.R` Plotting
-   `10_*.R` Analyses from the Systematic Monitoring chapter of the
    report
-   `20_*.R` Website-specific steps and outputs
-   `30_*.R` Mass communication like acknowledgement certificates

Scripts without prefixes may fall in multiple steps of the pipeline,
from the main analyses steps to auxiliary outputs. The primary scripts
for the main analysis are the ones referred to in order in
`00_pipeline.R`.

## Helper functions

### `analyses_metadata`

This sets up the folder structure for the analytical workflow that spans
multiple spatial scales (national, states, habitats) and analysis steps
(and therefore folders and files). The main use of this feature is
seamless operation of file paths. Therefore, the script
`01_create_metadata.R` need only be run *when there is any change* to
any folder/path structure. It also ensures the folder structures are
matched, by creating the necessary folders that do not already exist
(though it will not delete old folders). This script saves the metadata
as `analyses_metadata.RData`.

All other times, the helper function `get_metadata()` can be used to
obtain either the entire metadata base `get_metadata()` or the metadata
for the spatial unit/mask of interest `get_metadata(mask)`. The metadata
contains 26 columns of paths, which each correspond to a specific folder
or file path, and 3 columns relating to the mask itself. It is best to
simply select the required path directly from the output using `$`, like
`get_metadata("none")$SOIBMAIN.PATH`.

## Main analysis scripts

## One-time run scripts

Sysmon self-explanatory
