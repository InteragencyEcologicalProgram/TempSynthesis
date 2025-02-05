# TempSynthesis

Analysis of water temperature data from the Sacramento San-Joaquin Delta

Data have been downloaded from CDEC (<https://cdec.water.ca.gov/>) and cleaned.

Integrated datasets are published at: <https://portal.edirepository.org/nis/mapbrowse?packageid=edi.591.2>

Species thresholds are published at: <https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1286&revision=3>

## Code asssociated with Water Temperature Thresholds Manuscript

-   Go to **manuscript_code** folder for most up-to-date code.

### Data

-   **Data/StationsMetadata.csv**: Metadata about stations - lat/lon, start and end date of stations, names
-   **Data/Temp_filtered.rds**: Original QA/QCed filtered water temperature dataset
-   **Data/temp10years_20230613.rds**: Hourly dataset filtered down to desired stations and time periods
-   **Data/tempMonthly.rds**: Monthly temperature dataset used for trend modeling
-   **Data/SpeciesThresholds.xlsx**: Species temperature thresholds
-   **Data/daysExceedanceUpperDataForPlotting.csv**: Calculations for each species days exceeding upper thresholds
-   **Data/daysExceedanceDataForPlotting.csv**: Calculations for each species days exceeding lower thresholds
-   **Data/emmeans_results/**: Emmeans tables
-   **RosiesRegionsEdited**: Shapefile of regions used for map and analysis

### Code

-   **prepare_datasets.Rmd**: Script to filter dataset based on the location of the station and the amount of missing data for the station; creates annual, daily, monthly datasets including appropriate summary stats
-   **join_stations_to_regions.Rmd**: Script to join Rosie's regions shapefile to stations
-   **make_manuscript_map.Rmd**: Script to make original map - this got replaced by the cluster analysis map, which is based on this map.
-   **tempclusters.Rmd**: Cluster analysis
-   **model_temperature_trends.Rmd**: Mixed Model of maximum and minimum water temperature trends
-   **model_native_nonnative.Rmd**: Mixed Model of native/non-native and listed/unlisted species differences
-   **plot_days_exceedance_boxplot.Rmd**: Script to create days exceedance boxplots
-   **plot_fish_absence_presence_vulnerability.Rmd**: Script to create vulnerability tile plot
-   **model_temperature_trends.Rmd**: Mixed Model of maximum and minimum water temperature trends
-   **stationheatmaps.R**: Script to create heat map looking at station and regional suitability by day of year (identify no recovery areas)

### Figures

-   **model_validation_plots**: contain model validation plots for listed status model, maximum temperature model, minimum temperature model
-   **additional_figures**: contain heat map plots for data that could be of interest but were not in the manuscript
-   Other figures in the Figures folder are part of the manuscript.
