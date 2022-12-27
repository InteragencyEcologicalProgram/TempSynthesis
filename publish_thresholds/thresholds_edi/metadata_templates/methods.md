# Methods

## Data Collection Methods

### Temperature Thresholds

We compiled an integrated dataset (or catalogue) of temperature thresholds for fishes, invertebrates, aquatic vegetation, and cyanobacteria using documented values in peer-reviewed research, technical reports or field data when nothing was found in literature. While hundreds of native and non-native species exist in the Sacramento-San Joaquin Delta, we included only a subset of species focused around 1) resource management (e.g., state or federally listed endangered or threatened species), 2) species that may negatively affect management-relevant species (e.g., non-native predators, clams, aquatic vegetation), and 3) other native fishes where information is less known. For each species and life-stage (if found) we conducted a literature search and recorded documented suboptimum or tolerance temperature values. Often literature and experimental research values varied based on the metric (e.g. growth, metabolic scope, critical thermal maximum or lethal thermal maximum) or acclimation temperature where species can acquire thermal tolerance. For variable values, a range was documented in the dataset to capture the variability across literature.

### Life Stage Designations

Most monitoring programs report length data for fish catch, but do not report life stage. To connect temperature thresholds with monitoring data, we conducted a literature search to find documented lengths for each life stage (larvae, juvenile, adult) so that we could make life stage designations for monitoring data. See notes on data quality (below) for more details on how these designations were made. 

## Quality Assurance and Control

### Notes on Data Quality

**Life Stage data**

Due to the scarcity of literature, there were some differences in how life stage was designated, depending on the species. We generally used minimum length at maturity or minimum spawning length as the designation for minimum adult lengths, but sometimes literature sources would mention sizes for adults that did not specifically indicate how the life stage designation was made. For larvae, we often used maximum length at yolk-sac larvae completion stage, but not always. Juvenile ranges often encompassed the whole range between larval and mature adult lengths, and thus could include sub-adult life stages. If we could not find larval or juvenile cutoff lengths, we combined the two life stages. See life_stage_designations.csv for additional notes and references. Additionally, there were three types of length measurements that were referenced through our literature search: standard length, total length, and fork length. There can be differences of several millimeters depending on the type of measurement used. Given the uncertainty around the different ways maturity and life stage were designated, and the uncertainty in how to interpolate a standardized length for each species-life stage, we decided to stick to the recorded values despite different length measurements.

**Field data**

Each monitoring program used different gear types and methods to target specific habitats and species, and each monitoring program has a different temporal resolution (both in terms of how often the program samples, and the total duration of the survey). Thus, there is some inequality in sampling for the dataset. Nevertheless, we hope our summary statistics provide useful information about the temperatures in which each species-life stage was observed. 

## Calculations and Analysis

For each species and life-stage we calculated corresponding field temperatures using three fish monitoring datasets. Bashevkin et al. 2022 integrated data from nine surveys in the San Francisco Estuary (USFWS Delta Juvenile Fish Monitoring Program, USFWS Enhanced Delta Smelt Monitoring Program, UC Davis Suisun Marsh Study, CDFW Bay Study, CDFW Fall Midwater Trawl, CDFW Spring Kodiak Trawl, CDFW 20mm Survey, CDFW Smelt Larval Survey, and CDFW Summer Townet), while IEP et al. 2022 contains data from the Yolo Bypass Fish Monitoring Program, and CDFW 2022 contains data from the salvage facilities in the South Delta. Datasets were integrated into one dataset.
Datasets were filtered for our species of interest, regions of interest (using latitude and longitude data), and for observations that had length data so we could classify organisms by life-stage. Life stage cutoffs (described above) were used to designate each organisms to a life stage, based on reported length. The mean, maximum and upper quartile (75th percentile) temperature at which each species-life-stage grouping was observed was then calculated. 

## Review Processes

* Internal Review: Co-authors reviewed literature sources used for threshold and life stage cutoff information.  
* External Review: Integrated field data sources were previously QA/QC’ed by each individual monitoring program. Please see each monitoring program’s metadata or contact programs for more information.
