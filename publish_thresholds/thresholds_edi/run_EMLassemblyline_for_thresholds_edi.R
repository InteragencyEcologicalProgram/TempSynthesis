# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------
template_directories(path = "publish_thresholds", dir.name = "thresholds_edi")
# Update EMLassemblyline and load

#remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

# Define paths for your metadata templates, data, and EML
dir <- "publish_thresholds/thresholds_edi"
path_templates <- paste0(dir, "/metadata_templates")
path_data <- paste0(dir, "/data_objects")
path_eml <- paste0(dir, "/eml")

# Create metadata templates ---------------------------------------------------

# Below is a list of boiler plate function calls for creating metadata templates.
# They are meant to be a reminder and save you a little time. Remove the 
# functions and arguments you don't need AND ... don't forget to read the docs! 
# E.g. ?template_core_metadata

# Create core templates (required for all data packages)

EMLassemblyline::template_core_metadata(
  path = path_templates,
  license = "CCBY",
  file.type = ".md",
  write.file = TRUE)

# Create table attributes template (required when data tables are present)

EMLassemblyline::template_table_attributes(
  path = path_templates,
  data.path = path_data,
  data.table = c("Lifestage_length_designations.csv", "Temperature_thresholds.csv"))

# Create categorical variables template (required when attributes templates
# contains variables with a "categorical" class)

EMLassemblyline::template_categorical_variables(
  path = path_templates, 
  data.path = path_data)

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).

EMLassemblyline::template_geographic_coverage(
  path = path_templates, 
  data.path = path_data, 
  data.table = "", 
  lat.col = "",
  lon.col = "",
  site.col = "")

# Create taxonomic coverage template (Not-required. Use this to report 
# taxonomic entities in the metadata)

remotes::install_github("EDIorg/taxonomyCleanr")
library(taxonomyCleanr)

taxonomyCleanr::view_taxa_authorities()

EMLassemblyline::template_taxonomic_coverage(
  path = path_templates, 
  data.path = path_data,
  taxa.table = "Temperature_thresholds.csv",
  taxa.col = "Taxon",
  taxa.name.type = "scientific",
  taxa.authority = 3)

# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create 
# the EML.

EMLassemblyline::make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml, 
  dataset.title = "Temperature Thresholds for Aquatic Species in the Sacramento San-Joaquin Delta", 
  #temporal.coverage = c("YYYY-MM-DD", "YYYY-MM-DD"), 
  #geographic.description = "Sacramento San-Joaquin Delta", 
  #geographic.coordinates = c("N", "E", "S", "W"), 
  maintenance.description = "Completed, possible occasional updates", 
  data.table = c("Lifestage_length_designations.csv", "Temperature_thresholds.csv"), 
  data.table.name = c("Life Stage Length Designations", "Temperature thresholds"),
  data.table.description = c("Length cutoffs for species life stages", "Temperature thresholds for aquatic species"),
  other.entity = c("Metadata_SpeciesThresholds.pdf"),
  other.entity.name = c("Metadata for Species Thresholds"),
  other.entity.description = c("Metadata for Species Thresholds"),
  data.table.quote.character = c('"', '"'), # If you have columns that have commas in the text, you will need to use "quote = TRUE" when you write your R file (write.csv), and then use this to tell make_eml what is going around your character cells. c(apostrophe, quote, apostrophe, comma, etc...)
  user.id = "aquaticecology",
  user.domain = "EDI", 
  package.id = "edi.1286.2")
