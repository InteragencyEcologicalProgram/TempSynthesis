### This script creates flowchart for figure and table generation
### Created by Catarina Pien (cpien@usbr.gov)
### Last updated 6/23/2025

library(DiagrammeR)
library(tidyverse)
graph <- 
grViz("digraph {

# initiate graph, include title
graph [layout = dot, rank = same, 
label = 'Water Temperature Analysis Workflow\n\n',labelloc = t, fontsize = 30, fontname = Helvetica]

# global node settings
node [shape = rectangle, style = filled, fillcolor = Linen, fontname = Helvetica]

# label nodes with ID and label, shape, etc.
raw1 [label = 'StationsMetadata.csv', shape = cylinder, fillcolor = DarkSeaGreen]
raw2 [label = 'Temp_filtered.rds', shape = cylinder, fillcolor = DarkSeaGreen]
raw3 [label = '\ndeltafish package \n  Yolo Bypass Fish Data (edi.233.3) \n  salvage.rds (from https://filelib.wildlife.ca.gov/Public/salvage/) \n Sturgeon_individuals.csv (now available edi.1479.2)',
shape = cylinder, fillcolor = DarkSeaGreen]
raw4 [label = 'Rosies_regions_edited.shp', shape = cylinder, fillcolor = DarkSeaGreen]
rawThresholds [label = 'SpeciesThresholds.xlsx', shape = cylinder, fillcolor = DarkSeaGreen]

dataDaily [label = 'tempDaily.rds', shape = ellipse, fillcolor = MediumSeaGreen]
dataMonthly [label = 'tempMonthly.rds', shape = ellipse, fillcolor = MediumSeaGreen]
dataHourly [label = 'temp10years_20230601.rds', shape = ellipse, fillcolor = MediumSeaGreen]
dataStations [label = 'Stations_included_10year.csv', shape = ellipse, fillcolor = MediumSeaGreen]
dataStationsRegions [label = 'stations_w_regions_20230601.csv', shape = ellipse, fillcolor = MediumSeaGreen]
dataTempStats [label = 'temp_summary_stats_station.csv', shape = ellipse, fillcolor = MediumSeaGreen]
dataAbsencePresence [label = 'exportfishabsence_presence.csv', shape = ellipse, fillcolor = MediumSeaGreen]
dataExceedance [label = 'dataExceedanceUpperDataForPlotting.csv', shape = ellipse, fillcolor = MediumSeaGreen]

prepare [label =  'prepare_datasets.Rmd']
cluster [label = 'tempclusters.Rmd']
joinregions [label = 'join_stations_to_regions.R']
modelTrend [label = 'model_temperature_trends.Rmd']
modelSpecies [label = 'model_native_nonnative.Rmd']
plotMargins [label = 'plot_fish_absence_presence_vulnerability.Rmd']
plotBoxplots [label = 'plot_days_exceedance_boxplot.Rmd']
plotHeat22 [label = 'stationheatmaps.R']

figClusters [label = 'Fig1_map_clusters.png \n FigA1_dendplot.png',shape = septagon, fillcolor = LightSteelBlue]
figModels [label = 'Fig2_Watertemp_trends_max.tiff \n Fig3_Watertemp_trends_min.tiff \n Tables A2-A5',shape = septagon, fillcolor = LightSteelBlue]
figMargins [label = 'Fig4_Temp_margin_tile_plot_continuous.png \n FigA2_tempmargin_thresholds.png \n FigA3_Temp_margin_tile_plot_discrete.png ',shape = septagon, fillcolor = LightSteelBlue]
figBoxplots [label = 'Fig5_Opt_Tol.png', shape = septagon, fillcolor = LightSteelBlue]
figHeat22 [label = 'Fig6_stations_above22_all_warm.png \n FigA4_norecovery_barplot.png', shape = septagon, fillcolor = LightSteelBlue]

tabStations [label = 'Table A1', shape = septagon, fillcolor = LightSteelBlue]
tabSpeciesModel [label = 'Tables A6-A7', shape = septagon, fillcolor = LightSteelBlue]

# edge definitions with the node IDs (what connects to what)
{raw1 raw2}  -> prepare -> {dataDaily dataMonthly dataHourly}
dataDaily -> cluster -> figClusters
cluster -> raw4
{raw1 raw4} -> joinregions -> dataStationsRegions

dataDaily -> modelTrend
dataMonthly -> modelTrend -> figModels
modelTrend -> dataStations

plotHeat22 -> dataTempStats
{dataHourly raw3} -> plotMargins -> {figMargins}
rawThresholds -> plotMargins
plotMargins -> dataAbsencePresence
dataDaily -> plotBoxplots
dataHourly -> plotBoxplots -> figBoxplots
rawThresholds -> plotBoxplots -> dataExceedance -> modelSpecies -> tabSpeciesModel
dataAbsencePresence -> plotBoxplots
dataHourly -> plotHeat22 
dataDaily -> plotHeat22 -> figHeat22
{dataStations dataTempStats} -> tabStations
      }")

# Export diagram
tmp = DiagrammeRsvg::export_svg(graph)
tmp = charToRaw(tmp) # flatten
rsvg::rsvg_png(tmp, "manuscript_code/Figures/flowchart.png") # saved graph as png in current working directory
