# HEADER========================================================================
# File name: masterFile
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: this is the master file that runs all scripts in 'project_ldfDemand.Rproj' 
# sequentially from start to finish to reproduce all tables and figures that
# report results in the study 
# The simulation results were generated in GAMS and therefore outside of this script, 
# please direct questions on the GAMS code and input data to the author

# list all R files in project
fileNames <- list.files(pattern = "R$")
class(fileNames)
fileNames
# number of files in "scripts" folder to run 
# minus one as don't need to run the masterFile twice
# minus N if don't want to run last N files
nFiles <- length(fileNames) - 1
nFiles

# time all script run time
startTimingAll <- proc.time()

# run all files in the "scripts" folder
for (i in 1:nFiles) {
  source(paste0(fileNames[i], sep = ""), echo = TRUE)
}

timeAll <- proc.time() - startTimingAll
timeAll