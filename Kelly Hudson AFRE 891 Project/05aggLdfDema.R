# HEADER========================================================================
# Title: aggLdfDema
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description:
# 1) aggregate demand across all 6 ldf to have a aggregate ldf demand for all ldf
# 2) aggregate demand across all 3 red meat ldf to have a aggregate red meat demand
# for each cty and reg have a per person and total demand value

# red meat = beef, sheep (& goats), and pork 

# LIBRARIES=====================================================================
library("data.table")

# REGION AGG ALL 6 LDF==========================================================

# so can compare historical and projected demand for protein 
# aggregated over 6 ldf
ldfRegAll_dt <- readRDS(file = "procData_ldfRegAll_dt.Rda")


# remove per person demand as compute this after aggregate
ldfRegAll_dt$demaPerPersKg <- NULL

# demand aggregated over all 6 ldf for protein only
# value is per region per year for protein
setkey(ldfRegAll_dt, ldfUnit)
apdRegAll_dt <- ldfRegAll_dt[c("protein", "proteinGenus")][, .(
  apdTot000Tons =  sum(demaTot000Tons)
),
by = .(reg, region, year, ldfUnit, dataType, regType, scen)]

# historical population, so can calculate per person demand

# reg
popRegHist_dt <- readRDS(file = "procData_popRegHist_dt.Rda")
popRegHist_dt$regType <- "reg"

# wld
popWldHist_dt <- readRDS(file = "procData_popWldHist_dt.Rda")
popWldHist_dt$regType <- "wld"
popWldHist_dt$reg <- "WLD"

# bind population data
popRegWldHist_dt <- rbind(popRegHist_dt, popWldHist_dt)

popRegWldHist_dt$dataType <- "Historical"
popRegWldHist_dt$scen <- "hist"

popRegWldHist_dt
# add reg
impCtyReg_dt <- readRDS(file = "procData_impCtyReg_dt.Rda")
regRegion_dt <- unique(impCtyReg_dt[, c("reg", "region")])

# merge
popRegWldHist_dt <- merge(popRegWldHist_dt, regRegion_dt,
                       by = "reg",
                       all.x = TRUE)
popRegWldHist_dt[reg == "WLD", region := "World"]

# projected population, so can calculate per person demand
popRegProj_dt <- readRDS(file = "procData_popRegProj_dt.Rda")

# add dataType
popRegProj_dt$dataType <- "Projected"

# add regType

# label if reg or wld
popRegProj_dt[reg ==  "WLD", regType := "wld"]
popRegProj_dt[!reg == "WLD", regType := "reg"]

# bind proj and hist pop
popRegAll_dt <- rbind(popRegWldHist_dt, popRegProj_dt)

# merge population and agg demand
apdRegAll_dt <- merge(
  apdRegAll_dt,
  popRegAll_dt,
  by = c("year", "reg", "region", "regType", "dataType", "scen")
)

# per person demand in kg
apdRegAll_dt[, apdPerPersKg := (apdTot000Tons * 1000 * 1000) / (pop000Peop *
                                                                        1000)]

# remove population
apdRegAll_dt <- apdRegAll_dt[, pop000Peop := NULL]

# save
saveRDS(apdRegAll_dt, file = "procData_apdRegAll_dt.Rda")



# Check demand protein values 
# sum each ldf for total and person aggrgeated over 6 ldf
ldfRegAll_dt <- readRDS(file = "procData_ldfRegAll_dt.Rda")

# demand aggregated over all 6 ldf
# value is per region per year per data type
setkey(ldfRegAll_dt, ldfUnit)

apdRegAllCheck_dt <- ldfRegAll_dt[c("protein", "proteinGenus")][, .(
  apdTot000Tons =  sum(demaTot000Tons),
  apdPerPersKg  =  sum(demaPerPersKg)
),
by = .(reg, region, year, dataType, regType, scen, ldfUnit)]