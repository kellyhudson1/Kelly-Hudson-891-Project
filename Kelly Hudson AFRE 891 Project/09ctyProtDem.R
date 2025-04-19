# HEADER========================================================================
# Title: ctyProtDem
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: calculate the cty with largest demand for red meat in ref case and in fbs 
# by impact reg 8

# LIBRARIES=====================================================================
library("data.table")

# disable scientific notation
options(scipen = 999)

# ORG DATA======================================================================
# load and org cty data
if(!exists("ldfCtyAll_dt")) {
ldfCtyAll_dt <- readRDS(file = "procData_ldfCtyAll_dt.Rda")
}

ldfCtyAll_dt
# filter by years for protein
# projected for 2020 and 2050, and historical for 1980 and 2010
ldfCtyScen_dt <- ldfCtyAll_dt[(((year == "1980" |
                                    year == "2010") &
                                   (dataType == "Historical")
) |
  ((year == "2020" |
      year == "2050") &
     (dataType == "Projected" & scen == "SSP2_ref")
  ))
& ldfUnit == "proteinGenus"
, ]


# LDF RANK CTY BY REG===========================================================

# rank demand by cty for each ldf by reg for total country demand
setorder(setDT(ldfCtyScen_dt), -demaTot000Tons)[, ctyRankAgpByReg := seq_len(.N),
                                               by = c("year", "dataType", "ldf", "reg")]

# keep a list of largest total demand for protein in 2050 by reg for total country demand
setkey(ldfCtyScen_dt, ctyRankAgpByReg, year, ldf)
ctyListBeef2050_dt <- ldfCtyScen_dt[.(1, "2050", "Beef")]

# keep relevant columns
ctyListBeef2050_dt <- ctyListBeef2050_dt[, c("cty", "reg", "wbCtyIncClass")]


# RED MEAT RANK CTY IN REG======================================================
setkey(ldfCtyScen_dt, ldf)
redMeatCtyScen_dt <- ldfCtyScen_dt[CJ(
  c("Beef", "Sheep", "Pork"))][, .(
  demaTot000Tons = sum(demaTot000Tons),
  demaPerPersKg =  sum(demaPerPersKg)),
  by = .(year, cty, country, reg, dataType, wbCtyIncClass)]


# rank red meat demand by cty by reg for total country demand
setorder(setDT(redMeatCtyScen_dt),-demaTot000Tons)[, ctyRankRmByReg := seq_len(.N),
                                               by = c("year", "dataType", "reg")]

# rank red meat demand by cty by reg for per person demand
setorder(setDT(redMeatCtyScen_dt),-demaPerPersKg)[, ctyRankRmByRegPerPerson := seq_len(.N),
                                                   by = c("year", "dataType", "reg")]


# keep a list of largest total demand for red meat protein in 2050 by reg for total country demand
setkey(redMeatCtyScen_dt, ctyRankRmByReg, year)
ctyListRedMeat2050_dt <- redMeatCtyScen_dt[.(1, "2050")]

ctyListRedMeat2050_dt <- ctyListRedMeat2050_dt[, c("cty", "reg", "wbCtyIncClass")]

# save list
saveRDS(ctyListRedMeat2050_dt, file = "procData_ctyListRedMeat2050_dt.Rda")
