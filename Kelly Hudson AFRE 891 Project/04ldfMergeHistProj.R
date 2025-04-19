# HEADER========================================================================
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: merge historical and projected ldf demand
# by region and by country

# LIBRARIES=====================================================================
library("data.table")

# disable scientific notation
options(scipen = 999)

# REGION========================================================================

# * hist ldf demand====
ldfRegHist_dt <-
  readRDS(file = "procData_ldfRegHist_dt.Rda")

# add columns so know data is historical data
# from FAO Food Balance Sheets
ldfRegHist_dt$dataType <- "Historical"
ldfRegHist_dt$scen <- "hist"

head(ldfRegHist_dt)

# add reg
impCtyReg_dt <- readRDS(file = "procData_impCtyReg_dt.Rda")
regRegion_dt <- unique(impCtyReg_dt[, c("reg", "region")])

# merge
ldfRegHist_dt <- merge(ldfRegHist_dt, regRegion_dt,
                       by = "reg",
                       all.x = TRUE)

ldfRegHist_dt[reg == "WLD", region := "World"]

# * proj ldf demand====
ldfRegProj_dt <-
  readRDS(file = "procData_ldfRegProj_dt.Rda")

# add column so know it is projected data taken from IMPACT
ldfRegProj_dt$dataType <- "Projected"

# * merge hist & proj====
ldfRegAll_dt <-
  rbind(ldfRegProj_dt, ldfRegHist_dt)

# column order
ldfRegAll_dt <- setcolorder(
  ldfRegAll_dt,
  c(
    "reg",
    "regType",
    "dataType",
    "year",
    "ldfUnit",
    "ldf",
    "scen",
    "demaTot000Tons",
    "demaPerPersKg"
  )
)

# * save====
saveRDS(ldfRegAll_dt, file = "procData_ldfRegAll_dt.Rda")

# COUNTRY=======================================================================
# * hist ldf demand====
ldfCtyHist_dt <-
  readRDS(file = "procData_ldfCtyHist_dt.Rda")

# add column so know it is historical data
# from FAO Food Balance Sheets
ldfCtyHist_dt$dataType <- "Historical"
ldfCtyHist_dt$scen <- "hist"

# * proj ldf demand and pop====
ldfCtyProj_dt <-
  readRDS(file = "procData_ldfCtyProj_dt.Rda")

# add column so know it is projected data, taken from IMPACT
ldfCtyProj_dt$dataType <- "Projected"


# in ldfCtyRefProj_dt rename "impCountry" as "country" just for the merge
names(ldfCtyProj_dt)[names(ldfCtyProj_dt) == "impCountry"] <- "country"

# in ldfCtyHist_dt rename "isoCty" as "cty" and "faoCountry" as "country"
names(ldfCtyHist_dt)[names(ldfCtyHist_dt) == "isoCty"] <- "cty"
names(ldfCtyHist_dt)[names(ldfCtyHist_dt) == "faoCountry"] <- "country"


# * bind hist & proj====
colnames(ldfCtyProj_dt)
colnames(ldfCtyHist_dt)
ldfCtyAll_dt <- rbind(ldfCtyProj_dt, ldfCtyHist_dt)


# in ldfCtyRefProj_dt rename "impCountry" as "country"
names(ldfCtyProj_dt)[names(ldfCtyProj_dt) == "country"] <- "impCountry"

# label if reg or wld
ldfCtyAll_dt[reg ==  "WLD", regType := "wld"]
ldfCtyAll_dt[reg != "WLD", regType := "reg"]

# set column order
ldfCtyAll_dt <- setcolorder(
  ldfCtyAll_dt,
  c("cty",
    "country",
    "wbCtyIncClass",
    "reg",
    "regType",
    "dataType",
    "year",
    "ldfUnit",
    "ldf",
    "scen",
    "demaTot000Tons",
    "demaPerPersKg"
  )
)

# * save====
# ldfDemaCty_dt is for the reference case ssp2
saveRDS(ldfCtyAll_dt, file = "procData_ldfCtyAll_dt.Rda")

# * duplicate rows====
anyDuplicated(
  ldfCtyAll_dt,
  incomparables = FALSE,
  fromLast = FALSE,
  by = seq_along(ldfCtyAll_dt)
)