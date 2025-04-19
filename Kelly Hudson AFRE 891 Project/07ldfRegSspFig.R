# HEADER========================================================================
# Title: ldfRegSspFig
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: effect of SSP on change in per person and total demand 
# for each ldf over time by region
# annual percentage change is from CAGR

# LIBRARIES=====================================================================
library("data.table")
library("ggplot2")
library("RColorBrewer")

# disable scientific notation
options(scipen = 999)

# LOAD ALL DATA FOR REGIONS=====================================================

# * load region demand====
ldfRegProj_dt <- readRDS(file = "procData_ldfRegProj_dt.Rda")

# * load reg cal cont==== 

genusRegWld_dt <- fread("rawData_genusRegWld_dt.csv")
genusRegWld_dt$percProtRegGenus <- NULL

# data check

# total demand in each regType must be the same
ldfRegProjTotDemand_dt <- ldfRegProj_dt[, .(
  demaTot000Tons =  sum(demaTot000Tons)),
  by = .(scen, regType)]
ldfRegProjTotDemand_dt <- setorder(ldfRegProjTotDemand_dt, scen)


# * organize data====

# keep 3 SSP with ref income elasticities
# for each SSP in 2020 and 2050 for "food"

ldfProjScen_dt <- ldfRegProj_dt[(scen == "SSP1_ref" |
                                 scen == "SSP2_ref" |
                                 scen == "SSP3_ref") &
                                 (year == "2020" |
                                  year == "2050") &
                                  ldfUnit == "food", ]

# keep needed columns for tab and fig
ldfProjScen_dt <-
  ldfProjScen_dt[, c("reg",
                     "region",
                     "scen",
                     "year",
                     "ldf",
                     "demaTot000Tons",
                     "demaPerPersKg")]

# demand in millions of tons
ldfProjScen_dt[, demaTotMillionTons := demaTot000Tons / 1000]


# demand in one column (reshape: wide to long)
ldfProjScen_dt <- melt(ldfProjScen_dt,
                  id.vars = c("reg",
                              "region",
                              "scen",
                              "year",
                              "ldf"),
                  measure.vars = c("demaTotMillionTons",
                                   "demaPerPersKg"),
                  variable.name = "dataType",
                  value.name = "demand"
)


# year in seperate columns (reshape: long to wide)
ldfProjScen_dt <- dcast(ldfProjScen_dt,
                           region + reg + scen + ldf + 
                            dataType  ~ year, 
                            value.var = "demand")

# percent change over all years
# number years for annual % change 
nYrs <- 2050 - 2020
ldfProjScen_dt[ , annPC20502020 := round(100 * ((`2050` / `2020`) ^
                                             (1 / nYrs)- 1), 2)]

# * common color for ssps====
# http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
brewColorSsp <- brewer.pal(11, name = 'BrBG')
sspColor <- c(brewColorSsp[[8]], # SSP1
              brewColorSsp[[6]], # SSP2
              brewColorSsp[[3]]) # SSP3

# region as ordered factor
ldfProjScen_dt$region <-
  factor(
    ldfProjScen_dt$region,
    ordered = TRUE,
    levels = c(
      "World",
      "East Asia & Pacific",
      "Europe",
      "Former Soviet Union",
      "Latin America & Caribbean",
      "Middle East & North Africa",
      "North America",
      "South Asia",
      "sub-Saharan Africa"
    )
  )


# * ldf as ordered factor
ldfProjScen_dt$ldf <-
  factor(
    ldfProjScen_dt$ldf,
    ordered = TRUE,
    levels = c(
      "Beef",
      "Sheep",
      "Pork",
      "Poultry",
      "Eggs",
      "Milk"
    )
  )

# CHANGE IN INDIVIDUAL LDF DEMAND BY REGION=====================================

# * reg per person demand per ldf====
# filter data to keep
setkey(ldfProjScen_dt, dataType)

ldfProjScen_dt[, redmeat := "Not red Meat"]
ldfProjScen_dt[ldf == "Beef" | ldf == "Sheep"  | ldf == "Pork",
               redmeat := "Red Meat"]

ldfPerPersReg <- ggplot(data = ldfProjScen_dt[.("demaPerPersKg")],
            aes(x = ldf)) +
  geom_bar(
    aes(y = annPC20502020, fill = scen),
    stat = "identity",
    width = 0.8, 
    position = "dodge",
    color = "black"
  ) + facet_wrap( ~ region) +
  scale_fill_manual("Shared socioeconomic pathway (SSP)", 
                    labels = c("SSP1",
                               "SSP2",
                               "SSP3"),
                    values = sspColor) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) + 
  labs(x     = "Livestock-derived food",
       y     = "Annual percent change\n in per person demand (2020 to 2050)",
       title = "") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text.x = element_text(size = 16),
    legend.key.size = unit(1, "cm"),
    legend.position = "bottom",
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 18),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16, angle = 90),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 20),
    aspect.ratio = 1,
    panel.spacing = unit(0, "cm")
  ) 

# save reg per person plot
ggsave(
  file = "output_fig3_ldfPerPersReg.tiff",
  ldfPerPersReg,
  width  = 12,
  height = 12,
  compression = "lzw"
)

# * reg total demand per ldf====

# setkey to subset data
setkey(ldfProjScen_dt, dataType)

# gen reg tot plot 
# recycle per person region plot object (ldfPerPersReg) 
# and only change data source and ylab
ldfTotReg <- ldfPerPersReg %+% ldfProjScen_dt[.("demaTotMillionTons")] +
  labs(y = "Annual percent change\n in total demand (2020 to 2050)")

# save reg tot plot
ggsave(
  file = "output_figSI8_ldfTotReg.tiff",
  ldfTotReg,
  width  = 12,
  height = 12,
  compression = "lzw"
)


# tables for range of changes====
# want to know range of % changes 
# in total demand for ldf depending on SSP
# use protein (from GENuS)

# * organize data====
# keep 3 SSP with ref income elasticities
# for each SSP in 2020 and 2050 for "protein"

ldfProjPScen_dt <- ldfRegProj_dt[(scen == "SSP1_ref" |
                                  scen == "SSP2_ref" |
                                  scen == "SSP3_ref") &
                                  (year == "2020" |
                                     year == "2050") &
                                  ldfUnit == "proteinGenus", ]


# sum so get total for all ldf for protein (from GENuS)

aggAsfProtReg_dt <- ldfProjPScen_dt[, .(
  demaTot000Tons =  sum(demaTot000Tons),
  demaPerPersKg =  sum(demaPerPersKg)
),
by = .(reg, year, scen)]


# year in seperate columns (reshape: long to wide)
aggAsfProtReg_dt <- dcast(aggAsfProtReg_dt,
                        reg + scen
                          ~ year, 
                        value.var = c("demaTot000Tons", "demaPerPersKg"))

# percent change over all years
aggAsfProtReg_dt[, percChange5020Tot := round(100 * (demaTot000Tons_2050 - demaTot000Tons_2020) / demaTot000Tons_2020, 1)]
aggAsfProtReg_dt[, percChange5020PerPers := round(100 * (demaPerPersKg_2050 - demaPerPersKg_2020) / demaPerPersKg_2020, 1)]


# CALORIES DEMANDED=============================================================

# keep SSP2 ref and "food" and 2005 and 2050
{
setkey(ldfRegProj_dt, scen, ldfUnit, year)
ldfCal_dt <- ldfRegProj_dt[CJ(
  c("SSP2_ref"),
  "food",
  c("2005", "2050")
)]

ldfCal_dt <- ldfCal_dt[, c("reg", "ldf", "year",
                           "demaTot000Tons",
                           "demaPerPersKg")]

ldfCal_dt <- merge(ldfCal_dt, genusRegWld_dt,
      by = c("reg", "ldf"))

# total kcal demand in GJ
# 1 GJ = 238902957.6186153 kcal
gJkcal <- 238902957.6186153

ldfCal_dt[, gjDemTot := (demaTot000Tons * 1000 * 1000 *
            kcalPerKgGenus) / gJkcal]

ldfCal_dt[, calDemPerPers := (demaPerPersKg *
                           kcalPerKgGenus)]

ldfCal_dt <- ldfCal_dt[, c("reg", "ldf", "year", 
                           "gjDemTot",
                           "calDemPerPers")]
# sum cal over ldf
calTot_dt <- ldfCal_dt[,. (gjDemTot = sum(gjDemTot),
                           calDemPerPers = sum(calDemPerPers)
                           ),
                       by = .(year, reg)]

# long to wide
calTot_dt <- dcast(calTot_dt,
                       reg ~ year,
                       value.var = c("gjDemTot", "calDemPerPers"))

calTot_dt[, perPersChange := round(100 * (calDemPerPers_2050 - calDemPerPers_2005)/
            calDemPerPers_2005, 0)]
calTot_dt[, totChange := round(100 * (gjDemTot_2050 - gjDemTot_2005)/
            gjDemTot_2005, 0)]
}