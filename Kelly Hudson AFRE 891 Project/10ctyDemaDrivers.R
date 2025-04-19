# Title: ctyDemaDrivers 
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: drivers of demand in cty with largest total 
# beef demand per IMPACT region in 2050

# LIBRARIES=====================================================================
library("data.table")
library("ggplot2")
library("RColorBrewer")
library("scales")


# disable scientific notation
options(scipen = 999)

# LOAD CTY LIST=================================================================
# load list of 8 select cty
# cty in each of the 8 regions that has largest total demand for red meat in protein in 2050
# in ref case ssp2
ctyListRedMeat2050_dt <- readRDS(file = "procData_ctyListRedMeat2050_dt.Rda")
ctyListRedMeat2050_dt <- as.list(ctyListRedMeat2050_dt)

# report:
# 1) beef demand, extract total demand and per person demand
# 2) beef prices
# 3) per person GDP 
# 4) total population
# 5) income elasticity of demand


# LOAD BEEF DEMAND DATA=========================================================
if(!exists("ldfCtyProj_dt")) {
ldfCtyProj_dt <- readRDS("procData_ldfCtyProj_dt.Rda")
}

# keep 2020 and 2050 for 3 ref case ssp, 
# & SSP2_rm100LAllCty, and SSP2_rm100LHiCty	

setkey(ldfCtyProj_dt, year, scen, ldfUnit, ldf, cty)
beefDema8Cty_dt <- ldfCtyProj_dt[CJ(
  c("2020", "2050"),
  c("SSP1_ref",
    "SSP2_ref",
    "SSP3_ref",
    "SSP2_rm100LAllCty",
    "SSP2_rm100LHiCty"),
  "food",
  "Beef",
  ctyListRedMeat2050_dt[[1]]
)]

# keep relevant columns
beefDema8Cty_dt <- 
  beefDema8Cty_dt[, c("cty",
                      "wbCtyIncClass",
                      "year",
                      "scen",
                      "demaPerPersKg",
                      "demaTot000Tons")]

# LOAD PRICE DATA==========================================================

prices_dt <- fread("rawData_prices_dt.csv")

prices_dt$year <- as.character(prices_dt$year)

# keep 2020 and 2050 for beef and 8 cty
setkey(prices_dt, year, c, cty)
beefPrice8Cty_dt <- prices_dt[CJ(
  c("2020", "2050"),
 "cbeef",
 ctyListRedMeat2050_dt[[1]]
)]

# keep relevant columns
beefPrice8Cty_dt <- 
  beefPrice8Cty_dt[, c("cty", "year", "scen", "price")]

# rename scenarios
beefPrice8Cty_dt$scen
beefPrice8Cty_dt[scen == "PCX_SSP1_ref_dt", scen := "SSP1_ref"]
beefPrice8Cty_dt[scen == "PCX_SSP2_ref_dt", scen := "SSP2_ref"]
beefPrice8Cty_dt[scen == "PCX_SSP3_ref_dt", scen := "SSP3_ref"]
beefPrice8Cty_dt[scen == "PCX_SSP2_rm100LAllCty_dt", 
                 scen := "SSP2_rm100LAllCty"]
beefPrice8Cty_dt[scen == "PCX_SSP2_rm100LHiCty_dt", 
                 scen := "SSP2_rm100LHiCty"]

# merge price and demand
beefDemaPrice8Cty_dt <- merge(beefDema8Cty_dt, 
                              beefPrice8Cty_dt,
                              by = c("cty", 
                                     "year", 
                                     "scen"))

# LOAD POPULATION DATA==========================================================

popCtyProj_dt <- fread("rawData_popCtyProj_dt.csv")
popCtyProj_dt$year <- as.character(popCtyProj_dt$year)

# keep 2020 and 2050 for 5 scenarios	
setkey(popCtyProj_dt, year, scen, cty)
pop8Cty_dt <- popCtyProj_dt[CJ(
  c("2020", "2050"),
  c("SSP1_ref",
    "SSP2_ref",
    "SSP3_ref",
    "SSP2_rm100LAllCty",
    "SSP2_rm100LHiCty"),
  ctyListRedMeat2050_dt[[1]]

)]

# keep needed columns
pop8Cty_dt <- pop8Cty_dt[, c("cty", "scen", "year", "pop000Peop")]


# merge price, demand, and pop
beefDemaPricePop8Cty_dt <- merge(beefDemaPrice8Cty_dt, pop8Cty_dt,
                              by = c("cty", "scen", "year"))

# LOAD INCOME DATA==============================================================
# GDPX0(CTY, YRS) parameter is Final GDP (billion 2005 USD)

income_dt <- fread("rawData_income_dt.csv")

# change column names
colnames(income_dt) <- c("scen", "cty", "year", "incTot")
income_dt$year <- as.character(income_dt$year)

income_dt[scen == "INC_SSP1_ref_dt", scen := "SSP1_ref"]
income_dt[scen == "INC_SSP2_ref_dt", scen := "SSP2_ref"]
income_dt[scen == "INC_SSP3_ref_dt", scen := "SSP3_ref"]
income_dt[scen == "INC_SSP2_rm100LAllCty_dt", scen := "SSP2_rm100LAllCty"]
income_dt[scen == "INC_SSP2_rm100LHiCty_dt", scen := "SSP2_rm100LHiCty"]
income_dt[scen == "INC_SSP2_rm50LAllCty_dt", scen := "SSP2_rm50LAllCty"]
income_dt[scen == "INC_SSP2_rm50LHiCty_dt", scen := "SSP2_rm50LHiCty"]

saveRDS(income_dt, file = "procData_income_dt.Rda")

# keep 2020 and 2050
setkey(income_dt, year, cty, scen)
inc8Cty_dt <- income_dt[CJ(
  c("2020", "2050"),
  ctyListRedMeat2050_dt[[1]],
  c("SSP2_ref",
    "SSP2_rm100LAllCty",
    "SSP2_rm100LHiCty",
    "SSP1_ref",
    "SSP3_ref")
)]

# merge price, demand, pop, and total income
beefDrivers8Cty_dt <- merge(beefDemaPricePop8Cty_dt, inc8Cty_dt,
                                 by = c("scen", "cty", "year"))


# per person income
beefDrivers8Cty_dt[, incPerPers := (incTot * 1000 * 1000000)/
                             (pop000Peop * 1000)]


# INCOME AND POP DATA FIG/TAB====
# add population and income into one file
# compute 2020 and 2050 % change for each cty
# and for the world, IQR
# * data====

{
incPopCty_dt <- merge(income_dt, popCtyProj_dt,
      by = c("scen", "cty", "year"),
      all.x = TRUE)

incPopCty_dt <- incPopCty_dt[year == "2020" | 
                               year == "2050"]

incPopCty_dt <-
  incPopCty_dt[, c("scen",
                   "wbCtyIncClass",
                   "reg",
                   "region",
                   "cty",
                   "year",
                   "incTot",
                   "pop000Peop")]

incPopCty_dt <- incPopCty_dt[, perPersInc := (incTot * 1000 * 1000000)/
               (pop000Peop * 1000)]

# long to wide
incPopCty_dt <- dcast(incPopCty_dt,
                      scen + wbCtyIncClass + reg + region + cty ~ year,
                      value.var = c("incTot", "perPersInc", "pop000Peop"))


# * global value====

incPopWld_dt <- 
  incPopCty_dt[, .(incTot_2020 =  sum(incTot_2020),
                   incTot_2050 =  sum(incTot_2050),
                   pop000Peop_2020 =  sum(pop000Peop_2020),
                   pop000Peop_2050 =  sum(pop000Peop_2050)),
                 by = .(scen)]

incPopWld_dt[ , perPersInc_2020 := (incTot_2020 * 1000 * 1000000)/
               (pop000Peop_2020 * 1000) ]
incPopWld_dt[ , perPersInc_2050 := (incTot_2050 * 1000 * 1000000)/
               (pop000Peop_2050 * 1000) ]


# number year for annual % change 
nYrs <- 2050 - 2020

incPopWld_dt[ , annPCPPInc := round(100 * ((perPersInc_2050 / perPersInc_2020) ^
              (1 / nYrs)- 1), 2)]

incPopWld_dt[ , annPCPop := round(100 * ((pop000Peop_2050 / pop000Peop_2020) ^
                                           (1 / nYrs)- 1), 2)]

incPopWld_dt <- incPopWld_dt[, c("scen", "annPCPPInc", "annPCPop")]

# * cty range====

incPopCty_dt[ , annPCPPInc := round(100 * ((perPersInc_2050 / perPersInc_2020) ^
                                             (1 / nYrs)- 1), 2)]

incPopCty_dt[ , annPCPop := round(100 * ((pop000Peop_2050 / pop000Peop_2020) ^
                                           (1 / nYrs)- 1), 2)]

incPopCtyRange_tab <- 
  incPopCty_dt[, .(pPInc_min =  min(annPCPPInc),
                   pPInc_max =  max(annPCPPInc),
                   pPInc_iqr = round(IQR(annPCPPInc), 2),
                   pop_min =  min(annPCPop),
                   pop_max =  max(annPCPop),
                   pop_iqr = round(IQR(annPCPop), 2)
                   ),
               by = .(scen)]

incPopRange_tab <- merge(incPopWld_dt, incPopCtyRange_tab,
                         by = "scen")

# income global average, range, IQR
incPopRange_tab$rangePPInc <- paste(incPopRange_tab$pPInc_min, 
                               incPopRange_tab$pPInc_max, sep ="\226")
incPopRange_tab$rangeIqrPPInc <- paste(incPopRange_tab$rangePPInc, 
                               incPopRange_tab$pPInc_iqr, sep = ", ")
incPopRange_tab$rangeIqrPPInc <- paste0("(", incPopRange_tab$rangeIqrPPInc,")")
incPopRange_tab$pPInc <- paste(incPopRange_tab$annPCPPInc, 
                               incPopRange_tab$rangeIqrPPInc, sep = " ")

# pop global average, range, IQR
incPopRange_tab$rangePop <- paste(incPopRange_tab$pop_min, 
                                  incPopRange_tab$pop_max, sep ="\226")
incPopRange_tab$rangeIqrPop <- paste(incPopRange_tab$rangePop, 
                                     incPopRange_tab$pop_iqr, sep = ", ")
incPopRange_tab$rangeIqrPop <- paste0("(", incPopRange_tab$rangeIqrPop,")")
incPopRange_tab$pop <- paste(incPopRange_tab$annPCPop, 
                                  incPopRange_tab$rangeIqrPop, sep = " ")

incPopRange_tab <- incPopRange_tab[,c("scen", "pPInc", "pop") ]


# scenarios as ordered factor for scenario table
incPopRange_tab$scen <-
  factor(
    incPopRange_tab$scen,
    ordered = TRUE,
    levels = c("SSP1_ref",
               "SSP2_ref",
               "SSP3_ref",
               "SSP2_rm50LAllCty",
               "SSP2_rm100LAllCty",
               "SSP2_rm50LHiCty",
               "SSP2_rm100LHiCty"
    )
  )

incPopRange_tab <- setorder(incPopRange_tab, scen)

}

# save globe-cty
fwrite(incPopRange_tab, "output_table1_incPopRange.csv",
       row.names = FALSE)

# * reg value====

incPopReg_dt <- 
  incPopCty_dt[, .(incTot_2020 =  sum(incTot_2020),
                   incTot_2050 =  sum(incTot_2050),
                   pop000Peop_2020 =  sum(pop000Peop_2020),
                   pop000Peop_2050 =  sum(pop000Peop_2050)),
               by = .(scen, reg, region)]

incPopReg_dt[ , perPersInc_2020 := (incTot_2020 * 1000 * 1000000)/
                (pop000Peop_2020 * 1000) ]
incPopReg_dt[ , perPersInc_2050 := (incTot_2050 * 1000 * 1000000)/
                (pop000Peop_2050 * 1000) ]

incPopReg_dt[ , annPCPPInc := round(100 * ((perPersInc_2050 / perPersInc_2020) ^
                                             (1 / nYrs) - 1), 2)]

incPopReg_dt[ , annPCPop := round(100 * ((pop000Peop_2050 / pop000Peop_2020) ^
                                           (1 / nYrs) - 1), 2)]

incPopReg_dt <- incPopReg_dt[, c("scen", "reg", "region", "annPCPPInc", "annPCPop")]

incPopRegRange_tab <- 
  incPopCty_dt[, .(pPInc_min =  min(annPCPPInc),
                   pPInc_max =  max(annPCPPInc),
                   pPInc_iqr = round(IQR(annPCPPInc), 2),
                   pop_min =  min(annPCPop),
                   pop_max =  max(annPCPop),
                   pop_iqr = round(IQR(annPCPop), 2)
  ),
  by = .(scen, reg)]


incPopRegRange_tab <- merge(incPopReg_dt, incPopRegRange_tab,
                         by = c("scen", "reg"))

# income reg average, range, IQR
incPopRegRange_tab$rangeInc <- paste(incPopRegRange_tab$pPInc_min, 
                                  incPopRegRange_tab$pPInc_max, sep ="\226")
incPopRegRange_tab$rangeIqrInc <- paste(incPopRegRange_tab$rangeInc, 
                                     incPopRegRange_tab$pPInc_iqr, sep = ", ")
incPopRegRange_tab$rangeIqrInc <- paste0("(", incPopRegRange_tab$rangeIqrInc,")")
incPopRegRange_tab$valueInc <- paste(incPopRegRange_tab$annPCPPInc, 
                                  incPopRegRange_tab$rangeIqrInc, sep = " ")

# pop reg average, range, IQR
incPopRegRange_tab$rangePop <- paste(incPopRegRange_tab$pop_min, 
                                  incPopRegRange_tab$pop_max, sep ="\226")
incPopRegRange_tab$rangeIqrPop <- paste(incPopRegRange_tab$rangePop, 
                                     incPopRegRange_tab$pop_iqr, sep = ", ")
incPopRegRange_tab$rangeIqrPop <- paste0("(", incPopRegRange_tab$rangeIqrPop,")")
incPopRegRange_tab$valuePop <- paste(incPopRegRange_tab$annPCPop, 
                                  incPopRegRange_tab$rangeIqrPop, sep = " ")

incPopRegRange_tab <- incPopRegRange_tab[,c("scen", "reg", "region", "valueInc", "valuePop") ]

setkey(incPopRegRange_tab, scen)
incPopRegRange_tab <- incPopRegRange_tab[c("SSP2_ref",
               "SSP1_ref",
               "SSP3_ref")]

# scenarios as ordered factor for scenario table
incPopRegRange_tab$scen <-
  factor(
    incPopRegRange_tab$scen,
    ordered = TRUE,
    levels = c("SSP1_ref",
               "SSP2_ref",
               "SSP3_ref"
    )
  )

incPopRegRange_tab <- setorder(incPopRegRange_tab, reg, scen)
incPopRegRange_tab


# income group value====

setkey(incPopCty_dt, scen)
incPopCtyIncGrp_dt <- incPopCty_dt[c("SSP1_ref",
                                     "SSP2_ref",
                                     "SSP3_ref")]

# sum income and pop all cty by inc Grp
incPopCtyIncGrp_dt[!(wbCtyIncClass == "High income"), wbCtyIncClass := "LMIC"]
incPopCtyIncGrp_dt[wbCtyIncClass == "High income", wbCtyIncClass := "HIC"]

incPopIncGrp_dt <- 
  incPopCtyIncGrp_dt[, .(incTot_2020 =  sum(incTot_2020),
                   incTot_2050 =  sum(incTot_2050),
                   pop000Peop_2020 =  sum(pop000Peop_2020),
                   pop000Peop_2050 =  sum(pop000Peop_2050)),
               by = .(scen, wbCtyIncClass)]

# per person income 
incPopIncGrp_dt[, incPerPers_2020 := (incTot_2020 * 1000 * 1000000)/
                     (pop000Peop_2020 * 1000)]
incPopIncGrp_dt[, incPerPers_2050 := (incTot_2050 * 1000 * 1000000)/
                  (pop000Peop_2050 * 1000)]


incPopIncGrp_dt <- melt(incPopIncGrp_dt,
                        id.vars = c("scen", "wbCtyIncClass"))


incPopIncGrp_dt <- dcast(incPopIncGrp_dt,
                      scen + variable ~ wbCtyIncClass)
incPopIncGrp_dt[, percLMIC := round(100 * LMIC / (HIC + LMIC), 0)]
incPopIncGrp_dt[, percHICPerc := round(100 * HIC / (HIC + LMIC), 0)]

# plot income and pop growth====

# plot
incPopCty_dt
setkey(incPopCty_dt, scen)
incPopCtySSP_dt <- incPopCty_dt[ c("SSP1_ref",
                                    "SSP2_ref",
                                    "SSP3_ref")]

# add ssp col
incPopCtySSP_dt$ssp <- substr(incPopCtySSP_dt$scen, 
                               start = 1,
                               stop = 4)

incPopCtySSP_dt[ , annPCInc := round(100 * ((incTot_2050 / incTot_2020) ^
                                              (1 / nYrs)- 1), 2)]

# keep columns
incPopCtySSP_dt <-
  incPopCtySSP_dt[, c(
    "ssp",
    "reg",
    "region",
    "cty",
    "annPCPop",
    "annPCPPInc",
    "annPCInc"
  )]

# wide to long
incPopCtySSP_dt <- melt(
  incPopCtySSP_dt,
  id.vars = c("ssp",
              "reg",
              "region",
              "cty"))

incPopCtySSP_dt[variable == "annPCPop", variable := "Population"]
incPopCtySSP_dt[variable == "annPCInc", variable := "Total income"]
incPopCtySSP_dt[variable == "annPCPPInc", variable := "Per person income"]

incPopCtySSP_dt$variable <-
  factor(
    incPopCtySSP_dt$variable,
    ordered = TRUE,
    levels = c(
      "Population",
      "Total income",
      "Per person income"))
      
      


# plot====

# * common color for ssps====
# http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
brewColorSsp <- brewer.pal(11, name = 'BrBG')
sspColor <- c(brewColorSsp[[8]], # SSP1
              brewColorSsp[[6]], # SSP2
              brewColorSsp[[3]]) # SSP3

{
sspPopInc_plot <- ggplot(data = incPopCtySSP_dt,
                      aes(x = variable,
                          y = value,
                          fill = ssp)) + 
  geom_boxplot(outlier.shape = NA, 
               position = position_dodge(width = 0.7),
               color = "black",
               size = 0.3) + 
  facet_wrap(~ region, nrow = 2) +
  labs(x     = "",
       y     = "Annual percent change (2020 to 2050)") +
  theme_bw() +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) + 
  scale_x_discrete(labels = wrap_format(15)) +
  theme(
    legend.key.size = unit(1, "cm"),
    legend.position = "bottom",
    legend.margin = margin(t = -0.9, 
                           r = 0, 
                           b = -1, 
                           l = 0, unit = "cm"),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12),
    aspect.ratio = 1,
    panel.spacing = unit(0, "cm")
  ) +
  scale_fill_manual("Shared socioeconomic pathway (SSP)", 
                    labels = c("SSP1",
                               "SSP2",
                               "SSP3"),
                    values = sspColor)

# save plot
ggsave(
  file = "output_figSI4_popInc.tiff",
  sspPopInc_plot,
  width  = 9,
  height = 6,
  compression = "lzw"
)

}