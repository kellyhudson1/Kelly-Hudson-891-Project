# HEADER========================================================================
# Title: globeMod
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: globe vs. no globe. gdp and demand
# GLOBE is a SAM-based global CGE model 

# disable scientific notation
options(scipen = 999)

# LIBRARIES=====================================================================
library("data.table")
library("ggplot2")
library("scales")


# INCOME PRE VS POST GLOBE============================

# do for each ssp with ref elasticities and 
# for 100% all cty income elasticity scenario

incGlobe_dt <- fread("rawData_incGlobe_dt.csv")

# change column names 
colnames(incGlobe_dt) <- c("scen", "cty", "year", "incTot")

# add ssp col
incGlobe_dt$ssp <- substr(incGlobe_dt$scen, start = 5, stop = 8)


# add population
#popCtyProj_dt <- readRDS(file = "procData_popCtyProj_dt.Rda")
popCtyProj_dt <- fread("rawData_popCtyProj_dt.csv")

setkey(popCtyProj_dt,scen)
popCtyProj_dt <- popCtyProj_dt[ c("SSP1_ref","SSP2_ref", "SSP3_ref")]

# add ssp col
popCtyProj_dt$ssp <- substr(popCtyProj_dt$scen, start = 1, stop = 4)
popCtyProj_dt$scen <- NULL
popCtyProj_dt$wbCtyIncClass <- NULL


incGlobe_dt <- merge(incGlobe_dt, popCtyProj_dt,
      by = c("ssp", "cty", "year"))

# add scen-globe
incGlobe_dt$scenGlobe <- substr(incGlobe_dt$scen, 10, 
                                   nchar(incGlobe_dt$scen) - 3)

incGlobe_dt$incElas <- sub("_.*", "", incGlobe_dt$scenGlobe)
incGlobe_dt$globe <- sub(".*_", "", incGlobe_dt$scenGlobe)

incGlobe_dt[globe == "pre", globe := "GLOBE-adjusted income"]
incGlobe_dt[globe == "pos", globe := "Not GLOBE-adjusted income"]

incGlobe_dt$scenGlobe <- as.factor(incGlobe_dt$scenGlobe)
levels(incGlobe_dt$scenGlobe) <- 
  c("GLOBE-adjusted income & reference case elasticities",
    "Not GLOBE-adjusted income & reference case elasticities",
    "GLOBE-adjusted income & 100% lower red meat elasticities",
    "Not GLOBE-adjusted income & 100% lower red meat elasticities")


# add WB income groups
impCtyReg_dt <- readRDS(file = "procData_impCtyReg_dt.Rda")


incGlobe_dt <- merge(incGlobe_dt, impCtyReg_dt,
      by = c("cty", "region", "reg", "impCountry"))


# high income vs. not high income column
incGlobe_dt[!(wbCtyIncClass == "High income"), wbCtyIncClass := "All low- & middle income countires"]
incGlobe_dt[(wbCtyIncClass == "High income"), wbCtyIncClass := "All high-income countries"]


# subset data
incGlobe_dt <- incGlobe_dt[year==2050]


# total by income group
incGlobeIncGrp_dt <- incGlobe_dt[, .(
  incTot =  sum(incTot),
  pop000Peop =  sum(pop000Peop)),
  by = .(ssp, incElas, globe, scenGlobe, region)]

incGlobeIncGrp_dt[, incPerPers := (incTot * 1000 * 1000000)/
                    (pop000Peop * 1000)]

# PLOT INCOME===================================================================
# plot income in 2050 facet ssp-incE, with/without

# * plot options====
theme_opts <- list(
  theme(legend.position = "bottom",
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 16),
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 14),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        aspect.ratio = 1,
        panel.spacing = unit(0, "lines")
  )
)

# * bar plot====
setkey(incGlobeIncGrp_dt, incElas)
incGlobeIncGrp_plot <- ggplot(data = incGlobeIncGrp_dt["ref"],
                                aes(x = ssp)) +
           geom_bar(
             aes(y = incPerPers,
                 fill = globe),
             stat = "identity",
             width = 0.4, 
             position = "dodge",
             color = "black"
           ) + facet_wrap(~ region,
                          nrow = 2) +
           labs(fill = "",
                y = "2050 per person income (US $, constant 2005)",
                x = "Shared socioeconomic pathway (SSP)") +
           theme_bw() + theme_opts +
  scale_y_continuous(labels = comma) + 
  scale_fill_grey() +
  guides(fill = guide_legend(nrow = 1))

# save plot
ggsave(
  file = "output_figSI11_globeIncome.tiff",
  width = 12,
  height = 8,
  incGlobeIncGrp_plot,
  compression = "lzw"
)