# HEADER========================================================================
# Title: ipr
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: report intrinsic productivity growth rates (IPR) for LDFs

# IPRs described:
# https://doi.org/10.1088/1748-9326/10/8/085010

# IMPACT3 calibrated to ssp2 but only with gdp and population changing
# no change in IPRs or elasticities between SSPs

# disable scientific notation
options(scipen = 999)

# LIBRARIES=====================================================================
library("data.table")
library("ggplot2")

# IPRs: LOAD AND ORGANIZE=======================================================

ipr_dt <- fread("rawData_ipr_dt.csv")

ipr_dt <- ipr_dt[LVSYS == "urban"]
ipr_dt$LVSYS <- NULL
setnames(ipr_dt, c("ldf", "fpu", "year", "ipr"))

# change ldf names
ipr_dt[ldf == "jbeef", ldf := "Beef"]
ipr_dt[ldf == "jlamb", ldf := "Sheep"]
ipr_dt[ldf == "jpork", ldf := "Pork"]
ipr_dt[ldf == "jpoul", ldf := "Poultry"]
ipr_dt[ldf == "jmilk", ldf := "Milk"]
ipr_dt[ldf == "jeggs", ldf := "Eggs"]

ipr_dt$year <- as.numeric(as.character(ipr_dt$year))
ipr_dt <- ipr_dt[year >= 2020]

# # add cty to fpu
fpu2cty_dt <- fread("rawData_fpu2cty_dt.csv")

setnames(fpu2cty_dt, c("fpu", "cty"))


# add reg
impCtyReg_dt <- readRDS(file = "procData_impCtyReg_dt.Rda")

# merge
ctyRegFpu_dt <- merge(fpu2cty_dt, impCtyReg_dt,
                      by = "cty",
                      all.x = TRUE)

iprCty_dt <- merge(ipr_dt, ctyRegFpu_dt,
                by = "fpu")

iprCty_dt <- iprCty_dt[complete.cases(iprCty_dt), ]

iprCty_dt <- iprCty_dt[, c("ldf", "year", "cty", "region", "ipr")]

iprCtyAve_dt <- iprCty_dt[,. (aveIpr = mean(ipr)),
                          by = .(ldf, cty, region)]

globIpr_dt <- iprCtyAve_dt[,. (aveIpr = mean(aveIpr),
                               minIpr = min(aveIpr),
                               maxIpr = max(aveIpr),
                               iqrIpr = IQR(aveIpr)),
             by = .(ldf)]
globIpr_dt

regIpr_dt <- iprCtyAve_dt[,. (aveIpr = round(mean(aveIpr), 2),
                               minIpr = round(min(aveIpr), 2),
                               maxIpr = round(max(aveIpr), 2),
                               iqrIpr = round(IQR(aveIpr), 2)),
                           by = .(ldf, region)]
regIpr_dt <- setorder(regIpr_dt, ldf)

# plot
ipr_plot <- ggplot(data = iprCtyAve_dt,
           aes(x = ldf,
             y = aveIpr)) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~ region, nrow = 2) +
  labs(x     = "Livestock-derived food",
       y     = "Average annual\nintrinsic productivity growth rate (%) (2020 to 2050)") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.position = "right",
    legend.text  = element_text(size = 10),
    legend.title = element_text(size = 12),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12, angle = 90),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    aspect.ratio = 1,
    panel.spacing = unit(0, "cm"))


ipr_plot

# save plot
ggsave(
  file = "output_figSI10_ipr.tiff",
  ipr_plot,
  width  = 9,
  height = 6,
  compression = "lzw"
)


# TEST KCAL
{
QDX0_afg = 139 * 1000000
QDX0_aus = 791 * 1000000
QDX0_usa = 13520 * 1000000
POPX0_afg = 27 * 1000000
POPX0_aus = 20 * 1000000
POPX0_usa = 296 * 1000000
ppDemand_afg = QDX0_afg / POPX0_afg
ppDemand_afg
ppDemand_aus = QDX0_aus / POPX0_aus
ppDemand_aus
ppDemand_usa = QDX0_usa / POPX0_usa
ppDemand_usa

# kcal per person
kcalPP_afg = 25.2 * 1000
kcalPP_aus = 140 * 1000
kcalPP_usa = 165.2 * 1000

kcalPerKg_afg = kcalPP_afg / ppDemand_afg
kcalPerKg_afg

kcalPerKg_aus = kcalPP_aus / ppDemand_aus
kcalPerKg_aus

kcalPerKg_usa = kcalPP_usa / ppDemand_usa
kcalPerKg_usa

# bovine meat 291 kcal per 100 g GENuS for USA
# so is about 2910 kcal per kg beef
# beef is 290 for 100 g
# https://www.nutritionix.com/food/beef

}

kcalPerKg_usa
