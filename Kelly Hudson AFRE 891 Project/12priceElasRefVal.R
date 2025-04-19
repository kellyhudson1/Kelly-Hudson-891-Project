# HEADER========================================================================
# Title: priceElasRefVal
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: generate table of price elasticties of demand for each ldf by cty

# LIBRARIES=====================================================================
library("data.table")
library("ggplot2")

# disable scientific notation
options(scipen = 999)

# LOAD & ORG DATA===============================================================
# load all price elasticities, they are the same in all scenarios
priceElas_dt <- fread("rawData_priceElas_dt.csv")


# elasticity to 2 dp
priceElas_dt$FDELASH <- round(priceElas_dt$FDELASH, 2)


# keep 'urban' and ldf commodities
# keep when C and CC both the same as this is the own-price elasticity
priceElas_dt <- priceElas_dt[(H == "Urban") &
                             (C == "cbeef" & CC == "cbeef" |
                              C == "ceggs" & CC == "ceggs" |
                              C == "clamb" & CC == "clamb" |
                              C == "cmilk" & CC == "cmilk" |
                              C == "cpork" & CC == "cpork" |
                              C == "cpoul" & CC == "cpoul"), ]
# keep columns and rename
priceElas_dt <- priceElas_dt[, c('C', 'CTY', 'FDELASH')]
colnames(priceElas_dt) <- c('c', 'cty', 'priceElas')


# add LDF NAMES
# rename ldf to match IMPACT names
priceElas_dt[c == "cbeef", ldf := 'Beef']
priceElas_dt[c == "ceggs", ldf := 'Eggs']
priceElas_dt[c == "clamb", ldf := 'Sheep']
priceElas_dt[c == "cmilk", ldf := 'Milk']
priceElas_dt[c == "cpork", ldf := 'Pork']
priceElas_dt[c == "cpoul", ldf := 'Poultry']


# ldf as ordered factor 
priceElas_dt$ldf <-
  factor(
    priceElas_dt$ldf,
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

priceElas_dt

# add region
impCtyReg_dt <- readRDS(file = "procData_impCtyReg_dt.Rda")

# merge
priceElas_dt <- merge(priceElas_dt, impCtyReg_dt,
                      by = "cty",
                      all.x = TRUE)

# PLOT====

priceElas_plot <- ggplot(data = priceElas_dt,
                      aes(x = ldf,
                          y = priceElas)) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~ region, nrow = 2) +
  labs(x     = "Livestock-derived food",
       y     = "Price elasticity of demand") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    aspect.ratio = 1,
    panel.spacing = unit(0, "cm"))


# save pop plot
ggsave(
  file = "output_figSI3_priceElasReg.tiff",
  priceElas_plot,
  width  = 9,
  height = 6,
  compression = "lzw"
)