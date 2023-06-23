source("renv/activate.R")
library(ggplot2)

# Theme für plots:
# https://bookdown.org/content/2015/figures.html
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.title=element_blank(),
        axis.text.y=element_text(size = 12),
        axis.text.x=element_text(size = 12))


path_data_raw <- here::here("data/raw", "datashare.csv")
path_ret_raw <- here::here("data/raw", "ret.csv")
path_bond_raw <- here::here("data/raw", "tb3ms.csv")

path_data_premium_with_na <- here::here("data/processed", 
                                        "data_premium_with_na.csv")
path_data_premium_omitted <- here::here("data/processed", 
                                        "data_premium_omitted.csv")

#rm(list = ls())

