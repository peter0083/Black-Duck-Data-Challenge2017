library(feather)
library(tidyverse)
data <- read.csv("data/ubc_data_workshop.csv",sep = ";")
data <- unique(data) %>% 
  droplevels()

write_feather(data, "results/data.feather")

  