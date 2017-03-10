library(feather)
library(tidyverse)
data <- read_feather("results/data.feather")
pjt_by_lic <- data %>% 
  group_by(license_id) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

pjt_by_dns <- data %>% 
  group_by(dns) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

pjt_by_dws <- data %>% 
  group_by(dws) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

pjt_by_so <- data %>% 
  group_by(so) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))



data %>% 
  ggplot(aes(x = license_id))+
  geom_bar()
