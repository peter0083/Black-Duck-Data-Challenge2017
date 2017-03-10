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

pjt_by_uuid <- data %>% 
  group_by(d_r_uuid) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

pjt_by_so %>% 
  ggplot(aes(x = count))+
  geom_histogram()

pjt_by_dns %>% 
  ggplot(aes(x = count))+
  geom_histogram()

pjt_by_dws %>% 
  ggplot(aes(x = count))+
  geom_histogram()

pjt_by_uuid %>% 
  ggplot(aes(x = count))+
  geom_histogram()

pjt_by_lic %>% 
  ggplot(aes(x = count))+
  geom_histogram()

data %>% 
  ggplot(aes(x = license_id))+geom_bar()

top_lic <- left_join(head(pjt_by_lic,100), data)

top_uuid <- data %>% 
  filter(d_r_uuid %in% pjt_by_uuid$d_r_uuid) %>% 

top_dns <- data %>% 
  arrange(dns) %>% 
  head(100)

top_dws <- data %>% 
  arrange(dws) %>% 
  head(100)

top_so <- data %>% 
  arrange(so) %>% 
  head(100)
