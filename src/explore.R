library(feather)
library(tidyverse)

data <- read_feather("results/data.feather")
head(data)

top_projects <- data %>% 
  group_by(d_r_uuid, version, license_id) %>% 
  summarize(count= n()) %>% 
  arrange(desc(count)) 
head(top_projects)

top_projects2 <- data %>% 
  group_by(d_r_uuid, version) %>% 
  summarize(count= n()) %>% 
  arrange(desc(count)) 
head(top_projects2)

top_license <- data %>% 
  group_by(license_id) %>% 
  summarize(count= n()) %>% 
  arrange(desc(count)) 
head(top_license)

top_project3 <- data %>% 
  group_by(d_r_uuid) %>% 
  summarize(count= n()) %>% 
  arrange(desc(count)) 
head(top_project3)

unpopular_project <- data %>% 
  group_by(d_r_uuid) %>% 
  summarize(count= n()) %>% 
  arrange() %>% 
  filter(count<=100)
count(unpopular_project)
