library(feather)
library(tidyverse)
library(mclust)
library(cluster)
data <- read_feather("results/data.feather")
head(data)
pjt_by_lic <- data %>% 
  group_by(d_r_uuid) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  View()

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

# Hatice

data_unique <- data %>% unique() 

data_counts<- data %>% 
  group_by(d_r_uuid, version, license_id) %>% 
  mutate(license_id_c = n())%>%  
  ungroup() %>% 
  group_by(d_r_uuid, version, dns) %>% 
  mutate(dns_c=n()) %>% 
  ungroup() %>% 
  group_by(d_r_uuid, version, dws) %>% 
  mutate(dws_c=n()) %>% 
  ungroup() %>% 
  group_by(d_r_uuid, version, so) %>% 
  mutate(so_c=n()) %>% 
  ungroup() %>% 
  select(d_r_uuid,version, dns_c, dws_c, so_c, license_id_c)
  
data_small <- data_counts[1:500, 3:6]

# plot(data_small$so_c,data_small$license_id_c)


# cov_matrix <- cov(data_small)
# eigen_vectors <- eigen(cov_matrix)$vectors
# eigen_values <- eigen(cov_matrix)$values
# eigen_values[1]/sum(eigen_values)
# 
# y <- data.frame(pc1=as.matrix(data_small) %*% eigen_vectors[,1], pc2=as.matrix(data_small) %*% eigen_vectors[,2])
# 
# ggplot(y, aes(x=pc1,y=pc2)) + geom_jitter()

# center the data

data_small_c <- scale(data_small, center = TRUE)
cov_matrix_c <- cov(data_small_c)
eigen_vectors_c <- eigen(cov_matrix_c)$vectors
eigen_values_c <- eigen(cov_matrix_c)$values
(eigen_values_c[1] +eigen_values_c[2])/sum(eigen_values_c)

y_c <- data.frame(pc1_c=as.matrix(data_small_c) %*% eigen_vectors_c[,1], pc2_c=as.matrix(data_small_c) %*% eigen_vectors_c[,2])

ggplot(y_c, aes(x=pc1_c,y=pc2_c)) + geom_jitter()

# Factor1 explains dws_c and so_c  
fact_analysis <- factanal(data_small_c, factor=1, rotation = "varimax")
fact_analysis$loadings

# Plotting dws_c vs license_id_c

ggplot(as.data.frame(data_small), aes(x=license_id_c,y=dws_c)) + geom_jitter(alpha = .1,size = 2) + ylim(1,10)

# Plotting dns_c vs license_id_c

ggplot(as.data.frame(data_small), aes(x=license_id_c,y=dns_c)) + geom_jitter() 

# cluster analyis

m_clust <- Mclust(data_small)
clusters <- m_clust$classification
head(clusters)
table(clusters)

# kmeans (confirming two clusters)

d <- dist(data_small, method = "euclidean")
a <- pam(d, k = 2)
plot(a)

data_small$cluster <- m_clust$classification

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
