5 insights
================

``` r
library(feather)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- read_feather("../results/data.feather")
head(data)
```

    ## # A tibble: 6 × 6
    ##                               d_r_uuid                              dws
    ##                                 <fctr>                           <fctr>
    ## 1 317aad06-7ed0-4744-a578-d3e0a9657ed1 c019ab29f45a31e456dda5174791b935
    ## 2 ffe52b02-597e-4ff7-9775-14a05a1c94a8 a8ca1272252862556f7c99260633e94b
    ## 3 04b9fbf9-aca4-4dd0-b640-0860392541f0 a94cc38072ab839992b37610b491078f
    ## 4 4f0fb2c1-de88-415a-9c26-9682e1593bea ab412962926803b66b4aa518d0dae811
    ## 5 0e3ca7f9-b9fb-45a8-92a7-88abff6db958 aca0a3e7991e621d093c31c8a9c0440e
    ## 6 54491e47-0c03-47a1-b91f-f202b84e8fec ad06643bcbf7825dc68c525c552c75ff
    ## # ... with 4 more variables: dns <fctr>, so <fctr>, version <fctr>,
    ## #   license_id <int>

### 1. Most popular projects by license ID

``` r
top_projects <- data %>% 
  group_by(d_r_uuid, version, license_id) %>% 
  summarize(count= n()) %>% 
  arrange(desc(count)) 
head(top_projects)
```

    ## Source: local data frame [6 x 4]
    ## Groups: d_r_uuid, version [6]
    ## 
    ##                               d_r_uuid                      version
    ##                                 <fctr>                       <fctr>
    ## 1 3dec9166-bd9d-45d3-acae-9c78adb1a684                       2.15.2
    ## 2 23e8c6be-c7ff-4927-baf8-26bf92cfab89                      onnv_70
    ## 3 8af515ed-25f9-47ea-922b-3b95bfc3aff5              UDK2010.SR1.UP1
    ## 4 bd6e08a2-484a-4503-9d02-bd795525ddd6   v1.0.0.201112231337-helios
    ## 5 3186e8de-1fa1-4bc4-8f76-3fcdebbcea1d                        0.2.0
    ## 6 db9d602b-acf9-4bba-b6f8-3583a7f218fa REL_2.11-20151226230036+0000
    ## # ... with 2 more variables: license_id <int>, count <int>

Open source project ID 3dec9166-bd9d-45d3-acae-9c78adb1a684, version 2.15.2 with license ID 45 is the most popular. It has 7393 observations.

### 2. Most popular projects by uuid and version only

``` r
top_projects2 <- data %>% 
  group_by(d_r_uuid, version) %>% 
  summarize(count= n()) %>% 
  arrange(desc(count)) 
head(top_projects2)
```

    ## Source: local data frame [6 x 3]
    ## Groups: d_r_uuid [6]
    ## 
    ##                               d_r_uuid     version count
    ##                                 <fctr>      <fctr> <int>
    ## 1 2ff38a7c-238b-487f-af9b-64a2ec81d81c    8u72-b15 46680
    ## 2 3f28ddca-0c78-4a3a-9e9f-ead210db5368  7u85-2.6.1 32571
    ## 3 e6ec4531-9b98-4653-8f4b-5cb27326d101 6b18-1.8.13 22784
    ## 4 ca691cd3-5ded-46df-935d-2691bbf3c28d      38.5.2 19072
    ## 5 1a7d76e1-ceca-4da3-911c-c22e4368c32a       5.0.1 13794
    ## 6 590ef749-4814-4c87-80e5-0f31390a5cdf   5.0.1~rc2 13794

Open source project ID 2ff38a7c-238b-487f-af9b-64a2ec81d81c, verison 8u72-b15 (regardless of license types) is the most popular, It has 46680 observations.

### 4. Most common license ID used in this collection of open source projects

``` r
top_license <- data %>% 
  group_by(license_id) %>% 
  summarize(count= n()) %>% 
  arrange(desc(count)) 
head(top_license)
```

    ## # A tibble: 6 × 2
    ##   license_id  count
    ##        <int>  <int>
    ## 1         45 439800
    ## 2         -1 402164
    ## 3         34 388921
    ## 4         30 258647
    ## 5         11 211177
    ## 6          3 194419

License ID 45 is the most commonly used license in this collection of open source projects. It has 439800 observations.

### 3. Most popular project by uuid only

``` r
top_project3 <- data %>% 
  group_by(d_r_uuid) %>% 
  summarize(count= n()) %>% 
  arrange(desc(count)) 
head(top_project3)
```

    ## # A tibble: 6 × 2
    ##                               d_r_uuid count
    ##                                 <fctr> <int>
    ## 1 2ff38a7c-238b-487f-af9b-64a2ec81d81c 46680
    ## 2 3f28ddca-0c78-4a3a-9e9f-ead210db5368 32571
    ## 3 e6ec4531-9b98-4653-8f4b-5cb27326d101 22784
    ## 4 ca691cd3-5ded-46df-935d-2691bbf3c28d 19072
    ## 5 1a7d76e1-ceca-4da3-911c-c22e4368c32a 13794
    ## 6 590ef749-4814-4c87-80e5-0f31390a5cdf 13794

Open source project ID 2ff38a7c-238b-487f-af9b-64a2ec81d81c (regardless of verison and license ID) is the most popular, It has 46680 observations. It is the same project when sorted by uuid and version.

### 5. Number of unpopular projects

``` r
unpopular_project <- data %>% 
  group_by(d_r_uuid) %>% 
  summarize(count= n()) %>% 
  arrange() %>% 
  filter(count<=100)
count(unpopular_project)
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1 93830

There are 93830 projects (sorted by uuid) in this data set that have less than 100 observations.
