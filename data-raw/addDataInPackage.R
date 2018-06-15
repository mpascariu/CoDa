library(MortalityLaws)
library(dplyr)
library(tidyr)

cntr = 'USA'

HMD_Dx <- ReadHMD(what = 'Dx', 
                  countries = 'USA',
                  interval = '1x1',
                  username = 'your@email.com',
                  password = 'your_password',
                  save = FALSE)

years <- 1960:2014
CoDa.data <- HMD_Dx$data %>% filter(country == cntr, Year %in% years) %>% 
  select(Year:Female) %>% spread(key = Year, value = Female) %>% 
  select(-Age) %>% apply(., 2, function(x) x/sum(x))
rownames(CoDa.data) <- 0:110
head(CoDa.data)


devtools::use_data(CoDa.data)
