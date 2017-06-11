library(MortalityLaws)
library(dplyr)
library(tidyr)

cntr = 'USA'

HMD_Dx <- ReadHMD(what = 'Dx', 
                  countries = 'USA',
                  interval = '1x1',
                  username = 'email@outlook.com',
                  password = 'your_password',
                  save = FALSE)

years <- 1960:2014
edd <- HMD_Dx$data %>% filter(country == cntr, Year %in% years) %>% 
  select(Year:Female) %>% spread(key = Year, value = Female) %>% 
  select(-Age) %>% apply(., 2, function(x) x/sum(x))
rownames(edd) <- 0:110
head(edd)


devtools::use_data(edd)
