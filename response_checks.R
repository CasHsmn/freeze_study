# Freeze pilot

library(data.table)
library(dplyr)
library(careless)
library(car)

wd <- list()
wd$data <-"C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study4/data/"
wd$output <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study4/output/"

raw <- fread(paste0(wd$data, "freeze_pilot_2+May+2025_07.25.csv"))

df <- raw %>% 
  filter(Status == "IP Address" & Finished == "True" & Q20 == "I agree")

att_check <- df %>% 
  filter(cap_8 != "Strongly disagree")

careless_subset <- df %>% 
  select(cap_1:mot_6, -cap_8) %>% 
  slice(-c(1:2)) %>% 
  mutate_all(as.factor) %>% 
  mutate_all(as.integer)

boxplot.stats(irv(careless_subset))$out
Boxplot(longstring(careless_subset))

boxplot.stats(longstring(careless_subset))

cor(careless_subset[,1:7])
cor(careless_subset[,8:11])
cor(careless_subset[,12:17])

table(df$meat_freq)
