library(tidyverse)

setwd("C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP/Duncan_Index")
##  Step 1) Since we are repeating the same code again and again to calculate 
##  the duncan index we will define our function for doing it to save time

dindex <- function(x, y, sort.var){
  # sort.var is the ranking variable (i.e. 1 = most deprived)
  # x is name of col giving the number of non income deprived
  # y is name of col giving the number of income deprived
  
  ##  First check for completely or mostly missing cols and stops function (returns NA)
  count.na <- sort.var %>% is.na %>% sum
  prop.na <- count.na / length(sort.var)
  if(prop.na > 0.2){
    return(NA)}
  
  ##  Second the calculation
  k <- sort.var %>% order #save rank number of soring variable (lo to high) 
  N <- length(k) #how long is the data col
  x <- x[k] # saving reordered x col 
  y <- y[k] # saving reordered y col
  
  ## Calculate cumulative proportions
  a <- cumsum(y) / sum(y)
  b <- cumsum(x) / sum(x)
  
  ##  calculate duncan index
  output <- sum(a[1:(N - 1)] * b[2:N]) - sum(a[2:N] * b[1:(N - 1)]) #Exactly as the paper formula
  
  return(output)  
}

##  Step 2) Now we need to get all the data that we need from various files
sco14a <- read.csv('DIData/2014 IGZ1.csv')
sco15a <- read.csv('DIData/2015 IGZ1.csv')
sco16a <- read.csv('DIData/2016 IGZ1.csv')
sco17a <- read.csv('DIData/2017 IGZ1.csv')
sco18a <- read.csv('DIData/2018 IGZ1.csv')
sco19a <- read.csv('DIData/2019 IGZ1.csv')
sco20a <- read.csv('DIData/2020 IGZ1.csv')
sco21a <- read.csv('DIData/2021 IGZ1.csv')
sco22a <- read.csv('DIData/2022 IGZ1.csv')
sco23a <- read.csv('DIData/2023 IGZ1.csv')
sco24a <- read.csv('DIData/2024 IGZ1.csv')

combined.tab <- rbind(sco14a, sco15a, sco16a, sco17a,sco18a,sco19a, sco20a, sco21a, sco22a, sco23a,sco24a)

##  Step 3) Calculate the duncan index for every combination of year and ttwa / la

##  LA
la.tab <- combined.tab %>%
  group_by(year, la) %>% ## we want to calculate stats by year, ttwa and country
  summarise(total.pop = sum(pop), #name of summary statistic and how its calculated; i.e total.pop is sum of pop 
            crime = dindex(x = noninc.n, y = inc.n, sort.var = crime),
            CP = dindex(x = noninc.n, y = inc.n, sort.var = CP),
            Attain = dindex(x = noninc.n, y = inc.n, sort.var = Attain),
            PD = dindex(x = noninc.n, y = inc.n, sort.var = PD),
            EA = dindex(x = noninc.n, y = inc.n, sort.var = EA),
            Depop = dindex(x = noninc.n, y = inc.n, sort.var = Depop),
            EM = dindex(x = noninc.n, y = inc.n, sort.var = EM)
  )

la.tab
la.tab %>% write.csv('DIData/Duncan index by FINALOct26.csv')

##  End
