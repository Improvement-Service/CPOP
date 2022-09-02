###Calculate the rankings and deciles for income deprivation 

#Run this before PrepDecileData.R
setwd("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP")

library(readxl)
library(dplyr)
library(readr)
detach(package:plyr)
##read dataset
DZdta <- read_excel("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/DZ & IGZ income data.xlsx", sheet = 2)
IGZdta <- read_excel("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/DZ & IGZ income data.xlsx", sheet = 3)[-4]
names(IGZdta)[5] <- "CPP"
##get count of dzs, get ranks and deciles
DZdta$count <- ave(DZdta$Council_area, DZdta$Council_area, FUN = length)
DZdta <- DZdta %>% group_by(Council_area) %>%
  mutate(decl = ntile(`%`, 10))%>% ungroup()

#just double check IGZ ranks
IGZdta$count <- ave(IGZdta$CPP, IGZdta$CPP, FUN = length)
IGZdta<- IGZdta %>% dplyr::group_by(CPP) %>%
  mutate(decl = ntile(`% of income deprived`, 10)) %>% ungroup()
sum(IGZdta$`Decile with CPP` != IGZdta$decl)
#so 345 don't match - which ones?
noMatch <- IGZdta[IGZdta$decl != IGZdta$`Decile with CPP`,]

##most mismatches are to do with rounding, but some issues e.g. Eilean SIar has
#no decile 1 in previous figs, so will keep new deciles
##Need to make decile 9 for isles into decile 10 as they have below 10 IGZs
IGZdta[IGZdta$CPP== "Shetland Islands"&IGZdta$decl== 7, "decl"] <- 10
IGZdta[IGZdta$CPP== "Orkney Islands"&IGZdta$decl== 6, "decl"] <- 10
IGZdta[IGZdta$CPP== "Eilean Siar"&IGZdta$decl== 9, "decl"] <- 10

##Get Scotland deciles
IGZdta <- IGZdta %>% mutate(ScotDecl = ntile(`% of income deprived`, 10))
DZdta <- DZdta %>% mutate(ScotDecl = ntile(`%`,10))

IGZDeciles <- IGZdta[c(1,5,10,11)]
colnames(IGZDeciles) <- c("AreaCode", "CouncilName", "Decile", "ScotlandDec")
DZDeciles <- DZdta[c(1,2,8,9)]
colnames(DZDeciles) <- c("AreaCode", "CouncilName", "Decile", "ScotlandDec")

#merge and save
allDta <- rbind(IGZDeciles[1:4],DZDeciles[1:4])
write_csv(allDta, "C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/IncomeDeciles.csv")
