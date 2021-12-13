### Data cleaning
load("wbgt.RData")

## cut 1980-2018
DATA_list_1980_2018 <- DATA_list[ - (1 : ((1979 - 1910) * 12)) ]

## rbind each element in list to a dataframe
dataframe_1980_2018 <- DATA_list_1980_2018[[1]]
for(i in 2 : length(DATA_list_1980_2018)){
  dataframe_1980_2018 <- rbind(dataframe_1980_2018, DATA_list_1980_2018[[i]])
}

## cut sting of yyyymmddhh
dataframe_1980_2018$yyyy <- substr(dataframe_1980_2018$yyyymmddhh, 1, 4)
dataframe_1980_2018$mm <- substr(dataframe_1980_2018$yyyymmddhh, 5, 6)
dataframe_1980_2018$dd <- substr(dataframe_1980_2018$yyyymmddhh, 7, 8)
dataframe_1980_2018$hh <- substr(dataframe_1980_2018$yyyymmddhh, 9, 10)


## NA
dataframe_1980_2018_NA <- dataframe_1980_2018

dataframe_1980_2018_NA $ Ta <- ifelse(dataframe_1980_2018_NA $ Ta <= -9990, NA, dataframe_1980_2018_NA $ Ta)
dataframe_1980_2018_NA $ RH <- ifelse(dataframe_1980_2018_NA $ RH <= -9990, NA, dataframe_1980_2018_NA $ RH)
dataframe_1980_2018_NA $ SR <- ifelse(dataframe_1980_2018_NA $ SR <= -9990, NA, dataframe_1980_2018_NA $ SR)
dataframe_1980_2018_NA $ WS <- ifelse(dataframe_1980_2018_NA $ WS <= -9990, NA, dataframe_1980_2018_NA $ WS)

saveRDS(dataframe_1980_2018_NA, "dataframe_1980_2018_NA.rds")
