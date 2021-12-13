## select taipei station
st_taipei <- subset(dataframe_1980_2018, stno == 466920)

## seperate into different seasons

st_taipei$yyyy <- as.integer(st_taipei$yyyy)
st_taipei$mm <- as.integer(st_taipei$mm)
st_taipei$dd <- as.integer(st_taipei$dd)
st_taipei$hh <- as.integer(st_taipei$hh)

st_taipei_mm3 <- subset(st_taipei, mm == 3 | mm == 4 | mm == 5)
st_taipei_mm6 <- subset(st_taipei, mm == 6 | mm == 7 | mm == 8)
st_taipei_mm9 <- subset(st_taipei, mm == 9 | mm == 10 | mm == 11)
st_taipei_mm12 <- subset(st_taipei, mm == 12 | mm == 1 | mm == 2)

st_taipei_season_list <- list(st_taipei_mm3, st_taipei_mm6, st_taipei_mm9, st_taipei_mm12)

## calculate mean
# hourly wbgt mean per day for one season

# list to restore each season
mean_season_hh <- list()
seasonframe_taipei <- data.frame(hh = 1 : 24)

for(j in 1 : length(st_taipei_season_list)){
  
  mean_hh_list <- c()
  
  for(i in 1 : 24){
    st_taipei_hh <- subset(st_taipei_season_list[[j]], hh == i)
    mean_hh_list[i] <- mean(st_taipei_hh $ WBGT, na.rm = T)
  }
  
  seasonframe_taipei <- cbind(seasonframe_taipei, mean_hh_list)
  mean_season_hh[[j]] <- mean_hh_list
}

names(mean_season_hh) <- c("spring", "summer", "fall", "winter")
names(seasonframe_taipei) <- c("hh", "spring", "summer", "fall", "winter")

saveRDS(seasonframe_taipei, "seasonframe_taipei.rds")