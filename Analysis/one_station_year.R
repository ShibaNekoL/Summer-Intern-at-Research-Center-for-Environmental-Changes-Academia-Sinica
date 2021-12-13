# dataframe_1980_2018 <- readRDS("dataframe_1980_2018.rds")

dataframe_1980_2018_NA <- dataframe_1980_2018
dataframe_1980_2018_NA$ WBGT<- ifelse(dataframe_1980_2018_NA$ WBGT<= -9990, NA, dataframe_1980_2018_NA$ WBGT)

stnumber <- 467590



## select taipei station
st_taipei <- subset(dataframe_1980_2018_NA, stno == stnumber)

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

# calculate mean
# year wbgt mean for one season

# list to restore each season

seasonframe <- data.frame(stno = stnumber, st_en = "TAIPEI", yyyy = 1980 : 2018)

# each season
mean_yyyy_season_c <- c()

for(j in 1 : length(st_taipei_season_list)){
  
  # 1980-2018 38y mean for one season
  mean_yyyy_season <- mean(st_taipei_season_list[[j]] $ WBGT, na.rm = T)
  mean_yyyy_season_c[j] <- mean_yyyy_season
  
  # each year mean 1980-2018
  
  mean_yyyy_c <- c()
  
  for(k in 1980 : 2018){
    # one year mean
    st_yyyy <- subset(st_taipei_season_list[[j]], yyyy == k)
    mean_yyyy_c[k - 1979] <- mean(st_yyyy $ WBGT, na.rm = T)
  }
  seasonframe <- cbind(seasonframe, mean_yyyy_c)
}


## 距平
names(seasonframe) <- c("stno", "stname", "yyyy", "spring", "summer", "fall", "winter")

seasonframe_d_mean <-  seasonframe
for(j in 1 : length(st_taipei_season_list)){
  seasonframe_d_mean [, j + 3] <- seasonframe_d_mean[, j + 3] - mean_yyyy_season_c[j]
}

# st_seasonframe_d_mean <- rbind(seasonframe_d_mean, seasonframe_d_mean)



ggplot(data = seasonframe_d_mean) +  
  
  geom_line(aes(x = yyyy, y = summer), size = 1) + 
  geom_point(aes(x = yyyy, y = summer), size = 2) + 
  
  geom_smooth(aes(x = yyyy, y = summer) , size = 1) +
  # geom_hline(aes(yintercept = 0)) + 
  geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab(" WBGT") + ggtitle("Summer yearly change of WBGT \n at 恆春 for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 40),
        axis.text.y = element_text(vjust = 0.5, size = 40),
        axis.title = element_text(hjust = 0.5, size = 40),
        plot.title = element_text(hjust = 0.5, size = 40)
        ) # 將title置中


