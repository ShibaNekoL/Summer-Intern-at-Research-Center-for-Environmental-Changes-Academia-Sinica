
dataframe_1980_2018 <- readRDS("dataframe_1980_2018.rds")

# C:/Users/司波達也/OneDrive/桌面 1/RCEC/st.txt
# /data1/home/hsnutardis/st.txt
# import station list
st.list.name <- read.table("C:/Users/司波達也/OneDrive/桌面 1/RCEC/st.txt", header = F, sep = " ")
names(st.list.name) <- c("stno", "stname")

st_seasonframe <- data.frame("stno" = numeric() , "stname_en" = character(), "yyyy" = numeric() , "spring" = numeric(), "summer" = numeric(), "fall" = numeric(), "winter" = numeric())
st_seasonframe_d_mean <- data.frame("stno" = numeric() , "stname_en" = character(), "yyyy" = numeric() , "spring" = numeric(), "summer" = numeric(), "fall" = numeric(), "winter" = numeric())

dataframe_1980_2018_NA <- dataframe_1980_2018
dataframe_1980_2018_NA$WS <- ifelse(dataframe_1980_2018_NA$WS <= -9990, NA, dataframe_1980_2018_NA$WS)

# select one station

for(i in 1 : nrow(st.list.name)){
  
  st <- subset(dataframe_1980_2018_NA, stno == st.list.name[i, 1])
  
  # seperate into different seasons
  
  st$yyyy <- as.integer(st$yyyy)
  st$mm <- as.integer(st$mm)
  st$dd <- as.integer(st$dd)
  st$hh <- as.integer(st$hh)
  
  st_mm3 <- subset(st, mm == 3 | mm == 4 | mm == 5)
  st_mm6 <- subset(st, mm == 6 | mm == 7 | mm == 8)
  st_mm9 <- subset(st, mm == 9 | mm == 10 | mm == 11)
  st_mm12 <- subset(st, mm == 12 | mm == 1 | mm == 2)
  
  st_season_list <- list(st_mm3, st_mm6, st_mm9, st_mm12)
  
  
  # calculate mean
  # year wbgt mean for one season
  
  # list to restore each season
  
  seasonframe <- data.frame(stno = st.list.name[i, 1], st_en = st.list.name[i, 2], yyyy = 1980 : 2018)
  
  
  # each season
  mean_yyyy_season_c <- c()
  
  for(j in 1 : length(st_season_list)){
    
    # 1980-2018 38y mean for one season
    mean_yyyy_season <- mean(st_season_list[[j]] $ WS, na.rm = T)
    mean_yyyy_season_c[j] <- mean_yyyy_season
    
    # each year mean 1980-2018
    
    mean_yyyy_c <- c()
    
    for(k in 1980 : 2018){
      # one year mean
      st_yyyy <- subset(st_season_list[[j]], yyyy == k)
      mean_yyyy_c[k - 1979] <- mean(st_yyyy $ WS, na.rm = T)
    }
    seasonframe <- cbind(seasonframe, mean_yyyy_c)
  }
  
  names(seasonframe) <- c("stno", "stname", "yyyy", "spring", "summer", "fall", "winter")
  
  st_seasonframe <- rbind(st_seasonframe, seasonframe)
  
  seasonframe_d_mean <-  seasonframe
  for(j in 1 : length(st_season_list)){
    seasonframe_d_mean [, j + 3] <- seasonframe_d_mean[, j + 3] - mean_yyyy_season_c[j]
  }
  st_seasonframe_d_mean <- rbind(st_seasonframe_d_mean, seasonframe_d_mean)
}


saveRDS(st_seasonframe_d_mean, "st_seasonframe_d_mean.rds")
readRDS(st_seasonframe_d_mean, "st_seasonframe_d_mean.rds")


# install.packages("igraph")
# library(igraph)



library(ggplot2)
library(digest)

# png("Combination of Plots.png", width=600, height=600)

ggplot() +  
  
  geom_line(data = st_seasonframe_d_mean, aes(x = yyyy, y = summer, colour = stname, group = stname), size = 1) + 
  geom_point(data = st_seasonframe_d_mean, aes(x = yyyy, y = summer, colour = stname,group = stname), size = 2) +
  xlab("year") + ylab("WS") + ggtitle("Summer yearly change of WS of each station for 1980-2018") +
  theme()



# dev.off()

