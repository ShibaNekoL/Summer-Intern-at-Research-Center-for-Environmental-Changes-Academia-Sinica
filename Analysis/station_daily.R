
dataframe_1980_2018 <- readRDS("dataframe_1980_2018.rds")

# import station list
st.list.name <- read.table("C:/Users/司波達也/OneDrive/桌面 1/RCEC/st.txt", header = F, sep = " ")
names(st.list.name) <- c("stno", "stname_en", "stname_zh")

st_seasonframe <- data.frame("stno" = numeric , "stname" = str,"hh" = numeric , "spring" = numeric, "summer" = numeric, "fall" = numeric, "winter" = numeric)


# select one station

for(i in 1 : nrow(st.list.name)){
  
  
  st <- subset(dataframe_1980_2018, stno == st.list.name[i, 1])
  
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
  # hourly wbgt mean per day for one season
  
  # list to restore each season
  mean_season_hh <- list()
  seasonframe <- data.frame(stno = st.list.name[i, 1], stname = st.list.name[i, 3], hh = 1 : 24)
  
  # each season
  for(j in 1 : length(st_season_list)){
    
    mean_hh_list <- c()
    
    # each hour
    for(k in 1 : 24){
      st_hh <- subset(st_season_list[[j]], hh == k)
      mean_hh_list[k] <- mean(st_hh $ WBGT, na.rm = T)
    }
    
    seasonframe <- cbind(seasonframe, mean_hh_list)
    mean_season_hh[[j]] <- mean_hh_list
  }
  
  names(mean_season_hh) <- c("spring", "summer", "fall", "winter")
  names(seasonframe) <- c("stno", "stname","hh", "spring", "summer", "fall", "winter")
  
  st_seasonframe <- rbind(st_seasonframe, seasonframe)
  
}

saveRDS(st_seasonframe, "st_seasonframe.rds")



### ggplot
st_seasonframe <- readRDS("st_seasonframe.rds")


library(ggplot2)
library(digest)
 
# png("Combination of Plots.png", width=600, height=600)

ggplot() +  
  
  geom_line(data = st_seasonframe, aes(x = hh, y = fall, colour = stname, group = stname), size = 1) + 
  geom_point(data = st_seasonframe, aes(x = hh, y = fall, colour = stname,group = stname), size = 2) +
  xlab("hour") + ylab("WBGT") + ggtitle("Fall daily change of WBGT of each station for 1980-2018") +
  theme()



# dev.off()