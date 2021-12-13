frame.st_1980_2018_naomit <- readRDS("frame.st_1980_2018_naomit.rds")
# data <- frame.st_1980_2018[order(frame.st_1980_2018 $ stno, frame.st_1980_2018 $ yyyymmddhh), ]
data <- frame.st_1980_2018_naomit


## different type of station 
# city
data$sttype <- "others"
data$sttype <- ifelse(data$stno_en == "TAIPEI" | 
                 data$stno_en == "BANQIAO"  |
                 data$stno_en == "TAICHUNG"  |
                 data$stno_en == "TAINAN"  |
                 data$stno_en == "KAOHSIUNG", "city", data$sttype
               )
                 
# mountain
data$sttype <- ifelse(data$stno_en == "ANBU" | 
                        data$stno_en == "ZHUZIHU"  |
                        data$stno_en == "ALISHAN"  |
                        data$stno_en == "YUSHAN"  | 
                        data$stno_en == "SUN MOON LAKE", "mountain", data$sttype
)

# island
data$sttype <- ifelse(data$stno_en == "PENGJIAYU" |
                        data$stno_en == "DONGJIDAO" |
                        data$stno_en == "PENGHU"  |
                        data$stno_en == "LANYU", "island", data$sttype
)


# install.packages("ggplot2")
# install.packages("digest")


library(ggplot2)
library(digest)



### 離島 四季 WBGT 日變化 1980-2018

# create statistic frame
frame.data <- aggregate(data, by = list(data$sttype, data$season, data$hh), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

frame.clean <- subset(frame.data, Group.1 == "island")
names(frame.clean)[2] <- "Season"
names(frame.clean)[3] <- "HH"

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.clean) +  
  
  geom_line(aes(x = HH, y = WBGT, colour = Season, group = Season), size = 1) + 
  geom_point(aes(x = HH, y = WBGT, colour = Season, group = Season), size = 2) +
  # geom_smooth(aes(x = yyyy, y = summer) , size = 1) +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("hour") + ylab("WBGT") + ggtitle("hourly change of WBGT at island \n for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 40),
        axis.text.y = element_text(vjust = 0.5, size = 40),
        axis.title = element_text(hjust = 0.5, size = 40),
        plot.title = element_text(hjust = 0.5, size = 40), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  ) 

# dev.off()



### 離島 四季 WBGT 距平 日變化 1980-2018

# create statistic frame
frame.data <- aggregate(data, by = list(data$sttype, data$season, data$hh), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)
frame.mean <- aggregate(data, by = list(data$sttype, data$season), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)


frame.data.clean <- subset(frame.data, Group.1 == "island")
frame.mean.clean <- subset(frame.data, Group.1 == "island")



names(frame.data.clean)[3] <- "HH"

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data.clean) +  
  
  geom_line(aes(x = HH, y = WBGT, colour = Season, group = Season), size = 1) + 
  geom_point(aes(x = HH, y = WBGT, colour = Season, group = Season), size = 2) +
  # geom_smooth(aes(x = yyyy, y = summer) , size = 1) +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("hour") + ylab("WBGT") + ggtitle("hourly change of WBGT at island \n for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 40),
        axis.text.y = element_text(vjust = 0.5, size = 40),
        axis.title = element_text(hjust = 0.5, size = 40),
        plot.title = element_text(hjust = 0.5, size = 40), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  ) 

# dev.off()



### 城市、山區、海島 WBGT 年際變化 1980-2018

# create statistic frame
frame.data <- aggregate(data, by = list(data$sttype, data$yyyy), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)


names(frame.data)[1] <- "Station_type"
names(frame.data)[2] <- "YYYY"

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = YYYY, y = WBGT, colour = Station_type, group = Station_type), size = 1) + 
  geom_point(aes(x = YYYY, y = WBGT, colour = Station_type, group = Station_type), size = 2) +
  geom_smooth(aes(x = YYYY, y = WBGT, group = Station_type) , size = 1, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 2000)) +
  
  xlab("Year") + ylab("WBGT") + ggtitle("yearly change of WBGT \n for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 40),
        axis.text.y = element_text(vjust = 0.5, size = 40),
        axis.title = element_text(hjust = 0.5, size = 40),
        plot.title = element_text(hjust = 0.5, size = 40), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  ) 

# dev.off()




### 城市 四季 WBGT距平 年際變化 1980-2018

# create statistic frame
frame.data <- aggregate(data, by = list(data$sttype, data$yyyy, data$season), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)
frame.mean <- aggregate(data, by = list(data$sttype, data$season), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)


names(frame.data)[1] <- "Station_type"
names(frame.data)[2] <- "YYYY"
names(frame.data)[3] <- "Season"

names(frame.mean)[1] <- "Station_type"
names(frame.mean)[2] <- "Season"

frame.data.clean <- subset(frame.data, Station_type == "island")

frame.mean.clean <- subset(frame.mean, Station_type == "island", select = c(Season, WBGT))

frame.data.clean <- merge(frame.data.clean, frame.mean.clean, by = "Season")
frame.data.clean$WBGT.d <- frame.data.clean$WBGT.x - frame.data.clean$WBGT.y


# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data.clean) +  
  
  geom_line(aes(x = YYYY, y = WBGT.d, colour = Season, group = Season), size = 1) + 
  geom_point(aes(x = YYYY, y = WBGT.d, colour = Season, group = Season), size = 2) +
  geom_smooth(aes(x = YYYY, y = WBGT.d, colour = Season, group = Season) , size = 2, se = F) +
  
  geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("Year") + ylab("WBGT-mean") + ggtitle("yearly change of WBGT-mean \n at island for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 40),
        axis.text.y = element_text(vjust = 0.5, size = 40),
        axis.title = element_text(hjust = 0.5, size = 40),
        plot.title = element_text(hjust = 0.5, size = 40), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  ) 

# dev.off()


### 城市、山區、離島 WBGT距平 年變化 1980-2018

# create statistic frame
frame.data <- aggregate(data, by = list(data$sttype, data$mm), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)
frame.mean <- aggregate(data, by = list(data$sttype), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)


names(frame.data)[1] <- "Station_type"
names(frame.data)[2] <- "MM"

names(frame.mean)[1] <- "Station_type"

frame.data.clean <- subset(frame.data)

frame.mean.clean <- subset(frame.mean, select = c(Station_type, WBGT))

frame.data.clean <- merge(frame.data.clean, frame.mean.clean, by = "Station_type")
frame.data.clean$WBGT.d <- frame.data.clean$WBGT.x - frame.data.clean$WBGT.y
frame.data.clean$MM <- as.factor(frame.data.clean$MM)

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data.clean) +  
 
  geom_line(aes(x = MM, y = WBGT.d, colour = Station_type, group = Station_type), size = 1) + 
  geom_point(aes(x = MM, y = WBGT.d, colour = Station_type, group = Station_type), size = 2) + 
  # geom_smooth(aes(x = MM, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  
  
  geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("month") + ylab("WBGT-mean") + ggtitle("Monthly Change of WBGT-mean \n at city for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 40),
        axis.text.y = element_text(vjust = 0.5, size = 40),
        axis.title = element_text(hjust = 0.5, size = 40),
        plot.title = element_text(hjust = 0.5, size = 40), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  ) 

# dev.off()




### 城市、山區、離島 WBGT 年變化 1980-2018

# create statistic frame
frame.data <- aggregate(data, by = list(data$sttype, data$mm), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "Station_type"
names(frame.data)[2] <- "MM"


frame.data.clean <- subset(frame.data)

frame.data.clean$MM <- as.factor(frame.data.clean$MM)

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data.clean) +  
  
  geom_line(aes(x = MM, y = WBGT, colour = Station_type, group = Station_type), size = 1) + 
  geom_point(aes(x = MM, y = WBGT, colour = Station_type, group = Station_type), size = 2) + 
  # geom_smooth(aes(x = MM, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("month") + ylab("WBGT-mean") + ggtitle("Monthly Change of WBGT \n at city for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()



### 各類型測站 四季 12時 WBGT 年際變化 1980-2018

# create statistic frame
frame.data <- aggregate(data, by = list(data$sttype, data$yyyy, data$hh, data$season), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "Station_type"
names(frame.data)[2] <- "YYYY"
names(frame.data)[3] <- "HH"
names(frame.data)[4] <- "Season"

frame.data.clean <- subset(frame.data, HH == 12 & Season == "SON")

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data.clean) +  
  
  geom_line(aes(x = YYYY, y = WBGT, colour = Station_type, group = Station_type), size = 1) + 
  geom_point(aes(x = YYYY, y = WBGT, colour = Station_type, group = Station_type), size = 2) + 
  geom_smooth(aes(x = YYYY, y = WBGT, colour = Station_type, group = Station_type) , size = 1, se = T) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("Year") + ylab("SR") + ggtitle("Yearly Change of SR in 12pm \n in JJA for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()


### 各類型測站 四季 12時 SR 年際變化 1980-2018

# create statistic frame
frame.data <- aggregate(data, by = list(data$sttype, data$yyyy, data$hh, data$season), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "Station_type"
names(frame.data)[2] <- "YYYY"
names(frame.data)[3] <- "HH"
names(frame.data)[4] <- "Season"

frame.data.clean <- subset(frame.data, HH == 12 & Season == "DJF")

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data.clean) +  
  
  geom_line(aes(x = YYYY, y = SR, colour = Station_type, group = Station_type), size = 1) + 
  geom_point(aes(x = YYYY, y = SR, colour = Station_type, group = Station_type), size = 2) + 
  geom_smooth(aes(x = YYYY, y = SR, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("Year") + ylab("SR") + ggtitle("Yearly Change of SR in 12pm \n in DJF for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()


#### WBGT YEARLY
frame.data <- aggregate(data, by = list(data$yyyy), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"



# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = YYYY, y = WBGT, colour = "WBGT"), size = 1) + 
  geom_point(aes(x = YYYY, y = WBGT, colour = "WBGT"), size = 2) + 
  
  geom_line(aes(x = YYYY, y = SR, colour = "SR"), size = 1) + 
  geom_point(aes(x = YYYY, y = SR, colour = "SR"), size = 2) + 
  
  geom_line(aes(x = YYYY, y = RH, colour = "RH"), size = 1) + 
  geom_point(aes(x = YYYY, y = RH, colour = "RH"), size = 2) + 
  
  geom_line(aes(x = YYYY, y = Ta, colour = "Ta"), size = 1) + 
  geom_point(aes(x = YYYY, y = Ta, colour = "Ta"), size = 2) + 
  
  geom_line(aes(x = YYYY, y = WS, colour = "WS"), size = 1) + 
  geom_point(aes(x = YYYY, y = WS, colour = "WS"), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +

  xlab("Month") + ylab("WBGT(℃)") + ggtitle("Yearly Change of WBGT") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  ) 


#### WBGT YEARLY ALL STATION
frame.data <- aggregate(data, by = list(data$yyyy, data$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"
names(frame.data)[2] <- "STATION"

frame_table <- levels(data$stno_en)
stno_en_levels <- frame_table[order(data$asl_m)]
stno_en_levels <- stno_en_levels[!NA]
frame.data$stno_en2 <- factor(frame.data$stno_en, levels = stno_en_levels)
frame.clean <- subset(frame.data, stno_en2 != "HSINCHU") # 新竹好像讀不到資料

# png("Combination of Plots.png", width=600, height=600)
#####
ggplot(data = frame.data) +  
  
  # geom_line(aes(x = YYYY, y = WBGT, colour = STATION, group = STATION), size = 1) + 
  # geom_point(aes(x = YYYY, y = WBGT, colour = STATION, group = STATION), size = 2) + 
  # 
  geom_line(aes(x = YYYY, y = SR * 1000000 / ( 60 * 60 ), colour = STATION, group = STATION), size = 1) +
  geom_point(aes(x = YYYY, y = SR * 1000000 / ( 60 * 60 ), colour = STATION, group = STATION), size = 2) +

  # geom_line(aes(x = YYYY, y = RH, colour = "RH"), size = 1) + 
  # geom_point(aes(x = YYYY, y = RH, colour = "RH"), size = 2) + 
  # 
  # geom_line(aes(x = YYYY, y = Ta, colour = "Ta"), size = 1) + 
  # geom_point(aes(x = YYYY, y = Ta, colour = "Ta"), size = 2) + 
  # 
  # geom_line(aes(x = YYYY, y = WS, colour = "WS"), size = 1) + 
  # geom_point(aes(x = YYYY, y = WS, colour = "WS"), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("Year") + ylab("SR(W/㎡)") + ggtitle("1980-2018 Annual Average of SR") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20)
  ) 




#### WBGT YEARLY ALL STATION
datacut <- subset(data, season == "JJA")
frame.data <- aggregate(datacut, by = list(datacut$yyyy, datacut$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"
names(frame.data)[2] <- "STATION"


# png("Combination of Plots.png", width=600, height=600)
#####
ggplot(data = frame.data) +  
  
  geom_line(aes(x = YYYY, y = Ta, colour = STATION, group = STATION), size = 1) + 
  geom_point(aes(x = YYYY, y = Ta, colour = STATION, group = STATION), size = 2) + 
  

xlab("Year") + ylab("Ta(℃)") + ggtitle("Yearly Change of Ta") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  ) 