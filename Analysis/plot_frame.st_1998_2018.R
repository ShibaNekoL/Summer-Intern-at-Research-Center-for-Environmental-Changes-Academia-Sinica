# install.packages("ggplot2")
# install.packages("digest")
# install.packages("dplyr")
library(ggplot2)
library(digest)
library(dplyr)


frame.st_1998_2018_naomit <- readRDS("frame.st_1998_2018_naomit.rds")
# data <- frame.st_1980_2018[order(frame.st_1980_2018 $ stno, frame.st_1980_2018 $ yyyymmddhh), ]
data <- frame.st_1998_2018_naomit


### 資料長度

# create statistic frame
frame.data <- aggregate(data, by = list(data$stno_en, data$yyyy), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "stno_en"
names(frame.data)[2] <- "YYYY"
frame.data <- frame.data[ , -17]

frame.data <- merge(frame.data, subset(list.st, select = c(stno, type)), by = "stno")
names(frame.data)[29] <- "type"

#####
frame.clean <- subset(frame.data, type == "D")

ggplot(data = frame.clean) +  
  
  geom_line(aes(x = YYYY, y = WBGT, colour = stno_en, group = stno_en), size = 1) + 
  geom_point(aes(x = YYYY, y = WBGT, colour = stno_en, group = stno_en), size = 2) +
  # geom_smooth(aes(x = yyyy, y = summer) , size = 1) +
  
  # geom_hline(aes(yintercept = 0)) + 
  
  xlab("year") + ylab("WBGT") + ggtitle("Yearly Average WBGT \n for 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 



### 全年 各類測站 WBGT各旗色時數百分比/單一測站單一月份各旗色加總

# create statistic frame
# 分子
frame.data <- data.frame(table(MM = data$mm, type = data$type , FLAG = data$flag))

# percentage 算分母
groupby <- data.frame(table(MM = data$mm, type = data$type))

# 合併分子和分母
frame.data <- merge(frame.data, groupby, by = c("MM", "type"))
# 相除
frame.data$percentage <- frame.data$Freq.x / frame.data$Freq.y * 100


#####
frame.clean <- subset(frame.data, FLAG == "white")


# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.clean) +  
  
  geom_line(aes(x = MM, y = percentage, colour = type, group = type), size = 1) + 
  geom_point(aes(x = MM, y = percentage, colour = type, group = type), size = 2) +
  # geom_smooth(aes(x = yyyy, y = summer) , size = 1) +
  
  # geom_hline(aes(yintercept = 0)) + 
  
  xlab("Month") + ylab("hour(%)") + ggtitle("Monthly White Flag hours Percentages \n for 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 30),
        axis.text.y = element_text(vjust = 0.5, size = 30),
        axis.title = element_text(hjust = 0.5, size = 30),
        plot.title = element_text(hjust = 0.5, size = 30), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  ) 

# dev.off()

### 全年 各類測站 WBGT各旗色時數百分比/單一測站單一月份各旗色加總
# 一個測站一張圖

# create statistic frame
# 分子
frame.data <- data.frame(table(MM = data$mm, type = data$type , FLAG = data$flag))

# percentage 算分母
groupby <- data.frame(table(MM = data$mm, type = data$type))

# 合併分子和分母
frame.data <- merge(frame.data, groupby, by = c("MM", "type"))
# 相除
frame.data$percentage <- frame.data$Freq.x / frame.data$Freq.y


#####
frame.clean <- subset(frame.data, type == "B2")
# 固定因子順序
frame.clean$FLAG <- factor(frame.clean$FLAG, levels = c("white", "green", "yellow", "red", "black"))

ggplot(frame.clean, aes(x = MM, y = percentage, fill = FLAG)) +
  
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c("white","green", "gold", "red", "black")) +
  scale_y_continuous(label = scales::percent)+
  
  xlab("Month") + ylab("Percentage") + ggtitle("Monthly Flag Percentage \n at type B2 station for 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )


### 全年 各類測站 WBGT各旗色時數百分比/單一測站單一月份各旗色加總 TAIWAN
# 一個測站一張圖

# create statistic frame
# 分子
frame.data <- data.frame(table(MM = data$mm, FLAG = data$flag))

# percentage 算分母
groupby <- data.frame(table(MM = data$mm))

# 合併分子和分母
frame.data <- merge(frame.data, groupby, by = c("MM"))
# 相除
frame.data$percentage <- frame.data$Freq.x / frame.data$Freq.y


#####
frame.clean <- subset(frame.data)
# 固定因子順序
frame.clean$FLAG <- factor(frame.clean$FLAG, levels = c("white", "green", "yellow", "red", "black"))

ggplot(frame.clean, aes(x = MM, y = percentage, fill = FLAG)) +
  
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c("white","green", "gold", "red", "black")) +
  scale_y_continuous(label = scales::percent)+
  
  xlab("Month") + ylab("Percentage") + ggtitle("Monthly Flag Percentage \n in TAIWAN for 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )


### 單日 各類測站 WBGT旗色時數百分比/單一測站單一小時各旗色加總

# create statistic frame
# 分子
frame.data <- data.frame(table(HH = data$hh, type = data$type, FLAG = data$flag))

# percentage 算分母
groupby <- data.frame(table(HH = data$hh, type = data$type))

# 合併分子和分母
frame.data <- merge(frame.data, groupby, by = c("HH", "type"))
# 相除
frame.data$percentage <- frame.data$Freq.x / frame.data$Freq.y

#####
frame.clean <- subset(frame.data, type == "D")

# 固定因子順序
frame.clean$FLAG <- factor(frame.clean$FLAG, levels = c("white", "green","yellow",  "red", "black"))


ggplot(frame.clean, aes(x = HH, y = percentage, fill = FLAG)) +
  
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c("white","green", "gold", "salmon", "black")) +
  scale_y_continuous(label = scales::percent)+
  
  xlab("Hour") + ylab("Percentage") + ggtitle("Hourly Flag Percentage \n at type D station for 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 30),
        axis.title = element_text(hjust = 0.5, size = 30),
        plot.title = element_text(hjust = 0.5, size = 30), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  )




### 單日 各類測站 WBGT旗色時數百分比/單一測站單一小時各旗色加總

# create statistic frame
# 分子
frame.data <- data.frame(table(HH = data$hh, type = data$type, FLAG = data$flag))

# percentage 算分母
groupby <- data.frame(table(HH = data$hh, type = data$type))

# 合併分子和分母
frame.data <- merge(frame.data, groupby, by = c("HH", "type"))
# 相除
frame.data$percentage <- frame.data$Freq.x / frame.data$Freq.y

#####
frame.clean <- subset(frame.data, FLAG == "black")


ggplot(frame.clean, aes(x = HH, y = percentage, colour = type, group = type)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  
  scale_y_continuous(label = scales::percent)+
  
  xlab("Hour") + ylab("Percentage") + ggtitle("Hourly black Flag Percentage \n for 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 30),
        axis.title = element_text(hjust = 0.5, size = 30),
        plot.title = element_text(hjust = 0.5, size = 30), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  )




### 各測站 5月 8AM-15PM WBGT平均 旗色地圖 


# create statistic frame
frame.data <- aggregate(data, by = list(data$stno_en, data$mm, data$hh), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[2] <- "MM"
names(frame.data)[3] <- "HH"

# creat flag group
frame.data $ FLAG <- ifelse(frame.data $ WBGT < 27.78, "white",
                                    ifelse(frame.data $ WBGT < 29.44, "green",
                                           ifelse(frame.data $ WBGT < 31.11, "yellow",
                                                  ifelse(frame.data $ WBGT < 32.22, "red",
                                                         ifelse(frame.data $ WBGT >= 32.22, "black",         
                                                                NA
                                                         )
                                                  )
                                           )
                                    )
)


# 固定因子順序
frame.data $ FLAG <- factor(frame.data$FLAG, levels = c("white", "green", "yellow",  "red", "black"))


## list.st : import list of all station 
list.st <- read.csv("C:/Users/user/OneDrive/桌面 1/RCEC/st_utf8.csv", header = T)
# 加上type
frame.data <- merge(frame.data, subset(list.st, select = c(stno, type)), by = "stno")
names(frame.data)[32] <- "type"

##################################################################
install.packages("devtools") 
library(devtools)

devtools::install_github("dkahle/ggmap")
library(ggmap)

install.packages("ggrepel") 
library(ggrepel)

# save api key
register_google(key = "AIzaSyDQulMjEi9Sdwsg4tVGk5Gd45nqkuzzVMg")
##################################################################

map <- get_map(location = "Taiwan", zoom = 7) # , maptype = "toner-lite" , maptype = "satellite"


#####
frame.clean <- subset(frame.data, MM == 9 & HH == 14)


ggmap(map, darken = 0.5) + 
  geom_point(aes(x = lng, y = lat, fill = FLAG, group = stno_en), pch = 21, size = 4, data = frame.clean) + 
  scale_fill_manual(values = c("white","green", "gold", "salmon", "black")) +
  geom_label_repel(aes(x = lng, y = lat, label = Group.1, colour = type),
                   fontface = "bold",
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"),
                   segment.colour = "white",
                   size = 4,
                   data = frame.clean
                   ) +
  ggtitle("Average Flag in September at 14pm\n for 1998-2018") +
  theme(axis.text.x = element_text(vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10),
        axis.title = element_text(hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5, size = 30), # 將title置中
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )



### 各測站綠旗以上天數百分比

# create statistic frame
#####
frame.data <- subset(data, yyyy == 2014 | yyyy == 2015 | yyyy == 2016 | yyyy == 2017 | yyyy == 2018, select = c(stno, stno_en, type, flag, WBGT, yyyy, mm, dd, hh))

# 非白旗則1
frame.data $ count <- ifelse(frame.data $ flag != "white", 1, 0)

# table 各測站0 1 個數
frame.count <- data.frame(table(stno_en = frame.data$stno_en, count = frame.data$count))

# merge 0 1
frame.merge <- merge(frame.count[1:30, ], frame.count[31:60, ], by = c("stno_en"))
frame.merge $ Freq.xy <- frame.merge $ Freq.x + frame.merge $ Freq.y
frame.merge $ percentage <- frame.merge $ Freq.y / frame.merge $ Freq.xy

# 2014-2018
frame.clean1 <- subset(frame.merge, select = c("stno_en", "percentage"))
frame.clean1 $ year <- "2014-2018"
# 2009-2013
frame.clean2 <- subset(frame.merge, select = c("stno_en", "percentage"))
frame.clean2 $ year <- "2009-2013"
# 2004-2008
frame.clean3 <- subset(frame.merge, select = c("stno_en", "percentage"))
frame.clean3 $ year <- "2004-2008"
# 1999-2003
frame.clean4 <- subset(frame.merge, select = c("stno_en", "percentage"))
frame.clean4 $ year <- "1999-2003"

frame.clean <- rbind(frame.clean1, frame.clean2, frame.clean3, frame.clean4)

## x軸離散變數依y軸變數大小排列
frame_table <- table(frame.clean1$stno_en)
stno_en_levels <- names(frame_table)[order(frame.clean1$percentage)]
frame.clean$stno_en2 <- factor(frame.clean$stno_en, levels = stno_en_levels)
frame.clean <- subset(frame.clean, stno_en2 != "HSINCHU")

#####


ggplot(frame.clean, aes(x = stno_en2, y = percentage, colour = year, group = year)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  
  scale_y_continuous(label = scales::percent)+
  
  xlab("station") + ylab("Percentage") + ggtitle("各測站綠旗以上時數占總觀測時數百分比") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10, angle = 90),
        axis.text.y = element_text(vjust = 0.5, size = 30),
        axis.title = element_text(hjust = 0.5, size = 30),
        plot.title = element_text(hjust = 0.5, size = 30), # 將title置中
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)
  )

## WBGT月際變化

frame.data <- aggregate(data, by = list(data$mm, data$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "MM"
names(frame.data)[2] <- "STATION"

frame.data$MM <- as.factor(frame.data$MM)

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = MM, y = WBGT, colour = STATION, group = STATION), size = 1) + 
  geom_point(aes(x = MM, y = WBGT, colour = STATION, group = STATION), size = 2) + 
  # geom_smooth(aes(x = MM, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("Month") + ylab("WBGT(°C)") + ggtitle("Monthly Change of WBGT 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()


## WBGT月際變化 -mean

frame.data <- aggregate(data, by = list(data$mm, data$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)
frame.mean <- aggregate(data, by = list(data$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "MM"
names(frame.data)[2] <- "STATION"
names(frame.mean)[1] <- "STATION"

frame.data <- merge(frame.data, frame.mean, by = "STATION")

frame.data$MM <- as.factor(frame.data$MM)

frame.data$WBGT.d <- frame.data$WBGT.x - frame.data$WBGT.y

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = MM, y = WBGT.d, colour = STATION, group = STATION), size = 1) + 
  geom_point(aes(x = MM, y = WBGT.d, colour = STATION, group = STATION), size = 2) + 
  # geom_smooth(aes(x = MM, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("Month") + ylab("WBGT(°C) - mean") + ggtitle("Monthly Change of WBGT-mean 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()



## WBGT時際變化

frame.data <- aggregate(data, by = list(data$hh, data$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "HH"
names(frame.data)[2] <- "STATION"

frame.data$HH <- as.factor(frame.data$HH)

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = HH, y = WBGT, colour = STATION, group = STATION), size = 1) + 
  geom_point(aes(x = HH, y = WBGT, colour = STATION, group = STATION), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("Hour") + ylab("WBGT(°C)") + ggtitle("Hourly Change of WBGT 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()


## WBGT 時際變化 -mean

frame.data <- aggregate(data, by = list(data$hh, data$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)
frame.mean <- aggregate(data, by = list(data$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "HH"
names(frame.data)[2] <- "STATION"
names(frame.mean)[1] <- "STATION"

frame.data <- merge(frame.data, frame.mean, by = "STATION")

frame.data$HH <- as.factor(frame.data$HH)

frame.data$WBGT.d <- frame.data$WBGT.x - frame.data$WBGT.y

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = HH, y = WBGT.d, colour = STATION, group = STATION), size = 1) + 
  geom_point(aes(x = HH, y = WBGT.d, colour = STATION, group = STATION), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("Hour") + ylab("WBGT(°C) - mean") + ggtitle("Hourly Change of WBGT-mean 1998-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()  

## WBGT 4年月際變化
data <- subset(data, yyyy >= 1999)
data$yyyy_g <- ifelse(data$yyyy >= 1999 & data$yyyy <= 2002, 1,
                      ifelse(data$yyyy >= 2003 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2010, 3,
                                    ifelse(data$yyyy >= 2011 & data$yyyy <= 2014, 4,
                                           ifelse(data$yyyy >= 2015 & data$yyyy <= 2018, 5, NA
                                           )))))

frame.data <- aggregate(data, by = list(data$mm, data$yyyy_g), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "MM"
names(frame.data)[2] <- "YYYY_G"

frame.data$MM <- as.factor(frame.data$MM)

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = MM, y = WBGT, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = WBGT, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(name = "year", labels =  c("1999-2002","2003-2006","2007-2010","2011-2014","2015-2018")) +
  
  xlab("Month") + ylab("WBGT(°C)") + ggtitle("Monthly Change of WBGT 1999-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()


## WBGT 4年月際變化 08-17
data <- subset(data, yyyy >= 1999 & hh >= 8 & hh <= 17)
data$yyyy_g <- ifelse(data$yyyy >= 1999 & data$yyyy <= 2002, 1,
                      ifelse(data$yyyy >= 2003 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2010, 3,
                                    ifelse(data$yyyy >= 2011 & data$yyyy <= 2014, 4,
                                           ifelse(data$yyyy >= 2015 & data$yyyy <= 2018, 5, NA
                                           )))))

frame.data <- aggregate(data, by = list(data$mm, data$yyyy_g), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "MM"
names(frame.data)[2] <- "YYYY_G"

frame.data$MM <- as.factor(frame.data$MM)

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = MM, y = WBGT, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = WBGT, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(name = "year", labels =  c("1999-2002","2003-2006","2007-2010","2011-2014","2015-2018")) +
  
  xlab("Month") + ylab("WBGT(°C)") + ggtitle("Monthly Change of WBGT 1999-2018 at 08-17(UTC+8)") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()

## WBGT 3年月際變化
data <- subset(data, yyyy >= 2001 & hh >= 8 & hh <= 17)
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5, 
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6, NA
                                           ))))))

frame.data <- aggregate(data, by = list(data$mm, data$yyyy_g), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "MM"
names(frame.data)[2] <- "YYYY_G"

frame.data$MM <- as.factor(frame.data$MM)

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = MM, y = WBGT, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = WBGT, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(low = "green4",high = "oliverdrab2", name = "year", labels =  c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
  
  xlab("Month") + ylab("WBGT(°C)") + ggtitle("Monthly Change of WBGT 2001-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()


## WBGT時際變化
data <- subset(data, yyyy >= 1999)
data$yyyy_g <- ifelse(data$yyyy >= 1999 & data$yyyy <= 2002, 1,
                      ifelse(data$yyyy >= 2003 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2010, 3,
                                    ifelse(data$yyyy >= 2011 & data$yyyy <= 2014, 4,
                                           ifelse(data$yyyy >= 2015 & data$yyyy <= 2018, 5, NA
                                           )))))

frame.data <- aggregate(data, by = list(data$hh, data$yyyy_g), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "HH"
names(frame.data)[2] <- "YYYY_G"

frame.data$HH <- as.factor(frame.data$HH)

# png("Combination of Plots.png", width=600, height=600)

ggplot(data = frame.data) +  
  
  geom_line(aes(x = HH, y = WBGT, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = HH, y = WBGT, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(name = "year", labels =  c("1999-2002","2003-2006","2007-2010","2011-2014","2015-2018")) +
  
  xlab("Hour") + ylab("WBGT(°C)") + ggtitle("Hourly Change of WBGT 1999-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

# dev.off()
