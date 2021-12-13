install.packages("ggplot2", type = "source")
library(ggplot2)
library(digest)
update.packages(ask=FALSE, checkBuilt=TRUE) # 換R版本後用來更新套件


####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) { 
  library(grid) 
  
  # Make a list from the ... arguments and plotlist 
  plots <- c(list(...), plotlist) 
  
  numPlots = length(plots) 
  
  # If layout is NULL, then use 'cols' to determine layout 
  if (is.null(layout)) { 
    # Make the panel 
    # ncol: Number of columns of plots 
    # nrow: Number of rows needed, calculated from # of cols 
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
                     ncol = cols, nrow = ceiling(numPlots/cols)) 
  } 
  
  if (numPlots==1) { 
    print(plots[[1]]) 
    
  } else { 
    # Set up the page 
    grid.newpage() 
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) 
    
    # Make each plot, in the correct location 
    for (i in 1:numPlots) { 
      # Get the i,j matrix positions of the regions that contain this subplot 
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE)) 
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                                      layout.pos.col = matchidx$col)) 
    } 
  } 
}
####



frame.st_1998_2018_naomit <- readRDS("C:/Users/user/OneDrive - 國立台灣大學/RCEC/Summerframe.st_1998_2018_naomit.rds")
data <- frame.st_1998_2018_naomit
# subset data after 2000 (2000-2018)
data <- subset(data, yyyy >= 2000)


#######

### 2000-2018 08-17(UTC+8)全台平均每小時休息時間(分鐘)
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8)
frame.data <- aggregate(data_work, by = list(data_work$yyyy, data_work$season), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"
names(frame.data)[2] <- "SEASON"

###
ggplot(data = frame.data) +  
  
  geom_line(aes(x = YYYY, y = rest_h * 60, color = SEASON, group = SEASON), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_h * 60, color = SEASON, group = SEASON), size = 2) +
  geom_smooth(aes(x = yyyy, y = rest_h * 60, color = SEASON, group = SEASON), method ="lm" , size = 1) +
  
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("2000-2018 08-17(UTC+8)\n Rest time per hour of heavy work") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 


### 各組測站 2000-2018 JJA 08-17(UTC+8)平均每小時休息時間比例(分鐘)趨勢
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8 & season == "SON")
frame.data <- aggregate(data_work, by = list(data_work$yyyy), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"

ggplot(data = frame.data) +  
  
  geom_line(aes(x = YYYY, y = rest_h * 60, color = "rest_h"), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_h * 60, color = "rest_h"), size = 2) +
  geom_smooth(aes(x = yyyy, y = rest_h * 60), method ="lm" , size = 1) +
  
  geom_line(aes(x = YYYY, y = rest_m * 60, color = "rest_m"), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_m * 60, color = "rest_m"), size = 2) +
  geom_smooth(aes(x = yyyy, y = rest_m* 60), method ="lm" , size = 1) +
  
  geom_line(aes(x = YYYY, y = rest_l * 60, color = "rest_l"), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_l * 60, color = "rest_l"), size = 2) +
  geom_smooth(aes(x = yyyy, y = rest_l * 60), method ="lm" , size = 1) +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("Rest time of each hour of work\n in SON at 08-17(UTC+8) 2000-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 30), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 



### 各組測站 2000-2018 JJA 08-17(UTC+8)平均每小時休息時間比例(分鐘)趨勢
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8 & season == "JJA")
frame.data <- aggregate(data_work, by = list(data_work$type, data_work$yyyy), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "TYPE"
names(frame.data)[2] <- "YYYY"


ggplot(data = frame.data) +  
  
  geom_line(aes(x = YYYY, y = rest_l * 60, colour = TYPE, group = TYPE), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_l * 60, colour = TYPE, group = TYPE), size = 2) +

  # geom_smooth(aes(x = yyyy, y = summer) , size = 1) +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("2000-2018 JJA 08-17(UTC+8)\n Rest time per hour of light work") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 30), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 


### A組測站 2000-2018 JJA 08-17(UTC+8)平均每小時休息時間比例(分鐘)趨勢
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8 & season == "JJA")
frame.data <- aggregate(data_work, by = list(data_work$type, data_work$yyyy, data_work$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "TYPE"
names(frame.data)[2] <- "YYYY"
names(frame.data)[3] <- "STNO_EN"

frame.clean <- subset(frame.data, TYPE == "A")

ggplot(data = frame.clean) +  
  
  geom_line(aes(x = YYYY, y = rest_m * 60, colour = rest_m, group = rest_m), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_m * 60, colour = rest_m, group = rest_m), size = 2) +
  
  # geom_smooth(aes(x = yyyy, y = summer) , size = 1) +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("Rest time of each hour of median work\n in JJA at 08-17(UTC+8) 2000-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 30), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 




### 2000-2018 台北測站08-17(UTC+8) 平均每小時休息時間(分鐘)
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8)
frame.data <- aggregate(data_work, by = list(data_work$yyyy, data_work$season, data_work$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"
names(frame.data)[2] <- "SEASON"
names(frame.data)[3] <- "STATION"

####
frame.clean <- subset(frame.data, STATION == "KAOHSIUNG")

###
ggplot(data = frame.clean) +  
  
  geom_line(aes(x = YYYY, y = rest_h * 60, color = SEASON, group = SEASON), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_h * 60, color = SEASON, group = SEASON), size = 2) +
  geom_smooth(aes(x = yyyy, y = rest_h * 60, color = SEASON, group = SEASON), method ="lm" , size = 1) +
  
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("Rest time of each hour of heavy work\n at KAOHSIUNG 08-17(UTC+8) 2000-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 



### 2000-2018 JJA 12:00(UTC+8) 旗色地圖


# create statistic frame
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                     ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                     ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                     ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                     ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5,
                     ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6, NA
                            ))))))

frame.data <- aggregate(data, by = list(data$stno_en, data$season, data$hh, data$yyyy_g), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[2] <- "SEASON"
names(frame.data)[3] <- "HH"
names(frame.data)[4] <- "YYYY_G"

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


##################################################################

install.packages("devtools") 
library(devtools)

devtools::install_github("dkahle/ggmap")
library(ggmap)

# save api key
register_google(key = "AIzaSyDQulMjEi9Sdwsg4tVGk5Gd45nqkuzzVMg")
##################################################################

map <- get_map(location = "Taiwan", zoom = 7 , maptype = "toner-lite") #, maptype = "watercolor" , maptype = "toner-lite" , maptype = "satellite" , maptype = "roadmap" , color="bw"
s <- c("2001-2003", "2004-2006", "2007-2009", "2010-2012", "2013-2015", "2016-2018")

##### 批量作圖!!!!!!
for(i in 1 : 6){
  #不同年一張圖
  frame.clean <- subset(frame.data, YYYY_G == i & SEASON == "JJA" & HH == 12)
  p <- ggmap(map) + # , darken = 0.5
          geom_point(aes(x = lng, y = lat, fill = FLAG, group = stno_en), pch = 21, size = 3, data = frame.clean) +
          scale_fill_manual(values = c("white","green", "yellow", "red", "black")) +
          ggtitle(paste0(s[i], " JJA 12(UTC+8) Average Flag")) +
          theme(axis.text.x = element_text(vjust = 0.5),
                axis.text.y = element_text(vjust = 0.5),
                axis.title = element_text(hjust = 0.5),
                plot.title = element_text(hjust = 0.5), # 將title置中
                legend.text = element_text(),
                legend.title = element_text()
                )
  np <- paste0("p", i)
  assign(np, p)
}
multiplot(p1, p2, p3, p4, p5, p6, cols = 3) #還有layout參數可以調整圖片放置位置順序



### 2000-2018 JJA 08-17(UTC+8)各組測站 各旗色百分比

### START HERE
# create statistic frame
# 分子
datacut <- subset(data, hh >= 8 & hh <=17 & season == "DJF")
frame.data <- data.frame(table(YYYY = datacut$yyyy, TYPE = datacut$type, FLAG = datacut$flag))
# percentage 算分母
groupby <- data.frame(table(YYYY = datacut$yyyy, TYPE = datacut$type))
# 合併分子和分母
frame.data <- merge(frame.data, groupby, by = c("YYYY", "TYPE"))
# 相除
frame.data$percentage <- frame.data$Freq.x / frame.data$Freq.y

# create statistic frame WITH FULL TAIWAN
# 分子
frame.data_TW <- data.frame(table(YYYY = datacut$yyyy, FLAG = datacut$flag))
# percentage 算分母
groupby_TW <- data.frame(table(YYYY = datacut$yyyy))
# 合併分子和分母
frame.data_TW <- merge(frame.data_TW, groupby_TW, by = c("YYYY"))
# 相除
frame.data_TW$percentage <- frame.data_TW$Freq.x / frame.data_TW$Freq.y

# 固定因子順序
frame.data_TW$FLAG <- factor(frame.data_TW$FLAG, levels = c("white", "green","yellow",  "red", "black"))


for(i in 1 : 5){
  #####
  frame.clean <- subset(frame.data, TYPE == levels(data$type)[i])
  
  # 固定因子順序
  frame.clean$FLAG <- factor(frame.clean$FLAG, levels = c("white", "green","yellow",  "red", "black"))
  
  p <- ggplot(frame.clean, aes(x = YYYY, y = percentage, fill = FLAG)) +
    
    geom_bar(stat = 'identity') + 
    scale_fill_manual(values = c("white","green", "yellow", "red", "black")) +
    scale_y_continuous(label = scales::percent)+
    
    xlab("Year") + ylab("Percentage") + ggtitle(paste0("DJF 08-17pm Flag at type ", levels(data$type)[i], " station")) +
    
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
          axis.text.y = element_text(vjust = 0.5),
          axis.title = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.5), # 將title置中
          legend.text = element_text(),
          legend.title = element_text()
    )
  np <- paste0("p", i)
  assign(np, p)
}

p6 <- ggplot(frame.data_TW, aes(x = YYYY, y = percentage, fill = FLAG)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c("white","green", "yellow", "red", "black")) +
  scale_y_continuous(label = scales::percent)+
  xlab("Year") + ylab("Percentage") + ggtitle("DJF 08-17pm Flag in Taiwan") +
  
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
        axis.text.y = element_text(vjust = 0.5),
        axis.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5), # 將title置中
        legend.text = element_text(),
        legend.title = element_text()
  )

multiplot(p1, p2, p3, p4, p5, p6, cols = 3)


### 2000-2018 08-17(UTC+8) 平均每小時休息時間(分鐘) 
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8)
frame.data <- aggregate(data_work, by = list(data_work$yyyy, data_work$season, data_work$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"
names(frame.data)[2] <- "SEASON"
names(frame.data)[3] <- "STATION"

####
frame.clean <- subset(frame.data, SEASON == "JJA")

###
ggplot(data = frame.clean) +  
  
  geom_line(aes(x = YYYY, y = rest_h * 60, color = STATION, group = STATION), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_h * 60, color = STATION, group = STATION), size = 2) +
  # geom_smooth(aes(x = yyyy, y = rest_h * 60, color = STATION, group = STATION), method ="lm" , size = 1) +
  # scale_color_gradient(low = 'red', high = 'blue') +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("Rest time of each hour of heavy work\n 08-17(UTC+8) 2000-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 

### 2000-2018 08-17(UTC+8) 平均每小時休息時間(分鐘) 緯度分色
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8)
frame.data <- aggregate(data_work, by = list(data_work$yyyy, data_work$season, data_work$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"
names(frame.data)[2] <- "SEASON"
names(frame.data)[3] <- "STATION"

####
frame.clean <- subset(frame.data, SEASON == "JJA")

###
ggplot(data = frame.clean) +  
  
  geom_line(aes(x = YYYY, y = rest_h * 60, color = lat, group = STATION), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_h * 60, color = lat, group = STATION), size = 2) +
  # geom_smooth(aes(x = yyyy, y = rest_h * 60, color = STATION, group = STATION), method ="lm" , size = 1) +
  scale_color_gradient(low = 'red', high = 'blue') +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("Rest time of each hour of heavy work\n JJA 08-17(UTC+8) 2000-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 




### 2000-2018 08-17(UTC+8) 平均每小時休息時間(分鐘) 海拔分色
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8)
frame.data <- aggregate(data_work, by = list(data_work$yyyy, data_work$season, data_work$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"
names(frame.data)[2] <- "SEASON"
names(frame.data)[3] <- "STATION"

####
frame.clean <- subset(frame.data, SEASON == "JJA")

###
ggplot(data = frame.clean) +  
  
  geom_line(aes(x = YYYY, y = rest_h * 60, color = asl_m, group = STATION), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_h * 60, color = asl_m, group = STATION), size = 2) +
  # geom_smooth(aes(x = yyyy, y = rest_h * 60, color = STATION, group = STATION), method ="lm" , size = 1) +
  scale_color_gradient(low = 'brown', high = 'green3') +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("Rest time of each hour of heavy work\n JJA 08-17(UTC+8) 2000-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 



### 2000-2018 JJA 08-17(UTC+8) 平均每小時休息時間(分鐘) 
# create statistic frame
data_work <- subset(data, hh <=17 & hh >= 8)
frame.data <- aggregate(data_work, by = list(data_work$yyyy, data_work$season, data_work$stno_en), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY"
names(frame.data)[2] <- "SEASON"
names(frame.data)[3] <- "STATION"
 
####
# frame.clean <- subset(frame.data, STATION == "TAIPEI" | STATION == "BANQIAO"| STATION == "KAOHSIUNG"| STATION == "KINMEN")
frame.clean <- subset(frame.data, STATION == "YILAN" | STATION == "TAITUNG" | STATION == "DAWU")

frame.clean <- subset(frame.clean, SEASON == "JJA")

###
ggplot(data = frame.clean) +  
  
  geom_line(aes(x = YYYY, y = rest_l * 60, color = STATION, group = STATION), size = 1) + 
  geom_point(aes(x = YYYY, y = rest_l * 60, color = STATION, group = STATION), size = 2) +
  geom_smooth(aes(x = yyyy, y = rest_l * 60, color = STATION, group = STATION), size = 2, method ="lm", se = F) +
  # scale_color_gradient(low = 'brown', high = 'green3') +
  
  # geom_hline(aes(yintercept = 0)) + 
  # geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("rest time (minutes)") + ggtitle("Rest time of each hour of light work\n JJA 08-17(UTC+8) 2000-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 20),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  ) 





### 2001-2018 年份組別 日變化 休息時間

### START HERE
# create statistic frame

data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5,
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6, NA
                                                  ))))))
datacut <- subset(data)
frame.data <- aggregate(datacut, by = list(datacut$yyyy_g, datacut$season, datacut$hh), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY_G"
names(frame.data)[2] <- "SEASON"
names(frame.data)[3] <- "HH"
ss <- c("MAM", "JJA", "SON", "DJF")

#####
for(i in 1 : 4){
  
  frame.clean <- subset(frame.data, SEASON == ss[i])
  
  p <- ggplot(frame.clean) +
    
    geom_line(aes(x = HH, y = rest_l * 60, colour = YYYY_G, group = YYYY_G), size = 1) + 
    geom_point(aes(x = HH, y = rest_l * 60, colour = YYYY_G, group = YYYY_G), size = 2) +
    
    scale_color_continuous(name = "year", labels = c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
    
    scale_x_continuous(limits = c(1, 25), breaks = seq(1, 24, by = 2)) + # x軸標籤離散化
    
    xlab("Hour") + ylab("rest time (minutes)") + ggtitle(paste0("Rest time of each hour of \n light work in ", ss[i])) +
    theme(axis.text.x = element_text(vjust = 0.5),
          axis.text.y = element_text(vjust = 0.5, size = 15),
          axis.title = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
          legend.text = element_text(),
          legend.title = element_text()
    )
  assign(paste0("p", i), p)
}

multiplot(p1, p2, p3, p4, cols = 4)




### 2001-2018 年份組別 休息時間 月變化

### START HERE
# create statistic frame
# 分子
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5,
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6, NA
                                                  ))))))

frame.data <- aggregate(data, by = list(data$yyyy_g, data$mm), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY_G"
names(frame.data)[2] <- "MM"

#####


ggplot(frame.data) +
  
  geom_line(aes(x = MM, y = rest_h * 60, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = rest_h * 60, colour = YYYY_G, group = YYYY_G), size = 2) +
  
  scale_color_continuous(name = "year", labels = c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
  
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1)) + # x軸標籤離散化
  
  xlab("Month") + ylab("rest time (minutes)") + ggtitle(paste0("Rest time of each hour of \n heavy work in 2001-2018")) +
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 15),
        axis.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text()
  )


### 2001-2018 年份組別 月變化 08-17

### START HERE
# create statistic frame
# 分子
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5,
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6, NA
                                                  ))))))
datacut <- subset(data, hh >= 8 & hh <= 17)
frame.data <- aggregate(datacut, by = list(datacut$yyyy_g, datacut$mm), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "YYYY_G"
names(frame.data)[2] <- "MM"

#####


ggplot(frame.data) +
  
  geom_line(aes(x = MM, y = rest_m * 60, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = rest_m * 60, colour = YYYY_G, group = YYYY_G), size = 2) +
  
  scale_color_continuous(name = "year", labels = c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
  
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1)) + # x軸標籤離散化
  
  xlab("Month") + ylab("rest time (minutes)") + ggtitle(paste0("Rest time of each hour of \n median work in 2001-2018 at 08-17")) +
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 15),
        axis.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text()
  )




### 2001-2018 各測站綠旗以上天數百分比

### START HERE
# create statistic frame
# 分子
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5, 
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6,NA
                                                  ))))))


y <- c("2001-2003","2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")

# create statistic frame
#####

for(i in 1:6){
  
  frame.data <- subset(data, yyyy_g == i, select = c(stno, stno_en, type, flag, WBGT, yyyy, mm, dd, hh))
  
  # 非白旗則1
  frame.data $ count <- ifelse(frame.data $ flag != "white", 1, 0)
  
  # table 各測站0 1 個數
  frame.count <- data.frame(table(stno_en = frame.data$stno_en, count = frame.data$count))
  
  # merge 0 1
  frame.merge <- merge(frame.count[1:30, ], frame.count[31:60, ], by = c("stno_en"))
  frame.merge $ Freq.xy <- frame.merge $ Freq.x + frame.merge $ Freq.y
  frame.merge $ percentage <- frame.merge $ Freq.y / frame.merge $ Freq.xy
  
  assign(paste0("frame.clean", i), frame.merge)
}
frame.clean1 $ year <- 1
frame.clean2 $ year <- 2
frame.clean3 $ year <- 3
frame.clean4 $ year <- 4
frame.clean5 $ year <- 5
frame.clean6 $ year <- 6
frame.clean <- rbind(frame.clean1, frame.clean2, frame.clean3, frame.clean4, frame.clean5, frame.clean6)


## x軸離散變數依y軸變數大小排列
frame_table <- table(frame.clean5$stno_en)
stno_en_levels <- names(frame_table)[order(frame.clean5$percentage)]
frame.clean$stno_en2 <- factor(frame.clean$stno_en, levels = stno_en_levels)
frame.clean <- subset(frame.clean, stno_en2 != "HSINCHU") # 新竹好像讀不到資料

#####

windowsFonts(A=windowsFont("微軟正黑體")) 

ggplot(frame.clean, aes(x = stno_en2, y = percentage, colour = year, group = year)) +
  # geom_line(size = 1) + 
  geom_point(size = 6) +
  
  scale_y_continuous(label = scales::percent)+
  scale_color_continuous(name = "year", labels = y, high = "red", low = "green") +
  
  xlab("station") + ylab("Percentage") + ggtitle("2001-2018 各測站非白旗時數占總觀測時數百分比") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10, angle = 45),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20, family = "A", face = "bold"), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  )



### 2001-2018 MAM各測站綠旗以上天數百分比

### START HERE
# create statistic frame
# 分子
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5, 
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6,NA
                                                  ))))))


y <- c("2001-2003","2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")


# create statistic frame
#####
datacut <- subset(data, season == "DJF")
  
for(i in 1:6){
  
  frame.data <- subset(datacut, yyyy_g == i, select = c(stno, stno_en, type, flag, WBGT, yyyy, mm, dd, hh))
  
  # 非白旗則1
  frame.data $ count <- ifelse(frame.data $ flag != "white", 1, 0)
  
  # table 各測站0 1 個數
  frame.count <- data.frame(table(stno_en = frame.data$stno_en, count = frame.data$count))
  
  # merge 0 1
  frame.merge <- merge(frame.count[1:30, ], frame.count[31:60, ], by = c("stno_en"))
  frame.merge $ Freq.xy <- frame.merge $ Freq.x + frame.merge $ Freq.y
  frame.merge $ percentage <- frame.merge $ Freq.y / frame.merge $ Freq.xy
  
  assign(paste0("frame.clean", i), frame.merge)
}
frame.clean1 $ year <- 1
frame.clean2 $ year <- 2
frame.clean3 $ year <- 3
frame.clean4 $ year <- 4
frame.clean5 $ year <- 5
frame.clean6 $ year <- 6
frame.clean <- rbind(frame.clean1, frame.clean2, frame.clean3, frame.clean4, frame.clean5, frame.clean6)


## x軸離散變數依y軸變數大小排列
frame_table <- table(frame.clean5$stno_en)
stno_en_levels <- names(frame_table)[order(frame.clean5$percentage)]
frame.clean$stno_en2 <- factor(frame.clean$stno_en, levels = stno_en_levels)
frame.clean <- subset(frame.clean, stno_en2 != "HSINCHU") # 新竹好像讀不到資料

# 字體
windowsFonts(A=windowsFont("微軟正黑體")) 

#####



ggplot(frame.clean, aes(x = stno_en2, y = percentage, colour = year, group = year)) +
  # geom_line(size = 1) + 
  geom_point(size = 6) +
  
  scale_y_continuous(label = scales::percent)+
  scale_color_continuous(name = "year", labels = y, high = "red", low = "green") +
  
  xlab("station") + ylab("Percentage") + ggtitle("2001-2018 DJF各測站非白旗時數占總觀測時數百分比") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10, angle = 45),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20, family = "A", face = "bold"), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  )

#####


ggplot(frame.clean, aes(x = stno_en2, y = percentage, colour = year, group = year)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  
  scale_y_continuous(label = scales::percent)+
  scale_color_continuous(name = "year", labels =  c("1999-2002","2003-2006","2007-2010","2011-2014","2015-2018")) +
  
  xlab("station") + ylab("Percentage") + ggtitle("SON 各測站綠旗以上時數占總觀測時數百分比") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 10, angle = 45),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)
  )




## WBGT時際變化
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5, 
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6,NA
                                                  ))))))


y <- c("2001-2003","2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")

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
  scale_color_continuous(name = "year", labels = y, high = 'red4', low = "lightsalmon") +
  
  xlab("Hour") + ylab("WBGT(°C)") + ggtitle("2001-2018 Hourly Average of WBGT ") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
  ) 

# dev.off()

ggplot(data = frame.data) +  
  
  geom_line(aes(x = HH, y = Ta, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = HH, y = Ta, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(name = "year", labels = y, high = 'red4', low = "lightsalmon") +
  
  xlab("Hour") + ylab("Ta(°C)") + ggtitle("2001-2018 Hourly Average of Ta") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
  ) 


ggplot(data = frame.data) +  
  
  geom_line(aes(x = HH, y = RH, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = HH, y = RH, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(name = "year", labels = y, high = 'red4', low = "lightsalmon") +
  
  xlab("Hour") + ylab("RH(%)") + ggtitle("2001-2018 Hourly Average of RH") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
  ) 


ggplot(data = frame.data) +  
  
  geom_line(aes(x = HH, y = SR, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = HH, y = SR, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(name = "year", labels = y, high = 'red4', low = "lightsalmon") +
  
  xlab("Hour") + ylab("SR(MJ/㎡)") + ggtitle("2001-2018 Hourly Average of SR") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
  ) 


ggplot(data = frame.data) +  
  
  geom_line(aes(x = HH, y = WS, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = HH, y = WS, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(name = "year", labels = y, high = 'red4', low = "lightsalmon") +
  
  xlab("Hour") + ylab("WS(m/s)") + ggtitle("2001-2018 Hourly Average of WS") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
  ) 



## WBGT MAM時際變化
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5, 
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6,NA
                                                  ))))))


y <- c("2001-2003","2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")

#####
datacut <- subset(data, season == "DJF")
frame.data <- aggregate(datacut, by = list(datacut$hh, datacut$yyyy_g), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

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
  scale_color_continuous(name = "year", labels = y, high = 'red4', low = "lightsalmon") +
  
  xlab("Hour") + ylab("WBGT(°C)") + ggtitle("2001-2018 DJF Hourly Change of WBGT ") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  ) 

# dev.off()




## RH wbgt時際變化
data$yyyy_g <- ifelse(data$yyyy >= 2001 & data$yyyy <= 2003, 1,
                      ifelse(data$yyyy >= 2004 & data$yyyy <= 2006, 2,
                             ifelse(data$yyyy >= 2007 & data$yyyy <= 2009, 3,
                                    ifelse(data$yyyy >= 2010 & data$yyyy <= 2012, 4,
                                           ifelse(data$yyyy >= 2013 & data$yyyy <= 2015, 5, 
                                                  ifelse(data$yyyy >= 2016 & data$yyyy <= 2018, 6,NA
                                                  ))))))


y <- c("2001-2003","2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")


frame.data <- aggregate(data, by = list(data$hh, data$yyyy_g), FUN = mean, simplify = TRUE, na.action = NULL, na.rm = TRUE)

names(frame.data)[1] <- "HH"
names(frame.data)[2] <- "YYYY_G"

frame.data$HH <- as.factor(frame.data$HH)

# png("Combination of Plots.png", width=600, height=600)

#####
ggplot(data = frame.data) +  
  
  geom_line(aes(x = HH, y = SR, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = HH, y = SR, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous(name = "year", labels = y, high = 'red4', low = "lightsalmon") +
  
  xlab("Hour") + ylab("SR(MJ/㎡)") + ggtitle("2001-2018 Hourly Change of SR") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  ) 

# dev.off()



## WBGT 3年月際變化
data <- subset(data, yyyy >= 2001)
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
  scale_color_continuous( high= "darkgreen", low= "greenyellow", name = "year", labels =  c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
  
  xlab("Month") + ylab("WBGT(°C)") + ggtitle("2001-2018 Monthly Average of WBGT") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
  ) 



## WBGT SR 3年月際變化
data <- subset(data, yyyy >= 2001)
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
  
  geom_line(aes(x = MM, y = Ta, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = Ta, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous( high= "darkgreen", low= "greenyellow", name = "year", labels =  c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
  
  xlab("Month") + ylab("Ta(℃)") + ggtitle("2001-2018 Monthly Average of Ta") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  ) 




ggplot(data = frame.data) +  
  
  geom_line(aes(x = MM, y = RH, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = RH, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous( high= "darkgreen", low= "greenyellow", name = "year", labels =  c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
  
  xlab("Month") + ylab("RH(%)") + ggtitle("2001-2018 Monthly Average of RH") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  ) 



ggplot(data = frame.data) +  
  
  geom_line(aes(x = MM, y = SR, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = SR, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous( high= "darkgreen", low= "greenyellow", name = "year", labels =  c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
  
  xlab("Month") + ylab("SR(MJ/㎡)") + ggtitle("2001-2018 Monthly Average of SR") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  ) 



ggplot(data = frame.data) +  
  
  geom_line(aes(x = MM, y = WS, colour = YYYY_G, group = YYYY_G), size = 1) + 
  geom_point(aes(x = MM, y = WS, colour = YYYY_G, group = YYYY_G), size = 2) + 
  # geom_smooth(aes(x = HH, y = WBGT.d, colour = Station_type, group = Station_type) , size = 2, se = F) +
  
  # geom_hline(aes(yintercept = 0)) +
  # geom_vline(aes(xintercept = 2000)) +
  scale_color_continuous( high= "darkgreen", low= "greenyellow", name = "year", labels =  c("2001-2003", "2004-2006","2007-2009","2010-2012","2013-2015","2016-2018")) +
  
  xlab("Month") + ylab("WS(m/s)") + ggtitle("2001-2018 Monthly Average of WS") +
  
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, size = 20),
        axis.title = element_text(hjust = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 20), # 將title置中
        legend.text = element_text(),
        legend.title = element_text(size = 20)
  ) 


