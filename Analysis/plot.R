
st_seasonframe <- readRDS("st_seasonframe.rds")
# 
# library(ggplot2)
# library(digest)
# 
# png("Combination of Plots.png", width=600, height=600)

ggplot() +  
  
  geom_line(data = seasonframe, aes(x = hh, y = spring, colour = "spring"), size = 1) + 
  geom_point(data = seasonframe, aes(x = hh, y = spring, colour = "spring"), size = 2) +
  
  geom_line(data = seasonframe, aes(x = hh, y = summer, colour ="summer"), size = 1) + 
  geom_point(data = seasonframe, aes(x = hh, y = summer, colour = "summer"), size = 2)+
  
  geom_line(data = seasonframe, aes(x = hh, y = fall, colour ="fall"), size = 1) + 
  geom_point(data = seasonframe, aes(x = hh, y = fall, colour = "fall"), size = 2)+
  
  geom_line(data = seasonframe, aes(x = hh, y = winter, colour ="winter"), size = 1) + 
  geom_point(data = seasonframe, aes(x = hh, y = winter, colour = "winter"), size = 2)+
  
  scale_colour_manual("", values = c("spring" = "darkgreen", "summer" = "pink2", "fall" = "orange", "winter" = "blue2")) +
  xlab("hour") + ylab("WBGT") + ggtitle("Daily change of WBGT of each season for 1980-2018 at Taipei")
  theme()


  
# dev.off()
  
  
ggplot() +  
  
  geom_line(data = seasonframe, aes(x = yyyy, y = winter), size = 1) + 
  geom_point(data = seasonframe, aes(x = yyyy, y = winter), size = 2) + 
  
  geom_smooth(data = seasonframe, aes(x = yyyy, y = winter), method ="lm" , size = 1) +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 2000)) +
  
  xlab("year") + ylab("Ta") + ggtitle("Winter yearly change of Ta of each station for 1980-2018") +
  
  theme(axis.text.x = element_text(vjust = 0.5, size = 14),
        axis.text.y = element_text(vjust = 0.5, size = 14),
        axis.title = element_text(hjust = 0.5, size = 16),
        plot.title = element_text(hjust = 0.5, size = 16)
  ) # 將title置中
