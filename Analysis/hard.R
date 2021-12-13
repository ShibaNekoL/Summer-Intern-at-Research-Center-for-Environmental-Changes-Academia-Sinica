options(scipen = 999)

# 用list.file列出資料夾內txt檔的檔案路徑
file.name <- list.files("C:/Users/司波達也/OneDrive/桌面 1/RCEC", 
                        pattern = ".txt", 
                        full.names = T)

# 宣告跨越檔案的list
WBGT_list <- list()

# use for to import multiple datas
for (i in 1:length(file.name)){
  
  # import data.txt
  cwb_hr <- read.table(file.name[i], skip = 77) # skip 77 rows
  
  # create data.frame with wanted variables
  all_cwb_hr <- data.frame(stno = cwb_hr[1],
                           yyyymmddhh <- cwb_hr[2],
                           TX01 = cwb_hr[5],
                           RH01 = cwb_hr[8],
                           WD01 = cwb_hr[10],
                           SS02 = cwb_hr[19])
  heading <- c("stno", "yyyymmddhh", "TX01", "RH01", "WD01", "SS02")
  names(all_cwb_hr) <- heading
  
  wbgt_in <- read.csv(file.choose())
  View(wbgt_in)
  
  ### input
  
  ## Excel test data input
  Ta <- wbgt_in$Dry.Bulb..Temperature....
  RH <- wbgt_in$Relative.Humidity....
  WS <- wbgt_in$Wind.Speed..m.s.
  SR <- wbgt_in$Solar.Radiation..W.m2.
  W <- wbgt_in$WBGT_out....
  
  
  # TX01 氣溫 (℃)
  Ta <- all_cwb_hr$TX01 # 乾球溫度 (氣溫)
  # RH01 相對濕度 (%)
  RH <- all_cwb_hr$RH01 # 相對溼度 (%)
  # WD01 平均風風速 (m/s)
  WS <- all_cwb_hr$WD01 # 大氣風速
  # SS02 全天空日射量 (MJ/㎡)
  SRMJ <- all_cwb_hr$SS02 # 太陽輻射 (W/(m2))
  # 換算  J/s = W ; 3.6MJ/m2＝1kW/m2 
  SR <- SRMJ * 1000000 / ( 60 * 60 )
  
  
  ### output
  
  # K <- Ta + 273.15  # 絕對溫度
  # y <- 9.1018 * (10 ^ (-11)) * K ^ 2 + 8.8197 * 10 ^ (-8) * K - 1.0654 * 10 ^ (-5) # 熱擴散率
  # 
  # # where mu0 and T0 are reference values given at sea level stanfard conditions. The temperature is specified in degrees Rankine:
  # mu0 = 3.62 * 10 ^ (-7) 
  # T0 = 518.7 
  # R <- K * 1.8
  # P <- 
  # mu <- mu0 * ((R / T0)^1.5) * ((T0 + 198.72) / (R + 198.72)) * 47.880257 # absolute or dynamic viscosity (N s/m2)
  # rho <- 1.293 * 9.8066500286389 # density (N/m3)
  # v <- 0.0000157 / rho  # 運動黏滯係數 kinematic viscosity
  # 
  # ## Pr = 運動黏滯係數 / 熱擴散率
  # Pr <- v / y
  

  omega <- (K / 97 - 2.9) / 0.4 * (-0.034) + 1.048
  viscosity <- 0.0000026693 * (28.97 * K) ^ 0.5 / (3.617 ^ 2 * omega)
  diamWick <- 0.007
  Rair <- 8314.34 / 28.97
  thermal_con = (1003.5 + 1.25 * 8314.34 / 28.97) * viscosity
  Pair <- 101
  density = Pair * 100 / (Rair * K)   # kg/m3
  
  Pr <- 1003.5 / (1003.5 + 1.25 * Rair)
  Re <- WS * density * diamWick / viscosity
  
  Nu <- 2.0 + 0.6 * Re ^ (1 / 2) * Pr ^ (1 / 3)  # Nu紐賽數(Nusselt number)，Re雷諾數(Reynold number)，Pr普朗特數(Prandtl number)
  h <- k / D * Nu   # 對流熱傳遞係數(h)
  
  S0 = 1367 # W/m2
  Smax = S0 * cos(θ) /d #d為太陽與地球距離
  S. = S / Smax
  fdir = exp(3 - 1.34 * S. - 1.65/S.)
  
  # Tw
  Tw <- -5.806 + 0.672 * Ta - 0.006 * Ta^2 + (0.061 + 0.004 * Ta + 99*10^(-6) * Ta^2) * RH + (-33*10^(-6) - 5*10^(-6) *Ta - 1*10^(-7) * Ta^2) * RH^2
  
  # Tg
  Tg <- 0.009624 * SR + 1.102 * Ta - 0.00404 * RH - 2.2776
  
  # calculate WBGT
  WBGT <- 0.7 * Tw + 0.2 * Tg + 0.1 * Ta 
  time_WBGT <- data.frame(time = all_cwb_hr$yyyymmddhh, cal_WBGT = ESI)
  
  # WBGT_list  
  WBGT_list[[i]] <- time_WBGT
  
}

# naming each element in WBGT list with given month
month.name <- substring(file.name, 34, 39)
names(WBGT_list) <- month.name 


# model
plot(WBGT, W, xlab="R", ylab="Excel", pch=1)
model<-lm(W~WBGT)
abline(model, col="blue", lwd=1)
summary(model)

# intergroup r
install.packages("irr")
library(irr)
m <- matrix(rbind(ESI, W), nrow=368 , byrow = TRUE)
icc(m, model = "oneway", type = "consistency", unit = "single", r0 = 0, conf.level = 0.95)