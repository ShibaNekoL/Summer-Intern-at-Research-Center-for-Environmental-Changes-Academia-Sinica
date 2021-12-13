options(scipen <- 999)

# 用list.file列出資料夾內txt檔的檔案路徑
file.name <- list.files("C:/Users/司波達也/OneDrive/桌面 1/RCEC", 
                        pattern <- ".txt", 
                        full.names <- T)

# 宣告跨越檔案的list
WBGT_list <- list()

# use for to import multiple datas
for (i in 1:length(file.name)){
  
  # import data.txt
  cwb_hr <- read.table(file.name[i], skip = 77) # skip 77 rows
  
  # create data.frame with wanted variables
  all_cwb_hr <- data.frame(stno <- cwb_hr[1],
                           yyyymmddhh <- cwb_hr[2],
                           TX01 <- cwb_hr[5],
                           RH01 <- cwb_hr[8],
                           WD01 <- cwb_hr[10],
                           SS02 <- cwb_hr[19])
  heading <- c("stno", "yyyymmddhh", "TX01", "RH01", "WD01", "SS02")
  names(all_cwb_hr) <- heading
  
  wbgt_in <- read.csv(file.choose())
  View(wbgt_in)
  
  ### input
  
  # ## Excel test data input
  # Ta <- wbgt_in$Dry.Bulb..Temperature....
  # RH <- wbgt_in$Relative.Humidity....
  # WS <- wbgt_in$Wind.WS..m.s.
  # SR <- wbgt_in$Solar.Radiation..W.m2.
  # W <- wbgt_in$WBGT_out....
  
  
  # TX01 氣溫 (℃)
  Ta <- all_cwb_hr$TX01 # 乾球溫度 (氣溫)
  # RH01 相對濕度 (%)
  RH <- all_cwb_hr$RH01 # 相對溼度 (%)
  # WD01 平均風風速 (m/s)
  WS <- all_cwb_hr$WD01 # 大氣風速
  # SS02 全天空日射量 (MJ/㎡)
  SRMJ <- all_cwb_hr$SS02 # 太陽輻射 (W/(m2))
  # 換算  J/s <- W ; 3.6MJ/m2＝1kW/m2
  SR <- SRMJ * 1000000 / ( 60 * 60 )

  
  ### output
  
  
  # ## 計算濕球溫度
  # 
  # Tw <- -5.806 + 0.672 * Ta - 0.006 * Ta^2 + (0.061 + 0.004 * Ta + 99*10^(-6) * Ta^2) * RH + (-33*10^(-6) - 5*10^(-6) *Ta - 1*10^(-7) * Ta^2) * RH^2
  # 
  # ## 計算乾球溫度
  # 
  # # Calculation of dew point from RH
  # 
  # r <- (17.27 * Ta) / (237.7 + Ta) + ln(RH / 100)
  # Td <- (237.7 * r) / (17.27 - r)
  # 
  # # Qd 為逐日之太陽漫射量
  # # Qg 為逐時全天空日射量
  # # Qa 為Angot 值，可由下式算出
  # # S <- Qd/Qg + Qg/Qa <- 0.97 
  # # Diffuse Radiation as Related to Global Radiation and the Angot Value (Jen-hu Chang)
  # 
  # Qg <- SR 
  # Qa <- 
  # Qd <- (0.97 - Qg / Qa) * Qg 
  # 
  # P <- 
  # ea <- exp(17.67 * (Td - Ta) / (Td + 243.5)) * (1.0007 + 0.00000346 * P) * 6.112 * exp(17.502 * Ta / (240.97 + Ta))
  # epa <- 0.575 * ea^(1 / 7)
  # fdb <- Qg - Qd
  # fdif <- Qd
  # sigma <- 5.67 * 10^(-8)
  # h <- 0.315
  # z <- 
  # 
  # B <- SR * (fdb / (4 * sigma * cos(z)) + (1.2 / sigma) * fdif) + epa * Ta^4
  # C <- (h * WS^0.58) / (5.3865 * 10^(-8))
  # Tg <- (B + C * Ta + 7680000) / (C + 256000)

  #######################################################################  
  ### Function h_sphere_in_air_withdiam(Tair, Pair, WS, WSMin, diam)
  # Purpose: to calculate the convective heat tranfer coefficient for flow around a sphere.
  # Reference: Bird, Stewart, and Lightfoot (BSL), page 409.
  # Rair <- 8314.34 / 28.97
  # Pr <- 1003.5 / (1003.5 + 1.25 * Rair)
  # thermal_con <- (1003.5 + 1.25 * 8314.34 / 28.97) * viscosity
  # density <- Pair * 100 / (Rair * Tair)   # kg/m3
  # if(WS < WSMin){
  #   WS <- WSMin
  # }
  # Re <- WS * density * diam / viscosity
  # Nu <- 2 + 0.6 * Re ^ 0.5 * Pr ^ 0.3333
  # h_sphere_in_air_withdiam <- Nu * thermal_con / diam # W/(m2 K)
  # 
  # #######################################################################
  # ### fTg_withdiam(Ta, relh, WS, solar, diam)
  # fdir <- 0.8  # Assume a proportion of direct radiation <- direct/(diffuse + direct)
  # zenith <- 0  # angle of sun from directly above
  # WSMin <- 0.1   #　0 wind WS upsets log function
  # Pair <- 101
  # # Purpose: to calculate the globe temperature
  # #   Author:  James C. Liljegren
  # #   Decision and Information Sciences Division
  # #   Argonne National Laboratory
  # #   Pressure in kPa (Atm <-101 kPa)
  # # Fix up out-of bounds problems with zenith
  # if(zenith <<- 0){
  #   zenith <- 0.0000000001
  # }
  # if(zenith > 1.57){
  #   zenith <- 1.57
  # }
  # Pair <- Pair * 10
  # cza <- cos(zenith)
  # converge <- 0.05
  # alb_sfc <- SurfAlbedo
  # alb_globe <- 0.05
  # emis_globe <- 0.95
  # emis_sfc <- 0.999
  # Tair <- Ta + 273.15
  # RH <- relh * 0.01
  # Tsfc <- Tair
  # Tglobe_prev <- Tair
  # 
  # for(i in 1:1000){
  #   Tref <- 0.5 * (Tglobe_prev + Tair) # Evaluate properties at the average temperature
  #   h <- h_sphere_in_air_withdiam
  #   Tglobe <- (0.5 * (emis_atm * Tair ^ 4 + emis_sfc * Tsfc ^ 4) - h / (emis_globe * stefanb) * (Tglobe_prev - Tair) + solar / (2 * emis_globe * stefanb) * (1 - alb_globe) * (fdir * (1 / (2 * cza) - 1) + 1 + alb_sfc)) ^ 0.25
  #   dT <- Tglobe - Tglobe_prev
  #   if(abs(dT) < converge) {
  #     Tglobe <- Tglobe - 273.15
  #   }
  # }
  # fTg_withdiam <- Tglobe
  # 
  # 
  # #######################################################################  
  # ### Function viscosity(Tair)
  # # Purpose: Compute the viscosity of air, kg/(m s) given temperature, K
  # # Reference: BSL, page 23.
  # omega <- (Tair / 97 - 2.9) / 0.4 * (-0.034) + 1.048
  # viscosity <- 0.0000026693 * (28.97 * Tair) ^ 0.5 / (3.617 ^ 2 * omega)
  # 
  # 
  # #######################################################################      
  # ### Function esat(Tk)
  # #   Purpose: calculate the saturation vapor pressure (mb) over liquid water given the temperature (K).
  # #   Reference: Buck# s (1981) approximation (eqn 3) of Wexler# s (1976) formulae.
  # #   over liquid water
  # esat <- 6.1121 * exp(17.502 * (Tk - 273.15) / (Tk - 32.18))
  # esat <- 1.004 * esat  #  correction for moist air, if pressure is not available; for pressure > 800 mb
  # 
  # 
  # ####################################################################### 
  # ### Function emis_atm(Ta, RH)
  # #   Reference: Oke (2nd edition), page 373.
  # e <- RH * esat
  # emis_atm <- 0.575 * e ^ 0.143  
  # 
  # 
  # #######################################################################     
  # ### Function h_sphere_in_air(Tair, Pair, WS, WSMin)
  # #   Purpose: to calculate the convective heat tranfer coefficient for flow around a sphere.
  # #   Reference: Bird, Stewart, and Lightfoot (BSL), page 409.
  # Rair <- 8314.34 / 28.97
  # Pr <- 1003.5 / (1003.5 + 1.25 * Rair)
  # thermal_con <- (1003.5 + 1.25 * 8314.34 / 28.97) * viscosity
  # density <- Pair * 100 / (Rair * Tair)   #  kg/m3
  # ifelse(WS < WSMin, WSMin, WS)
  # Re <- WS * density * diamGlobe / viscosity
  # Nu <- 2 + 0.6 * Re ^ 0.5 * Pr ^ 0.3333
  # h_sphere_in_air <- Nu * thermal_con / diamGlobe #  W/(m2 K)
  # 
  # 
  # #######################################################################  
  # ### Function diffusivity(Tair, Pair)
  # #   Purpose: compute the diffusivity of water vapor in air, m2/s
  # #   Reference: BSL, page 505.
  # pcrit13 <- (36.4 * 218) ^ (1 / 3)
  # tcrit512 <- (132 * 647.3) ^ (5 / 12)
  # Tcrit12 <- (132 * 647.3) ^ 0.5
  # Mmix <- (1 / 28.97 + 1 / 18.015) ^ 0.5
  # diffusivity <- 0.000364 * (Tair / Tcrit12) ^ 2.334 * pcrit13 * tcrit512 * Mmix / (Pair / 1013.25) * 0.0001
  # 
  # #######################################################################    
  # ### Function h_cylinder_in_air(Tair, Pair, WS, WSMin)
  # # Purpose: to calculate the convective heat transfer coefficient for a long cylinder in cross flow.
  # #   Reference: Bedingfield and Drew, eqn 32
  # #   Author:  James C. Liljegren
  # #        Decision and Information Sciences Division
  # #        Argonne National Laboratory
  # 
  # Pr <- 1003.5 / (1003.5 + 1.25 * 8314.34 / 28.97)
  # thermal_con <- (1003.5 + 1.25 * 8314.34 / 28.97) * viscosity
  # density <- Pair * 100 / (8314.34 / 28.97 * Tair)
  # ifelse(WS < WSMin, WSMin, WS)
  # Re <- WS * density * diamWick / viscosity
  # Nu <- 0.281 * Re ^ 0.6 * Pr ^ 0.44
  # h_cylinder_in_air <- Nu * thermal_con / diamWick  #  W/(m2 K)
  #   
  # 
  # #######################################################################   
  # ### Function es(Ta)
  # #  calculates saturation vapour pressure over water in hPa for input air temperature (ta) in celsius according to:
  # #  Hardy, R.; ITS-90 Formulations for Vapor Pressure, Frostpoint Temperature, Dewpoint Temperature and Enhancement Factors in the Range -100 to 100 蚓;
  # #  Proceedings of Third International Symposium on Humidity and Moisture; edited by National Physical Laboratory (NPL), London, 1998, pp. 214-221
  # #  http://www.thunderscientific.com/tech_info/reflibrary/its90formulas.pdf (retrieved 2008-10-01)
  # Tk <- Ta + 273.15  #  air temp in K
  # es <- 2.7150305 * log(Tk) - 2836.5744 * Tk ^ (-2) - 6028.076559 / Tk + 19.54263612 - 0.02737830188 * Tk + 0.000016261698 * Tk ^ 2 + 7.0229056E-10 * Tk ^ 3 - 1.8680009E-13 * Tk ^ 4
  # es <- exp(es) * 0.01 #  *0.01: convert Pa to hPa
  #   
  #   
  # 
  # 
  # 
  # ### Tw
  # 
  # ### Tg
  # 

  
  ### calculate WBGT

  WBGT <- 0.7 * Tw + 0.2 * Tg + 0.1 * Ta
  
  # Estimate ESI
  ESI <- 0.63 * Ta - 0.03 * RH + 0.002 * SR + 0.0054 * (Ta * RH) - 0.073 * ((0.1 + SR) ^ (-1))
  
  time_WBGT <- data.frame(time <- all_cwb_hr$yyyymmddhh, cal_WBGT <- ESI)
  
  # WBGT_list  
  WBGT_list[[i]] <- time_WBGT
  
}

# naming each element in WBGT list with given month
month.name <- substring(file.name, 34, 39)
names(WBGT_list) <- month.name 

# model
plot(ESI, W, xlab<-"R", ylab<-"Excel", pch<-1)
model<-lm(W~ESI)
abline(model, col<-"blue", lwd<-1)
summary(model)

# intergroup r
install.packages("irr")
library(irr)
m <- matrix(rbind(ESI, W), nrow<-368 , byrow <- TRUE)
icc(m, model <- "oneway", type <- "consistency", unit <- "single", r0 <- 0, conf.level <- 0.95)
