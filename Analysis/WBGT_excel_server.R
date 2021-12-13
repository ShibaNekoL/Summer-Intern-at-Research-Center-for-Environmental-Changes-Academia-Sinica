##################################################################

### 宣告

SurfAlbedo <- 0.4
stefanb <- 0.000000056696
diamGlobe <- 0.05 # 0.05 <- 50mm diam globe
diamWick <- 0.007
lenWick <- 0.0254
propDirect <- 0.8  # Assume a proportion of direct radiation <- direct/(diffuse + direct)
ZenithAngle <- 0  # angle of sun from directly above
MinWindSpeed <- 0.1   # 0 wind speed upsets log function
AtmPressure <- 101  # Atmospheric pressure in kPa

##################################################################


# 用list.file列出資料夾內txt檔的檔案路徑
file.path <- list.files("/archive/Observation/CWB_Station/hr/", 
                        pattern = ".txt", 
                        full.names = T)

# 用list.file列出資料夾內txt檔的檔案路徑
file.name <- list.files("/archive/Observation/CWB_Station/hr/", 
                        pattern = ".txt", 
                        full.names = F)


# 宣告跨越檔案的list
DATA_list <- list()

# use for to import multiple datas
for (i in 1 : length(file.path)){
  
  
  ### importing data 
  
  # import data.txt 分列讀取
  cwb_hr_delete <- read.csv(file.path[i])
  
  # 擷取每列第一個字串
  first_list <- list()
  for(j in 1 : nrow(cwb_hr_delete)){
    first_list[j] <- substr(cwb_hr_delete[j, ], 1, 1)
  }
  
  # 找到最後出現#的列數
  m <- max(grep("#", first_list))
  
  # 刪除#之前的列
  cwb_hr_input <- read.table(file.path[i], skip = m) # skip m rows
  
  # 分割變項名稱
  header <- unlist(strsplit(as.character(cwb_hr_delete[m, ]), split = "   "))
  head <- unlist(strsplit(header[1], split = " "))
  head <- head[-1]
  header <- header[-1] 
  header <- append(header, head, after = 0)
  
  # 命名各行變項
  names(cwb_hr_input) <- header
  
  ### import vairables
  
  # TX01 氣溫 (℃)
  Ta <- cwb_hr_input $ TX01 # 乾球溫度 (氣溫)
  # RH01 相對濕度 (%)
  RH <- cwb_hr_input $ RH01 # 相對溼度 (%)
  # WD01 平均風風速 (m/s)
  WS <- cwb_hr_input $ WD01 # 大氣風速
  # SS02 全天空日射量 (MJ/㎡)
  SRMJ <- cwb_hr_input $ SS02 # 太陽輻射 (W/(m2))
  
  
  ### data cleaning
  
  Ta <- ifelse(Ta <= -9991, NA, Ta)
  RH <- ifelse(RH <= -9991, NA, RH)
  WS <- ifelse(WS <= -9991, NA, WS)
  SRMJ <- ifelse(SRMJ <= -9991, NA, SRMJ)
  
  # 換算  J/s <- W ; 3.6MJ/m2＝1kW/m2
  SR <- SRMJ * 1000000 / ( 60 * 60 )
  
  # Dim Edifference, Twguess, Ctemp, MBpressure, RH, E, previoussign, cursign, incr As Variant
  # Ewguess, Eguess, es, i, j, k, wetbulb, a, 
  # dry, wet, pres, humi, globe, rad, wind, wbgt_out, heat_index, 
  # Ta, WS, SR, RH
  
  # wbgt_in <- read.csv(file.choose())
  # Ta <- wbgt_in$Dry.Bulb..Temperature....
  # SR <- wbgt_in$Solar.Radiation..W.m2.
  # WS <- wbgt_in$Wind.Speed..m.s.
  # RH <- wbgt_in$Relative.Humidity....
  
  ### global variables
  Tnwb <- numeric()
  Tg <- numeric()
  Td <- numeric()
  
  ##################################################################
  
  ##################################################################
  
  ### Function fWBGTo(Ta, Tg, Td, WS, SR, RH, Tnwb)
  
  func.fWBGTo <- function(Ta, Tg, Td, WS, SR, RH, Tnwb){
    
    # check for blank entry
    
    # Calculate RH given Td, and Td given RH
    Td <- func.fTd(Ta, RH)
    
    # Check to make sure Td < Ta
    
    # Calculate Tg given SR, calculate SR given Tg
    Tg <<- func.fTg(Ta, RH, AtmPressure, WS, SR, propDirect, ZenithAngle, MinWindSpeed)
    
    # Calculate SR by iteration of Tg - in big steps then small steps
    # for(sol in seq(1, 2000, by = 100)){
    #   estTg <- func.fTg(Ta, RH, AtmPressure, WS, sol, propDirect, ZenithAngle, MinWindSpeed)
    #   smallsol <- ifelse(estTg >= Tg, sol, smallsol)
    # }
    # for(sol in seq(smallsol - 100, smallsol, by = 10)){
    #   estTg <- func.fTg(Ta, RH, AtmPressure, WS, sol, propDirect, ZenithAngle, MinWindSpeed)
    #   smallsol <- ifelse(estTg >= Tg, sol, smallsol)
    # }
    
    Tnwb <<- func.fTwb(Ta, Td, RH, AtmPressure, WS, MinWindSpeed, SR, propDirect, ZenithAngle, irad = 1)
    fWBGTo <- 0.7 * Tnwb + 0.2 * Tg + 0.1 * Ta
    
    return(fWBGTo)
  }
  
  
  ##################################################################
  
  ### 8
  ### Function fTwb(Ta, Td, relh, Pair, speed, speedMin, SR, fdir, zenith, irad)
  #   Purpose: to calculate the natural wet bulb temperature
  #   Author:  James C. Liljegren
  #        Decision and Information Sciences Division
  #        Argonne National Laboratory
  #  irad<-1 -> include radiation; irad<-0 -> no radiation (psychrometric web bulb temp)
  #  Pressure in kPa (Atm <-101 kPa)
  
  func.fTwb <- function(Ta, Td, relh, Pair, speed, speedMin, SR, fdir, zenith, irad){
    
    fdir <- propDirect
    Pair <- Pair * 10 # 1010
    emis_wick <- 0.95
    alb_wick <- 0.4
    emis_sfc <- 0.999
    alb_sfc <- SurfAlbedo
    converge <- 0.05
    ratio <- 1003.5 * 28.97 / 18.015
    Pr <- 1003.5 / (1003.5 + (1.25 * 8314.34 / 28.97))
    # Fix up out-of bounds problems with zenith
    zenith <- rep(zenith, times = length(SR))
    
    for(i in 1 : length(SR)){
      zenith[i] <- ifelse(zenith[i] <= 0, 0.0000000001, zenith[i])
      zenith[i] <- ifelse(SR[i] > 0 && zenith[i] > 1.57, 1.57, zenith[i])
      zenith[i] <- ifelse(SR[i] > 15 && zenith[i] > 1.54, 1.54, zenith[i])
      zenith[i] <- ifelse(SR[i] > 900 && zenith[i] > 1.52, 1.52, zenith[i])
    }
    
    Tdew <- Td + 273.15
    Tair <- Ta + 273.15
    RH <- relh * 0.01
    eair <- RH * func.esat(Tair)
    emis_at <- 0.575 * eair ^ 0.143
    Tsfc <- Tair
    Twb_prev <- Tdew #  First guess is the dew point temperature
    
    # Do iteration
    
    for(testno in 1 : 1000){
      Tref <- 0.5 * (Twb_prev + Tair) #  Evaluate properties at the average temperature
      evap <- (313.15 - Twb_prev) / 30 * (-71100) + 2407300
      h <- func.h_cylinder_in_air(Twb_prev, Pair, speed, speedMin)
      Fatm <- stefanb * emis_wick * (0.5 * (emis_at * Tair ^ 4 + emis_sfc * Tsfc ^ 4) - Twb_prev ^ 4) + (1 - alb_wick) * SR * ((1 - fdir) * (1 + 0.25 * diamWick / lenWick) + ((tan(zenith) / 3.1416) + 0.25 * diamWick / lenWick) * fdir + alb_sfc)
      ewick <- func.esat(Twb_prev)
      density <- Pair * 100 / (Tair * 8314.34 / 28.97)
      Sc <- func.viscosity(Tair) / (density * func.diffusivity(Tref, Pair))
      Twb <- Tair - evap / ratio * (ewick - eair) / (Pair - ewick) * (Pr / Sc) ^ 0.56 + Fatm / h * irad
      dT <- Twb - Twb_prev
      
      Twb_prev <- ifelse(dT < -converge, 0.9 * Twb_prev - 0.1 * Twb, Twb_prev)
      Twb_prev <- ifelse(dT > converge, 0.9 * Twb_prev + 0.1 * Twb, Twb_prev)
      ifelse(all(abs(dT) <= converge), break, dT)
    }
    fTwb <- Twb - 273.15
    return(fTwb)
  }
  
  
  ##################################################################
  
  ### 2
  ### Function fTg(Ta, relh, Pair, speed, SR, fdir, zenith, speedMin)
  #   Purpose: to calculate the globe temperature
  #   Author:  James C. Liljegren
  #        Decision and Information Sciences Division
  #        Argonne National Laboratory
  #  Pressure in kPa (Atm <-101 kPa)
  
  func.fTg <- function(Ta, relh, Pair, speed, SR, fdir, zenith, speedMin){
    
    # Fix up out-of bounds problems with zenith
    zenith <- ifelse(zenith <= 0, 0.0000000001, ifelse(zenith > 1.57, 1.57, zenith))
    
    Pair <- Pair * 10
    cza <- cos(zenith)
    converge <- 0.05
    alb_sfc <- SurfAlbedo
    alb_globe <- 0.05
    emis_globe <- 0.95
    emis_sfc <- 0.999
    Tair <- Ta + 273.15
    RH <- relh * 0.01
    Tsfc <- Tair
    Tglobe_prev <- Tair
    
    # Do iteration
    
    for (testno in 1 : 1000){
      Tref <- 0.5 * (Tglobe_prev + Tair) #  Evaluate properties at the average temperature
      h <- func.h_sphere_in_air(Tref, Pair, speed, speedMin)
      Tglobe <- (0.5 * (func.emis_atm(Tair, RH) * Tair ^ 4 + emis_sfc * Tsfc ^ 4) - h / (emis_globe * stefanb) * (Tglobe_prev - Tair) + SR / (2 * emis_globe * stefanb) * (1 - alb_globe) * (fdir * (1 / (2 * cza) - 1) + 1 + alb_sfc)) ^ 0.25
      dT <- Tglobe - Tglobe_prev
      
      Tglobe_prev <- ifelse(abs(dT) >= converge , 0.9 * Tglobe_prev + 0.1 * Tglobe, Tglobe_prev)
      Tglobe <- ifelse(abs(dT) < converge , Tglobe - 273.15, Tglobe)
      ifelse(all(abs(dT) < converge), break, dT) # <
    }
    
    fTg <- Tglobe
    return(fTg)
  }
  
  ##################################################################
  
  ### 1 
  ### Function fTd(Ta, RH)
  # Calculation of dew point from RH
  func.fTd <- function(Ta, RH){
    RHD <- RH / 100
    fTd <- 237.3 * (log(RHD, base = exp(1)) / 17.27 + Ta / (237.3 + Ta)) / (1 - log(RHD, base = exp(1)) / 17.27 - Ta / (237.3 + Ta))
    return(fTd)
  }
  
  
  ##################################################################
  
  ### 10
  ### Function h_cylinder_in_air(Tair, Pair, speed, speedMin)
  #  Purpose: to calculate the convective heat transfer coefficient for a long cylinder in cross flow.
  #  Reference: Bedingfield and Drew, eqn 32
  #  Author:  James C. Liljegren
  #       Decision and Information Sciences Division
  #       Argonne National Laboratory
  
  func.h_cylinder_in_air <- function(Tair, Pair, speed, speedMin){
    
    Pr <- 1003.5 / (1003.5 + 1.25 * 8314.34 / 28.97)
    thermal_con <- (1003.5 + 1.25 * 8314.34 / 28.97) * func.viscosity(Tair)
    density <- Pair * 100 / (8314.34 / 28.97 * Tair) # 1010
    speed <- ifelse(speed < speedMin, speedMin, speed)
    Re <- speed * density * diamWick / func.viscosity(Tair)
    Nu <- 0.281 * Re ^ 0.6 * Pr ^ 0.44
    h_cylinder_in_air <- Nu * thermal_con / diamWick  # W/(m2 K)
    
    return(h_cylinder_in_air)
  }
  
  
  ##################################################################
  
  ### 9
  ### Function diffusivity(Tair, Pair)
  #  Purpose: compute the diffusivity of water vapor in air, m2/s
  #  Reference: BSL, page 505.
  
  func.diffusivity <- function(Tair, Pair){
    
    pcrit13 <- (36.4 * 218) ^ (1 / 3)
    tcrit512 <- (132 * 647.3) ^ (5 / 12)
    Tcrit12 <- (132 * 647.3) ^ 0.5
    Mmix <- (1 / 28.97 + 1 / 18.015) ^ 0.5
    diffusivity <- 0.000364 * (Tair / Tcrit12) ^ 2.334 * pcrit13 * tcrit512 * Mmix / (Pair / 1013.25) * 0.0001
    
    return(diffusivity)
  }
  
  
  ##################################################################
  
  ### 3
  ### Function h_sphere_in_air(Tair, Pair, speed, speedMin) 
  #  Purpose: to calculate the convective heat tranfer coefficient for flow around a sphere.
  #  Reference: Bird, Stewart, and Lightfoot (BSL), page 409.
  
  func.h_sphere_in_air <- function(Tair, Pair, speed, speedMin){
    
    Rair <- 8314.34 / 28.97
    Pr <- 1003.5 / (1003.5 + 1.25 * Rair)
    thermal_con <- (1003.5 + 1.25 * 8314.34 / 28.97) * func.viscosity(Tair)
    
    ### 5
    
    density <- Pair * 100 / (Rair * Tair)   # kg/m3 # Pair = 1010
    speed <- ifelse(speed < speedMin, speedMin, speed)
    Re <- speed * density * diamGlobe / func.viscosity(Tair)
    Nu <- 2 + 0.6 * Re ^ 0.5 * Pr ^ 0.3333
    h_sphere_in_air <- Nu * thermal_con / diamGlobe # W/(m2 K)
    
    return(h_sphere_in_air)
  }
  
  
  ##################################################################
  
  
  ### 6
  ### Function emis_atm(Ta, RH)
  #   Reference: Oke (2nd edition), page 373.
  func.emis_atm <- function(Ta, RH){
    e <- RH * func.esat(Ta)
    emis_atm <- 0.575 * e ^ 0.143
    return(emis_atm)
  }
  
  
  ##################################################################
  
  ### 7
  ### Function esat(Tk)
  #  Purpose: calculate the saturation vapor pressure (mb) over liquid water given the temperature (K).
  #  Reference: Buck# s (1981) approximation (eqn 3) of Wexler# s (1976) formulae.
  #  over liquid water
  func.esat <- function(Tk){
    esat <- 6.1121 * exp(17.502 * (Tk - 273.15) / (Tk - 32.18))
    esat <- 1.004 * esat  # correction for moist air, if pressure is not available; for pressure > 800 mb
    return(esat)
  }
  
  
  ##################################################################
  
  ### 4
  ### Function viscosity(Tair)
  #  Purpose: Compute the viscosity of air, kg/(m s) given temperature, K
  #  Reference: BSL, page 23.
  
  func.viscosity <- function(Tair){
    omega <- (Tair / 97 - 2.9) / 0.4 * (-0.034) + 1.048
    viscosity <- 0.0000026693 * (28.97 * Tair) ^ 0.5 / (3.617 ^ 2 * omega)
    return(viscosity)
  }
  
  
  ##################################################################
  
  # create data.frame with wanted variables
  WBGT <- func.fWBGTo(Ta, Tg, Td, WS, SR, RH, Tnwb)
  
  cwb_hr_output <- data.frame(stno = cwb_hr_input $ stno ,
                              yyyymmddhh = cwb_hr_input $ yyyymmddhh ,
                              Ta = cwb_hr_input $ TX01 , 
                              RH = cwb_hr_input $ RH01 , 
                              WS = cwb_hr_input $ WD01 ,
                              SR = cwb_hr_input $ SS02 ,
                              Tnwb, Tg, WBGT
  )
  
  # DATA_list  
  DATA_list[[i]] <- cwb_hr_output
}


# naming each element in WBGT list with given month
yyyymm.name <- substring(file.name, 1, 6)
names(DATA_list) <- yyyymm.name
