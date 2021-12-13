### each station's data

frame.st_1980_2018 <- readRDS("frame.st_1980_2018.rds")


frame.st_1998_2018 <- subset(frame.st_1980_2018, yyyy >= 1998)



# creat flag group usa
frame.st_1998_2018 $ flag <- ifelse(frame.st_1998_2018 $ WBGT < 27.78, "white",
                                    ifelse(frame.st_1998_2018 $ WBGT < 29.44, "green",
                                           ifelse(frame.st_1998_2018 $ WBGT < 31.11, "yellow",
                                                  ifelse(frame.st_1998_2018 $ WBGT < 32.22, "red",
                                                         ifelse(frame.st_1998_2018 $ WBGT >= 32.22, "black",         
                                                                NA
                                                         )
                                                  )
                                           )
                                    )
)

# creat level group tw  高氣溫戶外作業勞工熱危害預防指引                       
frame.st_1998_2018 $ level <- ifelse(frame.st_1998_2018 $ WBGT < 26.7, 0,
                                     ifelse(frame.st_1998_2018 $ WBGT < 32.2, 1,
                                            ifelse(frame.st_1998_2018 $ WBGT < 40.6, 2,
                                                   ifelse(frame.st_1998_2018 $ WBGT < 54.4, 3,
                                                          ifelse(frame.st_1998_2018 $ WBGT >= 54.4, 4,         
                                                                 NA
                                                          )
                                                   )
                                            )
                                     )
)

# 高溫作業勞工作息時間標準
# creat level group tw work                         
frame.st_1998_2018 $ level_h <- ifelse(frame.st_1998_2018 $ WBGT < 25.9, 0,
                                       ifelse(frame.st_1998_2018 $ WBGT < 27.9, 1,
                                              ifelse(frame.st_1998_2018 $ WBGT < 30.0, 2,
                                                     ifelse(frame.st_1998_2018 $ WBGT < 32.1, 3,
                                                            ifelse(frame.st_1998_2018 $ WBGT >= 32.1, 4,         
                                                                   NA
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
frame.st_1998_2018 $ rest_h <- ifelse(frame.st_1998_2018 $ level_h == 0 , 0,
                                       ifelse(frame.st_1998_2018 $ level_h == 1, 0.25,
                                              ifelse(frame.st_1998_2018 $ level_h == 2, 0.5,
                                                     ifelse(frame.st_1998_2018 $ level_h == 3, 0.75,
                                                            ifelse(frame.st_1998_2018 $ level_h == 4, 1,         
                                                                   NA
                                                            )
                                                     )
                                              )
                                       )
                                  )
# creat level group tw work                         
frame.st_1998_2018 $ level_m <- ifelse(frame.st_1998_2018 $ WBGT < 28.0, 0,
                                       ifelse(frame.st_1998_2018 $ WBGT < 29.4, 1,
                                              ifelse(frame.st_1998_2018 $ WBGT < 31.1, 2,
                                                     ifelse(frame.st_1998_2018 $ WBGT < 32.6, 3,
                                                            ifelse(frame.st_1998_2018 $ WBGT >= 32.6, 4,         
                                                                   NA
                                                            )
                                                     )
                                              )
                                       )
)
frame.st_1998_2018 $ rest_m <- ifelse(frame.st_1998_2018 $ level_m == 0 , 0,
                                      ifelse(frame.st_1998_2018 $ level_m == 1, 0.25,
                                             ifelse(frame.st_1998_2018 $ level_m == 2, 0.5,
                                                    ifelse(frame.st_1998_2018 $ level_m == 3, 0.75,
                                                           ifelse(frame.st_1998_2018 $ level_m == 4, 1,         
                                                                  NA
                                                           )
                                                    )
                                             )
                                      )
)
# creat level group tw work                         
frame.st_1998_2018 $ level_l <- ifelse(frame.st_1998_2018 $ WBGT < 30.6, 0,
                                       ifelse(frame.st_1998_2018 $ WBGT < 31.4, 1,
                                              ifelse(frame.st_1998_2018 $ WBGT < 32.2, 2,
                                                     ifelse(frame.st_1998_2018 $ WBGT < 33.0, 3,
                                                            ifelse(frame.st_1998_2018 $ WBGT >= 33.0, 4,         
                                                                   NA
                                                            )
                                                     )
                                              )
                                       )
)
frame.st_1998_2018 $ rest_l <- ifelse(frame.st_1998_2018 $ level_l == 0 , 0,
                                      ifelse(frame.st_1998_2018 $ level_l == 1, 0.25,
                                             ifelse(frame.st_1998_2018 $ level_l == 2, 0.5,
                                                    ifelse(frame.st_1998_2018 $ level_l == 3, 0.75,
                                                           ifelse(frame.st_1998_2018 $ level_l == 4, 1,         
                                                                  NA
                                                           )
                                                    )
                                             )
                                      )
)

## list.st : import list of all station 
list.st <- read.csv("/data1/home/hsnutardis/st.csv", header = T)

# rename list.st
names(list.st) <- c("stname_zh", "stno_en", "stno", "lng", "lat", "asl_m", "city", "address", "date", "density", "population", "area", "type")
# delete the last character "m" of vairable asl, turn it into numeric
list.st $ asl_m <- as.numeric(gsub("m$", "", as.character(list.st $ asl_m))) 



# select one station

frame.st_1998_2018 <- merge(frame.st_1998_2018, subset(list.st, select = c(stno, density, population, area, type)), by = "stno")

saveRDS(frame.st_1998_2018, "frame.st_1998_2018.rds")   




### each station's data

frame.st_1998_2018 <- readRDS("frame.st_1998_2018.rds")
frame.st_1998_2018_naomit <- subset(frame.st_1998_2018, is.na(WBGT) == F)


saveRDS(frame.st_1998_2018_naomit, "frame.st_1998_2018_naomit.rds")


