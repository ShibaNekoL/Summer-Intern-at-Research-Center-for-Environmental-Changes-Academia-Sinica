### each station's data


dataframe_1980_2018_NA <- readRDS("dataframe_1980_2018_NA.rds")

# C:/Users/司波達也/OneDrive/桌面 1/RCEC/st.csv
# /data1/home/hsnutardis/st.csv

## list.st : import list of all station 
list.st <- read.csv("/data1/home/hsnutardis/st.csv", header = T)

# rename list.st
names(list.st) <- c("stname_zh", "stno_en", "stno", "lng", "lat", "asl_m", "city", "address", "date")
# delete the last character "m" of vairable asl, turn it into numeric
list.st $ asl_m <- as.numeric(gsub("m$", "", as.character(list.st $ asl_m))) 

# create data.frame
frame.st_1980_2018 <- dataframe_1980_2018_NA

# select one station

frame.st_1980_2018 <- merge(frame.st_1980_2018, list.st, by = "stno")

frame.st_1980_2018$yyyy <- as.integer(frame.st_1980_2018$yyyy)
frame.st_1980_2018$mm <- as.integer(frame.st_1980_2018$mm)
frame.st_1980_2018$dd <- as.integer(frame.st_1980_2018$dd)
frame.st_1980_2018$hh <- as.integer(frame.st_1980_2018$hh)

# creat season group
frame.st_1980_2018 $ season <- ifelse(frame.st_1980_2018 $ mm == 3 | frame.st_1980_2018 $ mm == 4 | frame.st_1980_2018 $ mm == 5, "MAM",
                                      ifelse(frame.st_1980_2018 $ mm == 6 | frame.st_1980_2018 $ mm == 7 | frame.st_1980_2018 $ mm == 8, "JJA",
                                             ifelse(frame.st_1980_2018 $ mm == 9 | frame.st_1980_2018 $ mm == 10 | frame.st_1980_2018 $ mm == 11, "SON",
                                                    ifelse(frame.st_1980_2018 $ mm == 12 | frame.st_1980_2018 $ mm == 1 | frame.st_1980_2018 $ mm == 2, "DJF", NA)
                                             )
                                      )
)



saveRDS(frame.st_1980_2018, "frame.st_1980_2018.rds")   