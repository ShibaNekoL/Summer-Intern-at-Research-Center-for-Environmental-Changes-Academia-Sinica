frame.st_1980_2018 <- readRDS("frame.st_1980_2018.rds")

## list.st : import list of all station 
list.st <- read.csv("/data1/home/hsnutardis/st.csv", header = T)

# rename list.st
names(list.st) <- c("stname_zh", "stno_en", "stno", "lng", "lat", "asl_m", "city", "address", "date", "density", "population", "area", "type")

frame.st_1980_2018 <- merge(frame.st_1980_2018, subset(list.st, select = c(stno, density, population, area, type)), by = "stno")


frame.st_1980_2018_naomit <- subset(frame.st_1980_2018, is.na(WBGT) == F)


saveRDS(frame.st_1980_2018_naomit, "frame.st_1980_2018_naomit.rds")
