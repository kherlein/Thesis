#ImportData
gis.data <- read.csv(file="GISData_Oct0119.csv", header=TRUE, sep=",")
colnames(gis.data)[colnames(gis.data)=="CAPTURE_DA"] <- "weather_date"
all.weather <- read.csv(file="20174stations.csv")
operations <- read.csv(file="FakeCrewData.csv")



require(dplyr)
require(lubridate)

rh.max <- all.weather %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(FireID, weather_date) %>% 
  summarize(RH_max = max(relative_humidity), temp_max = max(dry_bulb_temperature), wind_max = max(wind_gust_kmh, na.rm = TRUE ), agric_wind_max = max(wind_agric)) %>%
  as.data.frame()
  
gis.data.date <- gis.data %>%
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d"))

operations.date <- operations %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d"))



#checking that date conversion is correct
is.Date(gis.data.date$weather_date)
is.Date(rh.max$weather_date)

join.gis.weather <-  left_join(gis.data.date, rh.max, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.gis.weather.ops <- left_join(join.gis.weather, operations.date, by = c("FireID" = "FireID", "weather_date" = "weather_date"))




#changins 0's to 1's so that row doesn't get dropped from SFA
join.gis.weather.ops.nozero <- join.gis.weather.ops %>% 
  mutate(Daily_Held_noneg = ifelse(Daily_Held_Perim <= 0,1,Daily_Held_Perim))

#Continue to Frontier_R_Code for analysis code