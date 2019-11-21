#ImportData
gis.data <- read.csv(file="GISData_Oct0119.csv", header=TRUE, sep=",")
colnames(gis.data)[colnames(gis.data)=="CAPTURE_DA"] <- "weather_date"
all.weather <- read.csv(file="20174stations.csv")

air <- read.csv(file="Aircraft_HWF_2017.csv", header=TRUE, sep=",")
colnames(air)[colnames(air)=="DATE_WORKED"] <- "weather_date"
colnames(air)[colnames(air)=="FIRE_NUMBER"] <- "FireID"

equip <- read.csv(file="Equiprment_HWF_2017.csv", header=TRUE, sep=",")
colnames(equip)[colnames(equip)=="DATE_WORKED"] <- "weather_date"
colnames(equip)[colnames(equip)=="FIRE_NUMBER"] <- "FireID"

crew <- read.csv(file="Personnel_HWF_2017.csv", header=TRUE, sep=",")
colnames(crew)[colnames(crew)=="DATE_WORKED"] <- "weather_date"
colnames(crew)[colnames(crew)=="FIRE_NUMBER"] <- "FireID"

require(dplyr)
require(lubridate)

rh.max <- all.weather %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(FireID, weather_date) %>% 
  summarize(RH_max = max(relative_humidity), temp_max = max(dry_bulb_temperature), wind_max = max(wind_gust_kmh, na.rm = TRUE ), agric_wind_max = max(wind_agric)) %>%
  as.data.frame()

air.date <- air %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(FireID, weather_date, WING_TYPE, AIRCRAFT_CLASS) %>% 
  summarize(airtotal = sum(TOTAL_AIRCRAFT_PER_DAY, na.rm = TRUE) ) %>%
  as.data.frame()


  
gis.data.date <- gis.data %>%
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d"))


air.date <- air %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M")) 

equip.date <- equip %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M"))

crew.date <- crew %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M"))


#checking that date conversion is correct
is.Date(gis.data.date$weather_date)
is.Date(rh.max$weather_date)

join.gis.weather <-  left_join(gis.data.date, rh.max, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.gis.weather.ops <- left_join(join.gis.weather, operations.date, by = c("FireID" = "FireID", "weather_date" = "weather_date"))



#changins 0's to 1's so that row doesn't get dropped from SFA
join.gis.weather.ops.nozero <- join.gis.weather.ops %>% 
  mutate(Daily_Held_noneg = ifelse(Daily_Held_Perim <= 0,1,Daily_Held_Perim))

#Continue to Frontier_R_Code for analysis code