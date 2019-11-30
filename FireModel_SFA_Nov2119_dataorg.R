#ImportData
gis.data <- read.csv(file="GISData_Oct0119.csv", header=TRUE, sep=",")
colnames(gis.data)[colnames(gis.data)=="CAPTURE_DA"] <- "weather_date"

all.weather <- read.csv(file="20174stations.csv")

status.data <- read.csv(file="DailyFireStatus_GOVDATA.csv", header=TRUE, sep=",")
colnames(status.data)[colnames(status.data)=="Date"] <- "weather_date"

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
require(tidyr)

rh.max <- all.weather %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(FireID, weather_date) %>% 
  summarize(RH_max = max(relative_humidity), temp_max = max(dry_bulb_temperature), wind_max = max(wind_gust_kmh, na.rm = TRUE ), agric_wind_max = max(wind_agric)) %>%
  as.data.frame()

gis.data.date <- gis.data %>%
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d"))

status.data.date <- status.data %>%
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d"))

unique(air$AIRCRAFT_CLASS)
air.date.sum <- air %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d")) %>% 
  group_by(FireID, weather_date) %>% 
  summarize(airtotal = sum(TOTAL_AIRCRAFT_PER_DAY, na.rm = TRUE) ) %>%
   # spread(key= AIRCRAFT_CLASS,
   #       value=TOTAL_AIRCRAFT_PER_DAY) %>%
  # group_by(FireID, weather_date) %>% 
  as.data.frame()

unique(equip$EQUIPMENT_TYPE)

equip.date <- equip %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d")) %>% 
  group_by(FireID, weather_date) %>% 
  filter(EQUIPMENT_TYPE=="Truck -Water Tank Truck") %>% 
  select(c(FireID, weather_date, TOTAL_EQUIPMENT_PER_DAY)) %>% 
  as.data.frame()
colnames(equip.date)[colnames(equip.date)=="TOTAL_EQUIPMENT_PER_DAY"] <- "equiptotal"



unique(crew$POSITION)
unique(crew.date$POSITION)
crew.date.sum <- crew %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d")) %>% 
  group_by(FireID, weather_date) %>% 
  filter(FUNCTION=="Operations") %>% 
  summarize(crewtotal = sum(TOTAL_PERSONNEL_PER_DAY, na.rm = TRUE) ) %>%
  as.data.frame()




#checking that date conversion is correct
is.Date(gis.data.date$weather_date)
is.Date(rh.max$weather_date)

join.gis.weather <-  left_join(gis.data.date, rh.max, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.status <- left_join(join.gis.weather, status.data.date, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.crew <- left_join(join.status, crew.date.sum, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.air <- left_join(join.crew, air.date.sum, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.final <- left_join(join.air, equip.date, by = c("FireID" = "FireID", "weather_date" = "weather_date")) %>% 
  mutate(crewtotal=ifelse(is.na(crewtotal),0,crewtotal)) %>% 
  mutate(airtotal=ifelse(is.na(airtotal),0,airtotal)) %>% 
  mutate(equiptotal=ifelse(is.na(equiptotal),0,equiptotal))

#changins 0's to 1's so that row doesn't get dropped from SFA
#Adding dummy variables for inputs
join.final <- join.final %>% 
  mutate(Daily_Held_noneg = ifelse(Daily_Held_Perim <= 0,1,Daily_Held_Perim)) %>% 
  mutate(air_d = ifelse(airtotal == 0,1,0)) %>% 
  mutate(crew_d = ifelse(crewtotal == 0,1,0)) %>% 
  mutate(equip_d = ifelse(equiptotal == 0,1,0))

summary(join.final)
#Continue to Frontier_R_Code for analysis code
  
  
  