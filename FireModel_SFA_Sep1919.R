#ImportData
gis.data <- read.csv(file="/R_SFA_Data_Sep24.csv", header=TRUE, sep=",")
colnames(gis.data)[colnames(gis.data)=="CAPTURE_DA"] <- "weather_date"
all.weather <- read.csv(file="20174stations.csv")
operations <- read.csv(file="FakeCrewData.csv")

# read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "SWF107-PannyAuto")

#SWF107_PannyAuto <- read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "SWF107-PannyAuto")
#HWF221_LambertAuto <- read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "HWF221-LambertAuto")
#HWF236_LambertAuto <- read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "HWF236-LambertAuto")
#HWF280_BitschoLakeAuto <- read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "HWF280-BitschoLakeAuto")


require(dplyr)
require(lubridate)

rh.max <- all.weather %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(FireID, weather_date) %>% 
  summarize(RH_max = max(relative_humidity), temp_max = max(dry_bulb_temperature), wind_max = max(wind_gust_kmh, na.rm = TRUE )) %>%
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
  mutate(Daily_Held_noneg = ifelse(Daily_Held_noneg == 0,1,Daily_Held_noneg))

#Continue to Frontier_R_Code for analysis code