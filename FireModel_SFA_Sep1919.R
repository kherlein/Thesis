#ImportData
setwd("C:/Users/Kalli/OneDrive - ualberta.ca/Grad School/Thesis/FIRE Data/")

gis.data <- read.csv(file="GOV_DATA/R_SFA_Data_Sep24.csv", header=TRUE, sep=",")
colnames(gis.data)[colnames(gis.data)=="CAPTURE_DA"] <- "weather_date"
all.weather <- read.csv(file="GOV_DATA/WEATHERDATA_GOV/20174stations.csv")
operations <- read.csv(file="GOV_DATA/FakeCrewData.csv")

# read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "SWF107-PannyAuto")

#SWF107_PannyAuto <- read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "SWF107-PannyAuto")
#HWF221_LambertAuto <- read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "HWF221-LambertAuto")
#HWF236_LambertAuto <- read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "HWF236-LambertAuto")
#HWF280_BitschoLakeAuto <- read_xlsx("GOV_DATA/WEATHERDATA_GOV/New Fires Weather and Status 2017.xlsx", "HWF280-BitschoLakeAuto")
ma

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



require(sfa)
require(plm)

# Cobb-Douglas production frontier
cobbDouglas <- sfa( log( Daily_Held_noneg ) ~ log( Crew ) + log( Helicopter ) + log( Air ) + log( Engine ) + log( Dozer ),
                    data = join.gis.weather.ops.nozero )
summary( cobbDouglas )


# Error Components Frontier (Battese & Coelli 1992)
# with observation-specific efficiencies (ignoring the panel structure)
fire <- sfa( log( Daily_Held_noneg ) ~ log( Crew ) + log( Helicopter ) + log( Air ) + log( Engine ) + log( Dozer ),
             data = join.gis.weather.ops.nozero )
summary( fire )

# Error Components Frontier (Battese & Coelli 1992)
# with "true" fixed individual effects and observation-specific efficiencies
#fireTrue <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) + 
#                   factor( FireID ),  data = join.gis.weather.ops.nozero )
#summary( fireTrue )

# add data set with information about its panel structure
library( "plm" )
firePanel <- pdata.frame( join.gis.weather.ops.nozero, c( "FireID", "FireDayActual.x" ) )

# Error Components Frontier (Battese & Coelli 1992)
# with time-invariant efficiencies
fireTimeInv <- sfa( log( Daily_Held_noneg ) ~ log( Crew ) + log( Helicopter ) + log( Air ) + log( Engine ) + log( Dozer ),
                    data = firePanel )
summary( fireTimeInv )
efficiencies( fireTimeInv )


# Error Components Frontier (Battese & Coelli 1992)
# with time-variant efficiencies
fireTimeVar <- sfa( log( Daily_Held_noneg ) ~ log( Crew ) + log( Helicopter ) + log( Air ) + log( Engine ) + log( Dozer ),
                    data = firePanel, timeEffect = TRUE )
summary( fireTimeVar )
efficiencies( fireTimeVar )

# Technical Efficiency Effects Frontier (Battese & Coelli 1995)
# (efficiency effects model with intercept)
fireZ <- sfa( log( Daily_Held_noneg ) ~ log( Crew ) + log( Helicopter ) + log( Air ) + log( Engine ) + log( Dozer ) |
                EDYRS + BANRAT, data = fireProdPhil )
summary( fireZ )
efficiencies( fireZ )


# Technical Efficiency Effects Frontier (Battese & Coelli 1995)
# (efficiency effects model without intercept)
fireZ2 <- sfa( log( Daily_Held_noneg ) ~ log( Crew ) + log( Helicopter ) + log( Air ) + log( Engine ) + log( Dozer ) |
                 EDYRS + BANRAT - 1, data = fireProdPhil )
summary( fireZ2 )
efficiencies( fireZ2 )


# Cost Frontier (with land as quasi-fixed input)
fireProdPhil$cost <- fireProdPhil$LABOR * fireProdPhil$LABORP +
  fireProdPhil$NPK * fireProdPhil$NPKP
fireCost <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP )
                 + log( NPKP ), data = fireProdPhil, ineffDecrease = FALSE )
summary( fireCost )