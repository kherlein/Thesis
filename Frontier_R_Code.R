
require(frontier)
require(plm)


############################
############################
#   Dropping SWF fire due to no crew data!!!!

join.final.noSWF <- join.final %>% filter(FireID != "SWF107")


# Cobb-Douglas production frontier
# cobbDouglas <- sfa( log( Daily_Held_noneg ) ~ log( crewtotal ) + log( airtotal ) + log( equiptotal ),
#                     data = join.final )


# 8 Following Battese (1997) we create dummy variable for zero-resources such
# that crew_d ¼ 1 if crew ¼ 0 and crew_d ¼ 0 if crew>0. Log of the resources variables
# are created using: ln(crew) = ln(Max(crew, crew_d)).


cobbDouglas <- sfa( log( Daily_Held_noneg ) ~ crew_d + air_d + log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
                    data = join.final.noSWF )
summary( cobbDouglas )


# # Error Components Frontier (Battese & Coelli 1992)
# # with observation-specific efficiencies (ignoring the panel structure)
# fire <- sfa( log( Daily_Held_noneg ) ~ log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
#              data = join.final.noSWF )
# summary( fire )

# Error Components Frontier (Battese & Coelli 1992)
# with "true" fixed individual effects and observation-specific efficiencies
#fireTrue <- sfa( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) + 
#                   factor( FireID ),  data = join.final )
#summary( fireTrue )

# add data set with information about its panel structure
library( "plm" )
firePanel <- pdata.frame( join.final.noSWF, c( "FireID", "FireDayActual" ) )

# Error Components Frontier (Battese & Coelli 1992)
# with time-invariant efficiencies
fireTimeInv <- sfa( log( Daily_Held_noneg ) ~ log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
                    data = firePanel )
summary( fireTimeInv )
efficiencies( fireTimeInv )


# Error Components Frontier (Battese & Coelli 1992)
# with time-variant efficiencies
fireTimeVar <- sfa( log( Daily_Held_noneg ) ~ log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
                    data = firePanel, timeEffect = TRUE )
summary( fireTimeVar )
efficiencies( fireTimeVar )

# Technical Efficiency Effects Frontier (Battese & Coelli 1995)
# (efficiency effects model with intercept)
fireZ <- sfa( log( Daily_Held_noneg ) ~ log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d)) |
                RH_max + temp_max + agric_wind_max + perc_river + perc_road + Timber, data = firePanel )
summary( fireZ )
efficiencies( fireZ )


# Technical Efficiency Effects Frontier (Battese & Coelli 1995)
# (efficiency effects model without intercept)
# Added max and dummy variables to match Battese 1997 specification for frequent 0 observations
fireZ2 <- sfa( log( Daily_Held_noneg ) ~ crew_d + air_d + log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d))|
                 RH_max + temp_max + agric_wind_max +  perc_river + perc_road + Timber + PreviousFire - 1, data = firePanel )
summary( fireZ2 )
efficiencies( fireZ2 )


# Cost Frontier (with land as quasi-fixed input)
#fireProdPhil$cost <- fireProdPhil$LABOR * fireProdPhil$LABORP +
#  fireProdPhil$NPK * fireProdPhil$NPKP
#fireCost <- sfa( log( cost ) ~ log( PROD ) + log( AREA ) + log( LABORP )
#                 + log( NPKP ), data = fireProdPhil, ineffDecrease = FALSE )
#summary( fireCost )