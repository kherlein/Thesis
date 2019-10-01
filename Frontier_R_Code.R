
require(frontier)
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