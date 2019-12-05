
require(frontier)
require(plm)


################################################
################################################
#          Dropping SWF fire due to no crew data
join.final.noSWF <- join.final %>% filter(FireID != "SWF107")
summary(join.final.noSWF)

################################################
################################################
# add data set with information about its panel structure
library( "plm" )
firePanel <- pdata.frame( join.final.noSWF, c( "FireID", "FireDayActual" ) )

# require(ExPanDaR)
# ExPanD(df = firePanel, cs_id = "FireID", ts_id = "FireDayActual")

# Cobb-Douglas production frontier
# cobbDouglas <- sfa( log( Daily_Held_noneg ) ~ log( crewtotal ) + log( airtotal ) + log( equiptotal ),
#                     data = join.final )


# via Katuwal Following Battese (1997) we create dummy variable for zero-resources such
# that crew_d ¼ 1 if crew ¼ 0 and crew_d ¼ 0 if crew>0. Log of the resources variables
# are created using: ln(crew) = ln(Max(crew, crew_d)).
cobbDouglas <- sfa( log( Daily_Held_noneg ) ~ crew_d + air_d + log(pmax( crewtotal, crew_d))
                      + log(pmax( crew2, crew_d)) + log(pmax(airtotal, air_d)),
                      data = join.final.noSWF )
summary(cobbDouglas)



cobbDouglasFULL <- sfa(log( Daily_Held_noneg ) ~ crew_d + air_d + log(pmax( crewtotal, crew_d)) + 
                  log(pmax(airtotal, air_d)) + rh_lag + temp_max + agric_wind_max +  perc_river +
                  Timber + relativedays , data = join.final.noSWF )
summary(cobbDouglasFULL) #remember! dummy vars have opposite interpretation, 1 = not used
fitted( cobbDouglasFULL )
join.final.noSWF$fittedcobb <- fitted( cobbDouglasFULL, asInData = TRUE )
plot(join.final.noSWF$fittedcobb ~ crew_d + air_d + log(pmax( crewtotal, crew_d)) + 
       log(pmax(airtotal, air_d)), data = join.final.noSWF)



translogFULL <- frontierQuad(log( Daily_Held_noneg ) ~ crew_d + air_d  
                + lnair + lncrew + lnair2 + lncrew2 + aircrew
                + rh_lag + temp_max + agric_wind_max +  perc_river +
                  Timber + relativedays, data = join.final.noSWF)
summary(translogFULL)

#Test to see which is better functional form
#Sandeep's notes: (need to somehow call parameters from model output) 
#test LL2 = 0, accum test LK2 = 0, accum, test LL*LK=0, accum
#Sandeep: Test for returns to scale:
#test LL + LK = 1, test LL2 + LK2 + 2*LL*LK = 0, accum
#Should I restrict model to impose CRS or DRTS?

lrtest(cobbDouglasFULL,translogFULL)
AIC(cobbDouglasFULL,translogFULL)


# Error Components Frontier (Battese & Coelli 1992)
# with time-invariant efficiencies
fireTimeInv <- sfa(log( Daily_Held_noneg ) ~ crew_d + air_d + log(pmax(crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
                    data = firePanel )
summary( fireTimeInv )
efficiencies( fireTimeInv )


# Error Components Frontier (Battese & Coelli 1992)
# with time-variant efficiencies
fireTimeVar <- sfa( log(Daily_Held_noneg) ~ crew_d + air_d + log(pmax(crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
                    data = firePanel, timeEffect = TRUE)
summary(fireTimeVar)
efficiencies(fireTimeVar)

# Technical Efficiency Effects Frontier (Battese & Coelli 1995)
# (efficiency effects model with intercept)
#trying to add crew2 variables!!!
fireZ <- sfa(log( Daily_Held_noneg ) ~ crew_d + air_d  
             + lnair + lncrew
             | rh_lag + temp_max + agric_wind_max + perc_river
             + relativedays , data = firePanel )
summary(fireZ)
efficiencies(fireZ)
fitted( fireZ )
firePanel$fittedZ <- fitted( fireZ, asInData = TRUE )
plot(firePanel$fittedZ)

fireZ.trans <- frontierQuad(log( Daily_Held_noneg ) ~ crew_d + air_d  
             + lnair + lncrew + lnair2 + lncrew2 + aircrew 
             | rh_lag + temp_max + agric_wind_max + perc_river
             + relativedays , data = firePanel )
summary(fireZ.trans)
efficiencies(fireZ.trans)
fitted( fireZ.trans )
firePanel$fittedZ.trans <- fitted( fireZ.trans, asInData = TRUE )
plot(firePanel$fittedZ.trans)


#rejecting the null means the larger/ non-nested model is a better fit
#rejecting the null means the inefficiency model is superior to cobb-doug
#lower information criteria indicates better model fit
lrtest(fireZ, fireZ.trans)
AIC(fireZ, fireZ.trans)

lrtest(fireZ.trans)
lrtest(fireZ)


# Technical Efficiency Effects Frontier (Battese & Coelli 1995)
# (efficiency effects model without intercept)
# Added max and dummy variables to match Battese 1997 specification for frequent 0 observations
fireZ2 <- sfa( log( Daily_Held_noneg ) ~ crew_d + air_d + log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d))|
                 rh_lag + temp_max + agric_wind_max +  perc_river + Timber + relativedays - 1, data = firePanel)
summary( fireZ2 )
efficiencies( fireZ2 )

plot(log(Daily_Held_noneg) ~ crew_d + air_d + log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
     type = "p", col = "red", ylim = c(0,1), data = firePanel)
x.seq <- seq(0, 1, by = 0.01)
lines( log(Daily_Held_noneg) ~ x.seq, col = "blue", data=firePanel)





