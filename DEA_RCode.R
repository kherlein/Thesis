require(rDEA)

## load data
#######      IGNORING PANEL STRUCTURE     ############################
######################################################################
######################################################################

Ycross = join.gis.weather.ops.nozero[c('Daily_Held_noneg')]
Xcross = join.gis.weather.ops.nozero[c('Crew', 'Helicopter', 'Air', 'Engine', 'Dozer')]
#W = firePanel[c('labor_price', 'capital_price')]

## Naive Input-oriented DEA score under variable returns-to-scale
dea.xs = dea(XREF=Xcross, YREF=Ycross, X=Xcross, Y=Ycross, model="output", RTS="variable")
# Theta = Technical efficiency estimates
dea.xs$thetaOpt

dea.xs

#test for returns to scale
#not rejecting null indicates RTS are constant
# need to check which bandwidtch to use as smoothing parameter, cv ,bw.ucv, silverman, or bc.nrd0
rts.test(Xcross, Ycross, W=NULL, model = "output", "constant", "cv", 100, 0.05)




## Naive Input-oriented DEA score under variable returns-to-scale
dea.xs.robust = dea.robust(X=Xcross, Y=Ycross, model="output", RTS="variable")
# Theta = Technical efficiency estimates
dea.xs.robust$theta_hat

#dea.xs.robust

#######      PANEL STRUCTURE     #####################################
######################################################################
######################################################################
## inputs and outputs for analysis
Ypanel = firePanel[c('Daily_Held_noneg')]
Xpanel = firePanel[c('Crew', 'Helicopter', 'Air', 'Engine', 'Dozer')]
#W = firePanel[c('labor_price', 'capital_price')]

## Naive output-oriented DEA score under variable returns-to-scale
#firms=1:6
dea.panel = dea(XREF=Xpanel, YREF=Ypanel, X=Xpanel, Y=Ypanel, model="output", RTS="variable")
dea.panel$thetaOpt


#test for returns to scale
rts.test(X, Y, W=NULL, model, H0, bw, B, alpha)
