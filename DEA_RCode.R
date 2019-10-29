require(rDEA)

## load data
#######      IGNORING PANEL STRUCTURE     ############################
######################################################################
######################################################################

Ycross = join.gis.weather.ops.nozero[c('Daily_Held_noneg')]
Xcross = join.gis.weather.ops.nozero[c('Crew', 'Helicopter', 'Air', 'Engine', 'Dozer')]
#W = firePanel[c('labor_price', 'capital_price')]

## Input-oriented DEA score under variable returns-to-scale
di = dea(XREF=Xcross, YREF=Ycross, X=Xcross, Y=Ycross, model="output", RTS="variable")
di$thetaOpt
di

#test for returns to scale
#not rejecting null indicates RTS are constant
# need to check which bandwidtch to use as smoothing parameter, cv ,bw.ucv, silverman, or bc.nrd0
rts.test(X, Y, W=NULL, model = "output", "constant", "cv", 100, 0.05)


#######      PANEL STRUCTURE     #####################################
######################################################################
######################################################################
## inputs and outputs for analysis
Ypanel = firePanel[c('Daily_Held_noneg')]
Xpanel = firePanel[c('Crew', 'Helicopter', 'Air', 'Engine', 'Dozer')]
#W = firePanel[c('labor_price', 'capital_price')]

## Naive input-oriented DEA score for the first 20 firms under variable returns-to-scale
#firms=1:6
di_panel = dea(XREF=Xpanel, YREF=Ypanel, X=Xpanel, Y=Ypanel, model="output", RTS="variable")
di_panel$thetaOpt



## Naive DEA score in cost-minimization model for the first 20 firms under variable returns-to-scale
#ci_naive = dea(XREF=X, YREF=Y, X=X[firms,], Y=Y[firms,], W=W[firms,],
#               model="costmin", RTS="variable")
#ci_naive$XOpt
#ci_naive$gammaOpt

#test for returns to scale
rts.test(X, Y, W=NULL, model, H0, bw, B, alpha)
