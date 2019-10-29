require(rDEA)

## load data

## inputs and outputs for analysis
Ypanel = firePanel[c('Daily_Held_noneg')]
Xpanel = firePanel[c('Crew', 'Helicopter', 'Air', 'Engine', 'Dozer')]
#W = firePanel[c('labor_price', 'capital_price')]

## Naive input-oriented DEA score for the first 20 firms under variable returns-to-scale
#firms=1:6
di_panel = dea(XREF=Xpanel, YREF=Ypanel, X=Xpanel, Y=Ypanel, model="output", RTS="variable")
di_panel$thetaOpt

## inputs and outputs for analysis
Y = join.gis.weather.ops.nozero[c('Daily_Held_noneg')]
X = join.gis.weather.ops.nozero[c('Crew', 'Helicopter', 'Air', 'Engine', 'Dozer')]

## Naive input-oriented DEA score for the first 20 firms under variable returns-to-scale
#firms=1:6
di = dea(XREF=X, YREF=Y, X=X, Y=Y, model="output", RTS="variable")
di$thetaOpt

## Naive DEA score in cost-minimization model for the first 20 firms under variable returns-to-scale
#ci_naive = dea(XREF=X, YREF=Y, X=X[firms,], Y=Y[firms,], W=W[firms,],
#               model="costmin", RTS="variable")
#ci_naive$XOpt
#ci_naive$gammaOpt

#test for returns to scale
rts.test(X, Y, W=NULL, model, H0, bw, B, alpha)
