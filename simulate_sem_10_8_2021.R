# https://sjak.shinyapps.io/power4SEM/
# Simulting SEM models;  Plotting Power Curves

library(lavaan)
library(simsem)

popModel <- "
TP =~  0.939*TP7 + 0.910*TP3 + 0.884*TP2 + 0.881*TP8 + 
       0.844*TP13 + 0.827*TP1 + 0.818*TP6 +
       0.769*TP10 + 0.750*TP5 + 0.686*TP4 + 
       0.676*TP11 + 0.660*TP9 + 0.557*TP12

CP =~ 0.832*CP29 + 0.807*CP25 + 0.762*CP23 + 0.760*CP31 +  
      0.754*CP26 + 0.743*CP24 + 0.732*CP33 + 
      0.662*CP30 + 0.648*CP28 + 0.616*CP34 + 0.615*CP32 + 
      0.441*CP27

SP =~ 0.937*SP19 + 0.893*SP17 + 0.765*SP18 + 0.704*SP21 + 
      0.701*SP16 + 0.697*SP20 + 0.586*SP15 + 0.549*SP14 + 
      0.546*SP22

# Latent variable co(variances)
TP~~1*TP
CP~~1*CP
SP~~1*SP

TP ~~ 0.77*CP
TP ~~ 0.48*SP
CP ~~ 0.72*SP

# Item errors
 TP7~~(1-0.939^2)*TP7
 TP3~~(1-0.910^2)*TP3
 TP2~~(1-0.884^2)*TP2
 TP8~~(1-0.881^2)*TP8
TP13~~(1-0.844^2)*TP13
 TP1~~(1-0.827^2)*TP1
 TP6~~(1-0.818^2)*TP6
TP10~~(1-0.769^2)*TP10
 TP5~~(1-0.750^2)*TP5
 TP4~~(1-0.686^2)*TP4
TP11~~(1-0.676^2)*TP11
 TP9~~(1-0.660^2)*TP9
TP12~~(1-0.557^2)*TP12  

CP29~~(1-0.832^2)*CP29
CP25~~(1-0.807^2)*CP25
CP23~~(1-0.762^2)*CP23
CP31~~(1-0.760^2)*CP31
CP26~~(1-0.754^2)*CP26
CP24~~(1-0.743^2)*CP24
CP33~~(1-0.732^2)*CP33
CP30~~(1-0.662^2)*CP30
CP28~~(1-0.648^2)*CP28
CP34~~(1-0.616^2)*CP34
CP32~~(1-0.615^2)*CP32
CP27~~(1-0.441^2)*CP27

SP19~~(1-0.937^2)*SP19
SP17~~(1-0.893^2)*SP17
SP18~~(1-0.765^2)*SP18
SP21~~(1-0.704^2)*SP21
SP16~~(1-0.701^2)*SP16
SP20~~(1-0.697^2)*SP20
SP15~~(1-0.586^2)*SP15
SP14~~(1-0.549^2)*SP14
SP22~~(1-0.546^2)*SP22
"

###

analyzeModel <-"
TP =~  TP7 + TP3 + TP2 + TP8 + 
       TP13 + TP1 + TP6 +
       TP10 + TP5 + TP4 + 
       TP11 + TP9 + TP12

CP =~ CP29 + CP25 + CP23 + CP31 + 
      CP26 + CP24 + CP33 + 
      CP30 + CP28 + CP34 + CP32 + 
      CP27

SP =~ SP19 + SP17 + SP18 + SP21 + 
      SP16 + SP20 + SP15 + SP14 + 
      SP22

# Latent variable covariances
TP ~~ CP
TP ~~ SP
CP ~~ SP

# Item errors
 TP7~~TP7
 TP3~~TP3
 TP2~~TP2
 TP8~~TP8
TP13~~TP13
 TP1~~TP1
 TP6~~TP6
TP10~~TP10
 TP5~~TP5
 TP4~~TP4
TP11~~TP11
 TP9~~TP9
TP12~~TP12

CP29~~CP29
CP25~~CP25
CP23~~CP23
CP31~~CP31
CP26~~CP26
CP24~~CP24
CP33~~CP33
CP30~~CP30
CP28~~CP28
CP34~~CP34
CP32~~CP32
CP27~~CP27

SP19~~SP19
SP17~~SP17
SP18~~SP18
SP21~~SP21
SP16~~SP16
SP20~~SP20
SP15~~SP15
SP14~~SP14
SP22~~SP22

"

###

Ho_Model <-"
TP =~  c1*TP7 + c1*TP3 + c1*TP2 + c1*TP8 + 
       c1*TP13 + c1*TP1 + c1*TP6 +
       c1*TP10 + c1*TP5 + c1*TP4 + 
       c1*TP11 + c1*TP9 + c1*TP12

CP =~ c2*CP29 + c2*CP25 + c2*CP23 + c2*CP31 + 
      c2*CP26 + c2*CP24 + c2*CP33 + 
      c2*CP30 + c2*CP28 + c2*CP34 + c2*CP32 + 
      c2*CP27

SP =~ c3*SP19 + c3*SP17 + c3*SP18 + c3*SP21 + 
      c3*SP16 + c3*SP20 + c3*SP15 + c3*SP14 + 
      c3*SP22

# Latent variable covariances
TP ~ CP
TP ~ SP
CP ~ SP

# Item errors
 TP7~~TP7
 TP3~~TP3
 TP2~~TP2
 TP8~~TP8
TP13~~TP13
 TP1~~TP1
 TP6~~TP6
TP10~~TP10
 TP5~~TP5
 TP4~~TP4
TP11~~TP11
 TP9~~TP9
TP12~~TP12

CP29~~CP29
CP25~~CP25
CP23~~CP23
CP31~~CP31
CP26~~CP26
CP24~~CP24
CP33~~CP33
CP30~~CP30
CP28~~CP28
CP34~~CP34
CP32~~CP32
CP27~~CP27

SP19~~SP19
SP17~~SP17
SP18~~SP18
SP21~~SP21
SP16~~SP16
SP20~~SP20
SP15~~SP15
SP14~~SP14
SP22~~SP22
"

###############################################################
#
# Population Model

data <- simulateData(popModel, sample.nobs = 200)
names(data)
head(data)

sim.results <- cfa(analyzeModel, data = data, std.lv = TRUE)
summary(sim.results)
fitmeasures(sim.results)

############################################################

Output_analyze <- sim(1000, analyzeModel, n=200, generate=popModel,
                      lavaanfun = "cfa", std.lv=TRUE,
                      multicore=TRUE)

options(scipen=9999)
summary(Output_analyze)
summaryConverge(Output_analyze)
summaryFit(Output_analyze)
summaryParam(Output_analyze)

########################################################

library(simsem)
Output1 <- sim(1000, Ho_Model, n=200, generate=popModel,
               lavaanfun = "cfa", std.lv=TRUE,
               multicore=TRUE)

options(scipen=9999)
summary(Output1)
summaryConverge(Output1)
summaryFit(Output1)
summaryParam(Output1)

############################################################

# Varying sample size and plotting power as a function 
# of sample size

Output2 <- sim(NULL, analyzeModel, n=50:200, generate=popModel,
               lavaanfun = "cfa", std.lv=TRUE)
summary(Output2)

alpha=.05

powTable2 <- getPower(Output2, alpha=alpha)
head(powTable2)
tail(powTable2)

findPower(powTable2, "N", power=0.95)

plotPower(Output2, powerParam = "TP~~SP", 
          alpha=alpha, useContour=TRUE)

####################################################


Output3 <- sim(NULL, Ho_Model, n=50:200, generate=popModel,
               lavaanfun = "cfa", std.lv=TRUE)
summary(Output3)

alpha=.05

powTable3 <- getPower(Output3, alpha=alpha)
head(powTable3)
tail(powTable3)

plotPower(Output3, powerParam = "TP~SP", 
          alpha=alpha, useContour=TRUE)


####################################################
#
# 34 items
# 68 parameters (loadings and item errors)
#  3 parameters (latent variable covariances)
# 
# (34*(34+1))/2 - (68 - 3) = 524 df


library(semTools)
power.samp.size<-findRMSEApower(rmsea0=0.05, 
                                rmseaA=0.08, 
               df=524, 
               n=50:200, 
               alpha = 0.05, 
               group = 1)
power.samp.size

plot(50:200, power.samp.size, type="l")

data.frame(50:200, power.samp.size)





