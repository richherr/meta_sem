# Simulting SEM models;  Plotting Power Curves

library(lavaan)
popModel <- "
f1 =~ 0.6*y1 + 0.7*y2 + 0.8*y3
f2 =~ 0.6*y4 + 0.7*y5 + 0.8*y6
f1 ~~ 0.1*f2
f1 ~~ 1*f1
f2 ~~ 1*f2
y1 ~~ 0.64*y1
y2 ~~ 0.51*y2
y3 ~~ 0.36*y3
y4 ~~ 0.64*y4
y5 ~~ 0.51*y5
y6 ~~ 0.36*y6
"
data <- simulateData(popModel, sample.nobs = 200)

analyzeModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
"
data <- cfa(analyzeModel, data = data, std.lv = TRUE)

# Use simsem to simulate and analyze multiple data sets
library(simsem)
Output1 <- sim(1000, analyzeModel, n=200, generate=popModel,
               lavaanfun = "cfa", std.lv=TRUE)
summary(Output1)
summaryConverge(Output1)
summaryFit(Output1)
summaryParam(Output1)
getCutoff(Output1, alpha=0.05)
plotCutoff(Output1)

# Varying sample size
Output2 <- sim(NULL, analyzeModel, n=100:1000, generate=popModel,
               lavaanfun = "cfa", std.lv=TRUE)
summary(Output2)

powTable2 <- getPower(Output2, alpha=.10)
powTable2

findPower(powTable2, "N", 0.70)

plotPower(Output2, powerParam = "f1~~f2", alpha=.10)

