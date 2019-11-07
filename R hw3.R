#Task 2 Concrete Slump Data
#Setting working directory
setwd("-/")
setwd("C:/Users/rohit/Desktop/Rdown")
install.packages("readxl")
install.packages("car")
install.packages("gvlma")
install.packages("MASS")
install.packages("bootstrap")
install.packages("leaps")
library(readxl)
slump <- read_excel(file.choose())
cor(slump)
library(car)
library(gvlma)
library(bootstrap)
library(MASS)
library(leaps)
#PART 1
concrete = slump[,c(10,2:8)]
scatterplotMatrix(concrete, spread = FALSE, lty.smooth = 2, main= "Scatter Plot Matrix")
#Part2
#Model 1 MULTIPLE LINEAR REGRESSION 
slump <- read_excel(file.choose())
MLR1 = lm(slump$`Slump Flow`~ slump$Cement+ slump$Slag + slump$`Fly Ash`+ slump$Water+ slump$SP+ slump$`Coarse Aggregate`
          +slump$`Fine Aggregate`, data = concrete)
summary(MLR1)


#Model 2 Multiple linear Regression with interaction
MLR2= lm( slump$`Slump Flow`~ slump$Cement + slump$Slag + slump$`Fly Ash`+ slump$Water+ slump$SP+ slump$`Coarse Aggregate`
          +slump$`Fine Aggregate`+ slump$Slag: slump$Water+ slump$`Coarse Aggregate`:slump$`Fine Aggregate`+ slump$Cement: slump$`Fly Ash`
          +slump$SP: slump$Slag, data = concrete)
summary(MLR2)


#Model 3 Second-order regression
MLR3 = lm(slump$`Slump Flow`~ slump$Cement + slump$Slag +  slump$`Fly Ash`+ slump$Water+ slump$SP+ slump$`Coarse Aggregate`+  slump$`Fine Aggregate`+  slump$Cement:  slump$Slag +  slump$Cement:  slump$`Fly Ash` +  slump$Cement:  slump$Cement:  slump$SP+ slump$Cement:  slump$`Coarse Aggregate`+  slump$Cement:  slump$`Fine Aggregate`+
            slump$Slag:  slump$`Fly Ash`+ slump$Slag:  slump$Water+  slump$Slag:  slump$SP +  slump$Slag:  slump$`Coarse Aggregate`+  slump$Slag:  slump$`Fine Aggregate`+
            slump$`Fly Ash`:  slump$Water+  slump$`Fly Ash`: slump$SP + slump$`Fly Ash`:  slump$`Coarse Aggregate`+ slump$`Fly Ash`: slump$`Fine Aggregate`+
            slump$Water: slump$SP+ slump$Water: slump$`Coarse Aggregate`+ slump$Water: slump$`Fine Aggregate`+ slump$SP: slump$`Coarse Aggregate`+ slump$SP: slump$`Fine Aggregate`+  slump$`Coarse Aggregate`: slump$`Fine Aggregate`, data = concrete)


summary(MLR3)
#Part 3
#Regression diagnostics Typical and aenhanced approach
confint(MLR1)
confint(MLR2)
confint(MLR3)
#Typical Approach 
par(mfrow=c(2,2))
plot(MLR1)
plot(MLR2)
plot(MLR3)

#Enhanced Approach
par(mfrow=c(2,2))
qqPlot(MLR1, labels=row.names(concrete), id.method="identify", simulate=TRUE,
       main="Linear Regression Model Q-Q Plot")
qqPlot(MLR2, labels=row.names(concrete), id.method="identify", simulate=TRUE,
       main="Linear Regression Model Q-Q Plot")
qqPlot(MLR3, labels=row.names(concrete), id.method="identify", simulate=TRUE,
       main="Linear Regression Model Q-Q Plot")

#Part 4 Identify unusual observations and take corrective measures
#Studentized Residuals distribution
residPlot = function(fit, nbreaks = 10){
  z = rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE, xlab = "Studentized Residual", main = "Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)), add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y, col="red", lwd=2, lty=2)
  legend("topright", legend = c("Normal Curve", "Kernel Density Curve"), lty = 1:2, col=c("blue", "red"), cex=.7 )
}
residPlot(MLR1)
residPlot(MLR2)
residPlot(MLR3)

#Identify unusual observations
#outliers test
outlierTest(MLR1)
outlierTest(MLR2)
outlierTest(MLR3)

#Added value Plots
avPlots(MLR1, ask=FALSE, onepage=TRUE )
avPlots(MLR2, ask=FALSE, onepage=TRUE, id.method ="identify" )
avPlots(MLR3, ask=FALSE, onepage=TRUE, id.method ="identify" )

#Power Transforms
#Model1
summary(powerTransform(MLR1))
concretetrans = concrete[c(-69),]
TransLR1=concretetrans
TransLR1[,1]= TransLR1[,1]^1.707

MLR1Trans = lm(slump$`Slump Flow` ~ slump$Cement + slump$Slag + slump$`Fly Ash` + slump$Water + slump$SP +
                 slump$`Coarse Aggregate` + slump$`Fine Aggregate`, + data = TransLR1)
summary(powerTransform(MLR1Trans))

#Model2
summary(powerTransform(MLR2))
TransLR2=concretetrans
TransLR2[,1]= TransLR2[,1]^1.653

attach(concrete)
MLR2Trans = lm(`Slump Flow`~  Cement+Slag+ `Fly Ash`+Water+SP+ `Coarse Aggregate` + `Fine Aggregate`+ Slag : Water+
                 Coarse Aggregate : slump$`Fine Aggregate` +Cement: slump$`Fly Ash` + SP:Slag, data = TransLR2)
summary(powerTransform(MLR2Trans))

#Model3
summary(powerTransform(MLR3))
#does not need transformation to normality
               


#Corrective measures
#Adding or deleting variables
vif(MLR1)
sqrt(vif(MLR1))>2
vif(MLR2)
sqrt(vif(MLR2))>2
vif(MLR3)
sqrt(vif(MLR3))>2

#Part 4
#Selecting the best regression model
anova(MLR1, MLR2, MLR3)
AIC(MLR1, MLR2, MLR3) 
# for transformed models
anova(MLR1Trans, MLR2Trans)
AIC(MLR1Trans, MLR2Trans) 

#Part 5
#Fine Tune selection of predictor variables
#Backward Stepwise selection
stepAIC(MLR3, direction = "backward")
MLR3se = lm(formula = slump$`Slump Flow`~ slump$Cement +slump$Slag+slump$`Fly Ash`+slump$Water+slump$SP+slump$`Coarse Aggregate`
            +slump$`Fine Aggregate`+slump$Cement:slump$Water+slump$Slag:slump$`Fly Ash`+ slump$Slag:slump$Water+ slump$Slag:slump$`Coarse Aggregate`+
              slump$Slag:slump$`Fine Aggregate`+ slump$`Fly Ash`:slump$Water+slump$`Fly Ash`:slump$`Coarse Aggregate`+slump$`Fly Ash`:slump$`Fine Aggregate`+
              slump$Water:slump$`Coarse Aggregate`+slump$Water:slump$`Fine Aggregate`, data= concrete)
MLR3se




#Task 3 Forest Fires
#Install packages and load raw data
install.packages("readxl")
install.packages("car")
install.packages("gvlma")
install.packages ("MASS")
library(readxl)
library(car)
library(gvlma)
library(MASS)
ForestFireData123 = read_excel(file.choose())

#Transform to factor & Scatterplot matrix
ForestFireData123$X.fac = factor(ForestFireData123$X)
ForestFireData123$Y.fac= factor(ForestFireData123$Y)
ForestFireData123$Month.fac = factor(ForestFireData123$Month)				
ForestFireData123$Day.fac = factor(ForestFireData123$Day)
ForestFireData123$Area = log1p(ForestFireData123$Area)
Data2 = ForestFireData123[,c(13,14:17,5:12)]
scatterplotMatrix(Data2, spread = FALSE, lty.smooth = 2, main = "Scatter Plot Matrix" ) 

#Feature Selection Setup: STFWI
ModelSTFWI = lm(Area ~ X.fac + Y.fac + Month.fac + Day.fac + FFMC + DMC + DC + ISI, data=Data2) 
summary(ModelSTFWI)
#Feature Selection Setup: STM
ModelSTM = lm(Area ~ X.fac + Y.fac + Month.fac + Day.fac + Temp + RH + Wind + Rain, data=Data2) 
summary(ModelSTM)
#Feature Selection Setup: FWI
ModelFWI = lm(Area ~ FFMC + DMC + DC + ISI, data=Data2)
summary(ModelFWI)
#Feature Selection Setup: M
ModelM = lm(Area ~ Temp + RH + Wind + Rain, data=Data2)

#Confidence intervals
confint(ModelSTFWI)
confint(ModelSTM)
confint(ModelFWI)					
confint(ModelM) 

#Plotting the models
par(mfrow=c(2,2)) 
plot(ModelSTFWI) 
plot(ModelSTM)
plot(ModelFWI) 
plot(ModelM)

#OLS Regression: Normality
par(mfrow=c(2,2))
qqPlot(ModelSTFWI, labels=row.names(Data2), id.method="identify", simulate=TRUE, main="Q-Q Plot- Linear Regression Model")

qqPlot(ModelSTM, labels=row.names(Data2), id.method="identify", simulate=TRUE, main="Q-Q Plot- Linear Regression Model")

qqPlot(ModelFWI, labels=row.names(Data2), id.method="identify", simulate=TRUE, main="Q-Q Plot- Linear Regression Model")

qqPlot(ModelM, labels=row.names(Data2), id.method="identify", simulate=TRUE, main="Q-Q Plot- Linear Regression Model")

#Function for plotting Studentized Residuals
residPlot = function(fit, nbreaks = 10)
{
z = rstudent(fit)
hist(z, breaks = nbreaks, freq = FALSE, xlab = "Studentized Residual", main = "Distribution of Errors") 
rug(jitter(z), col="brown")
curve(dnorm(x, mean=mean(z), sd=sd(z)), 
add=TRUE, col="blue", lwd=2)
lines(density(z)$x, density(z)$y, 
col="red", lwd=2, lty=2)
legend("topright", 
legend = c("Normal Curve", "Kernel Density Curve"), 
lty = 1:2, col=c("blue", "red"), cex=.7 )
					
}

residPlot(ModelSTFWI)
residPlot(ModelSTM)
residPlot(ModelFWI)
residPlot(ModelM)

					
# OLS Regression: Indepedence of Errors
durbinWatsonTest(ModelSTFWI) 
durbinWatsonTest(ModelSTM) 
durbinWatsonTest(ModelFWI) 
durbinWatsonTest(ModelM) 

# OLS Regression: Linearity
crPlots(ModelSTFWI) 
crPlots(ModelSTM) 
crPlots(ModelFWI) 
crPlots(ModelM) 

# OLS Regression: Homoscedasticity
ncvTest(ModelSTFWI) 
spreadLevelPlot(ModelSTM)
ncvTest(ModelSTM) 
spreadLevelPlot(ModelSTFWI)
ncvTest(ModelSTFWI) 
spreadLevelPlot(ModelSTFWI)
ncvTest(ModelM) 
spreadLevelPlot(ModelM)

#Global validation of linear model assumption
gvlma(ModelSTFWI)
gvlma(ModelSTM)
gvlma(ModelFWI)					
gvlma(ModelM)

#Multicolinearity

vif(ModelSTFWI) 
sqrt(vif(ModelSTFWI))>2 
vif(ModelSTM) 
sqrt(vif(ModelSTM))>2 
vif(ModelFWI) 
sqrt(vif(ModelFWI))>2 
vif(ModelM) 
sqrt(vif(ModelM))>2 

#Outliers
outlierTest(ModelSTFWI)
outlierTest(ModelSTM)
outlierTest(ModelFWI)
outlierTest(ModelM)

#Added-Value Plots
avPlots(ModelSTFWI, ask=FALSE, onepage=TRUE, id.method="identify" ) 
avPlots(ModelSTM, ask=TRUE, onepage=TRUE, id.method ="identify" ) 
avPlots(ModelFWI, ask=TRUE, onepage=TRUE, id.method ="identify" ) 
avPlots(ModelM, ask=TRUE, onepage=TRUE, id.method ="identify" ) 

#Influential Observations- Cooks Distance
par(mfrow=c(2,2))
D.plot = function(fit, data) { 
cutoff = 4/(nrow(data)-length(fit$coefficients)-2)
 plot(fit, which = 4, cook.levels = cutoff) 
abline(h=cutoff, lty=2, col="red") 
}

D.plot(ModelSTFWI, Data2) 
D.plot(ModelSTM, Data2) 
D.plot(ModelFWI, Data2) 
D.plot(ModelM, Data2) 

#Deleting observations
Data2_Del = Data2[c(-239, -416, -421, -517, -305, -472, -105, -500, -200, -393),] 
#Model 1: STFWI
ModelSTFWI_Del = lm(Area ~ X.fac + Y.fac + Month.fac + Day.fac + FFMC + DMC + DC + ISI, data=Data2_Del) 
summary(ModelSTFWI_Del)
#Model 2: STM
ModelSTM_Del = lm(Area ~ X.fac + Y.fac + Month.fac + Day.fac + Temp + RH + Wind + Rain, data=Data2_Del) 
summary(ModelSTM_Del)
#Model 3 : FWI
ModelFWI_Del = lm(Area ~ FFMC + DMC + DC + ISI, data=Data2_Del)
summary(ModelFWI_Del)
#Model 4 : M
ModelM_Del = lm(Area ~ Temp + RH + Wind + Rain, data=Data2_Del)
summary(ModelM_Del)

# where lambda = 1 for Box-Cox transformation to normality
Data2_Trans = Data2
Data2_Trans$Area = Data2_Trans$Area+1 
summary(powerTransform(Data2_Trans$Area))

Data2_Trans[,1]= Data2_Trans[,1]^(-0.7143)
#Transformed STFWI
ModelSTFWI_Trans = lm(Area ~ X.fac + Y.fac + Month.fac + Day.fac + FFMC + DMC + DC + ISI, data= Data2_Trans) 
summary(powerTransform(ModelSTFWI_Trans))
#Transformed STM
ModelSTM_Trans = lm(Area ~ X.fac + Y.fac + Month.fac + Day.fac + Temp + RH + Wind + Rain, data=Data2_Trans) 
summary(powerTransform(ModelSTM_Trans)) 
#Transformed FWI
ModelFWI_Trans = lm(Area ~ FFMC + DMC + DC + ISI, data=Data2_Trans) 
summary(powerTransform(ModelFWI_Trans)) 
#Transformed M
ModelM_Trans = lm(Area ~ Temp + RH + Wind + Rain, data=Data2_Trans) 
summary(powerTransform(ModelM_Trans)) 

#Adding or Deleting variables
vif(ModelSTFWI_Trans)
sqrt(vif(ModelSTFWI_Trans))>2
Data2_Trans_Drop = Data2_Trans[,!(names(Data2_Trans) %in% c("DC"))] ModelSTFWI_Trans = lm(Area ~ X.fac + Y.fac + Month.fac + Day.fac + FFMC + DMC + ISI, data=Data2_Trans_Drop) 
vif(ModelSTM_Trans) 
sqrt(vif(ModelSTM_Trans))>2 
vif(ModelFWI_Trans) 
sqrt(vif(ModelFWI_Trans))>2 
vif(ModelM_Trans) 
sqrt(vif(ModelM_Trans))>2 

#Selecting best model
anova(ModelSTFWI_Trans, ModelSTM_Trans, ModelFWI_Trans, ModelM_Trans)
AIC(ModelSTFWI_Trans, ModelSTM_Trans, ModelFWI_Trans, ModelM_Trans)
summary(ModelSTM_Trans)
summary(ModelM_Trans) 

#Stepwise Selection
stepAIC(ModelSTM_Trans, direction = "backward")
ModelSTM_VS_BK = lm(formula = Area ~ X.fac + Y.fac + Month.fac + Wind, dat = Data2_Trans) 
stepAIC(ModelSTM_Trans, direction = "forward")
ModelSTM_VS_FD = lm(formula = Area ~ X.fac + Y.fac + Month.fac + Day.fac + Temp + RH + Wind + Rain, data = Data2_Trans) 
stepAIC(ModelSTM_Trans, direction = "both")

#Comparing backward and forward models
anova(ModelSTM_VS_BK, ModelSTM_VS_FD)
AIC(ModelSTM_VS_BK, ModelSTM_VS_FD)
