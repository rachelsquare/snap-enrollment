###
### TITLE: State Legislative Control and SNAP Participation 
### NAME: Rachel A. Skwerer
### DATE: 2017 04 16
### MAIL: skwerer.1@osu.edu
###

###
### Preamble
###

### Setting working directory
setwd("C:/Users/raskwerer/OneDrive/Data Projects GitHub/snap-enrollment")

### Importing 'ggplot2' library
library(ggplot2) 

install.packages("dotwhisker")
library(dotwhisker)


### Importing 'car'library
library(car)

source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")


### Importing dataset 'SNAPdata'with columns 'state'==State, 
### 'leg_con' == party id of political party in control of state 
### legislature (0-republican, 1-democrat, 2-split or n/a), 'pop'==
### population, 'num_poor'== population below poverty line, 
### 'SNAP_num'== population enrolled in the Supplemental Nutrition
### Assistance Program (SNAP), 'perc_poor'== percentage of population
### below poverty line, 'perc_partic'== percentage of population 
### participating in SNAP, and 50 rows. 
SNAPdata<- read.csv("Final Data.csv") 


### Creating new column 'leg' with variable 'leg_con' as a factor
### variable:
SNAPdata$leg = factor(SNAPdata$leg_con)


###
### Code body
###

### Building the model

## Plotting each independent variable against the dependent varibale 
## to get an idea of the general relationship:

SNAPdata<-SNAPdata[-51,]

levels(SNAPdata$leg)<-c("Republican", "Democrat","Non-partisan/Split")

ggplot(SNAPdata, aes(leg, perc_partic))+geom_boxplot(ymin=0, ymax=15)+
  labs(title="Participation in SNAP(%) versus Party ID of Legislature", 
       x= "Party ID of State Legislature", y="State Participation in SNAP(%)" ) + theme_minimal()


## Regressing percentage of population participating in SNAP on 
## partisan control of state legislator and percentage of population
## living in poverty
SNAPdata$leg<-relevel(SNAPdata$leg, ref="Republican")
model_1 <- lm(perc_partic ~ leg+perc_poor, data=SNAPdata)


## Summarizing model_1:
summary(model_1) 


## Plotting coefficients of model_1:
dwplot(model_1, show_intercept = TRUE) + ggtitle("Plot 1: Coefficients for SNAP Participation Predictors") + theme_minimal() 

plot2<- ggplot(SNAPdata, aes(perc_poor, perc_partic))+geom_point(aes(colour=leg)) + 
  labs(title="Plot 2: State Participation in SNAP(%) versus Population
       Below Poverty Line(%)", y="State Participation in SNAP(%)", x="
       Population Below Poverty Line(%)") + theme_minimal() 
plot2+stat_function(fun=function(x){ 0.7073 + x*(0.5701)}, colour="red") +
  stat_function(fun=function(x){ 0.7073 + x*(0.5701) + 2.29}, colour="green") +
  stat_function(fun=function(x){ 0.7073 + x*(2.29)}, colour="blue")



### Testing the model assumptions 

## Creating object, y_hat, for the predictions for model_1:
y_hat <- predict(model_1)

## Creating object, model_1_residuals, for the residuals of model_1:
model_1_resid <- model_1$residuals

## Plotting predictions for model_1 against residuals for model_1: 
## (The plot should look random, because ideally the errors around 
## the predicted values is random. If not, it would mean there is some 
## underlying mechanism that we haven't explained using the model.)
qplot(y_hat, model_1_resid)+ theme_minimal() + labs(title=
                                                      "Plot 3: Residuals versus Predictions")+ 
  geom_smooth() #The plot looks random. 

## Plotting residuals for model one against each of the independent 
## variables (The plots should look random, because ideally our
## residual are randomly distributed. If not, it would mean that
## there is some systematic relationship between the residuals and
## the independent variables that hasn't been explained using the
## model.):
qplot(model_1$model$leg, model_1_resid)+ theme_minimal()+
  labs(title="Plot 4: Residuals versus Party IDs 
       of State Legislatures", x= "Party ID of State Legislature 
       " ) #The plot looks random, but how do I know??
qplot(model_1$model$perc_poor, model_1_resid) + theme_minimal() + 
  labs(title="Plot 5: Residuals versus Population
       in Poverty(%)", x="Population Below Poverty Line(%)") #The plot looks random.

## Plotting partial residuals against the independent variables (If 
## the model is working, the red and green lines should basically 
## overlap.):
crPlots(model_1, main="Plot 6: Component + Residual Plots") 

## Running the Breusch-Pagan or non-constant variance (NCV) test (We
## test for homoscedasticity versus heteroscedasticity, or 
## non-constant variance, because we must assume constant variance 
## in the model. The test results that we use are the p-values. If 
## the value is less than a pre-defined p-value (e.g. 0.05), we 
## assume that there is heteroscedasticity or non-constant variance,
## and we have a problem.): 
ncvTest(model_1) # p = 0.802295, so we're ok.
ncv<-0.802295

## Testing normality of errors(When calculating the p-values, we 
## assume that the errors are normally distributed. In order to test
## this assumption, qqplot plots the quantiles from a true normal
## distribution against the quantiles from the residuals.If the 
## residuals followed a normal, the top 5% should only be 2 standard 
## deviations above 0; if the acutal top 5% are only 1.5 standard 
## deviations above 0,  our residuals may not be following a normal
## distribution. If most of the points in our plot fall within the 
## dashed lines,which represent confidence intervals, then our errors
## are distributed normally.):
qqPlot(model_1, main="Plot 7: Normality of Errors") #No points (quantiles) fall outside, so we're ok.

## Testing for multicollinearity (Computing the variance inflation
## factor, or VIF, indicates how inflated the standard errors
## are. The square root of the VIF can be interpreted as the 
## degree of inflation in the standard error. If the VIF is 
## 2, for example, the s.e. is twice as large as it would be
## if the indep. variable was uncorrelated with the others.): 
model_1_vif<-vif(model_1)
sqrt(model_1_vif)
vif<- 1.053586
stats<-c(ncv, vif)
labs<-c("ncv", "vif")

qplot(x= c("NCV", "VIF"),y=c(ncv,vif),geom="point", main="Plot 8: Diagnostics (NCV & VIF)", ylab="Level", xlab="Diagnostic") + theme_minimal()
