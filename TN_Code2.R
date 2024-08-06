#################################################################
#
# Project name: Temporal and thermal effects on the abundance of lethal fungal 
#pathogens of amphibians 
#
# Script authors: Sarah Dingel and Samina Hanif
#
# Main script:
# 1) load data
# 2) load models
# 3) run optim to fit to models to data
# 4) Create plots of model fits to data
# 5) Calculate AIC scores to quantify 'best-fit' of models to data
#################################################################

#load the necessary packages
library(readxl)
library(pracma)
library(deSolve)
library(npreg)
library(numDeriv)

source("~/Downloads/AIC4.R")

#make an array to store the AIC values
AIClogfun= rep(0,7)
AICgrowthfun= rep(0,7)
AICcarfun= rep(0,7)
AICgrowthcarfun= rep(0,7)

#create a string of the temperature variables with respect to the isolate data
tempOisolate=c(4,12,17,21,25,26,27)
nameOfile= 'Desktop/TN_Isolate_%d'
str(tempOisolate)

#import in the excel file
Isolate_Data <- read_excel("Downloads/Summer Research 2024/TN.Isolate.xlsx")

#create a for loop for the isolate data at different temperatures
AIC=rep(0,length(tempOisolate))
for (q in 1:length(tempOisolate)) {
  #for (q in 7) {
  
  Isolate = grep('TN',Isolate_Data$Isolate)
  
  #create variables for the particular temperatures within the isolate data
  Isolate1<-Isolate_Data[Isolate,]
  str(Isolate1[,3])
  
  Temp=which(Isolate1[,3]==tempOisolate[q])
  Temp1= Isolate1[Temp,]
  
  Fullnameoffile=sprintf(nameOfile,tempOisolate[q])
  show(Fullnameoffile)
  
  #assign names to the specific columns within the data excel file
  tid <- Temp1$Day
  Isolate_A <- Temp1$MTT
  MTTdata <- c(Isolate_A)
  
  #define the best guesses for each parameter
  #use the values from the logistic paper to determine these
  KAguess = max(MTTdata)
  X0Aguess = min(MTTdata)
  RAguess = 0.05
  
  #optimize the guesses for each parameter using the square error:
  result0 = optim(c(KAguess, X0Aguess,RAguess), errlogfun , method = "L-BFGS-B",
                  lower = c(0.01, 0.01, 0.001),
                  upper = c(510, 0.5, 1.2),MTTdata=MTTdata,tid=tid)
  
  #define the best guesses for each parameter
  # **values from the basic logistic model can be utilized to ensure a better fit
  KBguess =result0$par[1]
  X0Bguess = result0$par[2]
  R0Bguess= result0$par[3]
  R1Bguess= 0.01
  W1Bguess= 1
  t0guess = pi
  #optimize the guesses for each parameter using the square error
  result1 = optim(c(KBguess, X0Bguess,R0Bguess,R1Bguess,W1Bguess,t0guess), errgrowthfun, method = "L-BFGS-B",
                  lower = c(0.4*KBguess, 0.5*X0Bguess, 0.1*R0Bguess, -1, 0,0),
                  upper = c(1.5*KBguess, 1.2*X0Bguess, 2.5*R0Bguess, 1, 10,2*pi),
                  MTTdata=MTTdata,tid=tid)
  
  #define the best guesses for each parameter
  # **values from the previous models can be utilized to ensure a better fit
  X0Cguess = result0$par[2]
  RCguess = result0$par[3]
  W2Cguess = 0.2
  K0Cguess = result0$par[1]
  K1Cguess = 0.9
  t1guess = pi
  #optimize the guesses for each parameter using the square error
  result2 = optim(c(X0Cguess,RCguess,W2Cguess,K0Cguess,K1Cguess,t1guess), errcarfun, method = "L-BFGS-B",
                  lower = c(0.02*X0Cguess, 0.09*RCguess, 0 ,0.09*K0Cguess, -1,0),
                  upper = c(1.2*X0Cguess,     6*RCguess, 50, 1.5*K0Cguess, 1,2*pi),
                  MTTdata=MTTdata,tid=tid)
  
  #parameter values for the optimization solver: use results from prior models
  parameters <- c(X0D=result0$par[2],
                  R0D=result0$par[3],
                  R1D=result1$par[4],
                  W1D=result1$par[5],
                  W2D=result2$par[3],
                  K1D=result2$par[5],
                  K0D=result0$par[1],
                  t0 =result1$par[6],
                  t1 = result2$par[6])
  
  #define the best guesses for each parameter for the optimization solver
  X0Dguess=result0$par[2]
  R0Dguess=result0$par[3]
  R1Dguess=result1$par[4]
  W1Dguess=result1$par[5]
  W2Dguess=result2$par[3]
  K1Dguess=result2$par[5]
  K0Dguess=result0$par[1]
  t0guess =result1$par[6]
  t1guess = result2$par[6]
  #optimize the guesses for each parameter using the square error
  result3 = optim(c(X0Dguess, R0Dguess,R1Dguess,
                    W1Dguess,W2Dguess,
                    K1Dguess,K0Dguess,t0guess,t1guess),
                  errgrowthcarfun, method = "L-BFGS-B",
                  lower = c(0.02*X0Dguess, 0.99*R0Dguess, -1, 
                            0.001, 0.001,
                            -1, 0.99*K0Dguess,0,0),
                  upper = c(1.5*X0Dguess,1.1*R0Dguess, 1,
                            4.5*W1Dguess,7*W2Dguess,
                            1,1.02*K0Dguess,2*pi,2*pi),
                  MTTdata=MTTdata,tid=tid)
  
  result3$par
  result3$value
  
  #use the newly optimized values in the ode solver to generate an optimized logistic model
  tt <-seq(min(tid),max(tid),0.001)
  myDEs <- ode(y = c(result3$par[1]), times = tt, func = growthcarfun, parms =
                 c(result3$par[2],result3$par[3],
                   result3$par[4],result3$par[5],
                   result3$par[6],result3$par[7],
                   result3$par[8],result3$par[9]))
  
  #create plots
  source("~/Downloads/Plot3.R")
  
  #Compute AIC scores
  source("~/Downloads/AIC4.R")
1 
}

