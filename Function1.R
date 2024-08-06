#clear the work space of all variables
rm(list = ls())

# 1. The Basic Logistic Model

#define the basic logistic function
logfun <- function(t,KA,X0A,RA) {((KA*X0A*exp(RA*t))/((KA-X0A)+X0A*exp(RA*t)))}

#define a function to calculate the error between the basic logistic function
#and the data
errlogfun <-function(param, tid,MTTdata){
  KA <-param[1]
  X0A <-param[2]
  RA <-param[3]
  
  err0 <- sum((unlist(lapply(tid,logfun,KA,X0A,RA)) - MTTdata)^2)/mean(MTTdata)^2
  return(err0)
}
##################################################################################
# 2. Time-Varying Growth Rate R(t) Logistic Model
# W1 is associated with the periodic growth rate 

#define the time-varying growth rate logistic function
growthfun <- function(t,KB, X0B,R0B,R1B,W1B,t0) {
  (KB*X0B)/((KB-X0B)*exp(-((R0B*t)+((R1B*R0B)*sin(W1B*t-t0))/W1B))+X0B)
}

#define a function to calculate the error between the time-varying growth rate
#logistic function and the data
errgrowthfun <-function(param,tid,MTTdata){
  KB <-param[1]
  X0B <-param[2]
  R0B <-param[3]
  R1B <- param[4]
  W1B <- param[5]
  t0 <- param[6]
  err1 <- sum((unlist(lapply(tid,growthfun,KB,X0B,R0B,R1B,W1B,t0)) - MTTdata)^2)/mean(MTTdata)^2
  return(err1)
}
#####################################################################################
# 3. Time-Varying Carrying Capacity K(t) Logistic Model

#define the time-varying carrying capacity logistic function
#carfun <- function(t,X0C,RC,W2C,K0C,K1C,t1) {X0C*K0C*(RC^2+W2C^2)/(
#  ((((-1-K1C)*RC^2)-W2C^2)*X0C+K0C*(RC^2+W2C^2))*exp(-RC*t)+
#    X0C*(RC^2+W2C^2+RC^2*K1C*cos(W2C*(t-t1))+RC*K1C*W2C*sin(W2C*(t-t1))))}
carfun <- function(t,X0C,RC,W2C,K0C,K1C,t1) {
  K0C*(RC^2+W2C^2)*X0C*exp(RC*t)/(((1+K1C*cos(W2C*t-t1))*RC^2+RC*K1C*W2C*sin(W2C*t-t1)+W2C^2)*X0C*exp(RC*t)+((-1-K1C*cos(t1))*RC^2+RC*K1C*W2C*sin(t1)-W2C^2)*X0C+K0C*(RC^2+W2C^2))
}
#define a function to determine the error between the time-varying
#carrying capacity logistic function and the data
errcarfun <-function(param,tid,MTTdata){
  X0C <- param[1]
  RC <- param[2]
  W2C <- param[3]
  K0C <- param[4]
  K1C <- param[5]
  t1 <- param[6]
  err2 <- sum((unlist(lapply(tid,carfun,X0C,RC,W2C,K0C,K1C,t1)) - MTTdata)^2)/mean(MTTdata)^2
  return(err2)
}
#####################################################################################
# 4. Time-Varying Growth Rate R(t) & Time Varying Carrying Capacity K(t)
#Logistic Model

#define a function for the ode solver of the time-varying growth rate and
#carrying capacity function
growthcarfun <- function(t,x,parameters){
  R0D<-parameters[1]
  R1D<-parameters[2]
  W1D<-parameters[3]
  W2D<- parameters[4]
  K1D<-parameters[5]
  K0D<-parameters[6]
  t0 <-parameters[7]
  t1 <-parameters[8]
  
  with(as.list(c(parameters)), {
    
    list( c( (R0D+(R1D*R0D)*cos(W1D*t-t0))*x*(1-(1+K1D*cos(W2D*t-t1))*x/K0D)))
  })
}

#define a function to calculate the error between the time-varying growth rate
#and time-varying carrying capacity logistic function to the data
errgrowthcarfun <-function(parameters,MTTdata,tid){
  
  X0D<-parameters[1]
  R0D<-parameters[2]
  R1D<-parameters[3]
  W1D<-parameters[4]
  W2D<-parameters[5]
  K1D<-parameters[6]
  K0D<-parameters[7]
  t0 <-parameters[8]
  t1 <-parameters[9]
  
  #make a list of time values from the data values for the ODE solver
  tt <- seq(min(tid),max(tid),0.1)
  tt0 <- min(tid)
  
  #the initial conditions to pass through the ODE solver
  state = c(X0D = X0D)
  
  #parameters for the ODE solver
  parameters <- c(R0D=R0D, R1D=R1D, W1D=W1D, W2D=W2D, K1D=K1D, K0D=K0D, t0=t0,t1=t1)
  
  #define what is to be represented in the output from the ODE solver
  myDEs <- ode(y = state, times = tt, func = growthcarfun, parms = parameters)
  
  #define compartments by ODE output
  x <- myDEs[,2]
  
  #compute the square error
  errgrowthcarfun<- function(t,x,deltat,t00){x[((t-t00)/deltat)+1]}
  errfun <-unlist(lapply(tid,errgrowthcarfun,x,t00=tt0,deltat=(tt[2]-tt[1])))
  err3 = sum((errfun-MTTdata)^2)/mean(MTTdata)^2
  return(err3)
}


