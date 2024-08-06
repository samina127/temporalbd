#1. Basic Logistic Model
plot(tt,bty = 'n', logfun(tt,result0$par[1],result0$par[2],result0$par[3]),type='l',
     ylab = 'Zoospore Density',xlab = 'Time (in days)',col = "navyblue",ylim=c(0,1.5),lwd=1.5)
#2. Time-Varying Growth Rate Logistic Model
lines(tt, growthfun(tt,result1$par[1],result1$par[2],result1$par[3],
                     result1$par[4],result1$par[5],result1$par[6]),
      type='l',ylab = 'Absorbance',xlab = 'Time (in days)',col = "forestgreen",lwd=1.5)
#3. Time-Varying Carrying Capacity Logistic Model
lines(tt, carfun(tt,result2$par[1],result2$par[2],
                  result2$par[3],result2$par[4],result2$par[5],result2$par[6]),
      type='l',ylab = 'Absorbance',xlab = 'Time (in days)', col = "darkmagenta",lwd=1.5)
#4. Time-Varying Growth Rate & Carrying Capacity Logistic Model
lines(myDEs[,1],myDEs[,2],type='l', ylim=c(0,1),
      ylab = 'Absorbance', xlab = 'Time (in days)', col = "deeppink", lwd=1.5)

#replot data points on graphs
points(tid,MTTdata,col='black',pch=18)

#add legend to graph
#legend ("topleft",fill = c("navyblue","forestgreen","darkmagenta","deeppink"),
      #legend=c("basic logistic model","time-varying growth rate",
                #"time-varying carrying capacity","time-varying growth rate
                #& carrying capacity"),cex = 0.6)
