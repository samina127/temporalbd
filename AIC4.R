#AIC For Basic Logistic Growth Model
Na<-length(MTTdata)
SumSqErra<- result0$value*((mean(MTTdata))^2)
Pa<- 4
AIClogfun[q]<- (Na*(log(SumSqErra/Na))+2*Pa)
show(AIClogfun[q])

#AIC For Time-Varying Growth Rate Model
Nb<-length(MTTdata)
SumSqErrb<- result1$value*((mean(MTTdata))^2)
Pb<- 6
AICgrowthfun[q]<- (Nb*(log(SumSqErrb/Nb))+2*Pb)
show(AICgrowthfun[q])

#AIC For Time-Varying Carrying Capacity Model
Nc<-length(MTTdata)
SumSqErrc<- result2$value*((mean(MTTdata))^2)
Pc<- 6
AICcarfun[q]<- (Nc*(log(SumSqErrc/Nc))+2*Pc)
show(AICcarfun[q])

#AIC For Both Time-Varying Growth Rate & Carrying Capacity
Nd<-length(MTTdata)
SumSqErrd<- result3$value*((mean(MTTdata))^2)
Pd<- 7
AICgrowthcarfun[q]<- (Nd*(log(SumSqErrd/Nd))+2*Pd)
show(AICgrowthcarfun[q])

#Calculate The Difference Between AIC Values
Model1<-AIClogfun-AICgrowthfun
Model2<-AIClogfun-AICcarfun
Model3<-AIClogfun-AICgrowthcarfun

print(Model1)
print(Model2)
print(Model3)

