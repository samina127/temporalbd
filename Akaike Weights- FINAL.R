#akaike weight across isolates
#store aic scores in a matrix 
A = matrix(c(0,9.8977,	11.7321	,11.8487,
            0, -4.0421,	-30.6386,	-40.0018,
            0, -30.5403	,-5.196,	-46.1259,
            0, -62.7305,	-34.7602,	-63.3417,
            0, -37.5915,	-18.6128,	-18.9723,
            0, -36.1384	,-17.0124	,-18.6951,
            0, -20.6476,	-6.2104,	-19.4482),
           nrow = 7,
           ncol = 4,
           byrow = TRUE)
colnames(A) = c("logfun","growthfun","carfun","growthcarfun")
rownames(A) = c("4","12","17","21","25","26","27")           
print(A)

#temp akaike denominator values
temp4 <- sum(exp(-0.5*A[1,]))
temp12 <- sum(exp(-0.5*A[2,]))
temp17 <- sum(exp(-0.5*A[3,]))
temp21 <- sum(exp(-0.5*A[4,]))
temp25 <- sum(exp(-0.5*A[5,]))
temp26 <- sum(exp(-0.5*A[6,]))
temp27 <- sum(exp(-0.5*A[7,]))

#temp4 akaike
lapply(A[1,], function(x){exp(-0.5*x)/temp4})

#temp12 akaike
lapply(A[2,],function(x){exp(-0.5*x)/temp12})

#temp17 akaike
lapply(A[3,],function(x){exp(-0.5*x)/temp17})

#temp21 akaike
lapply(A[4,],function(x){exp(-0.5*x)/temp21})

#temp25 akaike
lapply(A[5,],function(x){exp(-0.5*x)/temp25})

#temp26 akaike
lapply(A[6,],function(x){exp(-0.5*x)/temp26})

#temp27 akaike
lapply(A[7,],function(x){exp(-0.5*x)/temp27})

#####################################################################
#akaike weight across temperatures 
#store aic scores in a matrix 
B = matrix(c(0,-22.4580,	6.1920,	-26.8940,
             0, -56.281,	-59.254,	-69.298,
             0, -48.887,	-32.015,	-76.564,
             0, -60.167,	-21.622,	-30.981),
           nrow = 4,
           ncol = 4,
           byrow = TRUE)
colnames(B) = c("logfun","growthfun","carfun","growthcarfun")
rownames(B) = c("LA","NM","OH","TN")           
print(B) 


#isolate akaike denominator values
LA <- sum(exp(-0.5*B[1,]))
NM <- sum(exp(-0.5*B[2,]))
OH <- sum(exp(-0.5*B[3,]))
TN <- sum(exp(-0.5*B[4,]))

#LA akaike
lapply(B[1,], function(x){exp(-0.5*x)/LA})

#NM akaike
lapply(B[2,],function(x){exp(-0.5*x)/NM})

#OH
lapply(B[3,],function(x){exp(-0.5*x)/OH})

#TN
lapply(B[4,],function(x){exp(-0.5*x)/TN})
