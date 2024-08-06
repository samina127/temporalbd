#generalizing AIC values
#coding the logistic model AICs into a matrix 
logfun = matrix(
  c(-462.3636, -325.4686, -130.1405,-103.1529, -200.5581, -189.2776, -258.9787,
    -619.2097,-400.2928, -128.9328, -162.9391, -226.5601,  -195.2315, -253.0702,
    -696.6526, -475.9088,
    -219.8419,-187.3592,  -197.5805, -171.9599,-229.7017,
    -775.0110, -411.7495,-233.4606, -189.9003, -179.9296,-204.2852,-257.2252), 
  nrow=4,
  ncol = 7,
  byrow = TRUE
)
#assigning names to the rows and columns
rownames(logfun) = c("LA","NM","OH","TN")
colnames(logfun) = c("4","12","17","21","25","26","27")
print(logfun)

#adding together all of the AICs across the columns or isolates
isolatelogfun <- colSums(logfun)

#subtracting the degrees of freedom multiplied by the number of parameters to 
#account for over counting
isolatelogfun - 4*3

#adding together all of the AICs across the rows or temperatures
templogfun <- rowSums(logfun)

#subtracting the degrees of freedom multiplied by the number of parameters to 
#account for over counting
templogfun - 4*6

#growth function matrix
#coding the time-varying growth rate model AICs into a matrix 
growthfun = matrix(
  c(-458.4049, -325.9351, -132.3306, -101.22, -198.1566,-200.1947,-264.156,
    -615.2448, -402.5727, -128.385,  -178.5106,-238.3816,-210.8139, -256.6085,
    -692.6929, -472.7163, -242.1374, -203.1161, -200.2775,-173.886, -231.0654,
    -770.9966,-410.2377,  -234.0631, -217.2353, -199.4041, -205.9980, -261.7935),
  nrow = 4,
  ncol = 7,
  byrow = TRUE
)
#assigning names to the rows and columns
rownames(growthfun) = c("LA","NM","OH","TN")
colnames(growthfun) = c("4","12","17","21","25","26","27")
print(growthfun)

#adding together all of the AICs across the columns or isolates
isolategrowthfun <- colSums(growthfun)

#subtracting the degrees of freedom multiplied by the number of parameters to 
#account for over counting
isolategrowthfun - 6*3

#adding together all of the AICs across the rows
tempgrowthfun <- rowSums(growthfun)

#subtracting the degrees of freedom multiplied by the number of parameters to 
#account for over counting
tempgrowthfun - 6*6

#carrying capacity matrix 
#coding the time-varying carrying capacity model AICs into a matrix
carfun = matrix(
  c(-456.5515, -324.586, -127.8308,-105.1036,-197.4213, -185.2767,-254.9784,
    -615.2201,-405.6032, -129.0806,  -193.0199, -235.6971, -198.9082,-255.9606,
    -692.6826,-490.6134, -222.7039, -186.1133, -194.0806, -178.0197,-234.8064,
    -771.0506, -417.2557, -231.9565, -187.8749,-190.0421, -209.5620,-253.4408),
  nrow = 4,
  ncol = 7,
  byrow = TRUE
)
#assigning names to the rows and columns
rownames(carfun) = c("LA","NM","OH","TN")
colnames(carfun) = c("4","12","17","21","25","26","27")
print(carfun)

#adding together all of the AICs across the columns or isolates
isolatecarfun <- colSums(carfun)

#subtracting the degrees of freedom multiplied by the number of parameters to 
#account for over counting
isolatecarfun - 6*3

#adding together all of the AICs across the rows or temperature
tempcarfun <- rowSums(carfun)

#subtracting the degrees of freedom multiplied by the number of parameters to 
#account for over counting
tempcarfun - 6*6

#growth rate and carrying capacity matrix
#coding the time-varying growth rate & carrying capacity model AICs into a matrix
growthcarfun = matrix(
  c(-459.3708, -329.7589,-130.5935, -105.2566,-195.7768,-195.8086, -262.2687,
    -613.2576, -399.7114,-151.653, -187.8518, -230.6192, -197.6728,-256.7678,
    -690.7234, -498.7264, -233.1347,-213.4574, -202.4619, -173.6464,-225.4191,
    -769.0364, -416.2248, -234.1205, -191.1274, -185.7427, -203.3215, -264.9684),
  nrow = 4,
  ncol = 7,
  byrow = TRUE
)
#assigning names to the rows and columns
rownames(growthcarfun) = c("LA","NM","OH","TN")
colnames(growthcarfun) = c("4","12","17","21","25","26","27")
print(growthcarfun)

#adding together all of the AICs across the columns or isolates
isolategrowthcarfun <- colSums(growthcarfun)

#subtracting the degrees of freedom multiplied by the number of parameters to 
#account for over counting
isolategrowthcarfun - 7*3

#adding together all of the AICs across the rows or temperature
tempgrowthcarfun <- rowSums(growthcarfun)

#subtracting the degrees of freedom multiplied by the number of parameters to 
#account for over counting
tempgrowthcarfun - 7*6
