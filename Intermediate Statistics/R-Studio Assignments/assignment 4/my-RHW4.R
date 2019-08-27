#Jeremiah ALbright
#RH4

###################################################################
#1 
#column 1
set.seed(101)
M<- matrix(NA, nrow = 1000, ncol = 1)
for (i in 1:1000){
  M[i,] <- rnorm(1, mean =45, sd=3)
}



# Vector to assign names to the columns of matrix A
a=c()
for (i in 1:1) {
  d=paste("X", i, sep ="")
  a=c(a,d)
}
a
colnames(M) <- a


#column 2
set.seed(185)
N<- matrix(NA, nrow = 1000, ncol = 1)
for (i in 1:1000){
  N[i,] <- rnorm(1, mean =55, sd=3)
}



# Vector to assign names to the columns of matrix A
a=c()
for (i in 1:1) {
  d=paste("X", i, sep ="")
  a=c(a,d)
}
a
colnames(N) <- a





#column 3
set.seed(177)
O<- matrix(NA, nrow = 1000, ncol = 1)
for (i in 1:1000){
  O[i,] <- rnorm(1, mean =65, sd=3)
}



# Vector to assign names to the columns of matrix A
a=c()
for (i in 1:1) {
  d=paste("X", i, sep ="")
  a=c(a,d)
}
a
colnames(O) <- a



#column 4
set.seed(197)
P<- matrix(NA, nrow = 1000, ncol = 1)
for (i in 1:1000){
  P[i,] <- rnorm(1, mean =75, sd=3)
}



# Vector to assign names to the columns of matrix A
a=c()
for (i in 1:1) {
  d=paste("X", i, sep ="")
  a=c(a,d)
}
a
colnames(P) <- a

#column 5
set.seed(109)
Q<- matrix(NA, nrow = 1000, ncol = 1)
for (i in 1:1000){
  Q[i,] <- rnorm(1, mean =85, sd=3)
}



# Vector to assign names to the columns of matrix A
a=c()
for (i in 1:1) {
  d=paste("X", i, sep ="")
  a=c(a,d)
}
a
colnames(Q) <- a


##combine them
grades = data.frame(M,N,O,P,Q)
colnames(grades)[1] <- "x=20"
colnames(grades)[2] <- "x=25"
colnames(grades)[3] <- "x=30"
colnames(grades)[4] <- "x=35"
colnames(grades)[5] <- "x=40"
View(grades)

#######################################################################
#2)

#b1=sxy/sxx
B1 = function(a,b){
  
  
  #get sxy
 d=0
 for(i in 1:length(a)){
   d=d+(a[i]*b[i])
   
   
 }
  
  sxy=d-((sum(a)*sum(b))/(length(a)))
  
  
  #get sxx
  sxx= (sum(a^2))-((sum(a)^2)/(length(a)))
  
  return(sxy/sxx)
  
}

#B0=mean(Y)-B1*mean(X)
B0=function(a,b){
  
  avg = (mean(b))-((B1(a,b))*(mean(a)))
  return (avg)
  
}


#SSE = syy-B1*sxx
SSE=function(a,b){
  syy= (sum(b^2))-((sum(b)^2)/(length(b)))
  sxx= (sum(a^2))-((sum(a)^2)/(length(a)))
  
  get=syy-((B1(a,b))*sxx)
  
  return(get)
  
  
}


#MSE = SSE/(n-2)

MSE = function(SSE, n) {
  
  
  return ((SSE)/(n-2))
  
  
  
}

####################################################################
#3

b0 = matrix(NA, nrow = 1000, ncol = 1)
b1 = matrix(NA, nrow = 1000, ncol = 1)
o2 = matrix(NA, nrow = 1000, ncol = 1)

mat =data.frame(b0,b1,o2)
colnames(mat)[1]="b0"
colnames(mat)[2]="b1"
colnames(mat)[3]="o^2"


View(mat)


for(i in 1:1000){
  
  
  
  
}

