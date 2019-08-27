#Jeremiah ALbright
#stats 103
#4/28/19




#set up 3 matrixes with mean =5, standard dev =3, and size n =5,7,9
m = 5
M = matrix(NA,nrow=1000,ncol=m)
n = 7
N = matrix(NA,nrow=1000,ncol=n)
o = 9
O= matrix(NA,nrow=1000,ncol=o)

std1=3
mean1= 10
set.seed(101)

for(i in 1:m){
M[,i] =  rnorm(1000,mean=mean1,sd=std1)
}


set.seed(185)

for(i in 1:n){
  N[,i] <- (rnorm(1000, mean = mean1, sd = std1))
}


set.seed(177)

for(i in 1:o){
  O[,i] <- (rnorm(1000, mean = mean1, sd = std1))
}

#1b
#create functions



#size of columns in each matrix
n1 = length(M[1,])
n2 = length(N[1,])
n3 = length(O[1,])
#overall size of all columns, or amount of numbers in 1 row of all matrixes
n = n1+n2+n3
#number of matrixes3
m=3



#mst function
mst = function(a,b,c){
    MST <- c()

    
    for(i in 1:1000){
      totalMean <- (sum(a[i,]) + sum(b[i,]) + sum(c[i,])) / n
      amean <- mean(a[i,])
      bmean <- mean(b[i,])
      cmean <- mean(c[i,])
      
    
      numerator <- (n1 * ((amean - totalMean) ^ 2)) + (n2 * ((bmean - totalMean) ^ 2)) + (n3 * ((cmean - totalMean) ^ 2))
      
      sigma <- numerator / (m - 1)
      
   
      
      
      MST <- c(MST, sigma)
    }
    return(MST)
  
}
#MSE function
mse = function(a,b,c){
MSE <- c()
for(i in 1:1000){
  amean <- mean(a[i,])
  bmean <- mean(b[i,])
  cmean <- mean(c[i,])
  
 
  
  
  numerator <- sum((a[i,] - amean) ^ 2) + sum((b[i,] - bmean) ^ 2) + sum((c[i,] - cmean) ^ 2)
  
  pool <- numerator / (n - m)
  MSE <- c(MSE, pool)
}
return(MSE)

}

mse(P,Q,R)[1]
#msto function


msto = function(a,b,c){
  MSTO <- c()
  for(i in 1:1000){
    totalMean <- (sum(a[i,]) + sum(b[i,]) + sum(c[i,])) / n
    

 
    numerator <- sum((a[i,] - totalMean) ^ 2) + sum((b[i,] - totalMean) ^ 2) + sum((c[i,] - totalMean) ^ 2)
    if(i==1){
      
      print(sum((a[i,] - totalMean) ^ 2) )
      print(sum((b[i,] - totalMean) ^ 2))
      print(sum((c[i,] - totalMean) ^ 2))
      
    }
    s <- numerator / (n-1)
    MSTO <- c(MSTO, s)
  }
  
  
  
  return(MSTO)
  
}

msto(P,Q,R)[1]

mean(msto(M,N,O))
mean(mse(M,N,O))
sqrt(mean(mst(M,N,O)))

#the mean of msto, mse, and mst indicate that they are unbiased estimators of sigma, as they are pretty close to 10


#2
#a




#set up 3 matrixes with mean =5, standard dev =3, and size n =5,7,9
p = 5
P = matrix(NA,nrow=1000,ncol=p)
q = 7
Q= matrix(NA,nrow=1000,ncol=q)
r = 9
R= matrix(NA,nrow=1000,ncol=r)


std1=3
mean2= 10
mean3 = 15
mean4 = 20
set.seed(25)

for(i in 1:p){
  P[,i] =  rnorm(1000,mean=mean2,sd=std1)
}


set.seed(36)

for(i in 1:q){
  Q[,i] <- (rnorm(1000, mean = mean3, sd = std1))
}


set.seed(49)

for(i in 1:r){
  R[,i] <- (rnorm(1000, mean = mean4, sd = std1))
}

#2b
mean(msto(P,Q,R))
mean(mse(P,Q,R))
mean(mst(P,Q,R))


#it appears that mse is an unbiased estimator of sigma,
# but msto and especially mst are very far off from sigma




#2c. compare means with theoretical expected values

averageMean <- (mean2 + mean3 + mean4) / m

numerator <- (n1 * ((mean2 - averageMean) ^ 2)) + (n2 * ((mean3 - averageMean) ^ 2)) + (n3 * ((mean4 - averageMean) ^ 2))
inter2 <- numerator / (n - 1)
eMSTO <- (std1 ^ 2) + inter2

eMSE <- std1 ^ 2

numerator2 <- (n1 * ((mean2 - averageMean) ^ 2)) + (n2 * ((mean3 - averageMean) ^ 2)) + (n3 * ((mean4 - averageMean) ^ 2))
inter2 <- numerator2 / (m - 1)
eMST <- (std1 ^ 2) + inter2

print(eMSTO) # very close
print(eMSE)  #extremely close
print(eMST)  #pretty close
mean(msto(P,Q,R))
mean(mse(P,Q,R))
mean(mst(P,Q,R))




# All three means of msto,mst,mse are close to their expected values, with alternative hypothesis is that at least one pair of means is different




#d

MST =(mst(M,N,O))
MSE = (mse(M,N,O))

#part 2, get vector of f's
f=MST/MSE


#get f-distrubition to compare against
#####################
#10%, only get right  
ten=qf(.9,df1=m-1,df2=n-m, lower.tail=FALSE)  
#got 0.106, as predicted
count1=0
for(i in 1:1000){
  
  if(f[i]>ten){
    
    count1=count1+1
  }
  
  
  
}
count1/1000


#got 89.8
#########################


twenty=qf(.8,df1=m-1,df2=n-m, lower.tail=FALSE)  
#got 22.6

count2=0
for(i in 1:1000){
  
  if(f[i]>twenty){
    
    count2=count2+1
  }
  
  
  
}
count2/1000

#got 79.9%



#########3
ninety=qf(.1,df1=m-1,df2=n-m, lower.tail=FALSE)  



count3=0
for(i in 1:1000){
  
  if(f[i]>ninety){
    
    count3=count3+1
  }
  
  
  
}
count3/1000
#got 12.9%, doesnt seem right


(sum(Q[1,])+sum(R[1,])+sum(P[1,]))/n





sink("homework11.txt")
print("part a dataset ")
M[1,]
N[1,]
O[1,]
print("msto,mst,and mse, respectively")
print(msto(M,N,O)[1])
print(mst(M,N,O)[1])
print(mse(M,N,O)[1])


print("dataset 2, part b")
P[1,]
Q[1,]
R[1,]
print("msto,mst,and mse, respectively")
print(msto(P,Q,R)[1])
print(mst(P,Q,R)[1])
print(mse(P,Q,R)[1])

sink()
