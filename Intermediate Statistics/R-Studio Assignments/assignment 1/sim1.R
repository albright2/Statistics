#Jeremiah Albright
#stat103
#simulation homework #1
#2/11/19








M<- matrix(NA, nrow = 1000, ncol = 5) 
for (i in 1:1000){   M[i,] <- rexp(5, rate = 1/2) } 

# Vector to assign names to the columns of matrix A 

a=c()
for (i in 1:5) {
  d=paste("X", i, sep ="")
  a=c(a,d) }
a 
colnames(M) <- a 
M 

View(M)





#a


#Obtain a 1000 x 1 vector containing the average of each sample. 
#(An approximate population of sample means from X). 


av1 = matrix(NA,nrow=1000, ncol=1)
for(it in 1:1000){  
  av1[it]=mean(M[it,]  )                }

av1


#Then obtain the mean and standard deviation of this population of sample means,

(m1=mean(av1))

sd(av1)

#and a histogram of this population of sample means of size n = 5. 
hist(av1,breaks=5)
#Do these results match what is stated in the Central Limit Theorem? no

sd(M)
sd(av1)


#b. Obtain a 1000 x 1 vector containing the standard deviation of each sample. 
av2 = matrix(NA,nrow=1000, ncol=1)
for(it in 1:1000){  
  av2[it]=sd(M[it,]  )                }

av2
  



#c
 
#use tvalue??? =2.57 instead of 1.96

 
#this number is close, about 3% off, but wouldnt be 95%CI
#Approxiametly, 98.6% of intervals constructed using the formula above contain the population mean ux
ss =sd(M)


upper = matrix(NA,nrow=1000, ncol=1)
for(it in 1:1000){  
  
  mean = mean(M[it,])
  #stnd = sd(M[it,]  )
  
  upper[it]=  mean + (2.57*ss  )            }
upper



lower = matrix(NA,nrow=1000, ncol=1)
for(it in 1:1000){  
  stnd = sd(M[it,]  )
  mean = mean(M[it,])
  lower[it]=  mean - (2.57*ss  )            }
lower

combined = data.frame(lower,upper)
combined


count =0
for(it in 1:1000){  
  
  if(combined[it,1]<=mean){if(combined[it,2]>=mean){count=count+1}}
}

count/1000

#d
#ss =sd(M)
#once again same number, yes close .986
upper = matrix(NA,nrow=1000, ncol=1)
for(it in 1:1000){  
 
  mean = mean(M[it,])
  stnd = sd(M[it,]  )
  upper[it]=  mean + (2.57*stnd  )            }
upper



lower = matrix(NA,nrow=1000, ncol=1)
for(it in 1:1000){  
  stnd = sd(M[it,]  )
  mean = mean(M[it,])
  lower[it]=  mean - (2.57*stnd  )            }
lower

combined = data.frame(lower,upper)
combined


count =0
for(it in 1:1000){  
  
  if(combined[it,1]<=mean){if(combined[it,2]>=mean){count=count+1}}
}

count/1000





#e

#z = (x-u)/o
u = mean(M)
x = 0
av3 = matrix(NA,nrow=1000, ncol=1)
for(it in 1:1000){  
  x=mean(M[it,])
  st = sd(M[it,])
  av3[it]=(x-u  )/st             }

av3
mean(av2) #average of z scores
sd(av3)#standard deviation
hist(av3,breaks=5)

