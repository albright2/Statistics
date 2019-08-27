#Jeremiah ALbright
#RH3

###################################################################
#1 

set.seed(101)
M<- matrix(NA, nrow = 1000, ncol = 9)
for (i in 1:1000){
  M[i,] <- rnorm(9, mean =10, sd=8)
}




######################################################################
#2

set.seed(106)
N<- matrix(NA, nrow = 1000, ncol = 9)
for (i in 1:1000){
  N[i,] <- rnorm(9, mean =5, sd=3)
}

N[1,]


########################################################################
#3

#mean for M
mat = matrix(NA,nrow=1000, ncol=4)
for(it in 1:1000){  
  mat[it,1]=mean(M[it,]  )                }





#mean  for N
for(it in 1:1000){  
  mat[it,2]=mean(N[it,]  )                }





#sd for M

for(it in 1:1000){  
  mat[it,3]=sd(M[it,]  )                }



#sd for N

for(it in 1:1000){  
  mat[it,4]=sd(N[it,]  )                }



colnames(mat) = c("mean1","mean2","s1","s2")





#######################################################
#4 
#compare each pair s1 and s2 to see if the pair is Not > 2x than the other
j=0
k=0
temp=0
count=0
for(i in 1:1000){
  
j=  mat[i,3]
 k= mat[i,4]
  
 
 if(j>=k){
 if(k*2>=j){
  
   count=count+1
 }
 
 
 }else{
  
   if(j*2>=k){
     
     count=count+1
   }
   
   
   
 }
}
1-count/1000

#78.9% of data paired stardard deviations are NOT close enough (most need to be not pooled aka V)

#  21.1% is the proportion of these pair of samples produced standard deviations 
#where the largest of them is NOT more than twice the value of the smallest of them


#21.1% of pairs we would want to NOT pool the standard deviation

########################################################
#5

##solving for V: NOT POOLING




top = function(s1,s2,n1,n2){
  
  return ((      ((s1^2)/n1)      +           (((s2^2)/n2))              ) )
  
  
  
}

bottom = function(s1,s2,n1,n2){
  
  return   (                    ((((s1^2)/n1)^2)/(n1-1))         +         ((((s2^2)/n2)^2)/(n2-1))     )
  
  
  
}


Vee = function(s1,s2,n1,n2){
  
  
  tob = top(s1,s2,n1,n2)/bottom(s1,s2,n1,n2)
  
  vice =  (floor(tob))
  
  return(qt(.975,vice))
  
}

variance = function(s1,s2,n1,n2){
  return( sqrt(((s1^2)/n1)+((s2^2)/n2)))
  
  
}


difSigmaInterval = function(mean1,mean2,s1,s2,n1,n2){
m=NULL
  upperCI= (mean1-mean2)+(Vee(s1,s2,n1,n2)*variance(s1,s2,n1,n2))
  lowerCI =(mean1-mean2)-(Vee(s1,s2,n1,n2)*variance(s1,s2,n1,n2))
  m[1] = lowerCI
  m[2] = upperCI
  
  return (m)
}


intervals = matrix(NA,nrow=1000,ncol=2)
for(i in 1:1000){
  
  int= difSigmaInterval( mat[i,1],mat[i,2],mat[i,3],mat[i,4],9,9)
  intervals[i,1]= int[1]
 intervals[i,2] = int[2]
 
} 

intervals


########################################################
#6


dif = 10-5 #population mean is 10-5




count=0
for(i in 1:1000){
  
  
  low =intervals[i,1]
  high = intervals[i,2]
  
  
  
  if(dif > low){
    
    if( dif<high){
      
      count=count+1
      
    }

  }
}



count/1000


#94.7% contain difference of population



#########################################################
#7 variance POOL


sp=function(s1,s2,n1,n2){
  
  
  return((((n1-1)*(s1^2))+((n2-1)*(s2^2)))    /    (n1+n2-2))
  
  
  
  }
sig=function(s1,s2,n1,n2){return(sp(s1,s2,n1,n2)*sqrt(((1/n1)+(1/n2)    )))}
tval=function(n1,n2){return(qt(.975,(n1+n2-2)))}
sameSigma = function(mean1,mean2,s1,s2,n1,n2){
  lower2 = (mean1-mean2)-(tval(n1,n2)*(sig(s1,s2,n1,n2)))
  upper2 = (mean1-mean2)+(tval(n1,n2)*(sig(s1,s2,n1,n2)))
  get[1] = lower2
  get[2] = upper2
  
  return(get)
  
}

interval2 = matrix(NA,nrow=1000,ncol=2)
for(i in 1:1000){
  
  int= sameSigma( mat[i,1],mat[i,2],mat[i,3],mat[i,4],9,9)
  interval2[i,1]= int[1]
  interval2[i,2] = int[2]
  
  
}
interval2



sameSigma(mat[1,1],mat[1,2],mat[1,3],mat[1,4],9,9)

############################################################
#8




dif=5
count2=0
for(i in 1:1000){
  
  
  if(dif > CI2[i,1]){
    
    if( dif<CI2[i,2]){
      
      count2=count2+1
      
    }
    
    
  }
}
count2/1000 
#94.3%

print("Upper and lower of")

###the same for either





sink("homework10.txt")
print("dataset 1")
M[1,]
print("interval for dataset 1 using V (not pooling)")

intervals[1,]

print("dataset 2")
N[1,]
print("interval for dataset 2 using pooling")
sameSigma(mat[1,1],mat[1,2],mat[1,3],mat[1,4],9,9)


print("average for dataset 1 & 2, standard deviation for dataset1 & 2")
mat[1,]

sink()

