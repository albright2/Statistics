#Jeremiah Albright
#written homework2

#link to free preview of text book
#https://books.google.com/books?id=5AJ0AwAAQBAJ&pg=PR4&lpg=PR4&dq=ISBN:+978-0-12-417113-8.&source=bl&ots=20APYGFz6e&sig=ACfU3U2zZuaMNmEkDRk_ugthDVig4SlIVA&hl=en&sa=X&ved=2ahUKEwiatJCS2JbgAhXiJDQIHbxZB28Q6AEwAnoECAkQAQ#v=onepage&q=ISBN%3A%20978-0-12-417113-8.&f=false

#Section 5.5 of the textbook. Obtain the confidence intervals of Exercises 5.5.20 and 5.5.23 following the following steps:

#is this a population OR Sample of points for both questions?

###########################################################
# A. Use R to place all data points in a vector.

#5.5.20




v1 = c(65.2,47,38.2,13.5,18,25.6,16.3,14,23.2,18.8,7.5,13.3,11,54.9,22,50.1,32.6,26,13,9,7.2,4.7,4.5,41.1,45.8,37,8.5,30.5,29.3,13.8,7.7,5.5,24.1,12.5,22.3,19,9.5,4.7,3)
n1=length(v1)


#################
#5.5.23



#n=35
v2 = c(10.6,13.3,15.5,10.7,9.6,12.1,11.8,10.9,9.9,13.2,9.3,11.7,9.9,13,12.3,11,13.1,11,12.5,13.9,14.1,14.8,15.1,12.8,14,7.1,14.1,12.7,9.6,12.5,9.0,12.7,13.6,12.5,12.6)
n2 =length(v2)
#########################################################
#B. Use R to obtain the sum of the squares of the values, and the sum of the values to manually get the standard deviation of the values using the computation formula for s.









#5.5.20

#sum of squares
(sq1=(sum(v1^2)))



#sum of values
(sum1 = (sum(v1)))







(var1 = (sq1-((sum1^2)/n1))/(n1-1))

(s1=sqrt(var1))













#################
#5.5.23

#sum of squares
(sq2=(sum(v2^2)))



#sum of values
(sum2 = (sum(v2)))







(var2 = (sq2-((sum2^2)/n2))/(n2-1))

(s2=sqrt(var2))



#########################################################

#C. Use R to obtain the standard deviation of the values in the vector you created in part A above.



#5.5.20

sd(v1) 

#################
#5.5.23

sd(v2) 

#########################################################

#D. Construct manually the confidence interval, and interpret it.
#   x(+-)z(a/2)*(O/sqrt(n))


(u1 = mean(v1))

#5.5.20

#obtain a  confidence interval from 95% CI level

(alpha1=(1-.95)/2)
#z-table of 0.025 is 1.96
CI1=1.96*(s1/sqrt(length(v1)))

CI1

#################
#5.5.23
(u2 = mean(v2))
#obtain a confidence interval from 98% CI level

(alpha2=(1-.98)/2)
z=2.326
CI2=2.326*(s2/sqrt(length(v2)))
CI2
#########################################################

#E. Use R to obtain the CI.
#x+- z(a/2)*(O/sqrt(n))



#5.5.20

#obtain a 95% confidence interval


(error1 <- qnorm(0.975)*s1/sqrt(n1))


#################

#5.5.23

(error2 <- qnorm(0.99)*s2/sqrt(length(v2)))









