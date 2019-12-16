
library(boot)
library(ggplot2)
#############################
#MINI PROJECT 5
#QUESTION 1
#BODY TEMPERATURE
MyData <- read.csv(file="C:/Users/Harrison/Desktop/bodytemp-heartrate.csv", header=TRUE, sep=",")
attach(MyData)
#seperate the data
men = MyData[1:65,]
women = MyData[66:130,]

mean(men$body_temperature)
mean(women$body_temperature)

#visuals
boxplot(body_temperature~gender,data=MyData, main="Body Temperatures by Gender", 
        xlab="Gender", ylab="Body Temperature")
hist(men$body_temperature, main="Male Body Temperature",xlab="Temperature",xlim=c(95,101))
hist(women$body_temperature, main = "Female Body Temperature", xlab="Temperature",xlim=c(95,101))

t.test(body_temperature~gender,data = MyData,var.equal=TRUE)
###########
#HEART RATE
mean(men$heart_rate)
mean(women$heart_rate)
#visuals
boxplot(heart_rate~gender,data=MyData, main="Heart Rate by Gender", 
        xlab="Gender", ylab="Heart Rate")
hist(men$heart_rate, main="Male Heart Rate",xlab="Heart Rate")
hist(women$heart_rate, main = "Female Heart Rate", xlab="Heart Rate",xlim=c(55,90))

t.test(heart_rate~gender,data = MyData,var.equal=TRUE)
###########
#HEART RATE BODY TEMP CORR
cor(heart_rate,body_temperature)
plot(heart_rate~body_temperature,MyData, main="Heart Rate vs. Body Temperature")
cor(men$heart_rate,men$body_temperature)
cor(women$heart_rate,women$body_temperature)
#########################################
#QUESTION 3

ns=c(5, 10, 30, 100,150,200)
lambdas=c(0.01,0.1,1,10)


getCIs =function(n=5,lambda=0.01){
  #Generates a sample of size n from exp(lambda)
  # then computes Z-interval and bootstrap CIs
  samp = rexp(n=n, rate=lambda)
  serror = sd(samp)
  Zci = c(mean(samp)-1.96*(serror/(sqrt(n))),mean(samp)+1.96*(serror/(sqrt(n))))
  meany = function(data, indices){
    return(mean(data[indices]))
  }
  res = boot(data=samp, statistic = meany, R=500)
  Bci = quantile(res$t, probs = c(0.025,0.975))
  return(c(Zci,Bci))
}

between=function(x,left,right){
  #Support function. Returns TRUE if x is in [left,right]
  if(x<left){return(FALSE)}
  if(x>right){return(FALSE)}
  else{return(TRUE)}
}

Coverage_Percent = function(n,lambda){
  coverage_percent = c(0,0)
  for(i in 1:5000){
    res = getCIs(n=n,lambda = lambda)
    if(between(1/lambda,res[1],res[2])){
      coverage_percent[1]=1+coverage_percent[1]
    }
    if(between(1/lambda,res[3],res[4])){
      coverage_percent[2]=1+coverage_percent[2]
    }
  }
  return(coverage_percent/5000)
}

z_est = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
b_est = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
for(i in 1:6){
  for(j in 1:4){
    print(ns[i])
    print(lambdas[j])
    print("Z-Interval Coverage    Bootstrap Coverage")
    cv= Coverage_Percent(n[i],lambda[j])
    print(cv)
    z_est[(4*(i-1))+j]=cv[1]
    b_est[(4*(i-1))+j]=cv[2]
  }
}
n = rep(ns,times=6)
lambda =rep(lambdas,times=6)
counts = data.frame(lambda,z_est, b_est)

write.csv(counts, file = "C:/Users/Harrison/Desktop/counts.csv")

