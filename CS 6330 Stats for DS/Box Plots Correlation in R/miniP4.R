library(boot)
###############################################################
#QUESTION 1
#Reading in the data
MyData <- read.csv(file="C:/Users/Harrison/Desktop/gpa.csv", header=TRUE, sep=",")
attach(MyData)
#Scatterplot + Correlation
plot(gpa, act, main="GPA vs ACT", xlab="GPA", ylab="ACT", pch=19, xlim=c(0,4.5))
cor(gpa,act)

#Bootstrapping
correl = function(data, indices){
  return(cor(data$gpa[indices], data$act[indices]))
}
results = boot(data=MyData,statistic = correl,R=10000)

plot(results)
#Bootstrapped StdError
sd(results$t)
#Bootstrapped bias
print(mean(results$t-results$t0))
#Bootsrap Percentile ConfInterval
boot.ci(results, type="perc")


#####################################
# QUESTION 2
MyData <- read.csv(file="C:/Users/Harrison/Desktop/MP4/VOLTAGE.csv", header=TRUE, sep=",")

#Seperating the data into remote/local
remote=MyData[1:30,]$voltage
local = MyData[31:60,]$voltage
#Visuals to compare distributions
hist(remote)
hist(local)
boxplot(voltage~location,data=MyData, main="Voltage Data", 
        xlab="location", ylab="Voltage")

#Normality Check
hist(remote-local)

#Confidence Interval
stderror = sqrt((var(remote)/29)+(var(local)/29))
mu = mean(remote-local)
c(mu+qt(.025,29)*stderror,mu+qt(.975,29)*stderror)

##################################################
# QUESTION 3
MyData <- read.csv(file="C:/Users/Harrison/Desktop/MP4/vapor.csv", header=TRUE, sep=",")

# Normality check
diffs = MyData$experimental-MyData$theoretical
hist(diffs,main="Histogram (Experimental-Theoretical)")
# Confidence interval calculation
mu = mean(diffs)
stderror = sqrt((var(MyData$experimental)/15)+(var(MyData$theoretical)/15))
c(mu+qt(.025,15)*stderror,mu+qt(.975,15)*stderror)


