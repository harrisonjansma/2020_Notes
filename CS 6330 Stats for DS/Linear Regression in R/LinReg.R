data = read.csv("C://Users//Harrison//Desktop//prostate_cancer.csv")
data$gleason = factor(data$gleason)

#make sure gleason is recognized as factor
str(data)

#Response and predictors
Y=data$psa

#baseline model = all the data
model = lm(psa ~ cancervol+weight+age+benpros+vesinv+capspen+gleason,data=data)
summary(model)

#Checking Hypothesis
#ei constant variance
plot(fitted(model),resid(model))
abline(h=0)
#ei normal
qqnorm(resid(model))
qqline(resid(model))
#time Series
plot(resid(model), type="l")
abline(h=0)

#Transform response
model = update(model, log(Y) ~ .)
#ei normal
qqnorm(resid(model))
qqline(resid(model))
#ei constant variance
plot(fitted(model),resid(model))
abline(h=0)

library(MASS)

# Forward selection
basemodel <- lm(log(psa) ~ 1 , data=data)
finalmodel <- stepAIC(basemodel, scope=list(upper=~cancervol+weight+age+benpros+vesinv+capspen+gleason,lower=~1) , direction="forward")
summary(finalmodel)

#prediction
gles = factor()
levels(gles)=c(6,7,8)
gles[1]=7
predY = data.frame(cancervol=mean(data$cancervol),benpros=mean(data$benpros),vesinv=0,gleason=gles) 

exp(predict(finalmodel,newdata=predY))
