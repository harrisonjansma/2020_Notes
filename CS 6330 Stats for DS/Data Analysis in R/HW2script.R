library(tidyverse)
df = read_csv("C://users//Harrison//Desktop//MiniProject2//roadrace.csv")
names(df)[c(7,8,9,10,11,12)] = c("State.Country","TIme.seconds","MilePace.seconds","From.USA","Maine","Time.minutes")
names(df)

#barplot of runners from Maine
ggplot(data=df)+
  geom_bar(mapping=aes(x=Maine))+
  ggtitle("Number of Runners from Maine")+
  theme(plot.title = element_text(hjust = 0.5))


allRunners=length(df$Maine)
mainers= sum(df$Maine=="Maine")
awayers= allRunners-mainers
percentMainers=mainers/allRunners

#Number of runners from Maine:  
mainers
#Number of runners from Away: 
awayers
#Percent of all runners from Maine
percentMainers


places=count(df,vars=State.Country,sort=TRUE)
places$percent=round(places$n / allRunners,digits=3)
#Top 10 runner origins after Maine
places[2:12,]

#barplot:top 10 runners from outside Maine
ggplot(data=places[2:12,], aes(x=reorder(vars,-n), y=n)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  ggtitle("Top 10 Other Origins")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Origin",y="Count")

#subframes of Maine and NonMaine
dfMaine=df[df$Maine=="Maine"]
dfNonMaine=df[df$Maine=="Away"]

#Histogram of Mainers
ggplot(dfMaine, aes(x=Time.minutes))+geom_histogram(bins=50,color="black",fill="white")+ 
  xlim(20,120)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Running Time From Maine")

#Histogram of nonMainers (Away)
ggplot(dfNonMaine, aes(x=Time.minutes))+geom_histogram(bins=50,color="black",fill="white")+ 
  xlim(20,120)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Running Time From Away (10K)")

#Summary stats for Mainers
mean(dfMaine$Time.minutes)  
sd(dfMaine$Time.minutes)
range(dfMaine$Time.minutes)
median(dfMaine$Time.minutes)
IQR(dfMaine$Time.minutes)

#Summary stats for Awayers
mean(dfNonMaine$Time.minutes)  
sd(dfNonMaine$Time.minutes)
range(dfNonMaine$Time.minutes)
median(dfNonMaine$Time.minutes)
IQR(dfNonMaine$Time.minutes)

#Boxplot for Runtime in Maine and NonMaine
ggplot(df, aes(x=Maine, y=Time.minutes)) + 
  geom_boxplot()

#remove NA values from sex
df = na.omit(df)

#Boxplot for Age and Gender
ggplot(df, aes(x=Sex, y=Age)) + 
  geom_boxplot()

#subframes for male and female subpops
dfF=df[df$Sex=="F",]
dfM=df[df$Sex=="M",]

#Summary stats for Female
mean(dfF$Age)  
sd(dfF$Age)
range(dfF$Age)
median(dfF$Age)
IQR(dfF$Age)

#Summary stats for Male
mean(dfM$Age)  
sd(dfM$Age)
range(dfM$Age)
median(dfM$Age)
IQR(dfM$Age)



cyclesdf = read_csv("C://users//Harrison//Desktop//MiniProject2//motorcycle.csv")
names(cyclesdf)=c("County","Deaths")

#Histogram of Deaths )
ggplot(cyclesdf, aes(x=Deaths))+geom_histogram(bins=50,color="black",fill="white")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Motorcycle Fatalities South Carolina 2009")


#barplot:Fatalities by county
ggplot(data=cyclesdf, aes(x=reorder(County,-Deaths), y=Deaths)) +
  geom_bar(stat="identity")+
  ggtitle("Fatalities by County")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="County",y="Deaths")


#Boxplot for Fatalities
ggplot(cyclesdf, aes(x=0,y=Deaths)) +
  xlim(-1,1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Motorcycle Fatalities South Carolina 2009")+
  geom_boxplot()

#Summary stats for Fatalities
mean(cyclesdf$Deaths)  
sd(cyclesdf$Deaths)
range(cyclesdf$Deaths)
median(cyclesdf$Deaths)
IQR(cyclesdf$Deaths)

outliers = cyclesdf[cyclesdf$Deaths>mean(cyclesdf$Deaths)+1.5*IQR(cyclesdf$Deaths),]
outliers

popdf = read.table(file="C://users//Harrison//Desktop//MiniProject2//population.txt",sep='\t',header=TRUE)
popdf=popdf[1:46,c(1,11)]
names(popdf)=c("County","Population.2009")
popdf$County=toupper(popdf$County)
cyclesdf = merge(cyclesdf,popdf,by="County")
as.numeric(levels(cyclesdf$Population.2009))[cyclesdf$Population.2009]

ggplot(cyclesdf,aes(x=Population.2009,y=Deaths))+
  geom_point()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Motorcycle Fatalities vs Population")

  