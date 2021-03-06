setwd ( "/Users/miriamcdelvillar/Desktop/Evolution/Task_02")
Data <- read.csv("http://jonsmitchell.com/data/beren.csv" , stringsAsFactors=F) 
beren <- Data
write.csv(Data, "rawdata.csv", quote=F)
Data
length(Data)
nrow(Data
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds<-which(Data [,9]  == "bottle")
berenMilk <- Data [Feeds,]
head(berenMilk)<-
Feeds <- which(Data[,"event"] == "bottle") 
Feeds <- which(Data$event == "bottle")
dayID <- apply(Data, 1, function(x) paste (x[1:3] , collapse="-"))
dateID <- sapply ( dayID, as.Date, format = "%Y-%m-%d" , origin = "2019-04-18")
Data$age <- dateID - dateID [which(Data$event == "birth")]
head(Data)
beren2 <- Data
beren3 <- beren2 [order (beren2$age),] 
write.csv(beren3, "beren_new.csv" , quote=F, row.names=FALSE )


Task 02b
Feeds <- which (beren3$event == "bottle") 
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply (beren3$value [Feeds] , beren3$age [Feeds] , mean)
varFeed <- tapply (beren3$value [ Feeds] , beren3$age [Feeds] , var)
numFeeds <- tapply (beren3$value [Feeds] , beren3$age [Feeds] , length) 
cor ( beren3$value [Feeds] , beren3$age [Feeds]) 
berenCor <- cor.test (beren3$value [Feeds] , beren3$age [Feeds])
summary (berenCor)
berenANOVA <- aov (beren3$value [Feeds] ~ beren3$caregiver [Feeds])
boxplot (beren3$value [Feeds] ~ beren3$caregiver [Feeds] , xlab= "who gave the bottle" , ylab = "amount of milk consumed (oz)")
par (las=1, mar=c (5, 5, 1, 1) , mgp=c (2, 0.5, 0) , tck = -0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=avgFeed, lty=2, col='red')
dev.off()
question #1:
In hypothesis 1, the weight has a lot of impact on his weight meaning a positive interaction and correlation. In hypothesis 2,  	I think the more he drinks milk, the more he will get sleepy and take a nap. 
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkByTime.pdf")