# ORGB 671 Illustrations
# curvilinear effect
# projecting DV values using new data based on model
library(geometry)

plot(20:70,
     sapply(20:70,function(x){
       (-3*x)+(0.035*x^2)+80
       }),
     ylab="Percent of employees at this age who exit",
     xlab="Employee age")

exit <- function(age){(-3*age)+(0.035*age^2)+80}


df <- data.frame(age = sample(20:65,1000,replace=T))
df$exit <- rbinom(n=1000,size=1,prob=exit(df$age)/100)

lm1 <- lm(exit~age,data=df)
lm2 <- lm(exit~age+I(age^2),data=df)
summary(lm1) # note: NO apparent age effect
summary(lm2) # age effects only clear when allowing for curvilinear term

df$exit_hat <- predict(lm2) # use lm2 to create a predicted exit probability

mean(df$exit)
mean(df$exit_hat)

roc <- function(threshold){
  pred_exit <- as.numeric(df$exit_hat > threshold)
  fpr <- sum((1-df$exit)*pred_exit)/sum(1-df$exit)
  tpr <- sum(df$exit*pred_exit)/sum(df$exit)
  return(c(fpr=fpr,tpr=tpr))
}

plot(0:100/100,0:100/100,type="l",xlab="False Positive Rate",ylab="True Positive Rate")
lines(do.call(rbind,lapply(1:99/100,roc)),col="blue")

mean(as.numeric(df$exit_hat>0.285))

# Better
trainingSet <- sort(sample(1:1000,500,replace=F))
testingSet <- (1:1000)[-1*trainingSet]

lm.train <- lm(exit~age+I(age^2),data=df[trainingSet,])

df$exit_pred <- NA
df$exit_pred[testingSet] <- predict(lm.train,newdata = df[testingSet,])

roc <- function(threshold,dataset,model){
  df <- dataset
  pred_exit <- as.numeric(predict(model,newdata=df) > threshold)
  fpr <- sum((1-df$exit)*pred_exit)/sum(1-df$exit)
  tpr <- sum(df$exit*pred_exit)/sum(df$exit)
  return(c(fpr=fpr,tpr=tpr))
}

plot(0:100/100,0:100/100,type="l",xlab="False Positive Rate",ylab="True Positive Rate")
lines(do.call(rbind,lapply(1:99/100,roc,df[testingSet,],lm.train)),col="blue")
text(x=0.55,y=0.45,labels=paste0("AUC=",round(
  polyarea(x=c(0,1,1,do.call(rbind,lapply(1:99/100,roc,df[testingSet,],lm.train))[,1]),
         y=c(0,0,1,do.call(rbind,lapply(1:99/100,roc,df[testingSet,],lm.train))[,2])),3)),col="blue")
lines(do.call(rbind,lapply(1:99/100,roc,df[trainingSet,],lm.train)),col="green")
text(x=0.45,y=0.35,labels=paste0("AUC=",round(
  polyarea(x=c(0,1,1,do.call(rbind,lapply(1:99/100,roc,df[trainingSet,],lm.train))[,1]),
           y=c(0,0,1,do.call(rbind,lapply(1:99/100,roc,df[trainingSet,],lm.train))[,2])),3)),col="green")

