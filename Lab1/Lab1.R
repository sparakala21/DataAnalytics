EPI_data <- read.csv("epi2024results06022024.csv")
attach(EPI_data)
fix(EPI_data) 
summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE) 
tf <- is.na(EPI.new)
E <- EPI.new[!tf]
View(EPI_data)
stem(EPI.new)
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(EPI.new)
boxplot(EPI.new, APO.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))
rug(EPI.new)


x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(ppoints(250)), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)

plot(ecdf(EPI.old), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.old); qqline(EPI.old) 
qqplot(rnorm(ppoints(250)), EPI.old, xlab = "Q-Q plot for norm dsn")
qqline(EPI.old)

plot(ecdf(ECO.old), do.points=FALSE, verticals=TRUE) 
qqnorm(ECO.old); qqline(ECO.old) 
qqplot(rnorm(ppoints(250)), ECO.old, xlab = "Q-Q plot for norm dsn")
qqline(ECO.old)


