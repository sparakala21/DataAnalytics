EPI_data <- read.csv("epi2024results06022024.csv")
attach(EPI_data)

populations_2023 <- read.csv("countries_populations_2023.csv")
populations <- populations_2023[-which(!populations_2023$Country %in% EPI_data$country),]
# sort populations by country
populations <- populations[order(populations$Country),]
# drop countries not in populations
EPI_data.sub <- epi.results[-which(!EPI_data$country %in% populations$Country),]
# sort epi results by country
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]
# only keep necessary columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new")]
# convert population to numeric
epi.results.sub$population <- as.numeric(populations$Population)
# compute population log base 10
epi.results.sub$population_log <- log10(epi.results.sub$population)
summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE) 
View(EPI_data) 

boxplot(MHP.new, MPE.new, PAR.new, names=c("MHP.new", "MPE.new", "PAR.new"))


qqnorm(EPI.new) 
qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)




qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)


plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE)
lines(ecdf(EPI.new))

plot(ecdf(EPI.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(EPI.new))

plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new ECDF")
lines(ecdf(EPI.new))

plot(ecdf(MPE.old), do.points=FALSE, main="MPE.old vs. MPE.new ECDF")
lines(ecdf(MPE.new))

plot(ecdf(Population), do.points=FALSE, main = "Population vs. MPE.new")
lines(ecdf(MPE.new))

