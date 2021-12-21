#Load Library
library(openintro)

#View Dataset
View(BloodPressure)

#View first few lines of dataset
head(BloodPressure)

#View key information of dataset
str(BloodPressure)

# Finding mu0, which is the POPULATION MEAN
mu0 <- mean(BloodPressure$Before)
mu0

# Finding mu, which is the Sample MEAN
mu<- mean(BloodPressure$After)
mu


# Specify the significance level
alpha <- 0.05

# sigma, Sample standard deviation
sigma <- sd(BloodPressure$After) 
sigma

# n, Get the sample size
n <- nrow(BloodPressure) 
n

#Calculate z
z<-(mu-mu0)/(sigma/sqrt(n))
z

#Calculate p-value
p = 2*pnorm(abs(z),lower.tail=FALSE)
p