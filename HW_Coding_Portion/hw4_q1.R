# Takao Oba
# 205615894
# STATS C183
# HW 4 Question 1


#Read your csv file:
a <- read.csv("/Users/takaooba/STATS 183/stockData.csv", sep=",", header=TRUE)

train <- a[1:60,]
test <- a[61:dim(a)[1],]

# price
a1 <- train[1:20, ]
a2 <- train[21:40, ]
a3 <- train[41:60, ]

# returns
r1 <- (a1[-1,3:ncol(a1)]-a1[-nrow(a1),3:ncol(a1)])/a1[-nrow(a1),3:ncol(a1)]

r2 <- (a2[-1,3:ncol(a2)]-a2[-nrow(a2),3:ncol(a2)])/a2[-nrow(a2),3:ncol(a2)]

r3 <- (a3[-1,3:ncol(a3)]-a3[-nrow(a3),3:ncol(a3)])/a3[-nrow(a3),3:ncol(a3)]


#Compute the variance covariance matrix of the returns for each period:
covmat1 <- var(r1)
covmat2 <- var(r2)
covmat3 <- var(r3)


#Compute the betas in each period:
beta1 <- covmat1[1,-1] / covmat1[1,1]
beta2 <- covmat2[1,-1] / covmat2[1,1]
beta3 <- covmat3[1,-1] / covmat3[1,1]

#Here is the plot of the betas in period 2 against the betas in perod 1:
plot(beta1, beta2)

#Correlation between the betas in the two periods:
cor(beta1, beta2)


#Adjust betas using the Blume's technique:
q1 <- lm 


beta3adj_blume <- q1$coef[1] + q1$coef[2]*beta2

#If we use beta2 as our forecasts for the betas in period 3 then our PRESS (prediction sum of squares) is:
PRESS1 <- sum((beta2-beta3)^2) / 30

#If we use beta3adjusted (the adusted betas) as our forecasts for the betas in period 3 then our PRESS (prediction sum of squares) is:
PRESS2 <- sum((beta3adj_blume-beta3)^2) / 30

#===================================
#===================================
#Adjusting the betas using the Vasicek's technique:
#Adjusting the betas using the Vasicek's technique:
#The adjustment is a weighted average of the average beta and 
#individual beta_i's.  The weights are the variance of the betas of the 
#30 stocks and the variance of beta_i_hat.
#We need only one historical period.
#We will use historical period 2 to forecast the betas in period 3.


#Vasicek's method:
beta2 <- rep(0,30)

alpha2 <- rep(0,30)

sigma_e2 <- rep(0,30)

var_beta2 <- rep(0,30)

for(i in 1:30){
  q <- lm(data=r2, formula=r2[,i+1] ~ r2[,1])
  beta2[i] <- q$coefficients[2] 
  alpha2[i] <- q$coefficients[1] 
  sigma_e2[i] <- summary(q)$sigma^2
  var_beta2[i] <- vcov(q)[2,2] 
}


#Adjusting the betas using the Vasicek's technique:
beta3adj_vasicek <- var_beta2*mean(beta2)/(var(beta2)+var_beta2) + 
  var(beta2)*beta2/(var(beta2)+var_beta2)

#==============================================
#==============================================               
#Now let's compare:
#Note:
#beta3:  Actual betas in period 3.
#beta2:  Betas in period 2 that can be used as forecasts for period 3.
#beta3adj_blume:  Adjusted betas (Blume) that can be used as forecast 
#                 for period 3.
#beta3adj_vasicek:  Adjusted betas (Vasicek) that can be used as forecast 
# for period 3.

cbind(beta3, beta2, beta3adj_blume, beta3adj_vasicek)

PRESS1 <- sum((beta2-beta3)^2) / 30


PRESS2 <- sum((beta3adj_blume-beta3)^2) / 30


PRESS3 <- sum((beta3adj_vasicek-beta3)^2) / 30


#==============================================
#==============================================   
#Decompositiom of  PRESS into different sources errors.

BETA <- cbind(beta3, beta2, beta3adj_blume, beta3adj_vasicek)


#Decomposition of PRESS using unadjusted betas:
#1.  Bias component:
U1 <- ( mean(beta3) - mean(beta2) )^2

#2.  Inefficiency component:
q1 <- lm(beta3 ~ beta2)
Sp12 <- (29/30)*var(beta2)
U2 <- (1-q1$coef[2])^2*Sp12

#3.  Random error component:
Sa2 <- (29/30)*var(beta3)
rap12 <- ( cor(beta2,beta3) )^2
U3 <- (1-rap12)*Sa2

U1+U2+U3

#Decomposition  of PRESS using Blume's technique:
#1.  Bias component:
B1 <- ( mean(beta3) - mean(beta3adj_blume) )^2

#2.  Inefficiency component:
q2 <- lm(beta3 ~ beta3adj_blume)
Sp22 <- (29/30)*var(beta3adj_blume)
B2 <- (1-q2$coef[2])^2*Sp22

#3.  Random error component:
Sa2 <- (29/30)*var(beta3)
rap22 <- ( cor(beta3adj_blume,beta3) )^2
B3 <- (1-rap22)*Sa2

B1+B2+B3


#Using Vasicek's technique:
#1.  Bias component:
V1 <- ( mean(beta3) - mean(beta3adj_vasicek) )^2

#2.  Inefficiency component:
q3 <- lm(beta3 ~ beta3adj_vasicek)
Sp32 <- (29/30)*var(beta3adj_vasicek)
V2 <- (1-q3$coef[2])^2*Sp32

#3.  Random error component:
Sa2 <- (29/30)*var(beta3)
rap32 <- ( cor(beta3adj_vasicek,beta3) )^2
V3 <- (1-rap32)*Sa2

V1+V2+V3

#===================================================
#===================================================
# #Summary:
# matrix(c(U1,U2,U3, B1,B2,B3, V1,V2,V3), 3,3)
# 
# [,1]                  [,2]               [,3]
# [1,] 0.02105727 0.0146119423 0.01334950
# [2,] 0.06830747 0.0005687896 0.02187651
# [3,] 0.18431326 0.1843132571 0.17763952

#What do you observe?

