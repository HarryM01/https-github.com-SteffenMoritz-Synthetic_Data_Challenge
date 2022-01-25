

load("satgpa.rda")

sat <- satgpa

options(digits=7)

library(semTools)
library(Rfast)
#library(mvtnorm)
library(Matrix)

# Easy example #

sat_sex1 <- sat[sat$sex == 1, ]
sat_sex1$sex <- NULL
sat_sex1$sat_sum <- NULL
sat_sex2 <- sat[sat$sex == 2, ]
sat_sex2$sex <- NULL
sat_sex2$sat_sum <- NULL


#sat_sex1_mn <- mvnorm.mle(as.matrix(sat_sex1))
#sat_sex2_mn <- mvnorm.mle(as.matrix(sat_sex2))
#rmvnorm(n = sum(sat$sex == 1), mu = sat_sex1_mn$mu, sigma = sat_sex1_mn$sigma)
#rmvnorm(n = sum(sat$sex == 2), mu = sat_sex2_mn$mu, sigma = sat_sex2_mn$sigma)

mu_sat_sex1 <- apply(sat_sex1, 2, mean)
mu_sat_sex2 <- apply(sat_sex2, 2, mean)
cov_sat_sex1 <- nearPD(cov(sat_sex1))$mat
cov_sat_sex2 <- nearPD(cov(sat_sex2))$mat

set.seed(4389)
syn_sat_sex1 <- rmvnorm(n = sum(sat$sex == 1), mu = mu_sat_sex1, sigma = cov_sat_sex1)
syn_sat_sex2 <- rmvnorm(n = sum(sat$sex == 2), mu = mu_sat_sex2, sigma = cov_sat_sex2)

syn_sat_sex1 <- as.data.frame(syn_sat_sex1)
syn_sat_sex2 <- as.data.frame(syn_sat_sex2)

syn_sat_sex1$sex <- 1
syn_sat_sex2$sex <- 2


syn_sat_mnorm <- rbind(syn_sat_sex1, syn_sat_sex2)
syn_sat_mnorm$sat_sum <- syn_sat_mnorm$sat_v + syn_sat_mnorm$sat_m
syn_sat_mnorm <- syn_sat_mnorm[, names(sat)] 
syn_sat_mnorm_1 <- syn_sat_mnorm
# Fertiger über multivariate Normalverteilung bedingt auf "sex"Datensatz: syn_sat_mnorm

plot(sat)
plot(syn_sat_mnorm)





# a little more advanced (Vale and Maurelli (1983) method)

sat_sex1 <- sat[sat$sex == 1, ]
sat_sex1$sex <- NULL
sat_sex1$sat_sum <- NULL
sat_sex2 <- sat[sat$sex == 2, ]
sat_sex2$sex <- NULL
sat_sex2$sat_sum <- NULL

mu_sat_sex1 <- apply(sat_sex1, 2, mean)
mu_sat_sex2 <- apply(sat_sex2, 2, mean)
cov_sat_sex1 <- as.matrix(nearPD(cov(sat_sex1))$mat)
cov_sat_sex2 <- as.matrix(nearPD(cov(sat_sex2))$mat)

ktss_sex1 <- apply(sat_sex1, 2, kurtosis)[1,]
ktss_sex2 <- apply(sat_sex2, 2, kurtosis)[1,]

skwn_sex1 <- apply(sat_sex1, 2, skew)
skwn_sex2 <- apply(sat_sex2, 2, skew)

set.seed(20129)
syn_sat_sex1_2 <- mvrnonnorm(n = sum(sat$sex == 1), mu = mu_sat_sex1, Sigma = cov_sat_sex1, skewness = skwn_sex1, kurtosis = ktss_sex1)
syn_sat_sex2_2 <- mvrnonnorm(n = sum(sat$sex == 2), mu = mu_sat_sex2, Sigma = cov_sat_sex2, skewness = skwn_sex2, kurtosis = ktss_sex2)

syn_sat_sex1 <- as.data.frame(syn_sat_sex1_2)
syn_sat_sex2 <- as.data.frame(syn_sat_sex2_2)

syn_sat_sex1$sex <- 1
syn_sat_sex2$sex <- 2

syn_sat_mnorm <- rbind(syn_sat_sex1, syn_sat_sex2)
syn_sat_mnorm$sat_sum <- syn_sat_mnorm$sat_v + syn_sat_mnorm$sat_m
syn_sat_mnorm <- syn_sat_mnorm[, names(sat)] 
syn_sat_mnorm_2 <- syn_sat_mnorm
# Fertiger über multivariate komplexe Verteiliung bedingt auf "sex" Datensatz: syn_sat_mnorm2

plot(sat)
plot(syn_sat_mnorm_2)

plot(density(sat$fy_gpa))
lines(density(syn_sat_mnorm_2$fy_gpa), col = 2)
lines(density(syn_sat_mnorm_1$fy_gpa), col = 3)






# 




