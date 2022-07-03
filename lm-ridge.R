# Confidence interval on a loglikelihood maximum
load('exercice-1-DM.RData')
lambda = travail$changements/travail$duree
a_mv = log(mean(travail$changements)/mean(travail$duree))
print(a_mv)
n = dim(travail)[1]
var_a = 1/(n*mean(travail$changements))
print(var_a)
inf_conf = a_mv-qnorm(0.95, 0, 1)*sqrt(var_a)
sup_conf = a_mv+qnorm(0.95, 0, 1)*sqrt(var_a)
cat("Intervalle de confiance : [", inf_conf, ", ", sup_conf, "]", sep="")



# Loading and preparing data
load('exercice-2-DM.RData')
imc = as.vector(paradoxe$imc)
unique(imc)
m = c()
p = c()
for (i in seq(length(unique(imc)))){
  g = subset(paradoxe, imc==unique(imc)[i])
  m[i] = mean(as.numeric(g$duree))
  p[i] = dim(subset(g, survie==0))[1]/dim(g)[1]
}

# Plotting BMI group vs. age
barplot(m, names.arg=unique(imc), xlab='IMC', 
        main='Répartition des patients par groupe d\'IMC')

# Plotting survival for each BMI group
barplot(100*p, names.arg=unique(imc), xlab='IMC',
        main = 'Proportion des patients ayant survécu par groupe d\'IMC')

# Building differents binomial regression models
paradoxe$imc = relevel(paradoxe$imc, ref="20-25")
modele_imc = glm(survie~imc, family=binomial, data=paradoxe)
odds.ratio(modele_imc)

paradoxe$age = relevel(paradoxe$age, ref="18-44")
modele_age = glm(survie~age, family=binomial, data=paradoxe)
odds.ratio(modele_age)

paradoxe$sexe = relevel(paradoxe$sexe, ref="M")
modele_genre = glm(survie~sexe, family=binomial, data=paradoxe)
odds.ratio(modele_genre)

modele = glm(survie~., family=binomial, data=paradoxe)
summary(modele)

library(questionr)
odds.ratio(modele)
relevel(odds.ratio(modele), ref="imc20-25")

library(ordinal)
modele_multi = clm(duree~., data=paradoxe)
summary(modele_multi)

modele_multi_optAIC = clm(duree~survie+imc, data=paradoxe)
summary(modele_multi_optAIC)

step(modele_multi, direction="backward")

step(modele_multi, direction="forward")


# Ridge regression

X1 = c(3, 7, 11, 15, 21, 23, 28, 31, 38, 39, 
       42, 49, 57, 68, 71, 89, 95, 97)
X2 = c(6, 7, 11, 12, 16, 17, 22, 16, 21, 27, 
       24, 32, 29, 36, 42, 51, 53, 55)
X3 = c(2, 11, 23, 26, 12, 16, 22, 28, 34, 27,
       31, 40, 42, 35, 39, 51, 60, 68)
X4 = c(8, 14, 33, 34, 5, 10, 15, 24, 31, 8, 
       16, 25, 21, 9, 15, 23, 29, 40)
Y = c(3, 15, 19, 27, 23, 23, 31, 39, 47, 51,
      47, 51, 55, 63, 67, 71, 71, 75)

library(glmnet)
# Setting regularization parameter Lambda range
lambda = 10^seq(2, -3, by = -.1)

# Building the model
ridge_reg = glmnet(cbind(X1, X2, X3, X4), Y, nlambda=25, alpha=0, 
                   family='gaussian', lambda=lambda)

modele_cv = cv.glmnet(cbind(X1, X2, X3, X4), Y, nlambda=25, alpha=0, 
                      family='gaussian', lambda=lambda)

# Plotting the MSE vs. log(Lambda)
plot(modele_cv)
summary(ridge_reg)


library(lmridge)
ridge_reg = lmridge(Y~., data=as.data.frame(cbind(X1, X2, X3, X4)), 
                    K=lambda, scaling="sc")
res = residuals(ridge_reg)
MSE = c()
for (i in seq(length(lambda))){
  MSE[i] = mean(res[,i]^2)
}

# Building linear regression model
linear_reg = lm(Y~., data=as.data.frame(cbind(X1, X2, X3, X4)))
MSE_lm = mean(linear_reg$coefficients^2)

# Comparing ridge regression  with ordinary least squares method
plot(log(lambda), MSE)
par(new=TRUE)
abline(MSE_lm, 0, col='red')
legend(-6, 400, legend=c("Ridge", "MCO"), pch=c(1, 30), 
       lty=c(0, 1), col=c("black", "red"))

length()
MSE = mean(residuals(ridge_reg)^2)
plot(lambda, residuals(ridge_reg))

a = aggregate(as.numeric(paradoxe[, 13])+3, list(paradoxe$imc), mean)

print(a)
print(levels(a$Group.1))

barplot(a$x, names.arg=a$Group.1)
