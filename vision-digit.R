# Exercice 2
data = read.csv("data/Visu.csv")

# (i)
V = ifelse(data["Y"]==1, "blue", "red")

# (ii)
plot(data$X1, data$X2, xlab="X1", ylab="X2", col=V, 
     title("Jeu de données en dimension 2"))

# (iii)
modele = glm(Y~X1+X2, data=data, family=binomial)
beta0_chap = modele$coefficients[1]
beta1_chap = modele$coefficients[2]
beta2_chap = modele$coefficients[3]

sprintf("Valeur Bêta0 : %f", beta0_chap)
sprintf("Valeur Bêta1 : %f", beta1_chap)
sprintf("Valeur Bêta2 : %f", beta2_chap)
# (iv)
par(new=FALSE)
abline(-beta0_chap/beta2_chap, -beta1_chap/beta2_chap)

# (v)
par(new=FALSE)
abline(-log(9)/beta2_chap-beta0_chap/beta2_chap, -beta1_chap/beta2_chap, 
       lty="dotted")
par(new=FALSE)
abline(-log(1/9)/beta2_chap-beta0_chap/beta2_chap, -beta1_chap/beta2_chap, 
       lty="dotted")

x = c(0, 0, 1/beta1_chap*(-beta2_chap-beta0_chap-log(9)),
      -1/beta1_chap*(beta2_chap+beta0_chap), 1, 
      1/beta1_chap*(-beta0_chap-log(1/9)))
y = c(-beta0_chap/beta2_chap, 1/beta2_chap*(-beta0_chap-log(9)), 1, 1, 
      1/beta2_chap*(-beta1_chap-beta0_chap-log(1/9)), 0)
polygon(cbind(x, y), col=adjustcolor("grey30", alpha.f=0.5), border=NA)

library(latex2exp)
legend("bottomright", fill=adjustcolor("grey30", alpha.f=0.5),
legend=TeX("$0.1\\leq(1+e^{-(\\hat{\\beta_0}+\\hat{\\beta_1}x+\\hat{\\beta_2}y)})^{-1}\\leq 0.9"), 
cex=1)

# Exercice 3
data3 = read.csv("data/acouphenes.csv", stringsAsFactors=TRUE)

# (i)
summary(data3)

# (ii)
table(data3$hauteur, data3$groupe)

# (iii)
chisq.test(data3$hauteur, data3$groupe)

# (iv)
chisq.test(data3$age, data3$groupe)
chisq.test(data3$intensité, data3$groupe)
chisq.test(data3$seuil, data3$groupe)
chisq.test(data3$THI, data3$groupe)

# (v)
modele3 = glm(groupe~age+intensité+seuil+THI, 
              data=data3, family=binomial)
summary(modele3)

# (vi)
step(modele3)

# (vii)
modele3_bis = glm(groupe~age+THI, 
                  data=data3, family=binomial)
predict(modele3_bis, newdata=list(age=28, THI=20))


# Exercice 4
dat = read.csv("data/2ou8.csv", stringsAsFactors=TRUE)

# (i)
data4 = dat[,-1]
var = diag(cov(data4))

vec = ifelse(sqrt(var) > 0.2, TRUE, FALSE)

# (ii)
par(mfrow=c(1,2))
image(matrix(as.numeric(dat[8, 2:170]), nrow=13)[, 13:1], 
      col=gray(12:1/12), main="Image brute")

couleur = rep(0, 169)
for (i in seq(1, 169)) {
  if (vec[i] == TRUE) {couleur[i] = "green"}
}

image(matrix(as.numeric(dat[8, 2:170]), nrow=13)[, 13:1], 
      col=couleur, main="Pixels significatifs")

# (iii)
data4_s = cbind(dat$label, dat[,vec])

# (iv)
modele4 = glm(dat$label~., data=dat, family=binomial)
summary(modele4)

# (v)
predict(modele4, dat)

# Exercice 5
X = c(2.3, 1.4, 1.6, 0.7, -0.4, 0.9, 0.2, 0.7)
Y = c(1, 1, 1, 1, 0, 0, 0, 0)

LogLikelihood = function(v){
  a = v[1]
  b = v[2]
  L = 0
  for (i in seq(1, 8)){
    L = L + 
      Y[i]*log(1/(1+exp(-(a*X[i]+b)))) + 
      (1 - Y[i])*log(1-1/(1+exp(-(a*X[i]+b))))
  }
  return(-L)
}

# (ii)
optim(c(0, 0), LogLikelihood)$par
glm(Y~X, family=binomial)$coefficients

# Calcul du nombre de décimales correctes
prec_a = 0
while (trunc(optim(c(0, 0), LogLikelihood)$par[1]*10^prec_a)/10^prec_a == 
       trunc(glm(Y~X, family=binomial)
             $coefficients[2]*10^prec_a)/10^prec_a) {
  prec_a = prec_a + 1
}
sprintf("La valeur de a est correcte à %d décimales près", prec_a-1)
prec_b = 0
while (trunc(optim(c(0, 0), LogLikelihood)
             $par[2]*10^prec_b)/10^prec_b == 
       trunc(glm(Y~X, family=binomial)$coefficients[1]*10^prec_b)/10^prec_b) {
  prec_b = prec_b + 1
}
sprintf("La valeur de b est correcte à %d décimales près", prec_b-1)
