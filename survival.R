library(survival)

# Loading and preparing the data
data = read.table('data/myelome.txt')
colnames(data) = data[1,]
data = data[-1,]

data$NOBS = as.numeric(data$NOBS)
data$T = as.numeric(data$T)
data$DECES = as.numeric(data$DECES)
data$LOG_UREE = as.numeric(data$LOG_UREE)
data$HB = as.numeric(data$HB)
data$PQ = as.numeric(data$PQ)
data$INFJ0 = as.numeric(data$INFJ0)
data$AGE = as.numeric(data$AGE)
data$SEXE = as.numeric(data$SEXE)
data$LOG_GB = as.numeric(data$LOG_GB)
data$FRACTURE = as.numeric(data$FRACTURE)
data$LOG_GBM = as.numeric(data$LOG_GBM)
data$P_LYMP = as.numeric(data$P_LYMP)
data$P_MYEL = as.numeric(data$P_MYEL)
data$PROT_U = as.numeric(data$PROT_U)
data$BENCE_J = as.numeric(data$BENCE_J)
data$PROT_S = as.numeric(data$PROT_S)
data$GLOBULIN = as.numeric(data$GLOBULIN)
data$CALCIUM = as.numeric(data$CALCIUM)
data$TEMPS = as.numeric(data$TEMPS)

summary(data)

# Extracting subsets (sex, bence-jones)
h = subset(data, SEXE==1)
f = subset(data, SEXE==0)
hist(h$AGE)
hist(f$AGE)

b0 = subset(data, BENCE_J==0)
b1 = subset(data, BENCE_J==1)
hist(b0$AGE)
hist(b1$AGE)

# Computing means by sex and by Bence-Jones index
mean(h$AGE)
mean(f$AGE)

mean(b0$AGE)
mean(b1$AGE)

cont = rbind(t(data$BENCE_J), t(data$SEXE))
chisq.test(cont)

v = unique(data$CALCIUM)
m = c()
for (i in seq(1,length(v))){
  grp = subset(data, CALCIUM==v[i])
  cat("Moyenne d'âge pour le niveau de consommation", v[i], "\n")
  m[i] = mean(grp$AGE)
  print(m[i])
}

plot(m, v, xlab="Âge moyen", ylab="Niveau de consommation de calcium")

my = subset(data, P_MYEL!=0)
survie_my = survfit(Surv(T, DECES)~1, data=my)
print(survie_my, print.rmean=TRUE)

survie_b = survfit(Surv(T, DECES)~BENCE_J, data=my)
print(survie_b, print.rmean=TRUE)

# Plotting Kaplan-Meier survival plot
plot(survie_b, col=2:3)
lLab = unique(data$BENCE_J)
legend("top", legend=lLab, lty=1, col=2:3, horiz=FALSE, bty='n')

survdiff(Surv(T, DECES)~BENCE_J, data=my)
