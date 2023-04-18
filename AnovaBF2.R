rm(list=ls()) ## clear workspace

set.seed(12345) ## reproducibility 

packages <- c("MASS", "BayesFactor", "bayestestR", "ez", "reshape2", "parallel", "doParallel", "foreach")

for (package in packages) {
  library(package, character.only = T)
}

# type 3 sum of squares 
options(contrasts = c("contr.sum", "contr.poly"))

### create 4 data frames
## data1
# set the parameters

{
  n <- 45
  mu1 <- c(0, 0)
  mu2 <- c(-0.5, 0.5)
  mu3 <- c(-0.7, 0.7)
  mu4 <- c(-0.71, 0.71)
  var <- 1
  Rho <- 0.3
  Sigma <- matrix(c(var, Rho, Rho, var), 2, 2)
}

# sampling from bivariate normal distributions
gp1 <- mvrnorm(n = n, mu = mu1, Sigma = Sigma)
gp2 <- mvrnorm(n = n, mu = mu2, Sigma = Sigma)
gp3 <- mvrnorm(n = n, mu = mu3, Sigma = Sigma)
gp4 <- mvrnorm(n = n, mu = mu4, Sigma = Sigma)

id <- 1:180
Group <- c(rep(1, 90), rep(2, 90))
factor1 <- c(gp1[,1], gp2[,1], gp3[,1], gp4[,1])
factor2 <- c(gp1[,2], gp2[,2], gp3[,2], gp4[,2])
data_wide <- cbind(id, Group, factor1, factor2)
data_wide <- as.data.frame(data_wide)

data1 <- melt(data_wide, id.vars = c("id", "Group"), measure.vars = c("factor1", "factor2"),
              variable.name = "factor", value.name = "dv")

## data 2
# set the parameters

{ 
  n <- 45
  mu1 <- c(-1, 1)
  mu2 <- c(-1.5, 1.5)
  mu3 <- c(-1.7, 1.7)
  mu4 <- c(-1.71, 1.71)
  var <- 1
  Rho <- 0.5
  Sigma <- matrix(c(var, Rho, Rho, var), 2, 2)
}

# sampling from bivariate normal distributions
gp1 <- mvrnorm(n = n, mu = mu1, Sigma = Sigma)
gp2 <- mvrnorm(n = n, mu = mu2, Sigma = Sigma)
gp3 <- mvrnorm(n = n, mu = mu3, Sigma = Sigma)
gp4 <- mvrnorm(n = n, mu = mu4, Sigma = Sigma)

id <- 1:180
Group <- c(rep(1, 90), rep(2, 90))
factor1 <- c(gp1[,1], gp2[,1], gp3[,1], gp4[,1])
factor2 <- c(gp1[,2], gp2[,2], gp3[,2], gp4[,2])
data_wide2 <- cbind(id, Group, factor1, factor2)
data_wide2 <- as.data.frame(data_wide2)

data2 <- melt(data_wide2, id.vars = c("id", "Group"), measure.vars = c("factor1", "factor2"),
              variable.name = "factor", value.name = "dv")

## data 3
# set the parameters

{ 
  n <- 45
  mu1 <- c(-2, 2)
  mu2 <- c(-2.5, 2.5)
  mu3 <- c(-2.7, 2.7)
  mu4 <- c(-2.71, 2.71)
  var <- 1
  Rho <- 0.6
  Sigma <- matrix(c(var, Rho, Rho, var), 2, 2)
}

# sampling from bivariate normal distributions
gp1 <- mvrnorm(n = n, mu = mu1, Sigma = Sigma)
gp2 <- mvrnorm(n = n, mu = mu2, Sigma = Sigma)
gp3 <- mvrnorm(n = n, mu = mu3, Sigma = Sigma)
gp4 <- mvrnorm(n = n, mu = mu4, Sigma = Sigma)

id <- 1:180
Group <- c(rep(1, 90), rep(2, 90))
factor1 <- c(gp1[,1], gp2[,1], gp3[,1], gp4[,1])
factor2 <- c(gp1[,2], gp2[,2], gp3[,2], gp4[,2])
data_wide3 <- cbind(id, Group, factor1, factor2)
data_wide3 <- as.data.frame(data_wide3)

data3 <- melt(data_wide3, id.vars = c("id", "Group"), measure.vars = c("factor1", "factor2"),
              variable.name = "factor", value.name = "dv")

## data 4
# set the parameters

{ 
  n <- 45
  mu1 <- c(-3, 3)
  mu2 <- c(-3.5, 3.5)
  mu3 <- c(-3.7, 3.7)
  mu4 <- c(-3.71, 3.71)
  var <- 1
  Rho <- 0.7
  Sigma <- matrix(c(var, Rho, Rho, var), 2, 2)
}

# sampling from bivariate normal distributions
gp1 <- mvrnorm(n = n, mu = mu1, Sigma = Sigma)
gp2 <- mvrnorm(n = n, mu = mu2, Sigma = Sigma)
gp3 <- mvrnorm(n = n, mu = mu3, Sigma = Sigma)
gp4 <- mvrnorm(n = n, mu = mu4, Sigma = Sigma)

id <- 1:180
Group <- c(rep(1, 90), rep(2, 90))
factor1 <- c(gp1[,1], gp2[,1], gp3[,1], gp4[,1])
factor2 <- c(gp1[,2], gp2[,2], gp3[,2], gp4[,2])
data_wide4 <- cbind(id, Group, factor1, factor2)
data_wide4 <- as.data.frame(data_wide4)

data4 <- melt(data_wide3, id.vars = c("id", "Group"), measure.vars = c("factor1", "factor2"),
              variable.name = "factor", value.name = "dv")

{
  data1$id <- as.factor(data1$id)
  data1$factor <- as.factor(data1$factor)
  data1$Group <- as.factor(data1$Group)
}

{
  data2$id <- as.factor(data2$id)
  data2$factor <- as.factor(data2$factor)
  data2$Group <- as.factor(data2$Group)
}

{
  data3$id <- as.factor(data3$id)
  data3$factor <- as.factor(data3$factor)
  data3$Group <- as.factor(data3$Group)
}

{
  data4$id <- as.factor(data4$id)
  data4$factor <- as.factor(data4$factor)
  data4$Group <- as.factor(data4$Group)
}

### Run the ANOVAs

system.time(model1 <- ezANOVA(data = data1, 
                              dv =. (dv), wid =. (id),
                              within =. (factor),
                              between =. (Group),
                              type = 3,
                              detailed = T))
print(model1)

system.time(model2 <- ezANOVA(data = data2, 
                              dv =. (dv), wid =. (id),
                              within =. (factor),
                              between =. (Group),
                              type = 3,
                              detailed = T))
print(model2)

system.time(model3 <- ezANOVA(data = data3, 
                              dv =. (dv), wid =. (id),
                              within =. (factor),
                              between =. (Group),
                              type = 3,
                              detailed = T))
print(model3)

system.time(model4 <- ezANOVA(data = data4, 
                              dv =. (dv), wid =. (id),
                              within =. (factor),
                              between =. (Group),
                              type = 3,
                              detailed = T))
print(model4)

### Run the Bayesian ANOVAs
## For timing reasons just run two Bayesian ANOVAs

system.time(ModelB1 <- anovaBF(dv ~ factor*Group + id, data = data1, whichRandom = "id", iterations = 5e5))
print(ModelB1)

system.time(ModelB2 <- anovaBF(dv ~ factor*Group + id, data = data2, whichRandom = "id", iterations = 5e5))
print(ModelB2)

# Bayes factors inclusion
BFincl1 <- bayesfactor_inclusion(ModelB1, match_models = T); BFincl1
Bfincl2 <- bayesfactor_inclusion(ModelB2, match_models = T); BFincl2

### parallel processing 
nb_cores <- detectCores()
print(nb_cores)

# 4 cores
cl <- makeCluster(4) 
registerDoParallel(cl)

# embed the data frames within a list
List <- list(data1, data2, data3, data4)

### run the ANOVAs in parallel
system.time(results1 <- foreach(data = List, .combine = "rbind") %dopar% {
  library(data.table)
  AovModel <- aov(dv ~ Group*factor + Error(id/factor), data = data)
  return(data.table(summary(AovModel)))
})

# It seems that I cannot correctly extract the outputs for those first ANOVAs
results1[[1]][[2]]
results1[[2]]
results1[[3]]
results1[[4]]

system.time(results2 <- foreach(data = List, .combine = "c") %dopar% { 
  library(data.table); library(ez)
  AovModel <- ezANOVA(data = data, dv =. (dv),
                      wid =. (id), 
                      within =. (factor),
                      between =. (Group),
                      type = 3,
                      detailed = T)
  return(data.table(AovModel))
}) 

results2[[1]]
results2[[2]]
results2[[3]]
results2[[4]]

### Run the Bayesian ANOVAs in parallel
system.time(results3 <- foreach(data = List, .combine = "c") %dopar% {
  library(BayesFactor); library(bayestestR); library(data.table)
  ModelBayes <- anovaBF(dv ~ factor*Group + id, data = data, whichRandom = "id", iterations = 5e5)
  return(data.table(ModelBayes))
})

results3[[1]]
results3[[2]]
results3[[3]]
results3[[4]]

# Bayes factors inclusion
ModelBF1 <- bayesfactor_inclusion(results3[[1]], match_models = T); ModelBF1
ModelBF2 <- bayesfactor_inclusion(results3[[2]], match_models = T); ModelBF2
ModelBF3 <- bayesfactor_inclusion(results3[[3]], match_models = T); ModelBF3
ModelBF4 <- bayesfactor_inclusion(results3[[4]], match_models = T); ModelBF4

stopCluster(cl)
