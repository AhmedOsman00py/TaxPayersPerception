library(rstan)
source("stanTools.R")
source("functions.R")


######### linear regression one predictor 
d <- data.frame(milk = c(685, 691, 476, 1151, 879, 725, 1190, 1107, 809, 539,
                         298, 805, 820, 498, 1026, 1217, 1177, 684, 1061, 834),
                hours = c(3, 7, 6, 10, 6, 5, 10, 11, 9, 3, 6, 6, 3, 5, 8, 11, 
                          12, 9, 5, 5))

stan_model <- "
data {
  int N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real sigma;
}
model {
  y ~ normal(alpha + beta * x, sigma);
}
"

N <- nrow(d)

fit <- stan(model_code = stan_model, data = list(N = N, x = d$hours, y = d$milk))


parametersToPlot <- c("alpha","beta")

#convergence check
mcmcHistory(fit, parametersToPlot)
mcmcDensity(fit, parametersToPlot,byChain = TRUE)
mcmcDensity(fit, parametersToPlot)

#estimation des parametres
ptable <- parameterTable(fit)



##regression linéaire en fréquentiste
lm(d$milk~d$hours)





########## linear regression with multiple predictors

d <- data.frame(milk = c(685, 691, 476, 1151, 879, 725, 1190, 1107, 809, 539,
                         298, 805, 820, 498, 1026, 1217, 1177, 684, 1061, 834),
                hours = c(3, 7, 6, 10, 6, 5, 10, 11, 9, 3, 6, 6, 3, 5, 8, 11, 
                          12, 9, 5, 5),
                age = c(3, 4, 5, 2, 5, 6, 4, 2, 5, 4, 6, 2, 5, 4, 3, 2, 3, 6, 4, 5))

stan_model <- "
data {
  int N;
  matrix[N, 2] x;
  vector[N] y;
}
parameters {
  vector[2] beta;
  real alpha;
  real sigma;
}
model {
  y ~ normal(alpha + x * beta, sigma);
}
"

N <- nrow(d)

fit <- stan(model_code = stan_model, data = list(N = N, x = d[, -1], y = d$milk))


summary(fit)

parametersToPlot <- c("alpha","beta")


#convergence check
mcmcHistory(fit, parametersToPlot)
mcmcDensity(fit, parametersToPlot,byChain = TRUE)
mcmcDensity(fit, parametersToPlot)

#estimation des parametres
ptable <- parameterTable(fit)



##regression linéaire en fréquentiste
lm(d$milk~d$hours+d$age)






########## linear regression with categorical predictors


d <- data.frame(milk = c(685, 691, 476, 1151, 879, 725, 1190, 1107, 809, 539,
                         298, 805, 820, 498, 1026, 1217, 1177, 684, 1061, 834),
                breed = factor(c("A", "B", "B", "A", "A", "B", "B", "A", "A", "A",
                                 "B", "A", "A", "B", "A", "A", "A", "B", "A", "A")),
                hours = c(3, 7, 6, 10, 6, 5, 10, 11, 9, 3, 6, 6, 3, 5, 8, 11, 
                          12, 9, 5, 5))

