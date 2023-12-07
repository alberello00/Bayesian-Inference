# Package installation
install.packages("rstanarm", dependencies = TRUE)
# load
library(rstanarm)
library(rstan)

## Data generation
set.seed(123)
n <- 100
sigma <- 1
beta <- c(1, 2)
x1 <- rnorm(n = n,mean = 1, sd = 0.5)
x2 <- rnorm(n = n,mean = 0.5, sd = 0.25)
X <- cbind(x1, x2)
y <- rnorm(n = n, mean = X %*% beta, sd = sigma)
## data created with two explanatory variables
summary(lm(y~X[,1]+X[,2]))


data_example <- data.frame(y,x1,x2)

## model equation: simple linear model
mod_equation <- y ~ x1+x2

stan_fit_lr <- stan_glm(formula = mod_equation, 
                        data = data_example,
                        family = "gaussian",
         prior = normal(0,10),
         prior_intercept = normal(0.10),
         prior_aux = cauchy(0,1))

#traceplot
stan_trace(stan_fit_lr, 
          pars = c("sigma","(Intercept)", "x1", "x2"))

#summary
summary(stan_fit_lr, pars=c("sigma","(Intercept)", "x1", "x2"), digits = 3)

library(ggplot2)
#acf
stan_ac(stan_fit_lr, 
     pars = c("sigma","(Intercept)", "x1", "x2"))


#hist
stan_hist(stan_fit_lr,pars = c("sigma","(Intercept)", "x1", "x2"))


#post estimates
plot(stan_fit_lr,pars = c("sigma","(Intercept)", "x1", "x2"))


#####################################
### Posterior predictive distribution

library(bayesplot)

#generate
post_pred <- posterior_predict(stan_fit_lr)

#posterior predictive densities
ppc_dens_overlay(y = y, yrep = post_pred[40:100,])

#statistics
ppc_stat(y = y, yrep = post_pred, stat = "mean")
ppc_stat(y = y, yrep = post_pred, stat = "sd")

#waic
waic(stan_fit_lr)
