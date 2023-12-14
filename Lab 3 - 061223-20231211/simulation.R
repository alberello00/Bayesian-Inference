library(loo)
library(bayesplot)
library(rstan)

library(rstanarm)
data(roaches)
# Rescale
roaches$roach1 <- roaches$roach1 / 100

# 1.
mod_a<-stan_glm(formula = y~roach1+senior, 
                   data = roaches,
                   family = "poisson")
prior_summary(mod_a)
summary(mod_a)

# 2.
stan_trace(mod_a, nrow=3,ncol=1)
library(ggplot2)
stan_ac(mod_a)

# 3.
mod_b<-stan_glmer(formula = y~roach1+senior+(1|treatment), 
                     data = roaches,
                     family = "poisson",
                     prior = normal(0,10, autoscale=T),
                     prior_intercept = normal(0,10, autoscale=T))
prior_summary(mod_b)
summary(mod_b)
mod_b <- update(mod_b, iter=4000, adapt_delta=.99)
summary(mod_b)
# 4.
stan_trace(mod_b, nrow=6, ncol = 1)
stan_ac(mod_b)
summary(mod_b)

# 5.
waic(mod_a)
waic(mod_b)

# 6.
y_tildeb<-posterior_predict(mod_b)
ppc_dens_overlay(y = roaches$y, yrep = y_tildeb[1100:1200,])
ppc_stat(y = roaches$y, yrep = y_tildeb, stat = "mean")

#7. 
mu <- posterior_epred(mod_b)
quantile(mu, probs = c(0.05,0.5,0.95))
