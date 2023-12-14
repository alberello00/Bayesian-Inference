############################################
########  EXAMPLE 4: Poisson model  ########
############################################
library(rstanarm)
library(loo)
library(bayesplot)
library(rstan)

data4<-read.csv("Data_Ex_4.csv")

#overview
str(data4)

#############
# Poisson GLM
#############
mod_ex4a<-stan_glm(formula = colonies~quinoline+log_quinoline, 
                   data = data4,
                   family = "poisson",
                   prior = normal(0,10, autoscale=T),
                   prior_intercept = normal(0,10, autoscale=T))
prior_summary(mod_ex4a)
summary(mod_ex4a)
mod_ex4a<-update(mod_ex4a, iter=4000)
summary(mod_ex4a)

# Convergence
stan_trace(mod_ex4a, nrow=3,ncol=1)
stan_ac(mod_ex4a)

# Posterior checks
y_tilde4a<-posterior_predict(mod_ex4a)
ppc_dens_overlay(y = data4$colonies, yrep = y_tilde4a[1100:1200,])
ppc_stat(y = data4$colonies, yrep = y_tilde4a, stat = "mean")
ppc_stat(y = data4$colonies, yrep = y_tilde4a, stat = "sd")
ppc_stat(y = data4$colonies, yrep = y_tilde4a, stat = "min")
ppc_stat(y = data4$colonies, yrep = y_tilde4a, stat = "max")


###################
# Poisson Mixed GLM
###################

mod_ex4b<-stan_glmer(formula = colonies~quinoline+log_quinoline+(1|plate), 
                     data = data4,
                     family = "poisson",
                     prior = normal(0,10, autoscale=T),
                     prior_intercept = normal(0,10, autoscale=T))

# Convergence
summary(mod_ex4b)

#increasing iterations
mod_ex4b<-update(mod_ex4b, iter=8000, adapt_delta=.95)
summary(mod_ex4b)
stan_trace(mod_ex4b, nrow=6, ncol = 1)
stan_ac(mod_ex4b)

# Posterior checks
y_tilde4b<-posterior_predict(mod_ex4b)
ppc_dens_overlay(y = data4$colonies, yrep = y_tilde4b[1100:1200,])
ppc_stat(y = data4$colonies, yrep = y_tilde4b, stat = "mean")
ppc_stat(y = data4$colonies, yrep = y_tilde4b, stat = "sd")
ppc_stat(y = data4$colonies, yrep = y_tilde4b, stat = "min")
ppc_stat(y = data4$colonies, yrep = y_tilde4b, stat = "max")

# Posterior inference: new covariate pattern
# quinoline=500
data4_new<-data.frame(quinoline=500, log_quinoline = log(500+10), plate ="A")

# evaluation linear predictor
mu_new <- posterior_linpred(mod_ex4b, 
                          newdata = data4_new,
                          transform = T)
# equivalent to posterior_epred()
# posterior predictive distribution
y_tilde_new <- posterior_predict(mod_ex4b, newdata = data4_new)

#comparison
plot(density(y_tilde_new), ylim=c(0,0.2), lwd=2,
     main = "Comparison of the varibility")
lines(density(mu_new), col="red", lwd=2)
legend("topright", pch=16,
       c("Posterior distribution \nof the conditional expectation", "Posterior predictive distribution"), col = c("red", "black"))


mean(mu_new);sd(mu_new)
mean(y_tilde_new);sd(y_tilde_new)
