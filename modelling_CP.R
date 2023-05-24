rm(list = ls())

df <- read.csv('datasets/wide_data_modelling.csv')

set.seed(123)
library(hts)
library(dplyr)
library(tidyr)
library(forecast)
library(vars)
library(purrr)
library(MTS)
library(car)
library(rstan)

# Adding lagged values
df <- df %>%
  arrange(postal_code, month_year) %>%
  group_by(postal_code) %>%
  mutate(diff_EV = value_EV - lag(value_EV, default = first(value_EV)),
         diff_CP = value_CP - lag(value_CP, default = first(value_CP)),
         diff_cars = value_cars - lag(value_cars, default = first(value_cars)),
         lag_EV = lag(value_EV, default = first(value_EV)),
         lag_CP = lag(value_CP, default = first(value_CP)),
         lag_cars = lag(value_cars, default = first(value_cars))) %>%
  ungroup()



# We also need to factorize the Postal Codes

df$postal_code <- as.integer(as.factor(df$postal_code))


# We also need to standardize the data 
# TODO: Unstandardize it before plotting the data!!!

df[,-c(1,2)] <- scale(df[,-c(1,2)])


model_code_basic <- 'data {
  int<lower=0> N; // number of observations

  vector[N] mon_price; // vector of gas prices for each month
  vector[N] diff_EV; // 1st diff of electric vehicles
  vector[N] lag_EV; // vector of lagged number of EVs
//  vector[N] lag_cars; // vector of lagged number of regular cars
//  vector[N] diff_cars; // 1st diff of regular vehicles
  int<lower=1> lag;  // Order of the autoregressive model
  

 
  int postal_code[N]; // vector of postal code per observation
  int N_postal_codes; // number of postal codes


  vector[N] CP; // vector of new charging points/per month
  
}


parameters {
  real alpha; // intercept (the same for each observation)
  real beta1; // coef of mon_price
  real beta2; // coef of diff_cars
  real beta3; // coef of lag_EV
  real<lower=0> sigma; // standard deviation of CP
  vector[lag] phi; // AR coefs

  vector[N_postal_codes] eta_PC;   // Postal code random effects

}


model {
  // PRIORS (we assume that everything is normal for now)
  alpha ~ normal(0,1); // intercept
  beta1 ~ normal(0,1); // gas price
  beta2 ~ normal(0,1); // diff EV
  beta3 ~ normal(0,1); // lag of EV
  phi ~ normal(0,1);

  eta_PC ~ normal(0,1);
  sigma ~ normal(0,1);

  
 for (t in lag+1:N) {
    vector[lag] x_ar;
    for (lag_idx in 1:lag) {
      x_ar[lag_idx] = CP[t-lag_idx];
    }
    CP[t] ~ normal(alpha + dot_product(phi, x_ar) + beta1 * mon_price[t] + beta2 * diff_EV[t] + beta3 * lag_EV[t]  + eta_PC[postal_code[t]], sigma);
  }
}
    
'
param.sims = 
  rstan::extract(
    fit,
    #pars = pars, # when we want to specify the parameters to be simulated
    permuted = TRUE, 
    inc_warmup = FALSE,
    include = TRUE)

array_of_draws <- as.array(fit)

#### Convergance diagnostics ####

# Rhat - should be within 0.95-1.1

Rhats <- c()
par_names <- c()
for (i in 1:length(dimnames(array_of_draws)[[3]])) {
  Rhats[i] <- Rhat(array_of_draws[,,i])
  par_names[i] <- dimnames(array_of_draws)[[3]][[i]]
}

max(Rhats)
min(Rhats)

Rhats <- tibble(Rhats, par_names)
head(Rhats, n= 7)

# Ess - both should be at least 100*chain num (however tail is less important)


ESSs_b <- c()
ESSs_t <- c()

for (i in 1:length(dimnames(array_of_draws)[[3]])) {
  ESSs_b[i] <- ess_bulk(array_of_draws[,,i])
  ESSs_t[i] <- ess_tail(array_of_draws[,,i])
}

# Should be above 100*chains = 800
max(ESSs_b)
min(ESSs_b)

max(ESSs_t)
min(ESSs_t)

ESSs <- tibble(ESSs_b, ESSs_t, par_names)
head(ESSs, n = 7)



plot(c(1:8532), df$diff_EV, type = 'l')
plot(c(1:8532), scale(df$diff_EV), type = 'l')

model_data <- list(N = nrow(df),
                   postal_code = df$postal_code,
                   mon_price = scale(df$monthly_price)[,1],
                   diff_EV = scale(df$diff_EV)[,1],
                   CP = scale(df$diff_CP)[,1],
                   lag_EV = scale(df$lag_EV)[,1],
                   N_postal_codes = length(unique(df$postal_code)),
                   lag = 12)


acf(model_data$CP)
fit <- stan(model_code = model_code_basic, 
            #pars = pars,
            data = model_data, 
            iter = 1000,
            warmup = 500,
            refresh = 10,
            thin = 4, # thinning serves to reduce the autocorrelation between posterior samples for each parameter - overall serves to `stimulate convergence` earlier than otherwise 
            cores = 8,
            chains = 8,
            verbose = T)



fit_summary <- summary(fit)


traceplot(fit, inc_warmup = TRUE)

param.sims = 
  rstan::extract(
    fit,
    #pars = pars,
    permuted = TRUE, 
    inc_warmup = FALSE,
    include = TRUE)


for(i in 1:237){
  hist(param.sims$eta_PC[,i],
       xlim = c(-5,6),
       ylim = c(0,500),
       col = adjustcolor(i,0.2),
       xlab = 'trend.control',
       main = 'posterior distirbution of country effects')
  par(new = T)
}

#### model evaluation ####

# loo 
log_lik <- extract_log_lik(fit, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 8) 
loo3 <- loo::loo(log_lik, r_eff = r_eff, cores = 8)
plot(loo)

# rsq
var_fit <- apply(param.sims$y_rep, 1 ,var)
var_res <- as.matrix(fit, pars = c("sigma"))^2

var_res.emp = apply(data$V-param.sims$mu,1,var)
var_mu.emp = apply(param.sims$mu,1,var)


rsq <- var_mu.emp/ (var_mu.emp + var_res.emp)

hist(rsq, sub = paste('Rsquared model 1 mean: ',
                      round(mean(rsq),3)))
abline(v=median(rsq), lty =2, col = 'blue')


plot(fit, show_density = TRUE, ci_level = 0.5, fill_color = "pink") + geom_vline(xintercept = 0, linetype = "dotted")

