rm(list = ls())



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
library(loo)
library(bayesplot)
library(rstantools)
library(MLmetrics)

# Adding lagged values
df <- read.csv('datasets/wide_data_modelling.csv')
df <- df %>%
  arrange(postal_code, month_year) %>%
  group_by(postal_code) %>%
  mutate(diff_EV = value_EV - lag(value_EV, default = first(value_EV)),
          diff_CP = value_CP - lag(value_CP, default = first(value_CP)),
         diff_cars = value_cars - lag(value_cars, default = first(value_cars)),
         lag_EV = dplyr::lag(value_EV, default = first(value_EV)),
         lag_CP_1 = stats::lag(value_CP, default = first(value_CP), k = 1),
         lag_CP_2 = stats::lag(value_CP, default = first(value_CP), k = 2),
         lag_CP_3 = stats::lag(value_CP, default = first(value_CP), k = 3),
         lag_cars = stats::lag(value_cars, default = first(value_cars))) %>%
  ungroup()



# We also need to factorize the Postal Codes

df$postal_code <- as.integer(as.factor(df$postal_code))


# We also need to standardize the data 
# TODO: Unstandardize it before plotting the data!!!


# We sample certain percentage of postal codes
PC_sample <- sample(unique(df$postal_code), 50)


# We make a training dataset of all - 6 months
train_df <- df %>% filter(postal_code %in% PC_sample) %>% filter(month_year <= "2022-06-01")
test_df <- df %>% filter(postal_code %in% PC_sample) %>% filter(month_year > "2022-06-01")

# We refactorize the postal codes
train_df$postal_code <- as.integer(as.factor(train_df$postal_code))
test_df$postal_code <- as.integer(as.factor(test_df$postal_code))

model_code_basic <-'data {
  int<lower=0> N; // number of observations
  vector[N] mon_price; // vector of gas prices for each month
  vector[N] diff_cars; //vector of differnces in cars 
  vector[N] lag_1; // lag1 of the dependent variable
  vector[N] lag_2; // lag2 of the dependent variable
//  vector[N] lag_3; // lag3 of the dependent variable
  int postal_code[N]; // vector of postal code per observation
  int N_postal_codes; // number of postal codes
  vector[N] CP; // vector of new charging points/per month
}

parameters {
  real alpha; // intercept (the same for each observation)
  real beta1; // coef of mon_price
  real beta2; // coef of diff cars
  real phi1; // coef of lag_1
  real phi2; // coef of lag_2
//  real phi3; // coef of lag_3
  real<lower=0> sigma; // standard deviation of CP
  vector[N_postal_codes] eta_PC;   // Postal code random effects
}

transformed parameters {
  vector[N] mu = rep_vector(0, N);
  
  for (t in 1:N) {
    mu[t] = alpha + beta1 * mon_price[t] + beta2 * diff_cars[t] + phi1 * lag_1[t] + phi2 * lag_2[t] + eta_PC[postal_code[t]]; // phi3 * lag_3[t] +
  }
}

model {
  // PRIORS (we assume that everything is normal for now)
  alpha ~ normal(0, 1); // intercept
  beta1 ~ normal(0, 1); // gas price
  beta2 ~ normal(0,1); // car_diff
  phi1 ~ normal(0, 1); // coef of lag_1
  phi2 ~ normal(0, 1); // coef of lag_2
 // phi3 ~ normal(0, 1); // coef of lag_3
  eta_PC ~ normal(0, 1);
  sigma ~ normal(0, 1);
  
  CP ~ normal(mu, sigma);
}

generated quantities {
  vector[N] y_rep = rep_vector(0, N); // vector for prior predictive check
  vector[N] log_lik = rep_vector(negative_infinity(), N); // log likelihood for model comparison
  
  for (t in 1:N) {
    y_rep[t] = normal_rng(mu[t], sigma);
    log_lik[t] = normal_lpdf(CP[t] | mu[t], sigma);
  }
}'

model_data <- list(N = nrow(train_df),
                   postal_code = train_df$postal_code,
                   mon_price = scale(train_df$monthly_price)[,1],
                   diff_EV = scale(train_df$diff_EV)[,1],
                   diff_cars = scale(train_df$diff_cars)[,1],
                   CP = scale(train_df$diff_CP)[,1],
                   lag_1 =scale(train_df$lag_CP_1)[,1],
                   lag_2 = scale(train_df$lag_CP_2)[,1],
                   lag_3 = scale(train_df$lag_CP_3)[,1],
                   lag_EV = scale(train_df$lag_EV)[,1],
                   N_postal_codes = length(unique(train_df$postal_code)))
??unscale

#### prior predictive check ####

prior_samples <- stan(model_code = model_code_basic, 
                          data = model_data, 
                          iter = 750, 
                          chains = 8, 
                          algorithm = "Fixed_param")


y_rep <- rstan::extract(prior_samples, "y_rep")$y_rep

# Fitting the draws against V
pp_check(model_data$CP, y_rep, fun = "dens_overlay")

#### fitting ####


fit <- stan(model_code = model_code_basic, 
            #pars = pars,
            data = model_data, 
            iter = 1500,
            warmup = 500,
            refresh = 50,
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


#### model evaluation ####

# loo 
log_lik <- extract_log_lik(fit, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 8) 
loo <- loo::loo(log_lik[,,-c(1:model_data$lag)], r_eff = r_eff[-c(1:model_data$lag)], cores = 8)
plot(loo)

# rsq
var_fit <- apply(param.sims$y_rep, 1 ,var)
var_res <- as.matrix(fit, pars = c("sigma"))^2

var_res.emp = apply(model_data$CP-param.sims$y_rep,1,var)
var_mu.emp = apply(param.sims$mu,1,var)


rsq <- var_mu.emp/ (var_mu.emp + var_res.emp)

hist(rsq, sub = paste('Rsquared model 1 mean: ',
                      round(mean(rsq),3)))
abline(v=median(rsq), lty =2, col = 'blue')



plot(fit, show_density = TRUE, ci_level = 0.5, fill_color = "pink") + geom_vline(xintercept = 0, linetype = "dotted")



#### Result interpretation  ####

for(i in 1:model_data$N_postal_codes){
  hist(param.sims$eta_PC[,i], breaks = 10,
       xlim = c(-5,6),
       ylim = c(0,500),
       col = adjustcolor(i,0.2),
       xlab = 'trend.control',
       main = 'posterior distirbution of postcode effects')
  par(new = T)
}



# We also need to evaluate out of sample - train test split traditionally



##### Using data to make predictions ####
newdata <- list(N = nrow(test_df),
                                   postal_code = test_df$postal_code,
                                   mon_price = scale(test_df$monthly_price)[,1],
                                   diff_EV = scale(test_df$diff_EV)[,1],
                                   diff_cars = scale(test_df$diff_cars)[,1],
                                  # CP = scale(test_df$diff_CP)[,1],
                                   lag_1 =scale(test_df$lag_CP_1)[,1],
                                   lag_2 = scale(test_df$lag_CP_2)[,1],
                                   lag_3 = scale(test_df$lag_CP_3)[,1],
                                   lag_EV = scale(test_df$lag_EV)[,1],
                                   N_postal_codes = length(unique(test_df$postal_code)),
                                   lag = 3)


# Create a mapping from postal_code to column index in eta_PC
postal_code_mapping <- match(newdata$postal_code, levels(as.factor(model_data$postal_code)))

# compute mean effect for each observation in newdata
eta_PC_samples <- rowMeans(param.sims$eta_PC[, postal_code_mapping])

newdata$preds <- data.frame(matrix(ncol =300, nrow = 2000))
for (i in 1:length(newdata$postal_code)) {
  preds <- param.sims$alpha + param.sims$beta1 * newdata$mon_price[i] + param.sims$beta2 * newdata$diff_cars[i]+
    param.sims$phi1 * newdata$lag_1[i] + param.sims$phi2 * newdata$lag_2[i] + param.sims$eta_PC[newdata$postal_code[i]]
  newdata$preds[,i] <- preds
}


# Calculating the point evaluation metrics

mean_preds <- colMeans(newdata$preds)
MAE(mean_preds, scale(test_df$diff_CP)[,1])


