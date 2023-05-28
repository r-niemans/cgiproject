rm(list = ls())
library(rstan)
library(loo)
library(combinat)
library(tidyverse)
options(mc.cores = parallel::detectCores())
set.seed(42)
#### getting the data ####
df <- read.csv('datasets/full_data_wide_am.csv')[,c(2:8)]
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

# scale the data

df[,-c(1,2,4)] <- scale(df[,-c(1,2,4)])[,1]

# adding factor variables
df <- cbind(df, read.csv('datasets/full_data_wide_am.csv')[,c(9,10,13,15)])
colnames(read.csv('datasets/full_data_wide_am.csv'))
# take a sample of the postcodes:

PC_sample <- sample(unique(df$postal_code), 50)


# making training and testing datasets

train_df <- df %>% filter(postal_code %in% PC_sample) %>% filter(month_year <= "2022-06-01")
test_df <- df %>% filter(postal_code %in% PC_sample) %>% filter(month_year > "2022-06-01")


# We refactorize the postal codes
train_df$postal_code <- as.integer(as.factor(train_df$postal_code))
test_df$postal_code <- as.integer(as.factor(test_df$postal_code))




# Create a data list for comparisons
model_data <- list(N = nrow(train_df),
                   postal_code = train_df$postal_code,
                   mon_price = train_df$monthly_price,
                   diff_EV = train_df$diff_EV,
                   diff_cars = train_df$diff_cars,
                   CP = train_df$diff_CP,
                   lag_1 = train_df$lag_CP_1, # we take either way
                   lag_2 = train_df$lag_CP_2,
                   lag_3 = train_df$lag_CP_3,
                   lag_EV = train_df$lag_EV,
                   Supermarket = train_df$Supermarket,
                   Conv_store = train_df$Convenience.stores,
                   Railw_st = train_df$Railway.station,
                   N_postal_codes = length(unique(train_df$postal_code)))

# Define your pars
sum <- 0
for (i in 4:7){
x<- ncol(combn(c(1:8),i))
sum <- sum + x}

pars <- c(names(model_data)[-c(1,2,7,6,12,14)])

distributions <- c("normal", "student_t")


# TODO: ALSO ADD FULL MODELS BY HAND!!!! 

for (i in 1:length(pars)) {
  
  for (k in 1:2) {
    combinations <- combinat::combn(pars, i) 
    distribution <- distributions[k]
    
    if (!is.matrix(combinations)) {
      emotion_combination <- combinations 
    }
    
    for (j in 1:ncol(combinations)) {
      emotion_combination <- combinations[,j]
      
      # Build the model code
      model_code <- paste0("
        data {
          int<lower=0> N;
          ", paste(paste0("vector[N] ", emotion_combination), collapse = "; "), ";
          vector[N] lag_1;
  int postal_code[N]; // vector of postal code per observation
  int N_postal_codes; // number of postal codes
  vector[N] CP; // vector of new charging points/per month

        }
        parameters {
          real alpha;
          real<lower=0> sigma;
          ", paste(paste0("real psi_", emotion_combination), collapse = "; "), ";
          real beta;
          vector[N_postal_codes] eta_raw;
          real<lower = 0> sigma_eta;
          real<lower = 0> nu;  // degrees of freedom for Student's t-distribution
        }
        transformed parameters {
          vector[N_postal_codes] eta = sigma_eta * eta_raw;
          vector[N] mu = alpha + ", paste(paste0("psi_", emotion_combination, " * ", emotion_combination), collapse = " + "), " + beta * lag_1  + eta[postal_code];
        }
        model {
          alpha ~ normal(0,1);
          ", paste(paste0("psi_", emotion_combination, " ~ normal(0,1)"), collapse = "; "), ";
          beta ~ normal(0,1); // coef of lag1
          eta_raw ~ normal(0,1);
          sigma_eta ~ normal(0,1);
          nu ~ cauchy(0, 5);  // weakly informative prior for degrees of freedom
      ")
      
      if (distribution == "normal") {
        model_code <- paste0(model_code, "
          CP ~ normal(mu, sigma);}
        ")
      } else if (distribution == "student_t") {
        model_code <- paste0(model_code, "
          CP ~ student_t(nu, mu, sigma) ;}
        ")
      if (distribution == "student_t") {
        model_code <- paste0(model_code, "
           generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = ", distribution, "_lpdf(CP[i] |nu, mu[i], sigma);
            y_rep[i] = ", distribution, "_rng(nu,mu[i], sigma);
          }
        }
      
      ")} else{
        model_code <- paste0(model_code, "
        generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = ", distribution, "_lpdf(CP[i] | mu[i], sigma);
            y_rep[i] = ", distribution, "_rng(mu[i], sigma);
          }
        }
      ") }
      
      # Fit the model
      fit <- stan(model_code = model_code, 
                  data = model_data, 
                  iter = 1000,
                  warmup = 500,
                  thin = 4, 
                  cores = 8,
                  chains = 8,
                  refresh = 100) # This line suppresses the output from Stan
      
      # Compute the loo
      log_lik <- extract_log_lik(fit, merge_chains = FALSE)
      r_eff <- relative_eff(exp(log_lik), cores = 8) 
      loo_result <- loo::loo(log_lik, r_eff = r_eff, cores = 8)
      
      # Save the loo object to a file
      saveRDS(loo_result, file = paste0("datasets/loo_results/", paste(emotion_combination, collapse = "_"), "_", distribution, ".rds"))
      
      # Print the current loop stage
      print(emotion_combination)
      print(distribution)
      
      # Clean the environment, but keep the necessary variables
      rm(list=setdiff(ls(), c("combinations", "i", "j", "k", "model_data", "pars", "distributions", "distribution")))
    }
  }
}
}



names_list <- list.files(path = "datasets/loo_results/",
                         pattern = ".rds", full.names = T)
model_names <- gsub("datasets/loo_results//|\\.rds", "", names_list)
loos <- lapply(names_list, FUN =readRDS)

comparison <- unlist(loo_compare(loos))
model_nums <- as.numeric(gsub("model", "",rownames(comparison)))

model_names <- model_names[model_nums]
comparison <- cbind(model_names,comparison)
comparison[,-1] <- round(as.numeric(comparison[,-1]), 5)
#### Extratcion to LateX ####
library(xtable)
extraction <- head(comparison[,-c(2,3,9,8)], n = 10)
rownames(extraction) <- NULL
extraction[,-1] <- round(as.numeric(extraction[,-1]), 3)





