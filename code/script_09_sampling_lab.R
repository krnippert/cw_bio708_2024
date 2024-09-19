# setup -------------------------------------------------------------------
rm(list = ls())

#source("code/set_library.R")
library(tidyverse)
library(patchwork)
# exercise 1 --------------------------------------------------------------

df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

## 50 measures
set.seed(3)
mu_i <- var_ub_i <- NULL

for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 50)
  
  mu_i[i] <- mean(df_i$height)
  
  var_ub_i[i] <- var(df_i$height)
}

df_sample <- tibble(mu_hat = mu_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
(g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram())

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() 

g_mu / g_var_ub


## 100 measures

mu_i <- var_ub_i <- NULL

for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 100)
  
  mu_i[i] <- mean(df_i$height)
  
  var_ub_i[i] <- var(df_i$height)
}

df_sample <- tibble(mu_hat = mu_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
    ggplot(aes(x = mu_hat)) +
    geom_histogram()

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram()

g_mu / g_var_ub

## second approach - combination of lapply, function for loop
df_m <- lapply(X = c(50,100),
               function(x){
                 
                 mu <- sigma <- NULL
                 
                 for(i in 1:100) {
                   df_i <- df_h0 %>% 
                     sample_n(x)
                   
                   mu[i] <- mean(df_i$height)
                   sigma[i] <- var(df_i$height)
                 }
                 
                 cout <- tibble(n = x,
                                mu = mu,
                                sigma = sigma)
                 
                 return(cout)
               }) %>% 
  bind_rows()

df_m %>% 
  ggplot(aes(x=mu,
             color = factor(n)))+
  geom_density()

df_m %>% 
  ggplot(aes(x=sigma,
             color = factor(n)))+
  geom_density()

# exercise 2 --------------------------------------------------------------

df_h10 <- df_h0 %>% 
  filter(height >= 10)

## 50 measures
mu_i <- var_i <- NULL

for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 50)
  
  mu_i[i] <- mean(df_i$height)
  
  var_i[i] <- var(df_i$height)
}

df_sample <- tibble(mu = mu_i,
                    var = var_i)

# histogram for mu
g_mu_10 <- df_sample %>% 
    ggplot(aes(x = mu)) +
    geom_histogram()

# histogram for unbiased variance
g_var_10 <- df_sample %>% 
  ggplot(aes(x = var)) +
  geom_histogram()

g_mu_10 / g_var_10

