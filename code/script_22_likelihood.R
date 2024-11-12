# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# Likelihood --------------------------------------------------------------

## likelihood is the method on how to estimate the alpha and beta in a model
## if you develop a simple model - usually use max. likelihood estimate (MLE)
## harder models - usually use Bayesian stats

### Poisson example ###
## the first argument is "k"
## the second is lambda (mean)
## Pr(y = 3) = 3.5^3 exp(-3.5) / 3!
dpois(x = 3, lambda = 3.5)

## write an equation
## Probability of observing 3 
## if the data follows a poisson dist. w/ mean of 3.5
3.5^3  * exp(-3.5) / factorial(3)

## change lambda from 0 to 10 by 0.1
lambda <- seq(0, 10, by = 0.1)

## prob
pr <- dpois(3, lambda = lambda)

## create dataframe
df_pois <- tibble(y = 3, 
                  lambda = lambda,
                  pr = pr)
print(df_pois)

## plotting
df_pois %>% 
  ggplot(aes(x = lambda, y = pr)) +
  geom_point()+
  geom_line()+
  labs(x = "lambda (mean of poisson dist)",
       y = "Pr(k = 3)")

## arrange by probability
df_pois %>% 
  arrange(desc(pr))

## multiple data points
pr <- dpois(c(3, 2, 5), lambda = 3)
print(pr)

## prob of observing 3, 2, 5 simultaneously
pr[1] * pr[2] * pr[3]
prod(pr)

## likelihood for y = 3, 2, 5
y <- c(3, 2, 5)
lambda <- seq(0, 10, by = 0.01)

pr <- sapply(X = lambda,
       FUN = function(z) prod(dpois(y, lambda = z)))

## make a data frame and arrange by pr
df_pr <- tibble(lambda = lambda, 
                pr = pr)

df_pr %>% 
  arrange(desc(pr)) %>% 
  print()

## visualize
df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr))+
  geom_line()+
  labs(y = "likelihood",
       x = "lambda")

mean(y)


# maximum likelihood method -----------------------------------------------

## MLE (simple case) -> lambda = sum(y) / N -> only applies w/ poisson
## MLE (general case) -> way more complicated 

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

## extracts the log likelihood from the model
logLik(m_pois)

## typically are negative - log of # less than 1 are negative
## internally use this value when evaluating a model
## all stats in ecology rely on the concept of likelihood
## estimate of an outcome of this glm are derived based on max likelihood methods

