# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# Count data --------------------------------------------------------------

df_count <- read.csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

## fit a normal model to count data 
m_normal <- lm(count ~ nitrate,
               df_count)
summary(m_normal)

## draw data
alpha <- coef(m_normal)[1]
beta <- coef(m_normal)[2]

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point(color = "darkorange") +
  geom_abline(intercept = alpha,
              slope = beta)
## can fit a line to the data, but doesn't represent it very well
## should find another way to model besides normal lm()

# Poisson Model
## Pois(lambda)
## variance is equal to the mean, assumes that dataset can be described by mean = variance
## numbers from this dist. are always >= 0, and are discrete values (ex. 1, 2, 3, ... n)
## always need to think of type of data in the response variable
## y ~ Poisson(lambda)
## log(lambda) = alpha + beta(x)
## lambda must always be >= 0
## l(i) = exp(a + b(i))
## if you take the log of lambda you can freely estimate intercept and slope

m_pois <- glm(count ~ nitrate,
               data = df_count,
               family = "poisson")
summary(m_pois)

## parameter estimates
theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta / se
print(z_value)

## make predictions
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))
y_normal <- predict(m_normal, newdata = df_pred)
y_pois <- predict(m_pois, newdata = df_pred) %>% exp()

df_pred <- df_pred %>% 
  mutate( y_normal, y_pois)

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes( y = y_normal),
                linetype = "dashed") +
  geom_line(data = df_pred, 
            aes(y = y_pois),
            color = "gold")


# offset term -------------------------------------------------------------
## this accounts for unequal area of sampling

df_count_ue <- df_count %>% 
  mutate(area = rpois(nrow(.), 10),
         count_ue = count * area)

## plot for area vs. count
df_count_ue %>% 
  ggplot(aes(x = area,
             y = count_ue)) +
  geom_point()

## glm w/ offset term
m_poisson_ue <- glm(count ~ nitrate + offset(log(area)),
                data = df_count_ue,
                family = "poisson")
summary(m_poisson_ue)

## one way to account for sampling and design


# proportional data -------------------------------------------------------

## binomial data
## y ~ Binomial(N, p)
## value of p is constrained from 0.0 - 1.0 since it is a probability
## we know y and N, calculate p using a binomial model
## logit transformation log(p/1-p) guarantees values of p are confined from 0-1

df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line() +
  labs( y = "x",
        x = "logit(x)")

df_mussel <- read_csv(here::here("data_raw/data_mussel.csv"))
print(df_mussel)

#calculate proportion of fertilized eggs
df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)

#plot
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point()+
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")

#glm with binomial error distribution
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized) %>% 
  head()

summary(m_binom)

#make prediction
df_pred <- df_mussel %>% 
  reframe(density = seq(min(density),
                        max(density),
                        length=100))

#y_binom is inv. logit-transformed bc predict() returns values in logit-scale
y_binom <- predict(m_binom, newdata = df_pred) %>% boot::inv.logit()


df_pred <- df_pred %>% 
  mutate(y_binom)

## draw on earlier fig
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point()+
  geom_line(data = df_pred, 
            aes(y = y_binom))+
  labs(y = "proportion of eggs fertilized",
       x = "Mussel density")

# binomial distribution w/ variable numbers of trials ---------------------

## create fake data w/ variable number of trials
df_mussel <- df_mussel %>% 
  mutate(n_examined = rpois(nrow(.), lambda = 40))

print(df_mussel)

## this model code naturally accounts for variation in n_examined
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")
summary(m_binom)
