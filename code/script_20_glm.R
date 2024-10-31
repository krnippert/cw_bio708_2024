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