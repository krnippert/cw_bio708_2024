# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# data structure ----------------------------------------------------------

df_algae <- read_csv(here::here("data_raw/data_algae.csv"))

#install.packages("skimr")
skimr::skim(df_algae)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) + 
  geom_point() +
  theme_light()


# linear formula ----------------------------------------------------------

## y = a + bx is not a perfect line - must express error (e (epsilon))
## e follows a normal distribution
## function lm() finds the best alpha and beta (model parameters) for the data
## lm() takes a formula as the first argument

m <- lm(biomass ~ conductivity,
        data = df_algae)
summary(m)

## a = 5.30 (estimate of intercept)
## b = 0.50 (estimate of conductivity)
## residual standard error = sigma (not ^2)
## y = 5.30 + 0.50x + e
## e ~ Normal(0, 4.6^2)

# get estimates
alpha = coef(m)[1]
beta = coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) +
  theme_bw()

## null hypothesis in regression analysis is B = 0
## higher t-values in lm() indicate a greater deviation from 0

# extract coefficients
theta <- coef(m)

#extract standard errors
se <- sqrt(diag(vcov(m)))

#t-value
t_value <- theta / se
print(t_value)

# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)
print(p_beta)


# unexplained variation ---------------------------------------------------

# visualize errors
## error is calculated along the y-axis

# get residuals 
# e = y - (a + bx)
eps <- round(df_algae$biomass - (alpha + (beta*df_algae$conductivity)), 4)
eps0 <- round(resid(m), 4)
mean(eps == eps0)
eps == eps0

ss <- sum(eps^2)

df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity, y = biomass))+
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) +
  geom_segment(aes(x = conductivity,
                   xend = conductivity,
                   y = biomass,
                   yend = biomass - eps), 
               linetype = "dashed",
               color = "orange") +
  theme_bw()

## coefficient of determination (R^2) <- want to be small

