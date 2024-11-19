# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# model fit and complexity ---------------------------------------------------------

set.seed(1)

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)

# create a design matrix
X <- model.matrix(~x1)

# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
y_hat <- drop(X %*% b)

# add normal errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point()

# correct model used to generate the data
m1 <- lm(y ~ x1, data = df0)
summary(m1)

# add column x2 which is irrelevant for y
df0 <- df0 %>% 
  mutate(x2 = rnorm(n))

# add x2 to the model
m2 <- lm(y ~ x1 + x2, data = df0)
summary(m2)


# adjusted r-squared ------------------------------------------------------

# adjusted R-square for m2 w/out x2
sm1 <- summary(m1)
print(sm1$adj.r.squared)

# adjusted R-square for m2 w/ x2
sm2 <- summary(m2)
print(sm2$adj.r.squared)

## r2 is good fit for normal model, but need likelihood to measure any non-normal distribution

# log likelihood
logLik(m1)
logLik(m2)

# likelihood ratio test ---------------------------------------------------

# test = "Chisq" specifies a chi-square distribution
# as a distribution of LR
anova(m1, m2, test = "Chisq")


# AIC ---------------------------------------------------------------------

# AIC concerns about the robustness of the model when you add new data points
# the model w/ the lowest AIC doesn't necessarily imply it is the "true" model
# AIC is not designed to infer causality 
# AIC = 2k - 2 ln L
# k = # of parameters, L = likelihood
# lower AIC values indicate better predictability of the model

AIC(m1)
AIC(m2)

