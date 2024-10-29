# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# exercise 1 --------------------------------------------------------------

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)
summary(m_iris)

eps <- resid(m_iris)

shapiro.test(eps)

# exercise 2 --------------------------------------------------------------

b <- coef(m_iris)
a <- NULL

#setosa intercept
a[1] <- b[1]

#versicolor intercept
a[2] <- b[1] + b[3]

#virginica intercept
a[3] <- b[1] + b[4]


# exercise 3 --------------------------------------------------------------

m_iris2 <- lm(Petal.Length ~ Petal.Width,
              data = iris)
print(m_iris2)

n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep)))

y_pred <- predict(m_iris2,
                  newdata = df_pred)
df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

print(df_pred)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))
