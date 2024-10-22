rm(list = ls())

source(here::here("code/set_library.R"))


# exercise 1 --------------------------------------------------------------

head(iris)

unique(iris$Species)

setosa <- iris %>% 
  filter(Species == "setosa")

versicolor <- iris %>% 
  filter(Species == "versicolor")

virginica <- iris %>% 
  filter(Species == "virginica")

sm <- lm(Sepal.Width ~ Petal.Width, data = setosa)
summary(sm)

vem <- lm(Sepal.Width ~ Petal.Width, data = versicolor)
summary(vem)

vim <- lm(Sepal.Width ~ Petal.Width, data = virginica)
summary(vim)


# exercise 2 --------------------------------------------------------------

sm2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = setosa)
summary(sm2)

vem2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = versicolor)
summary(vem2)

vim2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = virginica)
summary(vim2)

m_int <- m <- lm(Sepal.Width ~ Petal.Width + Petal.Length + Petal.Width:Petal.Length, data = iris)
summary(m_int)

# exercise 3 --------------------------------------------------------------

##manually calculate r2 value
## residual variance
ss <- sum(resid(sm2)^2)

v_y <- setosa %>%  pull(Sepal.Width)
## null variance
ss_0 <- sum((v_y - mean(v_y))^2)

##coefficient of variance
r2 <- 1 - ss / ss_0

print(r2)
summary(sm2)



