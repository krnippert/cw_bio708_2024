
# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# t test equivalence w/ lm -----------------------------------------------------------

df_fl <- read_csv(here::here("data_raw/data_fish_length.csv"))

#group means
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu) ## pull turns it into a vector from a dataframe

# mu_a: should be identical to intercept
v_mu[1]

# mu_b - mu_a: should be identical to slope
v_mu[2] - v_mu[1]

# in lm() letters are automaticallly converted to a 0/1 binary value
m <- lm(length ~ lake, 
        data = df_fl)
summary(m)

lake_a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

lake_b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x = lake_b, y = lake_a, var.equal = TRUE)


# anova equivalence w/ lm -------------------------------------------------

df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))
print(df_anova)

v_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

print(c(v_mu[1],
        v_mu[2] - v_mu[1],
        v_mu[3] - v_mu[1]))

m <- lm(length ~ lake,
        data = df_anova)
summary(m)

m_aov <- aov(length ~ lake,
             data = df_anova)
summary(m_aov)

# multiple types of predictors (ancova) --------------------------------------------

iris <- as_tibble(iris)
print(iris)

distinct(iris, Species)
# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)
summary(m_iris)

# create a data frame for prediction
# variable names must be identical to the original dataframe for analysis
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))

# make prediction based on supplied values of explanatory variables
y_pred <- predict(m_iris,
                  newdata = df_pred)
df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)
print(df_pred)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))

## alternative version <- faster/safer way to create df_pred; just less intuitive
df_pred0 <- iris %>% 
  group_by(Species) %>% 
  reframe(Petal.Width = seq(min(Petal.Width),
                            max(Petal.Width),
                            length = 100))
y_pred0 <- predict(m_iris,
                  newdata = df_pred0)
df_pred0 <- df_pred0 %>% 
  mutate(y_pred0 = y_pred0)
print(df_pred0)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred0,
            aes(y = y_pred0))

