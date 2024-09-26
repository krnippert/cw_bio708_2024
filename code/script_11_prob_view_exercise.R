# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# Exercise 1 ----------------------------------------------------

set.seed(7)
df_1 <- rnorm(50)

mu <- mean(df_1)

sigma <- sd(df_1)

df_1 <- as_tibble(df_1)

x <- seq(min(df_1$value), max(df_1$value), length = 50)

pd <- dnorm(x, mean = mu, sd = sigma)

x_min <- floor(min(df_1))

x_max <- ceiling(max(df_1))

bin <- seq(x_min, x_max, by = 0.2)

p <- NULL
for (i in 1:(length(bin) -1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu)
}

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_1))

### creating figure
df_1 %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 0.2)+
  geom_point(data = df_prob,
             aes(y = freq, x = bin),
             color = "purple")+
  geom_line(data = df_prob, 
            aes(x = bin, y = freq),
            color = "purple")


# exercise 2 --------------------------------------------------------------

set.seed(3)
df_2 <- rpois(1000, lambda = 17)

df_2 <- as_tibble(df_2)

df_2 %>% 
  ggplot(aes(x = value))+
  geom_histogram(binwidth = 0.5, center = 0)

x <- seq(0,100, by = 1)
lambda_hat <- mean(df_2$value)
pm <- dpois(x, lambda = lambda_hat)

tibble(y = pm, x = x) %>% 
  ggplot(aes(x = x, y = y))+
  geom_line()+
  geom_point()

df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_2))

df_2 %>% 
  ggplot(aes(x = value))+
  geom_histogram(binwidth = 0.5, 
                 center = 0,
                 fill = "skyblue")+
  geom_line(data = df_prob, 
            aes(x = x, y = freq),
            linetype = "dashed",
            color = "navy")+
  geom_point(data = df_prob, 
             aes(x= x, y = freq),
             color = "navy")
  
