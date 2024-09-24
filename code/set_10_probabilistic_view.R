# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# Continuous variables ----------------------------------------------------

df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

## histogram
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

## can describe histograms by a certain function. i.e. normal distribution
## x ~ Normal(mu, sigma^2)
## use ~ to show stochastic relationships (= is a 1 to 1 relationship)
## probability density function (PDF) can help summarize/understand a normal distribution


# pdf to frequency distribution -------------------------------------------

# vector of x values
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)

pd <- dnorm(x, mean = mu, sd = sigma)

# create fig
tibble(y=pd, x=x) %>% 
  ggplot(aes(x=x, y=y))+
  geom_line()+
  labs(y = "probability density")

## convert probability density to probability
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
p5 <- pnorm(q = 5, mean = mu, sd = sigma)
p10_p5 <- p10 - p5

x_min <- floor(min(df_h0$height))
x_max <- ceiling(max(df_h0$height))

bin <- seq(x_min, x_max, by = 1)

p <- NULL
for (i in 1: (length(bin) - 1)){
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - 
    pnorm(bin[i], mean = mu, sd = sigma)
}

df_prob <- tibble(p = p,
                  bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

# graphing the overlay
df_h0 %>% 
  ggplot(aes(x = height))+
  geom_histogram(binwidth = 1, 
                 center = 0.5)+
  geom_point(data = df_prob, 
             aes(y = freq, x = bin), color = "orchid") +
  geom_line(data = df_prob, 
            aes(y = freq, x = bin), color = "orchid")


# discrete variable -------------------------------------------------------

## probability mass function

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

df_count %>% 
  ggplot(aes(x = count))+
  geom_histogram(binwidth = 0.5,
                 center = 0)

## use a Poisson distribution to measure the observed distribution of the discrete variables
## x ~ Poisson(lambda)

## PMF to frequency distribution
x <- seq(0,10, by =1)

## calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

## figure
tibble(y = pm, x = x) %>% 
  ggplot(aes(x = x, y = y))+
  geom_line(linetype = "dashed")+
  geom_point()+
  labs(y = "probability", x = "count")

## convert to frequency
df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count))

df_count %>% 
  ggplot(aes(x = count))+
  geom_histogram(binwidth = 0.5,
                 center = 0)+
  geom_line(data = df_prob, 
             aes(x = x, y = freq),
             linetype = "dashed", color = "forestgreen") +
  geom_point(data = df_prob, 
             aes(x = x, y = freq), color = "forestgreen")
