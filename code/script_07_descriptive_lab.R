# setup -------------------------------------------------------------------
rm(list = ls())

library(tidyverse)


# exercise 1 --------------------------------------------------------------

## create a new vector z w/ length 1000 as exp(rnorm(n=1000, mean = 0, sd = 0.1))
## calc arithmetic mean, geometric mean, and median

z <- exp(rnorm(n=1000, mean = 0, sd = 1))

mu_z <- mean(z)
print(mu_z)

gm_z <-  prod(z)^(1/length(z))
print(gm_z)

med_z <- median(z)
print(med_z)

## draw a histogram of z using functions tibble(), ggplot(), and geom_histogram()

z <- as_tibble(z)

z %>% 
  ggplot(aes(x = value)) +
  geom_histogram()

## draw vertical lines of aritmetic mean, geometric mean, and median on the histogram
z %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50)+
  geom_vline(xintercept = mu_z, colour = "red")+
  geom_vline(xintercept = gm_z, colour = "blue")+
  geom_vline(xintercept = med_z, colour = "green")

## create new vector z_rev as -z = max(z) = 0.1 and repeat steps 1-4
z <- exp(rnorm(n=1000, mean = 0, sd = 1))

z_rev <- -(z) + max(z) + 0.1

#arithmetic mean
m_z_rev <- mean(z_rev)
print(m_z_rev)

#geometric mean
log_z <- log(z_rev)
m_zrev <- mean(log_z)
g_zrev <- exp(m_zrev)
print(g_zrev)

#median
med_zrev <- median(z_rev)
print(med_zrev)

# create histogram
z_rev <- as_tibble(z_rev)

g_hist <- z_rev %>% 
  ggplot(aes(x = value)) +
  geom_histogram()

## draw vertical lines of aritmetic mean, geometric mean, and median on the histogram
z_rev %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50)+
  geom_vline(xintercept = m_z_rev, colour = "goldenrod1", linewidth = 1, linetype = 3)+
  geom_vline(xintercept = g_zrev, colour = "orchid", linewidth = 1, linetype = 5)+
  geom_vline(xintercept = med_zrev, colour = "darkcyan", linewidth = 1)+
  theme_bw()

##or

df_mu <- tibble(mu = c(m_z_rev, g_zrev, med_zrev),
                type = c("arithmetic", "geometric", "median"))
                
g_hist + 
  geom_vline(data = df_mu, aes(xintercept = mu,
                               color = type))


# exercise 2 --------------------------------------------------------------

## understand and compare variation measures using 100 measurements of fish in "gram"
w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

## converting from grams [w] to mg [m]
m <- w * 1000
head(m)

## calculate sd and MAD for w and m
sd_w <-  sqrt(sum((w - mean(w))^2 / length(w)))
print(sd_w)

sd_m <- sqrt(sum((m - mean(m))^2 / length(m)))
print(sd_m)

mad_w <- median(abs(w-median(w)))
print(mad_w)

mad_m <- median(abs(m-median(m)))
print(mad_m)

## calculate cv and mad/median for w and m 

(cv_w <- sd_w / mean(w)) 
(cv_m <- sd_m / mean(m))

mm_w <- mad_w / median(w)
print(mm_w)

mm_m <- mad_m / median(m)
print(mm_m)
