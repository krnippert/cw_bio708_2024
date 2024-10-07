# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# partition variability ---------------------------------------------------


df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))
distinct(df_anova, lake)

df_anova %>% 
  ggplot(aes(x = lake, y = length))+
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.2) +
  geom_jitter(alpha = 0.2)

## if we observe greater between-group variability relative to within-group then it suggests that diff. among groups are substantial

## between group variability

##estimate overall mean
mu <- mean(df_anova$length)

## estimate group means and sample size 
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), 
            dev_g = (mu_g - mu)^2,
            n = n())
print(df_g)

df_g <- df_g %>% 
  mutate(ss = dev_g * n)
print(df_g)

s_b <- sum(df_g$ss)
print(s_b)

## within group variability
df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% 
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2)

# filter and slice: show first 3 rows for each group
print(df_i %>%  filter(lake == "a") %>%  slice(1:3))
print(df_i %>%  filter(lake == "b") %>%  slice(1:3))
print(df_i %>%  filter(lake == "c") %>%  slice(1:3))

df_i_g <- df_i %>% 
  group_by(lake) %>% 
  summarize(ss = sum(dev_i))

print(df_i_g)

s_w <- sum(df_i_g$ss)
print(s_w)

## variability to variance

# n_distinct() count the number of unique elements
n_g <- n_distinct(df_anova$lake)
s2_b <- s_b / (n_g - 1)
print(s2_b)

s2_w <- s_w / (nrow(df_anova) - n_g)
print(s2_w)


# Test statistic ----------------------------------------------------------


f_value <- s2_b / s2_w
print(f_value)

## null hypothesis
x <- seq(0, 10, by = 0.1)
y <- df(x = x, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value, 
             color = "skyblue")

## pf() estimates the probability of less than q
## Pr(F0 > F) is 1 - Pr(F0 < F)
p_value <- 1 - pf(q = f_value, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)
print(p_value)


# ANOVA in R --------------------------------------------------------------

#first argument is the formula
# second is the data frame for reference
# don't forget to specify data

m <- aov(formula = length ~ lake,
         data = df_anova)
print(m)
summary(m)
