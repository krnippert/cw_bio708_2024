# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))

# exercise 1 - binomial ---------------------------------------------------

y = c(2,2,0,0,3,1,3,3,4,3)
prob <- seq(0, 1, by = 0.01)

lh <- sapply(prob, function(x) prod(dbinom(x = y, 
                                           size = 10,
                                           prob = x)))
df_binom <- tibble(prob = prob,
                   lh = lh)       

df_binom %>%
  arrange(desc(lh)) %>% 
  print()

df_binom %>% 
  ggplot(aes(x = prob, y = lh))+
  geom_line()+
  geom_point()+
  labs(x = "probability",
       y = "likelihood")


## sample mean 
mean(y/10)

