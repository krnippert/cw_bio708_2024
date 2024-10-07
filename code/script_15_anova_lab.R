# setup -------------------------------------------------------------------
rm(list = ls())

source(here::here("code/set_library.R"))


# Application to plant growth --------------------------------------------------------------

pg <- as_tibble(PlantGrowth)
unique(pg)

pg %>% 
  ggplot(aes(x = group, y = weight))+
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2)+
  geom_jitter(alpha = 0.2)

aov_pg <- aov(formula = weight ~ group,
              data = pg)
print(aov_pg)
summary(aov_pg)

## there was significant differences that existed between the weights of the groups in the PlantGrowth data set
## I would report the f value and p value, and you could also perform a post-hoc test to see if that changes anything
