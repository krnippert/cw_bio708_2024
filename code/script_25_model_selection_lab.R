# setup -------------------------------------------------------------------
rm(list = ls())
library(palmerpenguins)
library(plyr)
source(here::here("code/set_library.R"))


# clean data --------------------------------------------------------------

penguin <- penguins_raw

colnames(penguin) <- c("study_name", "sample_number", "species",
                       "region", "island", "stage", "indiv_id",
                       "cc", "d_egg","cl", "cd", "fl", "bm",
                       "sex", "d_15n", "d_13c", "comments")

penguin$cc <- revalue(penguin$cc, c("Yes"=1))
penguin$cc <- revalue(penguin$cc, c("No"=0))
head(penguin$cc)

penguin$species <- sapply(penguin$species, switch, "Chinstrap penguin (Pygoscelis antarctica)" = 'chinstrap',
                          "Adelie Penguin (Pygoscelis adeliae)" = 'adelie',
                          "Gentoo penguin (Pygoscelis papua)" = 'gentoo')


penguin %>% drop_na(cl, cd, fl, bm, sex)

penguin$cc <- as.numeric(penguin$cc)
# analyze data ------------------------------------------------------------

m <- glm(cc ~ cl + cd + fl + bm + sex,
         family = "binomial",
         data = penguin)

summary(m)

library(MuMIn)
options(na.action = "na.fail")
m <- #model object
  m_set <- dredge(m, rank = "AIC")
subset(m_set, delta < 4)
