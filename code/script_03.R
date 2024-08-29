

# setup -------------------------------------------------------------------

rm(list = ls())

#load in library
library(tidyverse)
library(ggplot2)

#change to tibble
iris <- as_tibble(iris)

# data manipulations -------------------------------------------------------

##Row manipulations
#filter function
# single match "=="
filter(iris, Species == "virginica")

# multiple match "%in%"
filter(iris, Species %in% c("virginica", "versicolor"))

# except "!="
filter(iris, Species != "virginica")

# except multiple "!(x %in% c("a", "b"))
filter(iris, !(Species %in% c("virginica", "versicolor")))

# greater than ">"
filter(iris, Sepal.Length > 5)

# equal & greater than ">="
filter(iris, Sepal.Length >= 5)

# less than "<"
filter(iris, Sepal.Length < 5)

# equal & less than "<="
filter(iris, Sepal.Length <= 5)

##arrange()
# arrange in an ascending order
arrange(iris, Sepal.Length)

# arrange in an descending order
arrange(iris, desc(Sepal.Length))


##column manipulation
##select()
# select one column
select(iris, Sepal.Length)

# select multiple columns
select(iris, c(Sepal.Length, Sepal.Width))

# remove one column
select(iris, -Sepal.Length)

# remove multiple columns
select(iris, -c(Sepal.Length, Sepal.Width))

# select/remove multiple columns with a start rule
# starts_with("x")
select(iris, starts_with("Sepal"))
select(iris, -starts_with("Sepal"))

# select/remove multiple columns with an end rule
# ends_with("x")
select(iris, ends_with("Width"))
select(iris, -ends_with("Width"))

##mutate()
x <- 1:150
mutate(iris, id=x)


# piping ------------------------------------------------------------------

###HOTKEY FOR PIPING IS CTRL + SHIFT + M
# the following codes produce the same data frame
# apply functions separately
df_vir <- filter(iris, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)
print(df_vir_sl)

# piping
iris %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)

# reshape -----------------------------------------------------------------

##pivot_wider()
iris_w <- iris %>% 
  mutate(id = rep(1:50, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

print(iris_w)

iris_l <- iris_w %>% 
  pivot_longer(cols = c("setosa",
                        "versicolor",
                        "virginica"), # columns with values to be reshaped
               names_to = "Species", # column IDs move to "Species"
               values_to = "Sepal.Length") # column values move to "Sepal.Length"

print(iris_l)

##pivot_longer()
iris_l <- iris_w %>% 
  pivot_longer(cols = c("setosa",
                        "versicolor",
                        "virginica"), # columns with values to be reshaped
               names_to = "Species", # column IDs move to "Species"
               values_to = "Sepal.Length") # column values move to "Sepal.Length"

print(iris_l)


# group operations --------------------------------------------------------

##group_by() and summarize()
# grouping by "Species", then take means "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length))

# grouping by "Species", then take means & SD "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length),
            sd_sl = sd(Sepal.Length))

##group_by() and mutate()
# grouping by "Species", then take means "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()


# join --------------------------------------------------------------------

# matching by a single column
## left join by "Species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

left_join(x = df1,
          y = df2,
          by = "Species")
# matching by a single column
## left join by "Species": one to many
df3 <- tibble(Species = c("A", "A", "B", "C"),
              y = c(4, 5, 6, 7))

left_join(x = df1,
          y = df3,
          by = "Species")

# matching by a single column
## left join by "Species": one to missing
df4 <- tibble(Species = c("A", "A", "C"),
              y = c(4, 5, 7))

left_join(x = df1,
          y = df4,
          by = "Species")

# matching by multiple columns
## one to one
df5 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3),
              z = c("cool", "awesome", "magical"))

left_join(x = df1,
          y = df5,
          by = c("Species", "x"))

# matching by multiple columns
## one to many
df6 <- tibble(Species = c("A", "A", "B", "C"),
              x = c(1, 1, 2, 3),
              z = c("cool", "cool", "awesome", "magical"))

left_join(x = df1,
          y = df6,
          by = c("Species", "x"))

# matching by multiple columns
## one to missing
df6 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 4),
              z = c("cool", "awesome", "magical"))

left_join(x = df1,
          y = df6,
          by = c("Species", "x"))


