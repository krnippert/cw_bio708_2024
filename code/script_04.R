
# setup -------------------------------------------------------------------
rm(list = ls())

library(tidyverse)

iris <- as_tibble(iris)


# ggplot ------------------------------------------------------------------

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))

#scatter plot
iris %>% 
  ggplot(aes(x = Sepal.Length, 
             y = Sepal.Width))+
  geom_point()

# scatterplot -------------------------------------------------------------
## change color by speices
iris %>% 
    ggplot(aes(x = Sepal.Length, 
               y = Sepal.Width, 
               color = Species))+
    geom_point()

## change color uniformly  
iris %>% 
  ggplot(aes(x = Sepal.Length, 
            y = Sepal.Width))+
   geom_point(color = "red")

# line --------------------------------------------------------------------

df0 <- tibble(x = rep(1:50, 3),
              y = x*2)

df0 %>% 
  ggplot(aes(x = x, y = y))+
  geom_line()+
  geom_point()


# histogram ---------------------------------------------------------------

iris %>% 
  ggplot(aes(x = Sepal.Length))+
  geom_histogram()

iris %>% 
  ggplot(aes(x = Sepal.Length))+
  geom_histogram(binwidth = 0.5)

iris %>% 
  ggplot(aes(x = Sepal.Length, fill = Species))+
  geom_histogram(bins = 50)

iris %>% 
  ggplot(aes(x = Sepal.Length))+
  geom_histogram(bins = 50)

# box plot -----------------------------------------------------------------

iris %>% 
  ggplot(aes(x = Species, 
             y = Sepal.Length))+
  geom_boxplot()

#change color (outline) of boxes
iris %>% 
  ggplot(aes(x = Species, 
             y = Sepal.Length, 
             color = Species))+
  geom_boxplot()
    
#change fill of boxes   
iris %>% 
  ggplot(aes(x = Species, 
             y = Sepal.Length, 
             fill = Species))+
  geom_boxplot()

#change color transparency
iris %>% 
  ggplot(aes(x = Species, 
             y = Sepal.Length, 
             fill = Species))+
  geom_boxplot(alpha = 0.3)


# exercise 1 - ridgeline plot ---------------------------------------------

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0)) %>%
  filter(text %in% c("Almost Certainly","Very Good Chance","We Believe","Likely","About Even", "Little Chance", "Chances Are Slight", "Almost No Chance"))

library(ggridges)

data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text)) +
  geom_density_ridges(alpha=0.5, bandwidth=7) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )+
  xlab("") +
  ylab("Assigned Probability (%)")


# exercise 2 - network diagram --------------------------------------------
library(igraph)
library(ggraph)

# Load researcher data
dataUU <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyUndirectedUnweighted.csv", header=TRUE)

# Transform the adjacency matrix in a long format
connect <- dataUU %>%
  gather(key="to", value="value", -1) %>%
  na.omit()

# Number of connection per person
c( as.character(connect$from), as.character(connect$to)) %>%
  as_tibble() %>%
  group_by(value) %>%
  summarize(n=n()) -> coauth
colnames(coauth) <- c("name", "n")

# Create a graph object with igraph
mygraph <- graph_from_data_frame( connect, vertices = coauth )

# Make the graph
ggraph(mygraph, layout="fr") +
  geom_edge_density(edge_fill="#69b3a2") +
  geom_edge_link(edge_colour="black", edge_alpha=0.2, edge_width=0.3) +
  geom_node_point(aes(size=n, alpha=n)) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(rep(1,4), "cm")
  )           


# exercise 3 - violin plot ------------------------------------------------

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# sample size
sample_size = data %>% group_by(name) %>% summarize(num=n())

# Plot
data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=value, fill=name)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("")
