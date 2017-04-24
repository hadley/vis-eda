library(tidyverse)
library(ggrepel)
library(ggbeeswarm)

mtcars <- mutate(mtcars, name = rownames(mtcars))

ggplot(mtcars[sample(32, 10), ], aes(wt, mpg)) +
  geom_point(colour = "red") +
  geom_text_repel(aes(label = name))
ggsave("ggrepel.png", width = 4, height = 3)

ggplot(iris, aes(Species, Sepal.Length)) +
  geom_quasirandom()
ggsave("ggbeeswarm.png", width = 4, height = 3)
