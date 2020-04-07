library(tidyverse)

gapminder <- read_csv("data/gapminder_data.csv")
gapminder_1977 <- filter(gapminder,year == 1977)

ggplot(
  data = gapminder_1977,
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
  ) +
  geom_point() + 
  scale_x_log10()

#ggplot(<DATA>, <AESTHETIC MAPPINGS>) + <GEOMETRY LAYER> + <GEOMETRY LAYER>
#___________________________________________________________________

# using different variables

gapminder_1977 %>% 
  ggplot(mapping = aes(x = lifeExp, y = gdpPercap, colour = continent, 
                         size = pop)) +
  geom_point()


gapminder_1977 %>% 
  ggplot() +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
  geom_line(mapping = aes(x = gdpPercap, y = lifeExp))+
  scale_x_log10()

?geom_point

#shape instead of colour
gapminder_1977 %>% 
  ggplot() +
  geom_point(mapping = aes(x = gdpPercap, y = lifeExp, shape = continent, size = pop)) +
    scale_x_log10()


gapminder_1977 %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
  geom_point(colour = 'blue', size = 5) +
  scale_x_log10()

gapminder_1977 %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
  geom_point(colour = 'grey', shape = 'square') +
  scale_x_log10()


gapminder_1977 %>% 
  ggplot(mapping=aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point(shape=21, colour="black", fill="white", size=5, stroke=5)+
  scale_x_log10()

ggplot(
  data = gapminder_1977,
  mapping = aes(x = lifeExp, y = gdpPercap, colour = continent, size = pop) #mapped to aesthetics.
) +
  geom_point(shape = 25, alpha = 0.7, fill = 'blue', stroke = 2, colour = 'orange') + # addition of layer for geometry.
  scale_y_log10()

gapminder_1977 %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, shape = continent, size = pop)) +
  geom_point(alpha = .4) +
  scale_x_log10()

gapminder %>% 
  ggplot(mapping = aes(x = year, y = lifeExp, shape = continent, size = pop)) + 
  geom_point()



