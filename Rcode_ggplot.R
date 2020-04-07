library(tidyverse)

gapminder <- read_csv("data/gapminder_data.csv")
gapminder_1977 <- filter(gapminder,year == 1977)

ggplot(data = gapminder_1977,
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
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

# blue dot and set size
gapminder_1977 %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
  geom_point(colour = 'blue', size = 5) +
  scale_x_log10()

# different colours and shapes
gapminder_1977 %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
  geom_point(colour = 'grey', shape = 'square') +
  scale_x_log10()

# from Yuwan circles in 2 colours
gapminder_1977 %>% 
  ggplot(mapping=aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point(shape=21, colour="black", fill="white", size=5, stroke=5)+
  scale_x_log10()

#from Tristan blue and yellow traingles
ggplot(
  data = gapminder_1977,
  mapping = aes(x = lifeExp, y = gdpPercap, colour = continent, size = pop) #mapped to aesthetics.
) +
  geom_point(shape = 25, alpha = 0.7, fill = 'blue', stroke = 2, colour = 'orange') + # addition of layer for geometry.
  scale_y_log10()

#different shapes and alpha
gapminder_1977 %>% 
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp, shape = continent, size = pop)) +
  geom_point(alpha = .4) +
  scale_x_log10()

#full gapminder scatterplot shape per continent
gapminder %>% 
  ggplot(mapping = aes(x = year, y = lifeExp, shape = continent, size = pop)) + 
  geom_point()

#different alpha value
ggplot(data = gapminder,        mapping = aes(x=lifeExp, y=year, colour=continent)
) + geom_point(alpha = .2)  #from Jose

#grouped and summary then plotted
gapminder %>% 
  group_by(continent, year) %>% 
  summarise(mean_lifeExp=mean(lifeExp)) %>% 
  ggplot(aes(x=year, y=mean_lifeExp, colour=continent))+ 
  geom_point()# from Yuwan

#from Stephen per country with geom_line
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent, group = country)) + 
  geom_line()

# geom_line and geom_point
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent, group = country)) + 
  geom_line() +
  geom_point()

# swap order of layers
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent, group = country)) + 
    geom_point() +
geom_line()

# black dots, coloured lines
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent, group = country)) + 
  geom_line() +
  geom_point(colour = 'black')

# swap layers around
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent, group = country)) + 
  geom_point(colour = 'black') +
  geom_line()

# black dots, coloured lines, dealing with overplot
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent, group = country)) + 
  geom_line() +
  geom_point(colour = 'black', alpha = 0.3)

# or in geom function, not in plot as a whole
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, group = country)) + 
  geom_line(mapping = aes(colour = continent)) +
  geom_point(alpha = 0.3)

#plotting extra line over scatterplot and changing colour outside aes
gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.5, colour = 'blue') +
  scale_x_log10() +
  geom_smooth(method = "lm", size = 2, colour = 'red')

#plotting extra line over scatterplot and changing colour inside aes
gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth(method = "lm", size = 2)

#extra geom smooth with no colour by continent
gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(mapping = aes(colour = continent), size = 2) +
  scale_x_log10() +
  geom_smooth(mapping = aes(colour = continent), method = "lm", size = 2) +
  geom_smooth(method = "lm")   

#scales
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent)) +
  geom_point() +
  scale_colour_manual(values = c("red", "green", "blue", "purple", "black"))

#colours available
colours()
scale_colour_brewer()

?scale_colour_brewer()

#replot with colour brewer - greens
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent)) +
  geom_point() +
  scale_colour_brewer(palette = "Greens")

#replot with colour brewer - Dark2
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, colour = continent)) +
  geom_point() +
  scale_colour_brewer(palette = "Dark2")

# Separating plots
a_countries <- filter(gapminder, str_starts(country, "A"))

# per country in facet wrap
ggplot(a_countries, aes(x = year, y = lifeExp, colour = continent, group = country)) +
  geom_line() +
  facet_wrap(~country)

#per year facet wrapped
gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~year)

#ggplot(<DATA>, AESTHETIC MAPPINGS>) + <GEOM> + <GEOM> + <SCALES> + <FACETS>




