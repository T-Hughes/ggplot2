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

#13:
gapminder %>% 
  ggplot(aes(x = pop, fill = continent)) + 
  geom_density(alpha = 0.5) +
  facet_wrap(~year) +
  scale_x_log10()

#14:
install.packages("agridat")
library(agridat)
b_wheat <- blackman.wheat

b_wheat %>% 
  ggplot(aes (x = nitro, y = yield, colour = gen )) +
  geom_point() +
  facet_wrap(~loc)

b_wheat %>% 
  ggplot(aes (x = gen, y = yield, colour = nitro )) +
  geom_point() +
  facet_wrap(~loc)

#my favorite
b_wheat %>% 
  ggplot(aes (x = loc, y = yield, colour = gen )) +
  geom_point() +
  facet_wrap(~nitro)

#______________________________________________
#day12


roughplot <- ggplot(a_countries, aes(x = year, y = lifeExp, colour = continent)) +
  geom_line() +
  facet_wrap(~country)

roughplot +
  labs(title = "Figure 1", 
       x = "Year", 
       y = "Life Expectancy",
       colour = "Continent")

#change source include gapminder data and change title (I did subtitle,
# KM did caption)
roughplot +
  labs(title = "Growth in life expectancy for 'A' countries", 
       x = "Year", 
       y = "Life Expectancy",
       colour = "Continent",
       subtitle = "source: Gapminder Data")

roughplot +
  labs(title = "Growth in life expectancy for 'A' countries", 
       x = "Year", 
       y = "Life Expectancy",
       colour = "Continent",
       caption = "source: gapminder data")

roughplot +
  labs(title = "Growth in life expectancy for 'A' countries", 
       x = "Year", 
       y = "Life Expectancy",
       colour = "Continent",
       caption = "source: gapminder data") + 
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
#challenge applying theme
roughplot +
  labs(title = "Growth in life expectancy for 'A' countries", 
       x = "Year", 
       y = "Life Expectancy",
       colour = "Continent",
       caption = "source: gapminder data") + 
  theme_bw() + 
theme(
    strip.background = element_blank(),
    panel.grid.major = element_line(size = 1),
    axis.title  = element_text(size  = 10, colour = "blue"),
    legend.position = "bottom"
  )

#check out theme options
?theme

lifeExp_plot <- roughplot +
  labs(title = "Growth in life expectancy for 'A' countries", 
       x = "Year", 
       y = "Life Expectancy",
       colour = "Continent",
       caption = "source: gapminder data") + 
  theme_bw() + 
  theme(
    strip.background = element_blank(),
    panel.grid.major = element_line(size = 1),
    axis.title  = element_text(size  = 10, colour = "blue"),
    legend.position = "bottom"
  )

#saving files
ggsave(filename = "results/lifeExp.png" , 
       plot = lifeExp_plot,
       width = 12, height = 10, dpi = 300, units = "cm")

ggsave(filename = "results/lifeExp2.png" , 
       plot = lifeExp_plot,
       width = 5, height = 20, dpi = 300, units = "cm")
    
ggsave(filename = "results/lifeExp3.png" , 
       plot = lifeExp_plot,
       width = 20, height = 20, dpi = 300, units = "cm")

#cowplot
install.packages("cowplot")
library(cowplot)

plot1 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()
plot2 <- ggplot(gapminder, aes(x = continent, y = lifeExp)) +
  geom_boxplot()
plot3 <- ggplot(gapminder, aes(x = gdpPercap, y = pop)) +
  geom_point()
plot4 <- ggplot(gapminder, aes(x = lifeExp, y = pop)) +
  geom_point()

plot_grid(plot1, plot2, plot3, plot4)
 
#relative to each other 
plot_grid(plot1, plot2, plot3, plot4, rel_heights = c(1,3),
          rel_widths = c(4,1))

#labels in capitals
plot_grid(plot1, plot2, plot3, plot4, labels = "AUTO")    

#labels in regular
plot_grid(plot1, plot2, plot3, plot4, labels = "auto")


