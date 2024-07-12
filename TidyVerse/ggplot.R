library(ggplot2)
library(dplyr)
library(gapminder)
gapminder_1952 <- gapminder %>%
  
  filter(year == 1952)

# Change to put pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()
# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()
# Change this plot to put the x-axis on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()+ scale_x_log10()
# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952,aes(pop,lifeExp,color=continent,size=gdpPercap))
+geom_point()+scale_x_log10()
# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952,aes(pop,lifeExp))+
  geom_point()+scale_x_log10()+facet_wrap(~continent)
# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder,aes(gdpPercap,lifeExp, color=continent,size=pop))+
  geom_point()+scale_x_log10()+facet_wrap(~year)
