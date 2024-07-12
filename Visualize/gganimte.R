installed.packages("pacman")
library(pacman)
pacman::p_load(
  tidyverse,
  gganimate,
  babynames,
  gapminder,
  viridis,
  RColorBrewer
)

#Keep Only 3 Names

babynames %>% 
  filter(name %in% c("James","James","Andrew","John")) %>% 
  filter(sex=="M") %>% 
  ggplot(aes(x=year,
             y=n,
             group = name,
             color=name))+
  geom_line(linewidth=1.5)+
  theme_bw()+
  scale_color_viridis(discrete = TRUE)+
  labs(title="Popularity of American names in the previous",
       x="",
       y="",
       color="Names")+
  theme(plot.title=element_text(size=10,
                                color="steelblue"))+
  transition_reveal(year)#variable which is gonna create each frame of the gif
anim_save("287-smooth-animation-with-tweenr.gif")


ggplot(gapminder,
       aes(x=gdpPercap,
           y=lifeExp,
           size = pop,
           color=continent))+
  geom_point()+
  scale_x_log10()+
  theme_bw()+
  labs(title = "Year: {frame_time}",
       x="GDP per capita",
       y="Life expectancy")+
  theme(plot.title=element_text(size = 60,
                                hjust = 0.5,
                                color = "steelblue"))+
  transition_time(year)

#https://www.youtube.com/watch?v=HiDR3Twoz-s&ab_channel=RProgramming101













  