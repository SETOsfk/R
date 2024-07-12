library(tidyverse)
#nokta grafiği
mtcars |> 
  ggplot(aes(wt,mpg))+
  geom_point(size=3,
             colour= "steelblue",
             alpha = 0.6)+
  labs(title = "Weight vs Miles per Gallon",
       x= "Weight",
       y="Miles per Gallon")+
  theme_bw()
#line grafiği
Orange |> 
  ggplot(aes(age, circumference,
             colour = Tree))+
  geom_line()+
  labs(title = "Circumference as a function of age",
       x = "Age",
       y="Circumference")+
  theme_linedraw()
#bar grafiği
mpg |> 
  ggplot(mapping = aes(class))+ #mapping y tanımlamadan bar chart yapmanı sağlar
  #çünkü y sadece sayı olucak
  geom_bar(fill="steelblue",
           alpha= .5)+
  labs(title = "Number of cars in each class",
       x=" ",
       y=" ")+
  theme_minimal()

#histogram
mpg |> 
  ggplot(aes(cty))+
  geom_histogram(binwidth = 2, #ÖNEMLİ KULLAN
                 fill = "red",
                 alpha = .5)+
  labs(title = "Fuel efficency of cars in the city",
       x = "Miles per gallon",
       y="Number of cars")+theme_bw()


#density

mpg |> 
  ggplot(aes(x = cty))+
  geom_density(fill = "green",
               alpha = .5)+
  labs(title = "Fuel efficenc of cars in the city",
       x="Miles per gallon",
       y="Number of cars")+theme_dark()

#bunu multiple kategorileri karsılastırmak için de yapabilirsin
#farklı kategorilerin dağılımlarını karşılaştır


mpg |> filter(drv %in% c("f","r")) |> 
  ggplot(aes(x = cty,
             fill = drv,
             color = drv))+
  geom_density(alpha = .5)+
  labs(title = "Fuel efficenc of cars in the city",
       x="Miles per gallon",
       y="Number of cars")+theme_dark()

#boxplot
mpg |> 
  filter(cty<25) |> 
  ggplot(aes(x = cty,
             fill = drv))+
  geom_boxplot(alpha = .5)+
  labs(title = "Fuel efficency of cars in the city",
       x = "Miles per gallon",
       fill=" Drive")+
  theme_light()

#area plots

ggplot(mpg, aes(x=displ))+ #just x axsisi tanımladı
  #y tanımlamadı çünkü plotlardan birinin diğerinin üstünde olmasını istiyo
  #ve ikisinin de y axsisde farklı bir ölçek kullanmasını istiyor
  #yani birinin y hwy değerleri dierinin ki cty değerleri
  geom_area(aes(y= hwy, fill = "Highway"))+ #iki area için de farklı estetik tanımladık
  geom_area(aes(y=cty, fill = "City"))+
  labs(title = "Highway vs City driving",
       x = "Engine Displacement (L)",
       y= "Miles per Gallon",
       fill = "")+
  theme_minimal()

#raster plot

#you can look at 3 variables at the same time

faithfuld |> 
  ggplot(aes(x = waiting,
             y = eruptions,
             fill = density))+
  geom_raster()+
  labs(title = "Old Faithful Geyser",
       x="Waiting time between erruptions",
       y="Duration of erruptions",
       fill = "Density")+
  theme_minimal()





























