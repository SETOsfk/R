#kategorik ve nümerik veri için

library(tidyverse)
library(forcats)
library(dplyr)

msleep

theme_set(theme_bw()+
            theme(panel.grid = element_blank()))
#temayı önceden yazdık bu şekilde bütün grafiklere gidicek bu tema

?geom_segment #nedşr

msleep %>% 
  group_by(order) %>% 
  summarise(mean_sleep=mean(sleep_total)) %>% 
  mutate(order=fct_reorder(order,mean_sleep)) %>%  #her seyin orderlanmasını istedik
  ggplot(aes(x=order,y=mean_sleep))+
  geom_point(size=3,
             colour="orange")+
  geom_segment(aes(x=order,
                   y=mean(msleep$sleep_total),
                   xend=order,
                   yend = mean_sleep
                   ))+#line çiziyoruz, #line çizerken R a 4 şey söylemek gerekiyor,x,y starting point,end point
  geom_hline(yintercept = mean(msleep$sleep_total), #x e paralel bir line çizicez 
             #nerden başlayacağını belirttik, rengini ve sizeını belirttik
             colour="grey",
             size=1)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Average sleep time of mammals by order",
       x="",
       y="Hours")
 