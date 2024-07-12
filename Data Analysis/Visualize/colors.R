library(RColorBrewer)

display.brewer.all(colorblindFriendly = TRUE)
#orda gördüğün renk isimleri farklı renk setleri
#burdaki isimleri palette="" yazarak kullanabilirsin
mpg %>% 
  ggplot(aes(displ,hwy,color=drv))+
  geom_jitter(size=5)+
  scale_color_brewer(palette = "Set2")+
  theme_minimal()
#bunu yapmazsan R rastgele bir renk paleti atar.

#not: color=drv yerine fill kullansan
#scale_fill_brewer yazıcaktın

mpg %>% 
  ggplot(aes(displ,hwy,color=drv))+
  geom_jitter(size=5)+
  scale_color_manual(values = 
                       c("4"="darkblue",
                         "f"="darkred",
                         "r"="darkgreen"))+
  theme_minimal()
#burda renkleri biz ayarladık

