library(tidyverse)
candidate<- c("mario","peach","bowser")
votes<- c(100,200,150)
votes<-data.frame(candidate,votes)

first_graph <-ggplot(votes, aes(x=candidate,y=votes))+
  geom_col(
    aes(fill= candidate),
    show.legend = FALSE)+ #bir layer ekledik, sütunları içeren
  scale_fill_viridis_d("Candidate")+ #Renk körleri için özel renkler
  #içerdeki Candidate renk kısmının ismini değiştirmeye yarar.
scale_y_continuous(limits = c(0,250))+
  labs(
    x="Candidates",
    y="Votes",
    title="Election Results"
  ) +
  theme_minimal() #farklı temalar ekler, havalı şeyler var

ggsave(
  "votes.png",
  plot = first_graph,
  width = 1200,
  height = 900,
  units = "px"
  ) #grafiği pcne kaydetmeye yarar.



                   
                   