#VERİ SETİNDE kategorik verinin ismi x ise onu grafikte y olarak değiştirme 

pacman::p_load(car,
               tidyverse)
library(car)
library(tidyverse)

#set theme
theme_set(theme_bw()) #temayı önceden belirledik

#quick look at the data
head(Salaries)

#quick plot salary vs yrs since PHD
#color by rank

Salaries %>% 
  ggplot(aes(x=yrs.since.phd,
             y=salary,
             color=rank))+
  geom_point()


#make shape= discipline
#change geom_point to jitter
#add linear model by sex
#facet by sex
#add axis and legend labels

Salaries %>% 
  ggplot(aes(x= yrs.since.phd, y= salary))+ #eğer color=rank ı buraya koyarsan
  #her renk için farklı line çizmeye çalışıcak method=lm için
  #o yüzden renkleri sadece jitterlara özel yaptık
  geom_jitter(aes(color= rank, shape = discipline))+
  geom_smooth(method = lm)+
  facet_wrap(~sex)+
  labs(title = "Salary vs years since PHD",
       x= "Years since PhD",
       y= "Income",
       color = "Position",
       shape = "Research area")

#lets learn how to change tic marks and tic marks labels
#boxplot

Salaries %>% 
  filter(salary<150000) %>% 
  ggplot(aes( x= rank, y= salary, fill = sex))+
  geom_boxplot(alpha=0.5)+
  labs(title = "Faculty Salary by Rank and Gender",
       x="",
       y="",
       fill = "Gender") #burda direkt veri setinde dğeer neyse o olarak yazıyo
#biz farklı name isteyebiliriz. 

Salaries %>% 
  filter(salary<150000) %>% 
  ggplot(aes( x= rank, y= salary, fill = sex))+
  geom_boxplot(alpha=0.5)+#Kutuyu biraz daha transparan yapıyo
  scale_x_discrete(breaks=c("AsstProf","AssocProf",
                            "Prof"),
                   labels=c("Assistant\nProfessor",
                            "Associate\nProfessor",
                            "Full\nProfessor"))+
  scale_y_continuous(breaks = c(5000,10000,
                                150000,200000),
                     labels = c("$50K","$100K",
                                "$150K","$200K"))+
  labs(title = "Faculty Salary by Rank and Gender",
       x="",
       y="",
       fill = "Gender")+#legend burda dışarda ancak aşağıdaki kodu ekleyince içerde
  # + yı silince anlarsın
  theme(legend.position = c(.11,.78))

























