#Manipulate your data

#Rename a variable:
#Reorder variable
#Change a variable type
#Select variables to work with
#Filter and arrange data
#Recode data
#Change data(mutate)
#Conditions changes(if_else)
#Reshape data frame wide to long, long to wide.


library(tidyverse)
glimpse(msleep)
View(msleep)




#Rename a variable
msleep%>%
  rename("conserv"="conservation")



#Reorder
msleep%>%
  select(vore,name,everything()) 
#name ve genusu başa al gerisi eskis gibi kalsın


#variable type değiştirme 
class(msleep$vore)
msleep$vore<-as.factor(msleep$vore)

#Aynı işe yararlar yine karektere gdöndürdük.
msleep %>% 
  mutate(vore=as.character(vore)) %>% 
  glimpse()


#Select a variable to work with
names(msleep)
msleep%>%select(2:4,
                awake,starts_with("sleep"),
                contains("wt"))%>%names()
#2-3-4 numaraları satırları al
#awake,sleep ile başlayan, ve wt içeren kolonları se.

#Filter and arrange
unique(msleep$order)#ordaki bütün unique verileri gösterir.

msleep%>%
  filter((order=="Carnivora" | order=="Primates") &#order kolonu carnivora YA DA primates olanı VE 
              sleep_total>8)%>%#sleep-total kolonu 8 den büyük olanı seç
  select(name,order,sleep_total)%>%#Name,order,sleep totalı seç
  arrange(-sleep_total)%>%View#sleep totalı azalarak sırala. ve göster


#Biraz farklısı, basiti
msleep %>%
  filter(order %in% c("Carnivora","Primates") & 
         sleep_total>8)%>%
  select(name,order,sleep_total)%>% 
arrange(order)%>% #Karakter olduğu için alfabeye göre sıralar.
  View



#Change data (MUTATE)
msleep%>%
  mutate(brainwt=brainwt*1000)%>%#Üzerine yazabilirsin
  View

msleep%>%
  mutate(brainwt_in_grams=brainwt*1000)%>% #Ya da yeni kolon oluşturabilirsin.
  View()


#CONDITIONAL CHANGES(İF_ELSE)
#logical vector based on a condition #işine yarar bu#######
msleep$brainwt
msleep$brainwt > 0.01 #Brain weighti 0.01den büyük olanları göster
size_of_brain <- msleep%>%
  select(name,brainwt)%>%
  drop_na(brainwt)%>% #naleri at.
  mutate(brain_size=if_else(brainwt>0.01,"large","small"))
#eğer brain weight 0.01den büyükse LARGE de değilse SMALL de
size_of_brain

#Recode data rename a variable
size_of_brain%>%
  mutate(brain_size=recode(brain_size,
                           "large"=1, #largelara 1 de
                           "small"=2)) #smallara 2 de
#Bunu yaptıktan sonra bir class kontrol yap
#Numeriğe vb dönüştüyse kategoriğe dönüştür onu.

#Reshape data from wide to long or long to wide
#Baktıüında 10 tane afganistandan sonra albaniayı falan görüyon
#onu değiştiricez
library(gapminder)
data=select(gapminder,country,year,lifeExp)

wide_data=data%>%
  pivot_wider(names_from = year,values_from = lifeExp)%>%View

long_data=wide_data%>%
  pivot_longer(2:13,
               names_to = "year", 
               values_to = "lifeExp")
 












