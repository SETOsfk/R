library(tidyverse)
#What is cleaning your data?
#Variable types: Bütün verilerin doğru tipte olduğuna emin ol yani kategorik bir veri
#character olarak vb mi girilmiş.

#Select and filter: Çalışmak istediğin verileri belirle.
#Bazen çok büyük verisetleriyle analiz yapabilirsin ve bu ayrımı yapabilmek
#Anahtar haline gelebilir.

#Find and deal with missing data and duplicates: 
 
#Öncelikle verinin doğru tipte kategorize edildiğinden emin ol
#Select-Filter 
#Kayıp verileri ve duplicateleri  bul çöz
#Verileri recode et.

#Variable types
glimpse(starwars)
#Baktığımızda faktör(kategorik) olmadığını görüyoruz bazen işimize yarabilir bu 
#mesela small-mid-big


class(starwars$gender)
unique(starwars$gender)
#genderı kategorik haline getirelim
starwars$gender=as.factor(starwars$gender)
#farklı bir isimde verebilirdik ancak elimizdeki kolonun yerine 
#koymak için yeni kolon oluşturmamak için böyle yaptık..



class(starwars$gender)
levels(starwars$gender)#Levelları değiştirmek istiyoruz.
starwars$gender=factor((starwars$gender),
                       levels=c("masculine","feminine"))
#Select variables
names(starwars)
starwars%>%
  select(name,height,ends_with("color"))%>%
  names()#name,height ve color ile biten şeyleri seç



#Filter obverstaions
unique(starwars$hair_color)#bu baya işlevsel bir koddu 
#hair colordaki bütün şeyleri gösterir
#none, na, kızıl, kahverengi vb

starwars%>%select(name,height,ends_with("color"))%>%
  filter(hair_color %in% c("blond","brown")& height <180)#aboyu 180in altında olan ve 
#blond OR brown içeren saç renklerini filtrele
#İkisinden biri de olur ikisi birden olmak zorunda değil
#Ki olamaz da nasıl olsun.

#Missing data
mean(starwars$height, na.rm=TRUE) #Na removalamadan yaparsan hesaplayamaz.
starwars%>%
  select(name,gender,hair_color,height)
starwars%>%
  select(name,gender,hair_color,height)%>%
  na.omit()

#Çok da önerilmez de na leri böyle removalerasın. 
#neyi kaybettiğin bilmiyorsun belki önemli olabilir
#Şimdi na lerin nerde olduğnu bulalıom

starwars%>% 
  select(name,gender,hair_color,height)%>%
  filter(!complete.cases(.))
#Complete cases sana EKSİĞİ OLMAYAN verileri getirir
# !complete cases-IN COMPLETE- ise ESKİĞİ OLAN verileri getirir.


#Mesela hair colora baktığında na girilen karakterlerden bazılarının robot 
#olduğunu görüyoprsun aslında NA değil robotların saçı zaten yok 
#Veri setindeki hair_color için none diye bir ifade var saçı olmayanları kastediyor
#na i onunla değiştirebilirsin.


starwars%>% select(name,gender,hair_color,height)%>%
  filter(!complete.cases(.))%>%
  drop_na(height)#ama mesela boyu NA olanı bu şekilde bulamazsın o yüzden
#onu removalyabilirsin.
starwars%>% select(name,gender,hair_color,height)%>%
  filter(!complete.cases(.))%>%
  mutate(hair_color=replace_na(hair_color,"none"))
#Burda bir şeyi yanlış yaptık 5 tane hair color na di bunların 3ü robottu diğer ikisi
#direk na di onları removelayabilirdik KÖTÜ TUTORIAL........


#DUPLICATES
names=c("Peter","John","Andrew","Peter")
age=c(22,33,44,22)
friends=data.frame(names,age)
friends[!duplicated(friends),]#Duplicate olmayanları göster dersen duplicate 
#olanları kickler
friends%>%distinct()
#Recode
starwars%>%
  select(name,gender)%>%
  mutate(gender=recode(gender,"masculine"=1,"feminine"=2))




















