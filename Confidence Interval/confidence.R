controlpopulation=filter(dat, Sex=='F'&Diet=='chow')%>%select(Bodyweight)%>%unlist
hfpopulation=filter(dat, Sex=='F'&Diet=='hf')%>%select(Bodyweight)%>%unlist
mu_hf=mean(hfpop)
mu_con=mean(cpop)
print(mu_hf-mu_con)
#Tüm popülasyonu dahil ettiğimzi bu listeye bakarsak
#Alternatif hipotezi kabul ederiz.
#Ancak her zaman tüm popülasyona erişemeyiz.
set.seed(1)
N=5
hf=sample(hfpopulation,N)
control=sample(controlpopulation,N)
t.test(hf,control)$p.value
#Değer 0.05 ten küçük olmadığı için alternatif hipotezi red ederiz.
#Ama burda tip 2 hatayı yapmış oluruz çünkü popülasyona baktığımızda
#Alternatif hipotezin doğru olduğunu biliyoruz.
#Örneklem sayısı düşük olduğu için yanılgıya düştük.
set.seed(1)
N=12
hf=sample(hfpopulation,N)
control=sample(controlpopulation,N)
t.test(hf,cn)$p.value
set.seed(1)
N=30
hf=sample(hfpopulation,N)
control=sample(controlpopulation,N)
t.test(hf,control)$p.value

