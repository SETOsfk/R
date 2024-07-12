babynames_fraction %>%
  # Arrange the data in order of name, then year 
  arrange(name, year) %>%
  # Group the data by name
  group_by(name) %>%
  # Add a ratio column that contains the ratio of fraction between each year 
  mutate(ratio = fraction / lag(fraction))
#LAG en basit haliyle elindeki vektörleri bir adım öteye atarak yeni bir vektör serisi oluşturu
#v=c(1,2,3) lag(v)=c(NA,1,2) böylelikle yıllar arası doğan bebek farkını, yıllar arası kullanılan
#isim farkını hesaplayabilirrsin BAYA İŞLEVSEL BİR KOD. c-lagv=1,1,1 1er olarak artıyor  sonraki sayı yüksek ihtimalle 4tür vb
babynames_ratios_filtered %>%
  # Extract the largest ratio from each name 
  slice_max(ratio,n=1)%>%
  # Sort the ratio column in descending order 
  arrange(desc(ratio)) %>%
  # Filter for fractions greater than or equal to 0.001
  filter(fraction>=0.001)