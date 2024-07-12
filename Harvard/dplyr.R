library(tidyverse)
storms
#storms data setinde en yüksek rüzgar derecesine sahip kasırgayı seç
hurricanes<-storms |> 
  select(!c(lat,long,pressure, ends_with("diameter"))) |> 
  filter(status=="hurricane")|> 
  arrange(desc(wind),name) |>
  distinct(name, year, .keep_all = TRUE)

#oluşturduğun bu subseti csv olaak kaydet.

hurricanes |> 
  select(c(year,name,wind))|>
  write.csv("hurricanes.csv", row.names = FALSE)

hurricanes<- read.csv("hurricanes.csv")

#her grup(yıl) için en yüksek rüzgar değerini ver sonra grubu dağıt
hurricanes |> 
  group_by(year) |>
  slice_max(order_by = wind) |>
  ungroup()

#her grupta(yıl) kaç değer var
hurricanes |>
  group_by(year)|>
  summarize(hurricanes=n())





