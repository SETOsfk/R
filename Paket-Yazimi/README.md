# Paket Yazımı — Sıfırdan Basit Bir R Paketi

`paket_yazimi_rehberi.R` dosyasını RStudio'da açıp adım adım çalıştır. Sonunda `ilkpaketim` adında,
üç fonksiyonu (`merhaba()`, `standart_hata()`, `guven_araligi()`), yardım sayfaları ve testleri olan,
`library(ilkpaketim)` ile çağrılabilen bir paketin olur.

Adımlar: iskelet (`usethis`) → DESCRIPTION → fonksiyon yaz → `load_all()` ile dene → `document()` →
`test()` → `check()` → `install()`.
