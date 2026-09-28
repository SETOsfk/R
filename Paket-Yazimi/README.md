# Paket Yazımı — Sıfırdan Basit Bir R Paketi

`paket_yazimi_rehberi.R` dosyasını RStudio'da açıp satır satır (Ctrl+Enter) çalıştır. Her adımda
önce açıklama, sonra kod, sonra `#>` ile beklenen çıktı var.

Sonunda `ilkpaketim` adında, `library(ilkpaketim)` ile çağrılabilen bir paketin olur:

| Fonksiyon | Ne yapar |
|---|---|
| `merhaba()` | selam mesajı döndürür (ilk, en basit örnek) |
| `standart_hata()` | ortalamanın standart hatası |
| `guven_araligi()` | ortalama için t güven aralığı (`t.test()` ile aynı sonuç) |
| `sayisal_mi_kontrol()` | iç fonksiyon, kullanıcıya açık değil |

**Bölümler:** A) kavramlar · B) hazırlık (iskelet, DESCRIPTION) · C) fonksiyon yazma, `load_all()`,
`document()` · D) `test()`, `check()`, `install()` · E) sık hatalar, özet, alıştırma.
