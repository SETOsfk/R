###############################################################################
#
#   SIFIRDAN BASİT BİR R PAKETİ YAZMAK — ADIM ADIM, UYGULAMALI REHBER
#
#   NASIL KULLANILIR?
#   - Bu dosyayı RStudio'da aç.
#   - İmleci bir satıra getir, Ctrl+Enter (Mac: Cmd+Enter) ile o satırı çalıştır.
#   - Her adımda önce açıklamayı oku, sonra kodu çalıştır, sonra "#>" ile
#     başlayan satırlardaki beklenen çıktıyla kendi ekranını karşılaştır.
#
#   SONUNDA NE OLACAK?
#   "ilkpaketim" adında, kendi yazdığın bir paket. İçinde 3 fonksiyon,
#   yardım sayfaları (?fonksiyon) ve testler olacak. Tıpkı dplyr veya ggplot2
#   gibi library(ilkpaketim) yazarak kullanabileceksin.
#
#   İÇİNDEKİLER
#   BÖLÜM A — Kavramlar ........................ Adım 0
#   BÖLÜM B — Hazırlık ......................... Adım 1-3
#   BÖLÜM C — Fonksiyon yazma .................. Adım 4-8
#   BÖLÜM D — Test, kontrol, kurulum ........... Adım 9-11
#   BÖLÜM E — Sık hatalar, özet, alıştırma
#
###############################################################################



###############################################################################
#  BÖLÜM A — KAVRAMLAR
###############################################################################

# ---- ADIM 0: Paket nedir? Neden yazarız? -------------------------------------
#
# BENZETME:
#   Fonksiyon bir alettir (tornavida, çekiç...).
#   Paket bu aletleri koyduğun bir alet çantasıdır.
#   Çantayı bir kere hazırlarsın, sonra nereye gidersen yanında götürürsün.
#
# PAKETSİZ HAYAT:
#   Bir fonksiyon yazdın, çok beğendin. Yeni bir projede de lazım oldu.
#   Eski dosyayı bulup kopyala-yapıştır yapıyorsun. Sonra bir hata buldun,
#   düzelttin... ama diğer 5 projedeki kopyalar hâlâ hatalı. Karmaşa.
#
# PAKETLİ HAYAT:
#   Fonksiyon tek bir yerde (pakette) durur. Her projede library(paketim)
#   dersin. Hatayı pakette bir kere düzeltirsin, her yerde düzelmiş olur.
#   Üstelik yardım sayfası ve testi de yanında gelir.
#
# ÇOK KARIŞTIRILAN İKİ KAVRAM:
#   install.packages("dplyr")  -> KURMAK : paketi bilgisayara indirir.
#                                  Bir kere yapılır.
#   library(dplyr)             -> YÜKLEMEK: kurulu paketi o anki R oturumuna
#                                  açar. Her yeni oturumda yapılır.
#   Kendi paketimizde de aynısı olacak: önce kuracağız, sonra library().
#
# BİR PAKET ASLINDA SADECE BELLİ KURALLARA UYAN BİR KLASÖRDÜR:
#
#   ilkpaketim/
#   ├── DESCRIPTION  -> Kimlik kartı: adı, sürümü, yazarı, neye ihtiyaç duyduğu
#   ├── NAMESPACE    -> Hangi fonksiyonlar kullanıcıya açık? (otomatik oluşur)
#   ├── R/           -> Fonksiyonların kodu burada (.R dosyaları)
#   ├── man/         -> Yardım sayfaları (otomatik oluşur, elle dokunulmaz)
#   └── tests/       -> Fonksiyonlar doğru çalışıyor mu diye kontrol eden kodlar
#
# Sen sadece R/ ve tests/ içine kod yazacaksın, DESCRIPTION'ı dolduracaksın.
# Gerisini yardımcı paketler senin yerine üretecek.



###############################################################################
#  BÖLÜM B — HAZIRLIK
###############################################################################

# ---- ADIM 1: Yardımcı paketleri kur (hayatında bir kere yapman yeterli) -----
#
# Paket yazmayı kolaylaştıran 4 yardımcı paket var:
#
#   usethis  -> "Bana şu dosyayı oluştur" işlerini yapar (iskelet, lisans...)
#   devtools -> Paketi yükler, test eder, kontrol eder, kurar
#   roxygen2 -> Fonksiyonun üstüne yazdığın özel yorumlardan yardım sayfası üretir
#   testthat -> Test yazmak için
#
# Zaten kuruluysa bu satırı atlayabilirsin.

install.packages(c("usethis", "devtools", "roxygen2", "testthat"))

# Küçük bir yardımcı: bir dosyanın içini ekrana yazdırır.
# Rehber boyunca oluşan dosyaların içine bakmak için kullanacağız.
dosyayi_goster <- function(yol) cat(readLines(yol), sep = "\n")


# ---- ADIM 2: Paketin iskeletini oluştur -------------------------------------
#
# NE YAPIYORUZ?  Boş bir paket klasörü oluşturuyoruz.
# NEREYE?        Ana klasörüne (Belgeler / home), "ilkpaketim" adıyla.
#                İstersen yolu değiştir. Tek kural: başka bir RStudio projesinin
#                veya git reposunun İÇİNDE olmasın.
#
# PAKET ADI KURALLARI: sadece harf, rakam ve nokta. Harfle başlar.
#   ilkpaketim  -> olur
#   ilk_paketim -> OLMAZ (alt çizgi yasak)
#   ilk paketim -> OLMAZ (boşluk yasak)
#
# Rehberi baştan çalıştırmak istersen önce eski klasörü sil:
#   unlink(paket_yolu, recursive = TRUE)

paket_yolu <- file.path(path.expand("~"), "ilkpaketim")
paket_yolu   # klasörün tam adresi, nerede olduğunu gör

usethis::create_package(paket_yolu, open = FALSE)

# open = FALSE: RStudio yeni bir pencere açmasın, bu rehberde kalalım.
# Bundan sonraki bütün komutlar bu klasörün içinde çalışsın:
usethis::proj_set(paket_yolu)
setwd(paket_yolu)

# NE GÖRMELİSİN?
list.files()
#> "DESCRIPTION" "NAMESPACE" "R"
#
# (RStudio'da ayrıca "ilkpaketim.Rproj" ve gizli dosyalar da olabilir, normal.)
# R/ klasörü şu an boş. Fonksiyonları birazdan içine koyacağız.


# ---- ADIM 3: DESCRIPTION dosyasını doldur (paketin kimlik kartı) ------------
#
# create_package() içine "buraya şunu yaz" diyen şablon yazılar koydu:

dosyayi_goster("DESCRIPTION")
#> Package: ilkpaketim
#> Title: What the Package Does (One Line, Title Case)
#> ...
#
# Bunları kendi bilgilerimizle değiştireceğiz. Normalde dosyayı RStudio'da
# açıp elle düzenlersin: file.edit("DESCRIPTION")
# Burada herkes aynı sonucu alsın diye kodla yazıyoruz.
#
# r"( ... )" yazımı: içindeki metni olduğu gibi (tırnaklarıyla birlikte)
# almamızı sağlar. writeLines() de bu metni dosyaya yazar.

writeLines(r"(Package: ilkpaketim
Title: Basit Istatistik Yardimcilari
Version: 0.1.0
Authors@R: person("Adin", "Soyadin", email = "sen@ornek.com", role = c("aut", "cre"))
Description: Ilk R paketimi yazmayi ogrenirken hazirladigim kucuk fonksiyonlar:
    selamlama, standart hata ve guven araligi hesaplama.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.1
)", "DESCRIPTION")

# HER SATIR NE ANLAMA GELİYOR?
#   Package     -> Paketin adı. Klasör adıyla aynı olmalı.
#   Title       -> Tek satırlık başlık.
#   Version     -> Sürüm. Her değişiklikte artırırsın: 0.1.0 -> 0.1.1 -> 0.2.0
#   Authors@R   -> Yazar. "aut" = yazar, "cre" = sorumlu kişi (maintainer).
#   Description -> Paket ne işe yarar, bir paragraf. Alt satıra geçerken
#                  4 boşlukla girinti yapılır.
#   License     -> Başkaları kodunu nasıl kullanabilir? (aşağıda ekliyoruz)
#   Encoding    -> Karakter kodlaması, hep UTF-8 kalsın.
#   Roxygen...  -> roxygen2'nin ayarları, dokunma.

# Lisans dosyasını ekle. MIT = "herkes kullanabilir, adımı ansın" demek.
usethis::use_mit_license("Adin Soyadin")
#> ✔ Writing 'LICENSE'
#> ✔ Writing 'LICENSE.md'



###############################################################################
#  BÖLÜM C — FONKSİYON YAZMA
###############################################################################

# ---- ADIM 4: İlk fonksiyon — merhaba() --------------------------------------
#
# NE YAPIYORUZ?  R/ klasörüne merhaba.R diye bir dosya koyuyoruz.
#
# Normal hayatta: usethis::use_r("merhaba") yazarsın, R/merhaba.R dosyası
# oluşup RStudio'da açılır, içine yazarsın. Burada kodla yazıyoruz.
#
# Fonksiyonun ÜSTÜNDEKİ #' ile başlayan satırlar sıradan yorum değil,
# "roxygen yorumu". Yardım sayfası bunlardan otomatik üretilecek:
#
#   #' Kullaniciyi selamla        -> 1. satır: yardım sayfasının BAŞLIĞI
#   #'                            -> boş satır: bölüm ayırıcı
#   #' Verilen isme bir ...       -> AÇIKLAMA paragrafı
#   #' @param isim ...            -> "isim" argümanı ne demek
#   #' @return ...                -> fonksiyon ne döndürüyor
#   #' @examples ...              -> yardım sayfasındaki örnek kod
#   #' @export                    -> ÇOK ÖNEMLİ: "kullanıcı bu fonksiyonu
#                                    görebilsin". Yazmazsan library() sonrası
#                                    fonksiyon görünmez!
#
# UYARI: Paket kodunda tırnak içindeki metinlerde Türkçe karakter (ş, ı, ğ...)
# kullanma, kontrol aşamasında uyarı verir. Yorumlarda sorun yok.

writeLines(r"(#' Kullaniciyi selamla
#'
#' Verilen isme bir selam mesaji dondurur.
#'
#' @param isim Selamlanacak kisinin adi (karakter).
#' @return Bir karakter dizisi.
#' @examples
#' merhaba("Ayse")
#' @export
merhaba <- function(isim) {
  paste0("Merhaba ", isim, "! Ilk paketine hos geldin.")
}
)", "R/merhaba.R")

# Dosya gerçekten oluştu mu?
list.files("R")
#> "merhaba.R"

# ŞİMDİ DENEYELİM. Ama paket henüz kurulu değil, nasıl deneyeceğiz?
# Cevap: load_all(). R/ klasöründeki her şeyi, paket kuruluymuş gibi
# oturuma yükler. Geliştirirken hep bunu kullanırız.
# (RStudio kısayolu: Ctrl+Shift+L — paket projesi açıkken çalışır)

devtools::load_all()
#> ℹ Loading ilkpaketim

merhaba("Sertan")
#> [1] "Merhaba Sertan! Ilk paketine hos geldin."

# Çalıştı! Kendi adını yazıp dene.
#
# GELİŞTİRME DÖNGÜSÜ — paket yazarken hep bu döngüde dönersin:
#
#     ┌──> kodu yaz / değiştir
#     │           │
#     │           ▼
#     │     load_all()
#     │           │
#     │           ▼
#     └──── dene, beğenmediysen tekrar


# ---- ADIM 5: Yardım sayfasını üret — document() -----------------------------
#
# NE YAPIYORUZ?  #' yorumlarını okutup iki şey ürettiriyoruz:
#   1. man/merhaba.Rd  -> ?merhaba yazınca açılan yardım sayfası
#   2. NAMESPACE       -> @export yazdığın fonksiyonların listesi
# (RStudio kısayolu: Ctrl+Shift+D)

devtools::document()
#> Writing 'NAMESPACE'
#> Writing 'merhaba.Rd'

# NAMESPACE'e bakalım:
dosyayi_goster("NAMESPACE")
#> # Generated by roxygen2: do not edit by hand
#>
#> export(merhaba)        <- @export yazdığımız için eklendi
#
# "do not edit by hand" = elle düzenleme. Hep document() ile güncellenir.

# man/ klasöründe yardım dosyası oluştu:
list.files("man")
#> "merhaba.Rd"

# Kendi yardım sayfanı aç! (RStudio'da sağ alttaki Help panelinde görünür)
?merhaba


# ---- ADIM 6: İç (gizli) yardımcı fonksiyon — sayisal_mi_kontrol() ----------
#
# Bazen bir fonksiyon sadece paketin kendi içinde işe yarar, kullanıcının
# görmesine gerek yoktur. Mesela "girdi sayısal mı?" kontrolü.
# Birazdan yazacağımız iki fonksiyon da bu kontrolü kullanacak.
# Aynı kontrolü iki kere yazmak yerine bir kere yazıp ikisinden çağıracağız.
#
# FARK: Bu fonksiyonda @export YOK ve @noRd var.
#   @export yok -> kullanıcı library() sonrası bu fonksiyonu göremez
#   @noRd       -> yardım sayfası da üretilmesin (iç fonksiyon, gerek yok)

writeLines(r"(#' Girdinin sayisal oldugunu kontrol et (ic fonksiyon)
#'
#' @param x Kontrol edilecek nesne.
#' @noRd
sayisal_mi_kontrol <- function(x) {
  if (!is.numeric(x)) {
    stop("x sayisal bir vektor olmali. Sen su tipte verdin: ", class(x)[1])
  }
  invisible(TRUE)
}
)", "R/yardimcilar.R")

# invisible(TRUE): sorun yoksa sessizce devam et, ekrana bir şey yazma.


# ---- ADIM 7: İkinci fonksiyon — standart_hata() -----------------------------
#
# NE İŞE YARAR?  Bir örneklemin ortalamasının standart hatasını hesaplar.
#   Formül: SE = sd(x) / sqrt(n)
#   (sd = standart sapma, n = gözlem sayısı)
#
# İÇİNDE NE VAR?
#   1. Önce sayisal_mi_kontrol() ile girdiyi kontrol ediyor.
#   2. NA (eksik) değerleri atıyor.
#   3. Formülü uyguluyor.
#
# DİKKAT: sd() fonksiyonu R'ın "stats" paketinden gelir. Paket içinde başka
# bir paketin fonksiyonunu kullanırken paket::fonksiyon() şeklinde yazarız:
# stats::sd(). Böylece R, sd'nin nereden geldiğini kesin bilir.

writeLines(r"(#' Ortalamanin standart hatasi
#'
#' `sd(x) / sqrt(n)` formuluyle hesaplar. Eksik degerler (NA) atilir.
#'
#' @param x Sayisal bir vektor.
#' @return Tek bir sayi: standart hata.
#' @examples
#' standart_hata(c(2, 4, 4, 5, 7, 9))
#' standart_hata(mtcars$mpg)
#' @export
standart_hata <- function(x) {
  sayisal_mi_kontrol(x)
  x <- x[!is.na(x)]
  stats::sd(x) / sqrt(length(x))
}
)", "R/standart_hata.R")

# Başka bir paket (stats) kullandık. Bunu DESCRIPTION'a bildirmemiz gerekiyor
# ki paketimizi kuran kişide o paket de hazır olsun:
usethis::use_package("stats")
#> ✔ Adding 'stats' to Imports field in DESCRIPTION

# DESCRIPTION'ın sonuna bak, "Imports: stats" eklendi:
dosyayi_goster("DESCRIPTION")

# Yeni kodu yükle ve dene (geliştirme döngüsü!):
devtools::load_all()

standart_hata(c(2, 4, 4, 5, 7, 9))
#> [1] 1.013794

standart_hata(c(10, 12, NA, 14))   # NA'yı atıp kalan 3 sayıyla hesapladı
#> [1] 1.154701

# Bilerek yanlış girdi verelim, anlaşılır bir hata mesajı almalıyız.
# (try() sayesinde hata olsa da betik durmaz.)
try(standart_hata(c("a", "b")))
#> Error in sayisal_mi_kontrol(x) :
#>   x sayisal bir vektor olmali. Sen su tipte verdin: character


# ---- ADIM 8: Üçüncü fonksiyon — guven_araligi() -----------------------------
#
# NE İŞE YARAR?  Ortalama için t dağılımına dayalı güven aralığı hesaplar.
#   Formül: ortalama ± t * SE
#
# GÜZEL KISMI: Paketin içindeki fonksiyonlar birbirini kullanabilir.
#   guven_araligi() -> sayisal_mi_kontrol()'ü ve standart_hata()'yı çağırıyor.
#   Standart hatayı tekrar hesaplamaya gerek yok, zaten yazdık!
#
# ARGÜMANLAR:
#   x     -> veri
#   guven -> güven düzeyi. "= 0.95" varsayılan değer demek; kullanıcı
#            bir şey yazmazsa 0.95 kullanılır.

writeLines(r"(#' Ortalama icin t guven araligi
#'
#' Ortalamanin etrafinda `ortalama +/- t * SE` araligini hesaplar.
#'
#' @param x Sayisal bir vektor.
#' @param guven Guven duzeyi, 0 ile 1 arasinda. Varsayilan 0.95.
#' @return `alt` ve `ust` adli iki elemanli bir vektor.
#' @examples
#' guven_araligi(c(2, 4, 4, 5, 7, 9))
#' guven_araligi(c(2, 4, 4, 5, 7, 9), guven = 0.99)
#' @export
guven_araligi <- function(x, guven = 0.95) {
  sayisal_mi_kontrol(x)
  x <- x[!is.na(x)]
  n <- length(x)
  ortalama <- mean(x)
  t_degeri <- stats::qt((1 + guven) / 2, df = n - 1)
  pay <- t_degeri * standart_hata(x)
  c(alt = ortalama - pay, ust = ortalama + pay)
}
)", "R/guven_araligi.R")

# Yeni fonksiyon ekledik -> hem yardım sayfası hem NAMESPACE güncellensin,
# sonra yükle:
devtools::document()
devtools::load_all()

# NAMESPACE'te artık 3 fonksiyon olmalı. İç fonksiyon ise OLMAMALI:
dosyayi_goster("NAMESPACE")
#> export(guven_araligi)
#> export(merhaba)
#> export(standart_hata)
#                          <- sayisal_mi_kontrol burada yok, çünkü @export yok

# Deneyelim:
guven_araligi(c(2, 4, 4, 5, 7, 9))
#>      alt      ust
#> 2.560627 7.772706

guven_araligi(c(2, 4, 4, 5, 7, 9), guven = 0.99)   # %99 -> aralık genişler
#>      alt      ust
#> 1.078905 9.254428

# Doğru mu hesapladık? R'ın kendi t.test() fonksiyonuyla karşılaştıralım:
t.test(c(2, 4, 4, 5, 7, 9))$conf.int
#> [1] 2.560627 7.772706   <- Aynı sonuç, fonksiyonumuz doğru!

# Paketin şu anki hali:
list.files(recursive = TRUE)
#> "DESCRIPTION"  "LICENSE"  "LICENSE.md"  "NAMESPACE"
#> "man/guven_araligi.Rd"  "man/merhaba.Rd"  "man/standart_hata.Rd"
#> "R/guven_araligi.R"  "R/merhaba.R"  "R/standart_hata.R"  "R/yardimcilar.R"
#
# Dikkat: R/ altında 4 dosya var ama man/ altında 3 yardım sayfası.
# yardimcilar.R'deki iç fonksiyona @noRd dediğimiz için sayfası yok.



###############################################################################
#  BÖLÜM D — TEST, KONTROL, KURULUM
###############################################################################

# ---- ADIM 9: Test yaz — fonksiyonlar doğru mu çalışıyor? --------------------
#
# TEST NEDİR?  "Bu girdiyi verirsem şu çıktıyı beklerim" diye yazılmış
# küçük kontrollerdir. Yukarıda t.test() ile elle karşılaştırdık ya;
# test, o karşılaştırmayı kalıcı hale getirmektir.
#
# NEDEN?  6 ay sonra fonksiyonu değiştirdin. Bir şeyi bozdun mu?
# test() yazarsın, 2 saniyede cevabı alırsın.
#
# EN ÇOK KULLANILAN 3 KONTROL:
#   expect_equal(a, b)     -> a ile b eşit mi?
#   expect_error(kod)      -> bu kod hata veriyor mu? (vermesi gerekiyorsa)
#   expect_true(koşul)     -> koşul doğru mu?

# tests/ klasörünü ve gerekli ayarları kur:
usethis::use_testthat()
#> ✔ Adding 'testthat' to Suggests field in DESCRIPTION
#> ✔ Creating 'tests/testthat/'
#> ✔ Writing 'tests/testthat.R'

# Test dosyaları tests/testthat/ içinde, adı "test-" ile başlar.
# Normal hayatta: usethis::use_test("standart_hata") dosyayı oluşturup açar.
# 1 / sqrt(3) nereden geldi?  c(1, 2, 3) için sd = 1, n = 3 -> SE = 1/sqrt(3)

writeLines(r"(test_that("standart_hata dogru hesapliyor", {
  expect_equal(standart_hata(c(1, 2, 3)), 1 / sqrt(3))
})

test_that("standart_hata NA degerleri atiyor", {
  expect_equal(standart_hata(c(1, 2, 3, NA)), 1 / sqrt(3))
})

test_that("standart_hata sayisal olmayan veride hata veriyor", {
  expect_error(standart_hata("a"), "sayisal")
})
)", "tests/testthat/test-standart_hata.R")

writeLines(r"(test_that("guven_araligi t.test ile ayni sonucu veriyor", {
  x <- c(2, 4, 4, 5, 7, 9)
  beklenen <- as.numeric(t.test(x)$conf.int)
  expect_equal(unname(guven_araligi(x)), beklenen)
})

test_that("alt sinir ust sinirdan kucuk", {
  sonuc <- guven_araligi(c(10, 12, 14, 16))
  expect_true(sonuc["alt"] < sonuc["ust"])
})

test_that("guven_araligi sayisal olmayan veride hata veriyor", {
  expect_error(guven_araligi(c("a", "b")), "sayisal")
})
)", "tests/testthat/test-guven_araligi.R")

# Bütün testleri çalıştır (RStudio kısayolu: Ctrl+Shift+T):
devtools::test()
#> ✔ | 3 | guven_araligi
#> ✔ | 3 | standart_hata
#> [ FAIL 0 | WARN 0 | SKIP 0 | PASS 6 ]
#
# FAIL 0 = hiçbir test başarısız olmadı. PASS 6 = 6 kontrol geçti.
#
# MERAK ET: R/standart_hata.R dosyasında "sqrt(length(x))" yerine
# "length(x)" yazıp kaydet, test()'i tekrar çalıştır. FAIL göreceksin,
# testin hatayı yakaladığını görürsün. Sonra düzeltmeyi unutma!


# ---- ADIM 10: Genel kontrol — check() ----------------------------------------
#
# NE YAPAR?  Paketin "sağlık muayenesi". CRAN'ın paketleri kabul ederken
# kullandığı kontrolün aynısı. Onlarca şeye bakar:
#   - Her @export'lu fonksiyonun yardım sayfası var mı?
#   - Her argüman @param ile açıklanmış mı?
#   - @examples içindeki kodlar hatasız çalışıyor mu?
#   - Testler geçiyor mu?
#   - DESCRIPTION doğru doldurulmuş mu?
# Biraz uzun sürer (yarım dakika kadar). RStudio kısayolu: Ctrl+Shift+E

devtools::check()
#> ── R CMD check results ──────────────── ilkpaketim 0.1.0 ────
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
#
# HEDEF HEP BU SATIR.
#   error   -> mutlaka düzelt, paket bozuk
#   warning -> düzelt, ciddi bir sorun var
#   note    -> genelde küçük şeyler, yine de bak
# Mesajlar ne yapman gerektiğini çoğu zaman açıkça söyler.


# ---- ADIM 11: Paketi kur ve gerçek bir paket gibi kullan ---------------------
#
# Şimdiye kadar load_all() ile "geçici" yükledik. Artık gerçekten kuruyoruz.
# Kurduktan sonra paket, dplyr gibi, bilgisayarındaki diğer paketlerin
# yanında durur. (RStudio kısayolu: Ctrl+Shift+B)

devtools::install()
#> * DONE (ilkpaketim)

# load_all() ile yüklenen geçici sürümü kapatalım ki gerçek kurulumu
# denediğimizden emin olalım.
# (RStudio'da daha temiz yol: Session > Restart R, sonra buradan devam et.)
devtools::unload("ilkpaketim")

# İŞTE AN! Kendi paketini library() ile çağırıyorsun:
library(ilkpaketim)

merhaba("Dunya")
#> [1] "Merhaba Dunya! Ilk paketine hos geldin."

# Gerçek bir veriyle: mtcars'taki arabaların yakıt verimi (mpg)
standart_hata(mtcars$mpg)
#> [1] 1.065424

guven_araligi(mtcars$mpg)
#>      alt      ust
#> 17.91768 22.26357
#
# Yorum: Arabaların ortalama mpg'si %95 güvenle 17.9 ile 22.3 arasında.

# Paketin içinde kullanıcıya açık neler var?
ls("package:ilkpaketim")
#> [1] "guven_araligi" "merhaba"       "standart_hata"
#
# sayisal_mi_kontrol listede YOK. Çünkü @export yazmadık, o bir iç fonksiyon.

try(sayisal_mi_kontrol(5))
#> Error in sayisal_mi_kontrol(5) : could not find function "sayisal_mi_kontrol"
#
# (Merak edersen üç iki nokta ile yine de ulaşabilirsin:
#  ilkpaketim:::sayisal_mi_kontrol(5) — ama normal kullanıcı bunu yapmaz.)

# library() yazmadan tek seferlik kullanım: paket::fonksiyon
ilkpaketim::merhaba("Ayse")
#> [1] "Merhaba Ayse! Ilk paketine hos geldin."

# Yardım sayfaları da kurulu:
?guven_araligi
help(package = "ilkpaketim")   # paketin tüm yardım sayfalarının listesi

# TEBRİKLER! Artık herhangi bir projede, herhangi bir R oturumunda
# library(ilkpaketim) yazman yeterli.



###############################################################################
#  BÖLÜM E — SIK HATALAR, ÖZET, ALIŞTIRMA
###############################################################################

# ---- Sık karşılaşılan hatalar ve çözümleri ----------------------------------
#
# HATA: could not find function "fonksiyonum"
#   -> Kodu değiştirdikten sonra load_all() yapmayı unuttun.
#   -> Ya da library() sonrası görünmüyorsa: @export yazmayı veya
#      document() çalıştırmayı unuttun, ardından install() tekrar.
#
# UYARI: no visible global function definition for 'sd'
#   -> Başka paketin fonksiyonunu stats::sd() gibi yazmadın.
#      Düzelt, usethis::use_package("stats") ile de DESCRIPTION'a ekle.
#
# UYARI: Found the following file with non-ASCII characters
#   -> Kodda tırnak içinde ş, ı, ğ, ü, ö, ç kullandın. ASCII harflere çevir.
#
# UYARI: Undocumented arguments in documentation object
#   -> Bir argüman için @param yazmayı unuttun. Ekle, document() yap.
#
# HATA: Invalid package name
#   -> Paket adında alt çizgi, boşluk veya Türkçe karakter var.
#
# Değişiklik yaptım ama ?yardim sayfası eski gösteriyor:
#   -> document() çalıştır. Kurulu paketteyse install() de yap.


# ---- ÖZET: Paket yazmanın komutları ------------------------------------------
#
#  BİR KERE:
#   usethis::create_package("yol")   # iskeleti kur
#   usethis::use_mit_license("Ad")   # lisans ekle
#   usethis::use_testthat()          # test altyapısını kur
#
#  HER YENİ FONKSİYON İÇİN:
#   usethis::use_r("fonksiyon")      # R/ altına dosya aç, kodu + #' yorumları yaz
#   usethis::use_test("fonksiyon")   # test dosyası aç, testleri yaz
#   usethis::use_package("paket")    # başka paket kullandıysan bildir
#
#  SÜREKLİ (döngü):                  RStudio kısayolu
#   devtools::load_all()             Ctrl+Shift+L   dene
#   devtools::document()             Ctrl+Shift+D   yardım + NAMESPACE
#   devtools::test()                 Ctrl+Shift+T   testler
#   devtools::check()                Ctrl+Shift+E   genel kontrol
#   devtools::install()              Ctrl+Shift+B   kur -> library(paketin)
#
#  PAYLAŞMAK İÇİN (bonus):
#   Paket klasörünü GitHub'a yükle. Başkaları şöyle kurar:
#     install.packages("remotes")
#     remotes::install_github("kullanici_adin/ilkpaketim")


# ---- ALIŞTIRMA: Kendin bir fonksiyon ekle -----------------------------------
#
# GÖREV: Pakete degisim_katsayisi() fonksiyonunu ekle.
#   Formül: (sd(x) / mean(x)) * 100    -> yüzde olarak değişkenlik
#
# ADIMLAR:
#   1. setwd(paket_yolu)                         # paket klasöründe ol
#   2. usethis::use_r("degisim_katsayisi")       # dosyayı aç
#   3. Fonksiyonu #' yorumlarıyla yaz (başlık, @param, @return, @examples, @export)
#      İpucu: sayisal_mi_kontrol() ve stats::sd() kullan.
#   4. devtools::document() ve devtools::load_all()
#   5. degisim_katsayisi(mtcars$mpg) dene  -> yaklaşık 29.99 çıkmalı
#   6. usethis::use_test("degisim_katsayisi") ile bir test yaz
#   7. devtools::test(), devtools::check(), devtools::install()
#   8. Sürümü artır: DESCRIPTION'da Version: 0.2.0 yap
#
# ÇÖZÜM (önce kendin dene!):
#
# #' Degisim katsayisi
# #'
# #' Standart sapmanin ortalamaya orani, yuzde olarak.
# #'
# #' @param x Sayisal bir vektor.
# #' @return Tek bir sayi (yuzde).
# #' @examples
# #' degisim_katsayisi(mtcars$mpg)
# #' @export
# degisim_katsayisi <- function(x) {
#   sayisal_mi_kontrol(x)
#   x <- x[!is.na(x)]
#   stats::sd(x) / mean(x) * 100
# }
