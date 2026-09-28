###############################################################################
#  SIFIRDAN BASİT BİR R PAKETİ YAZMAK — ADIM ADIM, UYGULAMALI REHBER
#
#  Bu betiği RStudio'da açıp adım adım (Ctrl+Enter) çalıştır.
#  Sonunda "ilkpaketim" adında, 3 fonksiyonu olan, dokümantasyonu ve
#  testleri hazır, kurulup library() ile çağrılabilen bir paketin olacak.
###############################################################################


# ---- ADIM 0: Paket nedir, neden yazılır? ------------------------------------
#
# Paket = bir klasör. İçinde belli kurallara göre yerleştirilmiş dosyalar var:
#
#   ilkpaketim/
#   ├── DESCRIPTION   -> paketin kimliği (adı, sürümü, yazarı, bağımlılıkları)
#   ├── NAMESPACE     -> dışarıya hangi fonksiyonlar açılacak (otomatik oluşur)
#   ├── R/            -> fonksiyonların kodu (.R dosyaları)
#   ├── man/          -> yardım sayfaları (?fonksiyon) (otomatik oluşur)
#   └── tests/        -> fonksiyonların doğru çalıştığını kontrol eden testler
#
# Neden? Sürekli kopyala-yapıştır yaptığın fonksiyonları tek yerde toplarsın,
# library(ilkpaketim) diyerek her projede kullanırsın, başkasıyla paylaşırsın.


# ---- ADIM 1: Gerekli yardımcı paketleri kur (bir kere yapılır) --------------
#
# usethis  -> paket iskeletini ve dosyaları oluşturur
# devtools -> paketi yükler, test eder, kontrol eder, kurar
# roxygen2 -> fonksiyonun üstüne yazdığın yorumlardan yardım sayfası üretir
# testthat -> test yazmak için

install.packages(c("usethis", "devtools", "roxygen2", "testthat"))


# ---- ADIM 2: Paket iskeletini oluştur ---------------------------------------
#
# Paketi nereye kuracağını seç. (Başka bir RStudio projesinin veya git
# reposunun İÇİNDE olmasın.) Tekrar baştan başlamak istersen önce klasörü sil:
#   unlink(paket_yolu, recursive = TRUE)

paket_yolu <- file.path(path.expand("~"), "ilkpaketim")

usethis::create_package(paket_yolu, open = FALSE)

# Bundan sonraki bütün komutlar bu klasörde çalışsın:
usethis::proj_set(paket_yolu)
setwd(paket_yolu)

# Ne oluştu, bakalım:
list.files()
#> "DESCRIPTION" "NAMESPACE" "R"   <- paketin iskeleti hazır


# ---- ADIM 3: DESCRIPTION dosyasını doldur -----------------------------------
#
# DESCRIPTION paketin kimlik kartıdır. Normalde RStudio'da açıp elle
# düzenlersin; burada kolaylık olsun diye kodla yazıyoruz.

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

# Lisans dosyasını da ekleyelim (paylaşacaksan gerekli):
usethis::use_mit_license("Adin Soyadin")


# ---- ADIM 4: İLK FONKSİYON — merhaba() --------------------------------------
#
# Fonksiyonlar R/ klasörüne .R dosyası olarak konur.
# Normalde: usethis::use_r("merhaba")  -> R/merhaba.R dosyasını açar, içine
# yazarsın. Burada yine kodla yazıyoruz.
#
# #' ile başlayan satırlar roxygen yorumlarıdır; yardım sayfası bunlardan
# üretilecek. En önemlisi @export: "bu fonksiyonu kullanıcı görebilsin" demek.
#
# Not: Paket kodundaki metinlerde (tırnak içinde) Türkçe karakter kullanma,
# kontrol aşamasında uyarı verir. Yorumlarda kullanabilirsin.

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

# Paketi kurmadan, geliştirirken denemek için load_all() kullanılır.
# Bu komut R/ klasöründeki her şeyi sanki paket kuruluymuş gibi yükler.
devtools::load_all()

merhaba("Sertan")
#> [1] "Merhaba Sertan! Ilk paketine hos geldin."

# Çalıştı! Geliştirme döngüsü hep böyledir:
#   kodu yaz/değiştir  ->  load_all()  ->  dene  ->  tekrar


# ---- ADIM 5: Yardım sayfasını üret — document() -----------------------------
#
# roxygen yorumlarından man/merhaba.Rd dosyasını ve NAMESPACE'i oluşturur.

devtools::document()

readLines("NAMESPACE")
#> "export(merhaba)"   <- @export yazdığımız için buraya geldi

?merhaba
# Kendi yazdığın yardım sayfası açıldı!


# ---- ADIM 6: İKİNCİ FONKSİYON — standart_hata() ------------------------------
#
# Biraz daha işe yarar bir şey: bir vektörün ortalamasının standart hatası.
#   SE = sd(x) / sqrt(n)
# Ayrıca kullanıcı hatalı veri verirse anlaşılır bir hata mesajı gösterelim.
#
# DİKKAT: sd() "stats" paketinden gelir. Paket içinde başka paketlerin
# fonksiyonlarını paket::fonksiyon() şeklinde yazmak en güvenli yoldur.

writeLines(r"(#' Ortalamanin standart hatasi
#'
#' `sd(x) / sqrt(n)` formuluyle hesaplar. Eksik degerler (NA) atilir.
#'
#' @param x Sayisal bir vektor.
#' @return Tek bir sayi: standart hata.
#' @examples
#' standart_hata(c(2, 4, 4, 5, 7, 9))
#' @export
standart_hata <- function(x) {
  if (!is.numeric(x)) {
    stop("x sayisal bir vektor olmali.")
  }
  x <- x[!is.na(x)]
  stats::sd(x) / sqrt(length(x))
}
)", "R/standart_hata.R")

# Başka bir paketi kullandık, bunu DESCRIPTION'a bildirmemiz lazım:
usethis::use_package("stats")
# DESCRIPTION'a "Imports: stats" satırı eklendi. Paketini kuran kişide
# gerekli paketler otomatik kurulsun diye bu önemli.

devtools::load_all()

standart_hata(c(2, 4, 4, 5, 7, 9))
#> [1] 1.013794

standart_hata(c(10, 12, NA, 14))   # NA'yı atıp hesaplıyor
#> [1] 1.154701

try(standart_hata(c("a", "b")))    # Bilerek hata verdiriyoruz
#> Error in standart_hata(c("a", "b")) : x sayisal bir vektor olmali.


# ---- ADIM 7: ÜÇÜNCÜ FONKSİYON — guven_araligi() ------------------------------
#
# Paketin içindeki fonksiyonlar birbirini kullanabilir.
# guven_araligi(), az önce yazdığımız standart_hata()'yı çağırıyor.

writeLines(r"(#' Ortalama icin t guven araligi
#'
#' @param x Sayisal bir vektor.
#' @param guven Guven duzeyi, varsayilan 0.95.
#' @return `alt` ve `ust` adli iki elemanli bir vektor.
#' @examples
#' guven_araligi(c(2, 4, 4, 5, 7, 9))
#' guven_araligi(c(2, 4, 4, 5, 7, 9), guven = 0.99)
#' @export
guven_araligi <- function(x, guven = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  ortalama <- mean(x)
  t_degeri <- stats::qt((1 + guven) / 2, df = n - 1)
  pay <- t_degeri * standart_hata(x)
  c(alt = ortalama - pay, ust = ortalama + pay)
}
)", "R/guven_araligi.R")

devtools::document()   # yeni fonksiyonun yardım sayfası + NAMESPACE güncellensin
devtools::load_all()

guven_araligi(c(2, 4, 4, 5, 7, 9))
#>      alt      ust
#> 2.560627 7.772706

# R'ın kendi t.test() sonucuyla karşılaştıralım, aynı mı?
t.test(c(2, 4, 4, 5, 7, 9))$conf.int
#> [1] 2.560627 7.772706   <- Evet, aynı!


# ---- ADIM 8: TEST YAZ — fonksiyonlar doğru mu çalışıyor? ---------------------
#
# Test = "bu girdiyi verince şu çıktıyı bekliyorum" diye yazılmış kontroller.
# İleride kodu değiştirdiğinde bir şeyi bozup bozmadığını anında görürsün.

usethis::use_testthat()   # tests/ klasörünü kurar

# Normalde: usethis::use_test("standart_hata") dosyayı açar. Biz kodla yazıyoruz.
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
)", "tests/testthat/test-guven_araligi.R")

devtools::test()
#> [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ]   <- hepsi geçti


# ---- ADIM 9: Paketin genel kontrolü — check() --------------------------------
#
# CRAN'ın kullandığı kontrolün aynısı: dokümantasyon eksik mi, örnekler
# çalışıyor mu, testler geçiyor mu, DESCRIPTION doğru mu...

devtools::check()
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
#
# Hedef hep bu satırı görmek. Hata/uyarı çıkarsa mesaj ne yapman
# gerektiğini genelde açıkça söyler.


# ---- ADIM 10: Paketi kur ve normal bir paket gibi kullan ---------------------

devtools::install()

# load_all() ile yüklenen geliştirme sürümünü kaldır, gerçek kurulumu deneyelim.
# (RStudio'da en temizi: Session > Restart R, sonra aşağıdan devam.)
devtools::unload("ilkpaketim")

library(ilkpaketim)

merhaba("Dunya")
#> [1] "Merhaba Dunya! Ilk paketine hos geldin."

standart_hata(mtcars$mpg)
#> [1] 1.065424

guven_araligi(mtcars$mpg)
#>      alt      ust
#> 17.91768 22.26357

# Paketin içinde neler var?
ls("package:ilkpaketim")
#> [1] "guven_araligi" "merhaba"       "standart_hata"

# Yardım sayfaları da kurulu:
?guven_araligi
help(package = "ilkpaketim")

# Artık herhangi bir projede, herhangi bir R oturumunda
# library(ilkpaketim) yazman yeterli.


# ---- ÖZET: Paket yazmanın 6 komutu ------------------------------------------
#
#   usethis::create_package("yol")   # 1. iskeleti kur
#   usethis::use_r("fonksiyon")      # 2. R/ altına fonksiyon yaz (+ roxygen yorumları)
#   devtools::load_all()             # 3. dene
#   devtools::document()             # 4. yardım sayfası + NAMESPACE
#   devtools::test()                 # 5. testleri çalıştır
#   devtools::check()                # 6. genel kontrol
#   devtools::install()              #    ve kur -> library(paketin)
#
# BONUS — GitHub'a koyarsan başkaları şöyle kurar:
#   install.packages("remotes")
#   remotes::install_github("kullanici_adin/ilkpaketim")
