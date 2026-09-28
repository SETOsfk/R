# R for Everyone — worked examples in R

Short, runnable R scripts I wrote while learning and teaching statistics: from vectors and `dplyr` to the central
limit theorem, confidence intervals, permutation tests, power, SQL from R and Shiny. Each folder is independent —
open a script in RStudio and run it top to bottom. [Türkçe özet ↓](#türkçe-özet)

| Folder | What is inside |
|---|---|
| `Basic-R/` | first steps: loading data, vectors, sums, base graphics, a first `dplyr` pipeline |
| `TidyVerse/` | `dplyr` verbs, grouping and summarising, `ggplot2` basics and extensions |
| `Central-Limit-T/` | the central limit theorem by simulation, random variables and p-values (mouse-weight data) |
| `Confidence Interval/` | confidence intervals and exercises (`babies.txt`) |
| `Inference/` | permutation tests, power calculations, Monte Carlo simulation, association tests |
| `Data Analysis/` | a full analysis cycle: cleaning (`starwars`), exploration, t-test / ANOVA / χ², regression and a decision tree |
| `Visualize/` | `ggplot2` geometries, lollipop and encircled plots, `plotly`, `echarts4r`, `gganimate`, `gt` tables |
| `Harvard/` | `dplyr` and `ggplot2` exercises (storms → hurricanes), writing a package and unit tests with `testthat` |
| `Paket-Yazimi/` | step-by-step guide (Turkish) to writing a first R package with `usethis`, `devtools`, `roxygen2`, `testthat` |
| `SQL With R/` | creating and querying SQLite databases from R with `RSQLite`, `RODBC` and `RJDBC` (course labs) |
| `r-shiny/` | small Shiny apps: histogram, BMI calculator, iris species predictor |
| `Project/classification/` | the 2024 Bordeaux wine classification scripts (feature importance, near-zero variance, ROC) |
| `wine_app/` | the 2024 Shiny app built on those models |
| `Datasets/` | the data the scripts read (see below) |

The wine work has since been redone from scratch — leakage-checked validation, Python + R, cheese pairing and
Turkish equivalents: **[SETOsfk/wineapp](https://github.com/SETOsfk/wineapp)**.

## Run

```r
install.packages(c("tidyverse", "plotly", "gganimate", "gt", "echarts4r", "shiny", "RSQLite", "caret", "randomForest"))
```

Scripts use paths relative to their own folder; set the working directory to the script's folder first
(RStudio: *Session → Set Working Directory → To Source File Location*).

## Data

`Datasets/` holds teaching datasets from public course material and R packages (mouse phenotypes from the
HarvardX *genomicsclass* labs, LEGO and Stack Overflow tables used in DataCamp-style exercises, `babynames`,
UCI heart disease, an advertising example). They keep their original licences and are here only so the scripts run.

Code: MIT (see `LICENSE`). Author: [Sertan Şafak](https://setosfk.github.io/).

---

## Türkçe özet

İstatistik öğrenirken ve anlatırken yazdığım, baştan sona çalışan kısa R örnekleri: temel R ve `dplyr`'dan merkezi
limit teoremine, güven aralıklarına, permütasyon testlerine, güç analizine, R'dan SQL'e ve Shiny'ye kadar. Her
klasör bağımsız; betiği RStudio'da açıp yukarıdan aşağı çalıştırmak yeterli. Şarap projesinin 2024 sürümü
`Project/classification/` ve `wine_app/` altında; baştan yapılmış hâli **SETOsfk/wineapp** reposunda.
