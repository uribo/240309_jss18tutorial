library(tidyverse)
library(kuniumi)
library(sf)
if (length(list.files(here::here("data-raw"), recursive = TRUE, pattern = ".geojson$")) != 7L) {
  seq.int(8, 14) |>
    purrr::walk(
      function(x) {
        x <- sprintf("%02d", x)
        download.file(
          url = kuniumi:::zip_l02_url(year = 2020, pref_code = x),
          destfile = here::here(glue::glue("data-raw/L02-20_{x}_GML.zip"))
        )
        unzip(
          zipfile = here::here(glue::glue("data-raw/L02-20_{x}_GML.zip")),
          exdir = here::here(glue::glue("data-raw/L02-20_{x}_GML"))
        )
        }
    )
}

landprice_raw <-
  seq.int(8, 14) |>
  purrr::map(
    function(x) {
      x <- sprintf("%02d", x)
      read_ksj_l02(here::here(glue::glue("data-raw/L02-20_{x}_GML/L02-20_{x}.geojson")),
                   .year = 2020)
    }
  ) |>
  bind_rows()

landprice <-
  landprice_raw |>
  filter(!`基準地行政区域コード` %in% c("13361", "13362", "13363", "13364", "13381", "13382", "13401", "13402", "13421", "13900")) |>
  select(!c(contains("前年度基準地コード"),
            "基準地コード_見出し番号",
            "年度",
            contains("前面道路"),
            contains("側道"),
            "選定年次ビット",
            "利用現況",
            "基準地行政区域コード",
            "住居表示",
            "共通地点",
            "間口比率",
            "奥行比率",
            "形状",
            "公園区分",
            "森林区分",
            "駅名",
            "周辺の土地利用の状況",
            "建ぺい率",
            "容積率",
            num_range("調査価格_", 1983:2020), starts_with("属性移動")))

# glimpse(landprice)

# c("基準地コード_一連番号", "調査価格", "基準地市区町村名称",
#   "地積", "利用状況表示", "建物構造", "供給施設有無_水道",
#   "供給施設有無_ガス", "供給施設有無_下水",
#   "地上階層", "地下階層", "駅からの距離", "用途区分",
#   "防火区分", "都市計画区分")

# landprice |>
#   count(use_type)

landprice <-
  landprice |>
  purrr::set_names(c("id", "price", "administration",
                     "area", "status", "structure",
                     "water", "gas", "sewer",
                     "above_floor",
                     "under_floor",
                     "dist_from_st",
                     "use_type", "fire", "city_plan",
                     "geometry"))

# 供用状況
lp_supply <-
  landprice |>
  st_drop_geometry() |>
  select(price, water, gas, sewer, above_floor, under_floor, dist_from_st, fire) |>
  mutate(gas = as.factor(gas)) |>
  filter(!is.na(fire))
