library(targets)
# source(here::here("data-raw/lp.R"))
# tar_option_set(
#   packages = c("tidyverse", "tidymodels"),
#   progress = TRUE)

lp_data <-
  list(
    tar_target(
      landprice_raw,
      seq.int(8, 14) |>
        purrr::map(
          function(x) {
            x <- sprintf("%02d", x)
            kuniumi::read_ksj_l02(
              here::here(glue::glue("data-raw/L02-20_{x}_GML/L02-20_{x}.geojson")),
              .year = 2020)
          }
        ) |>
        dplyr::bind_rows()
    ),
    tar_target(
      landprice,
      landprice_raw |>
        dplyr::filter(!`基準地行政区域コード` %in% c("13361", "13362", "13363", "13364", "13381", "13382", "13401", "13402", "13421", "13900")) |>
        dplyr::select(!c(tidyselect::contains("前年度基準地コード"),
                  "基準地コード_見出し番号",
                  "年度",
                  tidyselect::contains("前面道路"),
                  tidyselect::contains("側道"),
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
                  tidyselect::num_range("調査価格_", 1983:2020),
                  tidyselect::starts_with("属性移動"))) |>
        sf::st_transform(crs = 4326) |>
        purrr::set_names(c("id", "price", "administration",
                           "area", "status", "structure",
                           "water", "gas", "sewer",
                           "above_floor",
                           "under_floor",
                           "dist_from_st",
                           "use_type", "fire", "city_plan",
                           "geometry"))
    ),
    tar_target(
      lp_supply,
      landprice |>
        sf::st_drop_geometry() |>
        dplyr::select(price, water, gas, sewer,
                      above_floor, under_floor,
                      dist_from_st, fire) |>
        dplyr::mutate(gas = as.factor(gas)) |>
        dplyr::filter(!is.na(fire))
    )
  )

list(
  lp_data
)

# targets::tar_make()
