## ----setup----------------------------------------------------------------------------------------------------------------------------------------------------
library(gh)
library(ggplot2)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
org <- "epiverse-trace"


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
repos <- gh::gh(
  "/orgs/{org}/repos",
  org = org,
  per_page = 100
) |>
  purrr::discard(\(x) x$private) |>
  purrr::map_chr("full_name")

## -------------------------------------------------------------------------------------------------------------------------------------------------------------
latest_referrals <- repos |>
  purrr::map(function(repo) {
    c(
      package = basename(repo),
      gh::gh("/repos/{repo}/traffic/popular/referrers", repo = repo)
    )
  })


## ----load-history-referrals-----------------------------------------------------------------------------------------------------------------------------------
if (file.exists(here::here("inst", "extdata", "traffic", "referrals.json"))) {
  history_referrals <- jsonlite::read_json(
    here::here("inst", "extdata", "traffic", "referrals.json")
  )
} else {
  history_referrals <- NULL
}


## ----merge-latest-history-{{ visit }}-------------------------------------------------------------------------------------------------------------------------
all_referrals <- c(
  setNames(list(latest_referrals), format(Sys.Date())),
  history_referrals
)


## ----save-date-{{ visit }}------------------------------------------------------------------------------------------------------------------------------------
jsonlite::write_json(
  all_referrals,
  here::here("inst", "extdata", "traffic", "referrals.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)

