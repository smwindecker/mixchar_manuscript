purrr::walk2(.x = c("rmarkdown", "knitr", "tinytex", "devtools"), .y = c("1.10", "1.20", "0.9", "2.0.1"), ~devtools::install_version(package = .x, version = .y))
devtools::install_github(c("smwindecker/mixchar@7dc30e0380e924e4bc595cc272609d1d4ada15e2", "yihui/xaringan@edbd4a7e3b223bd7689e8bd4ca36d97b8d66be34"))
