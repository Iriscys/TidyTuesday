# Big Tech Stock Prices

The data this week comes from Yahoo Finance via [Kaggle](https://www.kaggle.com/datasets/evangower/big-tech-stock-prices) (by [Evan Gower](https://github.com/rfordatascience/tidytuesday/issues/509)).

> This dataset consists of the daily stock prices and volume of 14 different tech companies, including Apple (AAPL), Amazon (AMZN), Alphabet (GOOGL), and Meta Platforms (META) and more!

A number of articles have examined the collapse of "Big Tech" stock prices, including [this article from morningstar.com](https://www.morningstar.com/articles/1129535/5-charts-on-big-tech-stocks-collapse).

Note: All `stock_symbol`s have 3271 prices, except META (2688) and TSLA (3148) because they were not publicly traded for part of the period examined.

### Data Dictionary

# `big_tech_stock_prices.csv`

| variable     | class     | description                                                                                                                                                                                                                       |
|:-------------|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| stock_symbol | character | stock_symbol                                                                                                                                                                                                                      |
| date         | double    | date                                                                                                                                                                                                                              |
| open         | double    | The price at market open.                                                                                                                                                                                                         |
| high         | double    | The highest price for that day.                                                                                                                                                                                                   |
| low          | double    | The lowest price for that day.                                                                                                                                                                                                    |
| close        | double    | The price at market close, adjusted for splits.                                                                                                                                                                                   |
| adj_close    | double    | The closing price after adjustments for all applicable splits and dividend distributions. Data is adjusted using appropriate split and dividend multipliers, adhering to Center for Research in Security Prices (CRSP) standards. |
| volume       | double    | The number of shares traded on that day.                                                                                                                                                                                          |

# `big_tech_companies.csv`

| variable     | class     | description               |
|:-------------|:----------|:--------------------------|
| stock_symbol | character | stock_symbol              |
| company      | character | Full name of the company. |

### Cleaning Script

``` r
library(fs)
library(tidyverse)
library(here)
library(janitor)

# Source for datasets. The datasets were downloaded and extracted to an
# "archive" folder within the working directory for processing, but they are not
# included in this repo.
"https://www.kaggle.com/datasets/evangower/big-tech-stock-prices"

# This is mostly equivalent to fs::dir_map, but we need to keep the info from
# the filename.
big_tech_stock_prices_list <- purrr::map(
  fs::dir_ls(
    here::here("data", "2023", "2023-02-07", "archive"),
    glob = "*.csv"
  ),
  \(path) {
    ticker <- fs::path_file(path) |> fs::path_ext_remove()
    readr::read_csv(
      file = path,
      col_types = cols(
        Date = col_date(format = ""),
        Open = col_double(),
        High = col_double(),
        Low = col_double(),
        Close = col_double(),
        `Adj Close` = col_double(),
        Volume = col_double()
      )
    ) |> 
      dplyr::mutate(stock_symbol = ticker, .before = 1)
  }
)

big_tech_stock_prices <- purrr::list_rbind(big_tech_stock_prices_list) |> 
  janitor::clean_names()
dplyr::glimpse(big_tech_stock_prices)

readr::write_csv(
  big_tech_stock_prices,
  here::here(
    "data", "2023", "2023-02-07",
    "big_tech_stock_prices.csv"
  )
)

big_tech_stock_prices |> 
  dplyr::count(stock_symbol, sort = TRUE)


# Make a lookup for the symbols.
tibble::tibble(
  stock_symbol = c(
    "AAPL",
    "ADBE",
    "AMZN",
    "CRM",
    "CSCO",
    "GOOGL",
    "IBM",
    "INTC",
    "META",
    "MSFT",
    "NFLX",
    "NVDA",
    "ORCL",
    "TSLA"
  ),
  company = c(
    "Apple Inc.",
    "Adobe Inc.",
    "Amazon.com, Inc.",
    "Salesforce, Inc.",
    "Cisco Systems, Inc.",
    "Alphabet Inc.",
    "International Business Machines Corporation",
    "Intel Corporation",
    "Meta Platforms, Inc.",
    "Microsoft Corporation",
    "Netflix, Inc.",
    "NVIDIA Corporation",
    "Oracle Corporation",
    "Tesla, Inc."
  )
) |> 
  readr::write_csv(
    here::here(
      "data", "2023", "2023-02-07",
      "big_tech_companies.csv"
    )
  )
```
