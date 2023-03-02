botDividendsUS = function() {

  require(tidyverse)
  from = '2018-01-01'
  to  = Sys.Date() + 1 ## yahoo parece que es hasta exclusive. Entonces le sumo un dìa

  tickers = purrr::map_dfr(
    "pricesUS",
    methodsPPI::sets
  )

  dividends = tidyquant::tq_get(
    tickers$ticker,
    get = "dividends",
    from = from,
    to = to
  )

  dividends = dividends %>% select(ticker = symbol, date, value)

  dividends %>% writexl::write_xlsx('~/Downloads/temp/dividendsUS.xlsx')
}
