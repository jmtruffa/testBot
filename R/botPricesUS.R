botPricesUS = function() {

  require(tidyverse)
  from = '2018-01-01'
  to  = Sys.Date() + 1 ## yahoo parece que es hasta exclusive. Entonces le sumo un dìa

  tickers = purrr::map_dfr(
    "pricesUS",
    methodsPPI::sets
  )

  prices = tidyquant::tq_get(
    tickers$ticker,
    get = "stock.prices",
    from = from,
    to = to
  )

  prices = prices %>% select(ticker = symbol, date, price = adjusted, volume, openingPrice = open, max = high, min = low)

  prices %>% writexl::write_xlsx('~/Downloads/temp/pricesUS.xlsx')
}
