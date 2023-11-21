
botPricesArg = function() {
  require(tidyverse)
  #require(methodsPPI)
  #require(writexl)
  #require(lubridate)
  #require(purrr)


  PPI = methodsPPI::getPPILogin2()
  if (length(PPI) == 2) {
    from = '2018-01-01'
    to  = Sys.Date()
    settlement = 'A-48HS'

    ### traigo todos los tickers que pertenecen al proceso totalReturn
    ### usando la función methodsPPI::sets
    tickers = map_dfr(
      "pricesArg",
      methodsPPI::sets
    )

    ### Creo el df de fails para devolver los que fallaron
    fails = tibble(
      ticker = character()
    )

    ### la función getPPIPriceHistoryMultiple3 ya funciona vectorizada
    ### por lo que puedo hacerle una llamada directamente con todos dentro,
    ### y allí hago una pegada por cada una.
    ### la que no está vectorizada es la de PPI.

    ### uso la función para bajarme todos los precios de los tickers de una sola vez
    df = methodsPPI::getPPIPriceHistoryMultiple3(PPI$token,
                                   ticker = tickers$ticker,
                                   type = tickers$type,
                                   from = from,
                                   to = to,
                                   settlement = settlement)

    if (length((df[[2]][1]$ticker)) != 0) {
      ## hubo errores
      fails = rbind(fails, df[[2]])

    }
    df = df[[1]]

    ### en fails me quedan los tickets que fallaron. Los buscaré en la base
    con = DBI::dbConnect(RSQLite::SQLite(), '/home/juant/data/historicalData.sqlite3')
    bonosOld = dplyr::as_tibble(tbl(con, "prices")) %>%
      filter(ticker %in% fails$ticker)
    bonosOld$date = as.Date(as.POSIXct.Date(bonosOld$date, origin = "1970-01-01"))
    DBI::dbDisconnect(con)

    bonosOld = bonosOld %>%
         relocate(ticker, date, price = close, volume = volume, openingPrice = openingPrice, max = max, min = min) %>%
         filter(date >= from)

    ### Los que la API no devolvió aún están en fails
    obtenidos = bonosOld %>% distinct(ticker)
    fails = fails %>% filter(!ticker  %in% obtenidos$ticker)

    ### pego los dos df
    prices = do.call("rbind", list(df, bonosOld))
    prices %>% writexl::write_xlsx('~/Downloads/temp/pricesArg.xlsx')



    return(fails)

    } else {
      stop("API Error. Try again later")
    }

}
