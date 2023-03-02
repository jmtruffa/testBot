
botPricesArg = function() {
  require(tidyverse)
  #require(methodsPPI)
  #require(writexl)
  #require(lubridate)
  #require(purrr)


  PPI = getPPILogin2()
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

    #### AY24 no está en PPI. lo baja de Rava (si no está) y los junta
    tickersBonosOld = c('AY24', 'AY24D', 'AY24C', 'T2V2')
    fileDirectory = '~/Downloads/temp/'

    ### Chequeo si los archivos están con la data vieja. Sino los bajo
    ### Luego esto habrá que ponerlo en la base de datos para hacerlo más rápido
    for (i in seq_along(tickersBonosOld)){
      if (!file.exists(paste0(fileDirectory, tickersBonosOld[i], '.csv'))) {
        download.file(paste('http://clasico.rava.com/empresas/precioshistoricos.php?e=',tickersBonosOld[i],'&csv=1', sep=''),
                      paste0(fileDirectory,tickersBonosOld[i], '.csv'), mode = 'wb')
      }
    }
    bonosOld = tibble(
      ticker = character(),
      fecha = lubridate::Date(),
      apertura = double(),
      maximo = double(),
      minimo = double(),
      cierre =double(),
      volumen = double(),
      openint = double()
    )

    for (i in seq_along(tickersBonosOld)){
      temp = read_csv(paste0(fileDirectory, tickersBonosOld[i], '.csv'))
      temp$ticker = tickersBonosOld[i]
      bonosOld = rbind(bonosOld, temp)
    }

    bonosOld = bonosOld %>%
      relocate(ticker, date = fecha, price = cierre, volume = volumen, openingPrice = apertura, max = maximo, min = minimo) %>%
      select(-openint) %>%
      filter(date >= from)


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


    ### pego los dos df
    prices = do.call("rbind", list(df, bonosOld))
    prices %>% writexl::write_xlsx('~/Downloads/temp/pricesArg.xlsx')


    return(fails)

    } else {
      stop("API Error. Try again later")
    }

}
