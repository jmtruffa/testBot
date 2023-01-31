
botAnalisisTotalReturn2 = function() {
  require(tidyverse)
  require(methodsPPI)
  require(writexl)
  require(lubridate)


  PPI = getPPILogin2()
  if (length(PPI) == 2) {
  from = '2018-01-01'
  to  = Sys.Date()
  settlement = 'A-48HS'

  tickersStock = c(
    'ALUA', 'BYMA', 'CEPU', 'LOMA', 'PAMP', 'TGNO4', 'TGSU2', 'TXAR', 'YPFD', 'GGAL', 'BMA', 'SUPV', 'CAPX', 'MIRG'
  )
  typeStock = rep('ACCIONES', length(tickersStock))

  tickersBond = c(
    'GD30', 'GD30D', 'GD30C', 'GD35', 'GD38', 'GD41',
    'AL30', 'AE38', 'AL29',
    'TX23', 'T2X3', 'TX24', 'TX26', 'TX28',
    'TO23', 'TO26', 'TV23', 'TV24', 'BA37D', 'CO26',
    'DICP', 'CUAP', 'PARP'
  )
  typeBond = rep('BONOS', length(tickersBond))


  tickersCedear = c(
    'CAAP', 'GLOB', 'SPY','QQQ', 'VIST', 'MELI'
  )

  typeCedear = rep('CEDEARS', length(tickersCedear))

  fails = tibble(
    ticker = character()
  )

  #### AY24 no está en PPI. lo baja de Rava (si no está) y los junta
  tickersBonosOld = c('AY24', 'AY24D', 'AY24C', 'T2V2')
  fileDirectory = '~/Downloads/temp/'

  for (i in seq_along(tickersBonosOld)){
    if (!file.exists(paste0(fileDirectory, tickersBonosOld[i], '.csv'))) {
      download.file(paste('http://clasico.rava.com/empresas/precioshistoricos.php?e=',tickersBonosOld[i],'&csv=1', sep=''),
                    paste0(fileDirectory,tickersBonosOld[i], '.csv'), mode = 'wb')
    }
  }
  bonosOld = tibble(
    ticker = character(),
    fecha = Date(),
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


  stocks = getPPIPriceHistoryMultiple3(PPI$token,
                                       ticker = tickersStock,
                                       type = typeStock,
                                       from = from,
                                       to = to,
                                       settlement = settlement)
  if (length((stocks[[2]][1]$ticker)) != 0) {
    ## hubo errores
    fails = rbind(fails, stocks[[2]])

  }
  stocks = stocks[[1]] ## retenemos acá los resultados y en fails retuvimos los errores


  bonds = getPPIPriceHistoryMultiple3(PPI$token,
                                      ticker = tickersBond,
                                      type = typeBond,
                                      from = from,
                                      to = to,
                                      settlement = settlement)
  if (length((bonds[[2]][1]$ticker)) != 0) {
    ## hubo errores
    fails = rbind(fails, bonds[[2]])

  }

  bonds = bonds[[1]]

  cedears = getPPIPriceHistoryMultiple3(PPI$token,
                                        ticker = tickersCedear,
                                        type = typeCedear,
                                        from = from,
                                        to = to,
                                        settlement = settlement)
  if (length((cedears[[2]][1]$ticker)) != 0) {
    ## hubo errores
    fails = rbind(fails, cedears[[2]])

  }

  cedears = cedears[[1]]

  prices = do.call("rbind", list(stocks, bonds, cedears, bonosOld))
  prices %>% write_xlsx('~/Downloads/temp/prices.xlsx')


  return(fails)

  } else {
    stop("API Error. Try again later")
  }

}
