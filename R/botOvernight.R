botOvernight = function() {
  require(tidyverse)
  require(methodsPPI)
  require(writexl)
  require(tidyquant)
  require(zoo)
  require(scales)

  PPI = getPPILogin2()
  if (length(PPI) == 2) {
  from = '2019-01-01'
  to  = Sys.Date()
  settlement = 'INMEDIATA'

  tickers = c(
    'PESOS1',
    'PESOS2',
    'PESOS3',
    'PESOS4',
    'PESOS5',
    'PESOS6'
  )
  type = rep('CAUCIONES', length(tickers))

  cauciones = getPPIPriceHistoryMultiple(PPI$token,
                             ticker = tickers,
                             type = type,
                             from = from,
                             to = to,
                             settlement = settlement)

  serieCaucion = cauciones %>%
    group_by(date) %>%
    select(ticker, date, price) %>%
    pivot_wider(names_from = ticker, values_from = price) %>%
    mutate(
      tasa = case_when(
        !is.na(PESOS1) ~ PESOS1/100,
        !is.na(PESOS2) ~ PESOS2/100,
        !is.na(PESOS3) ~ PESOS3/100,
        !is.na(PESOS4) ~ PESOS4/100,
        !is.na(PESOS5) ~ PESOS5/100,
        !is.na(PESOS6) ~ PESOS6/100)
      ,
      dias =  case_when(
        !is.na(PESOS1) ~ 1,
        !is.na(PESOS2) ~ 2,
        !is.na(PESOS3) ~ 3,
        !is.na(PESOS4) ~ 4,
        !is.na(PESOS5) ~ 5,
        !is.na(PESOS6) ~ 6,
      )
    ) %>%
    arrange(date) %>%
    select(date, tasa, dias)

  firstDate = first(serieCaucion$date)

  serie = tibble(date = seq(firstDate, Sys.Date(), by = 'days'))

  markup1 = 0.03
  markup2 = 0.05
  serieCapitalizada = left_join(serie, serieCaucion) %>%
    mutate(
      ted = ( 1+ tasa / 365 * dias)^(1/dias) -1,
      ted2 = ( 1+ (tasa + markup1)  / 365 * dias)^(1/dias) -1,
      ted3 = ( 1+ (tasa + markup2)  / 365 * dias)^(1/dias) -1,
    ) %>%
    mutate(
      ted = na.locf(.$ted),
      ted2 = na.locf(.$ted2),
      ted3 = na.locf(.$ted3),
      tasa = na.locf(.$tasa),
      factor = ted + 1,
      factor2 = ted2 + 1,
      factor3 = ted3 + 1

    ) %>%
    mutate(
      cap = Reduce(
        function(a,b) (a * b), .$factor, accumulate = TRUE
        ),
      cap1 = Reduce(
        function(a,b) (a * b), .$factor2, accumulate = TRUE
      ),
      cap2 = Reduce(
        function(a,b) (a * b), .$factor3, accumulate = TRUE
      ),
    )

  serieCaucion %>% write_xlsx('~/Downloads/temp/SerieCaucion.xlsx')
  serieCapitalizada %>% write_xlsx('~/Downloads/temp/SerieCaucionCapitalizada.xlsx')
  } else {
    stop("API Error. Try again later")
  }
}





