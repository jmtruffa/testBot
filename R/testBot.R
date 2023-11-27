# Set the library path
#paths=c("/home/juant/R/x86_64-pc-linux-gnu-library/4.2/testBot", "/home/juant/R/x86_64-pc-linux-gnu-library/4.2", "/usr/local/lib/R/site-library","/usr/lib/R/site-library","/usr/lib/R/library")

#.libPaths(c(.libPaths(), paths))
library(telegram.bot)
library(methodsPPI)
library(purrr)
library(writexl)
library(tidyverse)
library(testBot)
library(logr)
library(functions)
library(DBI)
library(RSQLite)
library(tidyquant)
library(xtable)
library(tableHTML)

tmpGraphPath = '"~/Downloads/temp/"'
tmp = file.path('~/Downloads/temp/', "test.log")

lf = log_open(tmp)
bot = Bot(token = bot_token("jmtTestBot"))
#print(bot$getMe())

updates = bot$getUpdates()

# chat_id = "CHAT_ID" # you can retrieve it from bot$getUpdates() after sending a message to the bot
# bot$sendMessage(chat_id = chat_id, text = "TestReply")


updater = Updater(token = Sys.getenv('R_TELEGRAM_BOT_jmtTestBot'))


start = function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Hola %s!, mis comandos por el momento son: \n

/dolar Te devuelve el valor del CCL, MEP y Canje via GD30
/dolarBook Devuelve CCL, MEP y Canje calculado con las puntas. Para T0 y T2
/dolarGraph yyyy-mm-dd (tene en cuenta el formato de la fecha) Te devuelve igual que /dolar pero un gráfico. Por default desde hace 30 días
/caucion Te devuelve dos archivos (excel) con la Caución Overnight (1 día o lo que haya disponible) y la otra capitalizada
/ARGprices Devuelve la serie de precios de tickers de Arg para la construcción de un Total Return.
/USdividends.dividends Devuelve la serie de dividendos de las acciones de /pricesUS
/USprices Devuelve la serie. Idem pero con activos de US.
/currentRofex Devuelve la curva de tasas implícitas de Rofex vs el último operado Spot CAM1 de MAE
/cclComp Devuelve una lista comparativa del CCL a través de GD30 y ADRs. Todo T+2 precio LAST
/TC Tipos de Cambio
", update$message$from$first_name))
  log_print("Start: ")
  log_print(update$effective_user())
}

updater = updater + CommandHandler("start", start)

CCLComp = function(bot, update){
  bot$send_message(chat_id = update$message$chat_id, text = "Me pongo a trabajar en ello...")
  error = FALSE
  tryCatch(
    {
      con = DBI::dbConnect(RSQLite::SQLite(), '~/data/test.sqlite3')
      token = getPPILogin2()$token
    },
    error = function(e) {error <<- TRUE; print("Falló el connect o el Login");
    bot$send_message(chat_id = update$message$chat_id,
                     text = "Fallo en DB o Login.") }

  )


  if (!error) {
    tickers = as_tibble(DBI::dbReadTable(con, "ratiosADR"))
    dbDisconnect(con)

    fails = tibble(
      ticker = character()
    )

    resultLocal = getPPIPrice2(token = token, ticker = tickers$tickerLocal, type = tickers$typeLocal, settlement = "A-48HS")
    fails = rbind(fails, resultLocal[[2]])
    resultLocal = resultLocal[[1]]
    resultExterior = getPPIPrice2(token = token, ticker = tickers$tickerExterior, type = tickers$typeExterior, settlement = "A-48HS")
    fails = rbind(fails, resultExterior[[2]])
    resultExterior = resultExterior[[1]]

    GD30 = getPPIPrice2(token = token, ticker = "GD30", type = "BONOS", settlement = "A-48HS")
    fails = rbind(fails, GD30[[2]])
    GD30 = GD30[[1]]
    GD30C = getPPIPrice2(token = token, ticker = "GD30C", type = "BONOS", settlement = "A-48HS")
    fails = rbind(fails, GD30C[[2]])
    GD30C = GD30C[[1]]
    temp=cbind(GD30, GD30C)
    colnames(temp) = c("tickerLocal", "date.x", "price.x", "tickerExterior", "date.y", "price.y")
    temp = temp %>% mutate(
      TCImplicito = price.x / price.y
    ) %>%
      filter(date.x == date.y) %>%
      relocate(tickerLocal, tickerExterior, price.x, price.y, TCImplicito) %>%
      select(-date.x, -date.y)

    full = left_join(tickers, resultLocal, by = c('tickerLocal' = 'ticker')) %>%
      left_join(resultExterior, by = c("tickerExterior" = "ticker")) %>%
      drop_na() %>%
      mutate(
        TCImplicito = (price.x * qLocal) / (price.y * qExterior)
      ) %>%
      relocate(tickerLocal, tickerExterior, price.x, price.y, TCImplicito) %>%
      filter(date.x == date.y) %>%
      select(-typeLocal, -typeExterior, -qLocal, -qExterior, -date.x, -date.y) %>%
      rbind(temp) %>%
      arrange(TCImplicito) %>%
      as.data.frame() %>%
      mutate(
        diff = num((TCImplicito / FIRST(TCImplicito) - 1) * 100, digits = 2),
        TCImplicito = num(TCImplicito, digits = 2)
      ) %>%
      rename(precioLocal = price.x, precioExterior = price.y)
    out = update$effective_user()
    out$call = "Comparativos CCL"
    log_print(out)

    print(full)
    outHTML = xtable(full)


    n = sample.int(10000, 1)
    file = paste0("~/Downloads/temp/cclComp",n,".html")
    print(outHTML, type='html', file = file )

    bot$send_document(chat_id =update$message$chat_id,
                      document = file)
    file.remove(file)

    #bot$sendMessage(chat_id = update$message$chat_id,
    #                text = outHTML,
    #                parse_mode = 'html')
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Los datos son LAST T+2.")
    if (length(fails$ticker) != 0) {
      bot$sendMessage(chat_id = update$message$chat_id,
                      text = paste0("Los tickers que fallaron son: ", toString(paste((fails$ticker)))))
    }


  } else {
    print(error)
  }

}

updater = updater + CommandHandler("cclComp", CCLComp)

# TC = function(bot, update) {
#   getPPIPuntas = function(ticker, type, token,settlement = "INMEDIATA") {
#     ### devuelve vacío cuando un ticker falla. Normalmente puede fallar porque consulto un ticker viejo
#     require(tidyverse)
#     require(jsonlite)
#     require(httr2)
#     require(lubridate)
#
#     # print(ticker)
#     # print(type)
#     # print(token)
#
#     ##Esto es para probar la funcion de manera directa
#     # token = PPI$token
#     # settlement = "INMEDIATA"
#     # ticker ="S16D2"
#     # type = "LETRAS"
#
#     url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
#     urlMarketData = 'MarketData/Book'
#
#     fail = tibble(
#       ticker = character()
#     )
#     responseBody = tibble(
#       date = Date(),
#       bids = numeric(),
#       offers = numeric(),
#       settlement = character()
#
#     )
#     error = FALSE
#     tryCatch(
#       {
#         rBook = request(paste0(url, urlMarketData)) %>%
#           req_headers(Authorization = token,
#                       AuthorizedClient = 'API-CLI',
#                       ClientKey = 'pp19CliApp12',
#                       `User-Agent` = "http://github.com/jmtruffa"
#           ) %>%
#           req_method("GET") %>%
#           req_url_query(Ticker = ticker, Type = type, Settlement = settlement) %>%
#           req_perform()
#
#         responseBody = fromJSON(rawToChar(rBook$body))
#       },
#       error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker) }
#     )
#
#     if (!length(responseBody$offers) == 0) {
#       date = responseBody$date
#       bids = tibble(cbind(SIDE = rep("BID", length(responseBody$bids$position)),(responseBody$bids)))
#       offers = tibble(cbind(SIDE = rep("OFFER", length(responseBody$offers$position)),(responseBody$offers)))
#       #returnValue = tibble(rbind(cbind(date = rep(date, length(bids$SIDE)), bids),
#       #                           cbind(date = rep(date, length(bids$SIDE)), offers) ))
#       returnValue = cbind(
#         date = as.Date(rep(date, length(bids$position) + length(offers$position))),
#         ticker = rep(ticker, length(bids$position) + length(offers$position)),
#         settlement = rep(settlement, length(bids$position) + length(offers$position)),
#         rbind(bids, offers))
#     } else { ### Acá viene tanto el offer no exista o bien haya salido por error
#       date = Sys.Date()
#       bids = tibble(SIDE = rep("BID", 1),
#                     position = 0,
#                     price = 0,
#                     quantity = 0)
#       offers = tibble(SIDE = rep("OFFER", 1),
#                       position = 0,
#                       price = 0,
#                       quantity = 0)
#       returnValue = cbind(
#         date = as.Date(date),
#         ticker = rep(ticker, 1),
#         settlement = rep(settlement, 1),
#         rbind(bids, offers))
#     }
#   }
#
#   dataframe_to_string <- function(df) {
#
#     capture <- capture.output(print(df))
#     paste(capture, collapse = "\n")
#   }
#
#   PPI = methodsPPI::getPPILogin2()
#
#   tickers = c("S31O3", "SO3D",
#               "S31O3", "SO3C",
#               "GD30", "GD30D",
#               "GD30", "GD30C",
#               "AL30", "AL30D",
#               "AL30", "AL30C",
#               "GD30", "GD30D",
#               "GD30", "GD30C",
#               "AL30", "AL30D",
#               "AL30", "AL30C",
#               "KO", "KOD",
#               "AAPL", "AAPLD",
#               "SPY", "SPYD",
#               "MELI", "MELID")
#
#   settle = c(rep("INMEDIATA", 12),
#              rep("A-48HS",16))
#   type = c(rep("LETRAS",4),
#            rep("BONOS",16),
#            rep("CEDEARS", 8))
#
#   prices = pmap_dfr(list(
#     PPI$token,
#     ticker = tickers,
#     type = type,
#     settlement = settle),
#     getPPIPuntas)
#
#
#   out = tibble(
#     date = Date(),
#     ticker = character(),
#     settlement = character(),
#     BIDPeso = double(),
#     BIDDolar = double(),
#     OFFERPeso = double(),
#     OFFERDolar = double(),
#     compra = double(),
#     venta = double()
#   )
#
#   for (i in seq(1, length(tickers), 2)) {
#
#     temp = prices %>%
#       filter(ticker == tickers[i] | ticker == tickers[i+1],
#              settlement == settle[i] | settlement == settle[i+1]) %>%
#       group_by(ticker, SIDE) %>%
#       do(head(., n=1)) %>%
#       pivot_wider(names_from = SIDE, values_from = price) %>%
#       pivot_wider(names_from = ticker, values_from = c("BID", "OFFER")) %>%
#       fill(starts_with(c("BID", "OFFER")), .direction = "down") %>%
#       fill(starts_with(c("BID", "OFFER")), .direction = "up") %>%
#       head(n= 1) %>%
#       mutate(
#         compra = round(1 / (.[[8]] / .[[5]]), 2),
#         venta = round(.[[7]] / .[[6]],2)
#       ) %>%
#       select(c(1,2,5:10)) %>%
#       mutate(ticker = paste0(tickers[i], "-", tickers[i+1])) %>%
#       relocate(ticker, .before = settlement)
#     colnames(temp) = colnames(out)
#     out = add_row(out, temp)
#
#   }
#   out = out %>% relocate(date, ticker, settlement, compra, venta) %>% as.data.frame()
#   textOut = dataframe_to_string(out)
#
#   bot$sendMessage(chat_id = update$message$chat_id,
#                   text =  textOut)
#   out = update$effective_user()
#   out$call = "Dolar"
#   log_print(out)
#
#
# }
#
# update = updater + CommandHandler("TC", TC)

dolar = function(bot, update){
   desde = Sys.Date() - 4
   dlr = getPPIDLR(from = desde)[[1]] %>% tail(n=1)
   date = dlr %>% pull(date)
   mep = dlr %>% pull(mepGD)
   mepAL = dlr %>% pull(mepAL)
   ccl = dlr %>% pull(cclGD)
   canje = dlr %>% pull(Canje)
   time = format(Sys.time(), format="%H:%M:%S")
   textOut = sprintf("Hora solicitud: %s.\n\nPrecios (LAST T+0) del %s. \n \nEl CCL via GD30 vale %.2f \nEl MEP via GD30 vale %.2f. \nEl canje queda en %.2f%%
 MEP Via AL30 %.2f",
                     time,
                     date,
                     ccl,
                     mep,
                     canje * 100,
                     mepAL)

   bot$sendMessage(chat_id = update$message$chat_id,
                   text =  textOut)
   out = update$effective_user()
   out$call = "Dolar"
   log_print(out)
 }

updater = updater + CommandHandler("dolar", dolar)

test = function(bot, update){
  getTasa = function(ticker, type, token,settlement = "INMEDIATA") {
    ### devuelve vacío cuando un ticker falla. Normalmente puede fallar porque consulto un ticker viejo
    require(tidyverse)
    require(jsonlite)
    require(httr2)
    require(lubridate)

    url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
    urlMarketData = 'MarketData/Book'

    fail = tibble(
      ticker = character()
    )
    responseBody = tibble(
      date = Date(),
      bids = numeric(),
      offers = numeric(),
      settlement = character()

    )
    error = FALSE
    tryCatch(
      {
        rBook = request(paste0(url, urlMarketData)) %>%
          req_headers(Authorization = token,
                      AuthorizedClient = 'API-CLI',
                      ClientKey = 'pp19CliApp12',
                      `User-Agent` = "http://github.com/jmtruffa"
          ) %>%
          req_method("GET") %>%
          req_url_query(Ticker = ticker, Type = type, Settlement = settlement) %>%
          req_perform()

        responseBody = fromJSON(rawToChar(rBook$body))
      },
      error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker) }
    )

    if (!length(responseBody$offers) == 0) {
      date = responseBody$date
      bids = tibble(cbind(SIDE = rep("BID", length(responseBody$bids$position)),(responseBody$bids)))
      offers = tibble(cbind(SIDE = rep("OFFER", length(responseBody$offers$position)),(responseBody$offers)))
      #returnValue = tibble(rbind(cbind(date = rep(date, length(bids$SIDE)), bids),
      #                           cbind(date = rep(date, length(bids$SIDE)), offers) ))
      returnValue = cbind(
        date = as.Date(rep(date, length(bids$position) + length(offers$position))),
        ticker = rep(ticker, length(bids$position) + length(offers$position)),
        settlement = rep(settlement, length(bids$position) + length(offers$position)),
        rbind(bids, offers))
    } else { ### Acá viene tanto el offer no exista o bien haya salido por error
      date = Sys.Date()
      bids = tibble(SIDE = rep("BID", 1),
                    position = 0,
                    price = 0,
                    quantity = 0)
      offers = tibble(SIDE = rep("OFFER", 1),
                      position = 0,
                      price = 0,
                      quantity = 0)
      returnValue = cbind(
        date = as.Date(date),
        ticker = rep(ticker, 1),
        settlement = rep(settlement, 1),
        rbind(bids, offers))
    }
  }
  tickers = c("GD30D", "AL30D", "GD38D", "AE38D")
  type = "BONOS"
  settlement = c("INMEDIATA", "A-48HS")

  activos = as_tibble(
    expand.grid(tickers = tickers,
                settlement = settlement,
                type = type)
  )


  PPI = getPPILogin2()
  df = pmap_dfr(
    list(
      activos$tickers,
      activos$type,
      PPI$token,
      activos$settlement
    ),
    getTasa #getPPIBook2
  ) %>%
    group_by(ticker, SIDE, settlement) %>%
    do(head(., n=1)) %>%
    as.data.frame() %>%
    filter( (SIDE ==  "OFFER" & settlement == "INMEDIATA") | (SIDE == "BID" & settlement == "A-48HS")) %>%
    pivot_wider(names_from = c(SIDE, settlement),
                values_from = price)  %>%
    relocate(date, ticker, position, quantity, OFFER_INMEDIATA, `BID_A-48HS`) %>%
    mutate(
      tasa =  `BID_A-48HS` / lead(OFFER_INMEDIATA) - 1
    )

  #textOut = (toString(df))
  print(df)
  bot$sendMessage(chat_id = update$message$chat_id,
                  #text = print(xtable::xtable(df, digits = 6, caption = "Tasa entre Px", type = "html"), type = "html"),
                  text =  knitr::kable(df, format = 'latex'),
                  parse_mode = 'HTML')


}

updater = updater + CommandHandler("test", test)

 dolarBook = function(bot, update){
   result = getPPIDLR2()
   date = result %>% head(n=1) %>% pull(date)
   time = format(Sys.time(), format="%H:%M:%S")
   string = "GD30"
   bidt0pesos = result %>% filter(ticker == string,
                                   SIDE == "BID",
                                   settlement == "INMEDIATA") %>% pull(price)
   offert0C = result %>% filter(str_ends(ticker, "C"),
                                SIDE == "OFFER",
                                settlement == "INMEDIATA") %>% pull(price)
   offert0pesos = result %>% filter(ticker == string,
                                    SIDE == "OFFER",
                                    settlement == "INMEDIATA") %>% pull(price)
   bidt0C = result %>% filter(str_ends(ticker, "C"),
                              SIDE == "BID",
                              settlement == "INMEDIATA") %>% pull(price)
   offert0D = result %>% filter(str_ends(ticker, "D"),
                                SIDE == "OFFER",
                                settlement == "INMEDIATA") %>% pull(price)
   bidt0D = result %>% filter(str_ends(ticker, "D"),
                              SIDE == "BID",
                              settlement == "INMEDIATA") %>% pull(price)

   bidt2pesos = result %>% filter(ticker == string,
                                  SIDE == "BID",
                                  settlement == "A-48HS") %>% pull(price)
   offert2C = result %>% filter(str_ends(ticker, "C"),
                                SIDE == "OFFER",
                                settlement == "A-48HS") %>% pull(price)
   offert2pesos = result %>% filter(ticker == string,
                                    SIDE == "OFFER",
                                    settlement == "A-48HS") %>% pull(price)
   bidt2C = result %>% filter(str_ends(ticker, "C"),
                              SIDE == "BID",
                              settlement == "A-48HS") %>% pull(price)
   offert2D = result %>% filter(str_ends(ticker, "D"),
                                SIDE == "OFFER",
                                settlement == "A-48HS") %>% pull(price)
   bidt2D = result %>% filter(str_ends(ticker, "D"),
                              SIDE == "BID",
                              settlement == "A-48HS") %>% pull(price)


   textOut = sprintf(
     "Precios del %s a las %s, utilizando %s.

 En T0
 CCL vale %.2f / %.2f
 MEP vale %.2f / %.2f
 El canje queda en %.2f%% / %.2f%%

 EN T2
 CCL vale %.2f / %.2f
 MEP vale %.2f / %.2f",
 date,
 time,
 string,
 bidt0pesos / offert0C,
 offert0pesos / bidt0C,
 bidt0pesos / offert0D,
 offert0pesos / bidt0D,
 ((bidt0pesos / offert0C) / (bidt0pesos / offert0D)  - 1) * 100,
 ((offert0pesos / bidt0C) / (offert0pesos / bidt0D)  - 1) * 100,
 bidt2pesos / offert0C,
 offert2pesos / bidt2C,
 bidt2pesos / offert2D,
 offert2pesos / bidt2D
 )

   print(textOut)
 bot$sendMessage(chat_id = update$message$chat_id,
               text =  textOut)
   out = update$effective_user()
   out$call = "Dolar Book"
   log_print(out)
 }

 updater = updater + CommandHandler("dolarBook", dolarBook)


 ARGprices = function(bot, update){
   bot$send_message(chat_id = update$message$chat_id, text = "Me pongo a trabajar en ello. Dame unos segundos...")
   error = FALSE
   tryCatch(
     {
       fails =  botPricesArg()
     },
     error = function(e) {
       error <<- TRUE;
       print("Falló la llamada a la API")
       bot$send_message(chat_id = update$message$chat_id, text = "Falló la llamada a la API")
     }
   )
   if (!error) {
     bot$send_document(chat_id =update$message$chat_id,
                       document = '~/Downloads/temp/pricesArg.xlsx')
     bot$sendMessage(chat_id = update$message$chat_id,
                     text = "Archivo PricesArg.")
     if (length(fails$ticker) != 0) {
       bot$sendMessage(chat_id = update$message$chat_id,
                       text = paste0("Los tickers que fallaron son: ", toString(paste((fails$ticker)))))
     }
   }
   out = update$effective_user()
   out$call = "Prices Arg"
   log_print(out)
 }

updater = updater + CommandHandler("ARGprices", ARGprices)

USprices = function(bot, update){
  bot$send_message(chat_id = update$message$chat_id, text = "Me pongo a trabajar en ello. Dame unos segundos...")
  error = FALSE
  #fails = botPricesUS()
  tryCatch(
    {
      fails =  botPricesUS()
    },
    error = function(e) {
      error <<- TRUE;
      print("Falló la llamada a botPricesUS")
      bot$send_message(chat_id = update$message$chat_id, text = "Falló la llamada a la API")
    }
  )
  if (!error) {
    bot$send_document(chat_id =update$message$chat_id,
                      document = '~/Downloads/temp/pricesUS.xlsx')
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Archivo PricesUS.")
    ### Luego tendré que ver cómo atrapar los fallos para informar
    # if (length(fails$ticker) != 0) {
    #   bot$sendMessage(chat_id = update$message$chat_id,
    #                   text = paste0("Los tickers que fallaron son: ", toString(paste((fails$ticker)))))
    # }
  }
  out = update$effective_user()
  out$call = "Prices US"
  log_print(out)
}

updater = updater + CommandHandler("USprices", USprices)

USdividends = function(bot, update){
  bot$send_message(chat_id = update$message$chat_id, text = "consultando los dividendos. Dame unos segundos...")
  error = FALSE
  tryCatch(
    {
      fails =  botDividendsUS()
    },
    error = function(e) {
      error <<- TRUE;
      print("Falló la llamada a la API")
      bot$send_message(chat_id = update$message$chat_id, text = "Falló la llamada a la API")
    }
  )
  if (!error) {
    bot$send_document(chat_id =update$message$chat_id,
                      document = '~/Downloads/temp/dividendsUS.xlsx')
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Archivo DividendsUS.")
    ### Luego tendré que ver cómo atrapar los fallos para informar
    # if (length(fails$ticker) != 0) {
    #   bot$sendMessage(chat_id = update$message$chat_id,
    #                   text = paste0("Los tickers que fallaron son: ", toString(paste((fails$ticker)))))
    # }
  }
  out = update$effective_user()
  out$call = "Dividends US"
  log_print(out)
}

updater = updater + CommandHandler("USdividends", USdividends)

 dolarGraph = function(bot, update, args){
    #Ajusto de entrada a la fecha de ayer
   bot$send_message(chat_id = update$message$chat_id, text = "Me pongo a trabajar en ello. Dame unos segundos...")
   date = Sys.Date() - 30
   print(date)
   error = FALSE
   if (length(args) > 0L) {
      #Hay parámetros
      #Ver si son 10 y están bien
     print("Hay argumentos")
     if (nchar(args) == 10) {
       print("son 10")
        #Son 10. Tratar de convertirlo a fecha
       tryCatch(
         {
           date = as.Date(args)
         },
         error = function(e) {error <<- TRUE; print("error convirtiendo fecha.");
         bot$send_message(chat_id = update$message$chat_id, text = "El formato de la fecha es incorrecto. Probá nuevamente. Formato yyyy-mm-dd") }
       )
     } else {
       print("Hay argumentos PERO NO SON 10. Salimos con error")
       error = TRUE
       print(error)
       bot$send_message(chat_id = update$message$chat_id, text = "Hay Argumentos PERO NO SON 10. Probá nuevamente. Formato yyyy-mm-dd")
     }
   }
   print(error)
   if (error) {
     print("Salimos con error")
   } else {
     print("no tiramos error. vamos con el proceso")
     n = sample.int(10000, 1)
     file = paste0("~/Downloads/temp/dlrGraph",n,".png")
     tryCatch(
       {
         dlrGraph = getPPIDLR(date)[[2]]
       },
       error = function(e) {error <<- TRUE; print("El proceso getPPIDLR falló. Probar luego.");
       bot$send_message(chat_id = update$message$chat_id,
                         text = "Fallo la API. Probar luego") }
     )
     if (!error) {
       png(file)
       print(dlrGraph)
       dev.off()
       bot$send_document(chat_id =update$message$chat_id,
                         document = file)
       file.remove(file)
     }
   }
   out = update$effective_user()
   out$call = "Dolar Graph"
   log_print(out)
 }

updater = updater + CommandHandler("dolarGraph", dolarGraph, pass_args = TRUE)


 caucion = function(bot, update){
   bot$send_message(chat_id = update$message$chat_id, text = "Me pongo a trabajar en ello. Dame unos segundos...")
   error = FALSE
   tryCatch(
     {
       botOvernight()
     },
     error = function(e) {
       error <<- TRUE;
     print("Falló la llamada de botOvernight");
     bot$send_message(chat_id = update$message$chat_id, text = "Falló la llamada a la API. Probá en un rato")
     }
   )
   if (!error) {
     bot$send_document(chat_id =update$message$chat_id, document = '~/Downloads/temp/SerieCaucion.xlsx')
     bot$send_document(chat_id =update$message$chat_id, document = '~/Downloads/temp/SerieCaucionCapitalizada.xlsx')
   }
   out = update$effective_user()
   out$call = "Caución: "
   log_print(out)
 }

updater = updater + CommandHandler("caucion", caucion)

 echo = function(bot, update){
   bot$sendMessage(chat_id = update$message$chat_id,
                   text = paste0("No entiendo esto: ", update$message$text))

 }

updater = updater + MessageHandler(echo, MessageFilters$text)

 kill = function(bot, update){
   bot$sendMessage(chat_id = update$message$chat_id,
                   text = "Bye!")
    #Clean 'kill' update
   bot$getUpdates(offset = update$update_id + 1L)
    #Stop the updater polling
   updater$stop_polling()
   log_close()
 }

updater = updater + CommandHandler("kill", kill)

currentRofex = function(bot, update){
  #Ajusto de entrada a la fecha de ayer
  bot$send_message(chat_id = update$message$chat_id, text = "Me pongo a trabajar en ello. Esto toma más de lo habitual ya que debo obtener los datos de MAE...")
   error = FALSE

   n = sample.int(10000, 1)
   file = paste0("~/Downloads/temp/currentRofex",n,".png")
   tryCatch(
     {
       rofex = getPPICurrentRofex()
     },
     error = function(e) {error <<- TRUE; print("El proceso de obtención de Rofex falló. Probar luego.");
     bot$send_message(chat_id = update$message$chat_id,
                      text = "Fallo la API. Probar luego") }
   )
   if (!error) {
     png(file)
     print(rofex[[2]])
     dev.off()
     bot$send_document(chat_id =update$message$chat_id,
                       document = file)
     file.remove(file)
   }
   out = update$effective_user()
   out$call = "Current Rofex"
   log_print(out)
 }

updater = updater + MessageHandler(currentRofex, MessageFilters$command)

unknown = function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Perdón, todavía no tengo ese comando implementado")
}

updater = updater + MessageHandler(unknown, MessageFilters$all)



updater$start_polling()
