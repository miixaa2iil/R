library(data.table)
library(stringr)

adresy <- read.csv2("spispna — kopia.csv",
                    stringsAsFactors = FALSE#,
                    #header = FALSE
                    )

nazwy <- c("kod", 
           "gm",
           "pow",
           "woj"
           )

adresy <- as.data.table(adresy)

adres <- adresy[1:134461] # ewentualnie zmienić

adres <- adres[str_detect(Poczta.Polska.,
                  "\\d{2}-\\d{3}\\s"
                  )
      ]

adres[, nazwy[1] := str_sub(Poczta.Polska.,
                       1,
                       6
                       )
      ]

adres[, nazwy[2] := str_extract(Poczta.Polska., 
                                             "(?<=[:space:]{3})[:upper:]{1}[:lower:]{1,}[[:space:]-]{0, 1}[:alpha:]{0,}[:space:]{0,1}[:upper:]{9,1}[:lower:]{0,}[:space:]{0,}](?=[:space:]{3}[:lower:])"
                                          )

      ]

adres[, nazwy[3] := str_extract(Poczta.Polska.,
                                "(?<=[:space:]{3})([:lower:]{1,}[^:lower:]?[:lower:]{1,}(-)?)(?=[:space:]{3})"
                                )
      ]

adres[, nazwy[4] := str_extract(Poczta.Polska.,
                                "[:lower:]{1,}(-)?[:lower:]{1,}$"
                                )
      ]

adres[, Poczta.Polska. := NULL]

adres <- adres[, lapply(.SD, 
                        function(x) str_trim(x, 
                                             "both"
                                             )
                        )
               ]

adres <- unique(adres)

adres[is.na(pow), pow := gm]

adres[, (2:4) := lapply(.SD, as.factor), .SDcols = 2:4]

levels(adres$pow)[levels(adres$pow) %in% c("czarnkowsko-", 
                                           "kędzierzyńsko-", 
                                           "ropczycko-"
                                           )
                  ] <- c("czarnkowsko-trzcianecki",
                         "kędzierzyńsko-kozielski",
                         "ropczycko-sędziszowski")

adres[, (2:4) := lapply(.SD, as.character), .SDcols = 2:4]



#adres[is.na(pow), pow := gm]
