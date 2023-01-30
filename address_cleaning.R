slibrary(data.table)

X <- data.table(adres = c("11 LISTOPADA 11", 
                          "11 LISTOPADA 11A", 
                          "11 LISTOPADA 11AB", 
                          "11 LISTOPADA 11 A", 
                          "11-GO LISTOPADA 11", 
                          "11-EGO LISTOPADA 11", 
                          "11 LISTOPADA 1", 
                          "11 LISTOPADA 1A", 
                          "11 LISTOPADAA 1AB",
                          "11 LISTOPADA 1 A",
                          "11 LISTOPADA 1 AB"
                          )
                )


czy_nr <- function (x) # sprawdza czy po nazwie ulic jest nr z ewentualną (spacją i) literą
{
  str_detect(x, "\\d{1,3}\\s*?([A-Z]{0,1})$") # zakładamy, że nr domu maksymalnie trzycyfrowy oraz tylko jedna litera może być na końcu
}

czy_go <- function (x) # sprawdza, czy w nazwie ulicy występuję cząstka -(E)GO
{
  str_detect(x, "^\\d{1,2}-([E]{0,1})GO\\s*?([:alpha:]*?)$") #ponieważ dni w miesiącu jest maksymalnie 31, toteż maksymalnie 2 cyfry mogą wystąpić
}

czy_go_nr <- function (x)  # robi za 2 ww. funkcje
{
  str_detect(x, "^\\d{1,2}-([E]{0,1})GO\\s*?([:alpha:]*?)\\s*?\\d{1,3}\\s*?([A-Z]{0,1})$")
}

dodaj_nr <- function(x) #wyciąga nr z ulicy
{
  gsub("\\s", 
       "", 
       str_extract(x, 
                   "\\d{1,3}\\s*?([:alpha:]{0,1})$" #ww. założenia co do formatu
                   )
       )
}

przytnij_ulice <- function(x) # wywala nr z nazwy ulicy
{
  str_extract(x, "\\d{0,2}\\D+[:alpha:]") # na początku nazwy mogą być cyfry (maksymalnie 2)
}

zmien_ulice <- function(x) #usuwa cząstkę -(E)GO
{
  gsub("-(E?)GO",
       "",
       x)
}

popraw_adresy <- function(X)
{
  stopifnot(is.data.table(X))
  
  X[MIASTO == ULICA,
      `:=`(UWAGI = "Nazwę ulicy usunięto",
           ULICA = ""
           )
      ]
  
  
  X[czy_go_nr(ULICA), 
    `:=`(UWAGI = "Nazwę ulicy zmodyfikowano i nr domu z ulicy usunięto",
         NR_DOMU = dodaj_nr(ULICA), 
         ULICA = zmien_ulice(przytnij_ulice(ULICA))
         )
        ]
  
  X[czy_go(ULICA), 
    `:=`(UWAGI = "Nazwę ulicy zmodyfikowano",
         ULICA = zmien_ulice(ULICA)
         )
      ]
  
  X[MIASTO == str_extract(ULICA, 
                          "^(\\D*)[:alpha:]"
                          ),
    `:=`(UWAGI = "Nazwę ulicy usunięto wraz z nrem domu",
         NR_DOMU = dodaj_nr(ULICA),
         ULICA = ""
        )
      ]
  
  X[czy_nr(ULICA),
    `:=`(UWAGI = "Nr domu usunięto z ulicy",
         NR_DOMU = dodaj_nr(ULICA),
         ULICA = przytnij_ulice(ULICA)
         )
        ]
}



