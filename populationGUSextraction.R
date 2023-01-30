sciezka_w <- "W:/WMichal/Demo/csv"
sciezka_z <- "W:/WMichal/Demo/rdata"
sciezka_f <- "W:/WMichal/Demo/rds"
library(readxl)
library(data.table)
library(stringi)

wczytaj_dane <- function(co)
{
  as.data.table(read.csv2(paste0(sciezka_w, "/", co, ".csv"), fileEncoding = "UTF8"))
}

wczytaj_excel <- function(co)
{
  read_xls(paste0(sciezka_w, "/", co, ".xls"), sheet = "tabl. 114(144)")
}

zapisz_dane <- function(co)
{
 saveRDS(co, file = paste0(sciezka_z, "/", deparse(substitute(co)), ".rds"))
}

znajdz_woje <- function(kolumna)
{
  stri_extract(kolumna, 
               regex = "^[:upper:]{2}.+"
               )
}

nazwy <-  list(rep(c("ogolem", "miasto", "wies"), each = 3), rep(c("ogolem", "mezczyzni", "kobiety"), times = 3)) # kombinacja miasto/wies, mezczyzna/kobieta oraz razem

woje <- data.table(kod = sprintf("%02d", 
                                 1:16*2
                                 ), 
                   nazwa = c("dolnoslaskie", 
                             "kujawsko-pomorskie", 
                             "lubelskie", 
                             "lubuskie", 
                             "lodzkie", 
                             "malopolskie", 
                             "mazowieckie", 
                             "opolskie", 
                             "podkarpackie", 
                             "podlaskie", 
                             "pomorskie", 
                             "slaskie", 
                             "swietokrzyskie", 
                             "warminsko-mazurskie", 
                             "wielkopolskie", 
                             "zachodniopomorskie"
                             )
                   ) # lista wojewodztw

#wiersze <-  as.vector(sapply(13:17, function(x) x+seq(0, 91, 7)))

for (i in 1:16){
  probka <- wczytaj_excel(paste0("pl_lud_2017_", woje[i]$kod, "_05"))

colnames(probka)[2:10] <- paste(nazwy[[1]], nazwy[[2]], sep = "_") # zmiana nazw kolumn

kolumny <- 2:10 # wybrane kolumny

probka <- as.data.table(probka) # data.table jest cool
probka[probka == "X"] <- 0 # zamiana X na -, przy kobieta w wieku rozrodczym
probka[probka == "-"] <- 0
probka[, (colnames(probka)[(kolumny)]) := lapply(.SD, function(x) str_extract(x, "^\\d.*")), .SDcols = kolumny] # wyczyszczenie napisow
probka[, (colnames(probka)[(kolumny)]) := lapply(.SD, as.integer), .SDcols = kolumny]
 
powiaty <- stri_extract(probka$TABL.5, regex = "^[:alpha:].*") # wwyszykujemy nazw zaczynajacych sie od liter
powiaty <- powiaty[!is.na(powiaty)] # wywalenie NA

powiaty <- powiaty[-(1:3)] # wywalenie trzech pierwszych wierszy
grupy <- powiaty[1:8] # grupy wiekowe
powiaty <- powiaty[!(powiaty %in% grupy)] # usuniecie grup wiekowych
powiaty <- c("województwo", powiaty) # pierwsze dane w wojewodztwie dotycza wojewodztwa


stworz_liste <- function(x)
{
  list(zbiorcze = probka[x, .SD, .SDcols = kolumny], 
        wiek = data.table(wiek = 0:69, 
                          probka[as.vector(sapply(3:7+x, function(y) y+seq(0, 91, 7))), 
                                 .SD, 
                                 .SDcols = kolumny
                                 ]
        ),
        grupowo = data.table(przedzial = c(paste0(seq(0, 69, 5), "\u2013", seq(4, 69, 5)), "70 i wiêcej")#probka[seq(x+2,
                                                    #x+100,
                                                    #7
                                                    #)
                                                #]$TABL.5
                               ,
                             probka[seq(x+2, 
                                        x+100, 
                                        7
                                        ), 
                                    .SD, 
                                    .SDcols = kolumny
                                    ]
        ),
        `wiek przedprodukcyjny` = probka[x+102, 
                                         .SD, 
                                         .SDcols = kolumny
                                         ],
        `wiek produkcyjny` = probka[x+104, 
                                    .SD, 
                                    .SDcols = kolumny
                                    ],
        `wiek mobilny` = probka[x+108, 
                                .SD, 
                                .SDcols = kolumny
                                ],
        `wiek niemobilny` = probka[x+110, 
                                   .SD, 
                                   .SDcols = kolumny
                                   ],
        `Wiek poprodukcyjny` = probka[x+114, 
                                      .SD, 
                                      .SDcols = kolumny
                                      ],
        `biologiczne grupy wieku` = cbind(przedzial = c("0\u201314", 
                                                        "15\u201364", 
                                                        "65 i wiêcej", 
                                                        "suma"), 
                                          probka[c(119:121, 
                                                   118
                                                   )+x, 
                                                 .SD, 
                                                 .SDcols = kolumny
                                                 ]
                                          ),
        `edukacyjne grupy wieku` = cbind(przedzial = c("3\u20136", 
                                                       "7\u201312", 
                                                       "13\u201315", 
                                                       "16\u201318", 
                                                       "19\u201324", 
                                                       "suma"
                                                       ), 
                                         probka[c(124:128, 
                                                  123
                                                  )+x, 
                                                .SD, 
                                                .SDcols = kolumny
                                                ]
                                         ),
        `kobiety w wieku rozrodczym` = probka[x+130, 
                                              2:10
                                              ]
  )
}



A <- lapply(seq(10, nrow(probka), 132), function(x) stworz_liste(x))

names(A) <- powiaty

zapisz_dane(assign(woje[i]$nazwa, 
       A
       ))

remove(A)

}




policz_udzial <- function(X, kol)
{
  get("grupowo", X)[, (paste("proc", kol, sep = "_")) := lapply(.SD, prop.table), .SDcols = kol] # wyliczenia procentowego udzialu grup wiekowych
}

dla_woj <- function(X, kol)
{
  wojek <<- get("województwo", X)[, .SD, .SDcols = kol]
}

# wzgledem_woj <- function(X, kol)
# {
#   X[, (paste("do_woj", kol, sep = "_")) := lapply(.SD, function(x) x/wojek[, 1]), .SDcols = kol] # wzgledem
# }


nowe[, (paste("do_woj", kolki, sep = "_")) := .SD/nowe[jednostka == "województwo", .SD, .SDcols = kolki], .SDcols = kolki, by = jednostka]
nowe[, (paste("do_pl", kolki, sep = "_")) := .SD/nowe[jednostka == "kraj", .SD, .SDcols = kolki], .SDcols = kolki, by = jednostka]



#### Wojewodztwo, co trzeba ####

library(plotly)

wybierz_woj <- "02"

woik <- ludy[OW_KOMORKI == wybierz_woj]
woik[order(o_ogolem)]


# ludnoœc
#plot_ly(woik[order(-o_ogolem)][-1], x=~o_ogolem, y=~nazwa, type = "bar", marker = list(color = "magenta"), orientation = "h") %>% layout(yaxis = list(title = "nazwa powiatu", categoryarray =~o_ogolem), xaxis = list(title = "liczba ludnoœci"))

## srednia dlugosc zycia

zycie <- wczytaj_dane("PTDZ")

zycie <- as.data.table(zycie)
zycie[, Kod := sprintf("%07d", Kod)]
zycie[, X := NULL]

#zycie[, OW_KOMORKI := stri_sub(Kod, 1, 2)]
kolejki = c(1:8, 21:26) 
zycie <- zycie[, .SD, .SDcols = kolejki]
kolki <- c("Kod", "Nazwa")
melcik <- zycie %>% melt(vars.id = kolki, variable.name = "plec", value.name = "lata")
melcik[, rok := as.integer(stri_extract(plec, regex = "\\d+"))]
melcik[, plec := stri_extract(plec, regex = "[:alpha:]+")]

melcik[, lata_zycia :="+"(lata, rok)]
melcik[, rok := as.character(rok)]
melcik[plec == "mê¿czyŸni", procent_kobiet_lata := melcik[plec == "mê¿czyŸni"]$lata/melcik[plec == "kobiety"]$lata*100]
melcik[plec == "mê¿czyŸni", procent_kobiet_lata_zycia := melcik[plec == "mê¿czyŸni"]$lata_zycia/melcik[plec == "kobiety"]$lata_zycia*100]
write.csv2(melcik, file = "Demografia/PDTZ_gotowe.csv", row.names = FALSE)

#plot_ly(melcik, x=~rok, y=~lata, color =~plec, type = "bar", opacity =.9) %>% layout(barmode = "overlay", title = "Przewidywane dalsze trwanie ¿ycia", xaxis = list(title = "rok ¿ycia"), yaxis = list(title = "lata ¿ycia do œmierci"))
#plot_ly(melcik, x=~rok, y=~lata_zycia, color =~plec, type = "bar", opacity =.9) %>% layout(barmode = "overlay", title = "Przewidywanie trwanie ¿ycia", xaxis = list(title = "rok ¿ycia"), yaxis = list(title = "lata ¿ycia od narodzenia do œmierci"))

### gestosc ###

gestosc <- as.data.table(read.csv2("Demografia/gestosc.csv", fileEncoding = "UTF8"))

gestosc[, X := NULL]

colnames(gestosc)[3:5] <- c("gestosc", "gestosc_zab", "zmiana")

write.csv2(gestosc, file = "Demografia/gestosc_gotowa.csv")

pl <- gestosc[Kod == 0]$gestosc


### mapa


### smierc okoloporodowa

okolo <- readxl::read_xls("Demografia/10_zgony_RD'2018.xls", sheet = "tabl. 114(144)")

okolo <- okolo[15:nrow(okolo),]

okolo <-  as.data.table(okolo)
okolo <- okolo[-2,-2]
okolo[1,1] <- "Polska"
colnames(okolo) <-c("jednostka", "ogolem_bezw",	"miasta_bezw",	"wies_bezw", 	"ogolem_wsp",	"miasta_wsp",	"wies_wsp")

okolo <- okolo[1:17, c(1, 5:7)]

okolo <- melt(okolo, id.vars = "jednostka", variable.name = "teren", value.name = "wspolczynnik")

levels(okolo$teren) <- c("ogólem", "miasto", "wieœ")

write.csv2(okolo, file = "Demografia/smierc_okolo.csv")

### plodnosc ####
# urodzenia <- as.data.table(read.csv2("Demografia/urodzenia.csv", fileEncoding = "UTF8"))
# kolki <- as.character(2015:2017)
# colnames(urodzenia)[3:5] <- kolki 
# urodzenia[, X := NULL]
# urodzenia[, srednia := rowMeans(urodzenia[,.SD, .SDcols =kolki])]
# kolki <- c(kolki, "srednia")
# urodzenia[, (kolki) := lapply(.SD, function(x) x*1000), .SDcols = kolki]



demo1 <- readxl::read_xlsx("Demografia/plodnosc_1p_2015.xlsx", sheet = "Ogó³em")

znajdz_zywe_1p <- function(co, rok)
{
  a <- which(get("TABL. 1P.", co) == "¿ywe")
  a <- a[seq(1,length(a), 3)]
  nazwa <- co[a-1, 1]
  ile <- co[a, 2]
  wynik <- cbind(nazwa, ile)
  colnames(wynik) <- c("Nazwa", rok)
  return(wynik)
}
d1 <- znajdz_zywe(d1, 2015)

demo2 <- readxl::read_xlsx("Demografia/plodnosc_1p_2016.xlsx", sheet = "Ogó³em")

d2 <- znajdz_zywe_1p(d2, 2016)

demo3 <- readxl::read_xlsx("Demografia/plodnosc_1p_2017.xlsx", sheet = "Ogó³em")

d3 <- znajdz_zywe1p(d3, 2017)


### 5p ###

setwd("C:/Users/MSW/Desktop/Pracunia/Wejœciowe")



#"Demografia/plodnosc_5p_2015.xlsx"

popraw_5p <- function(x, rok)
{
  D <- as.data.table(readxl::read_xls(paste0(x, 
                                              rok, 
                                              ".xls"
                                              ), 
                                       sheet = "Ogó³em"
                                       )
                     )
  
  D <- D[, 
         .SD, 
         .SDcols = c(1, 
                     10
                     )
         ] # wybrane kolumny, mo¿e siê zmieniæ
  
 colnames(D) <- c("Nazwa", "wsp_dz")
 
 D <- D[complete.cases(D)]
 
 D <- D[-1] # wywalany pierwszy wiersz
 
 kolka <- colnames(D)[1]
 
 D[get(kolka) %in% znajdz_woje(get(kolka)), jednostka := "wojewodztwo"]
 
 kolka <- colnames(D)[2]
 
 D[, 
   (kolka) := lapply(.SD, 
                     as.numeric
                     ), 
   .SDcols = kolka
   ]
 
 D[,
   do_woj := .SD/get(kolka, 
                     D[jednostka == "wojewodztwo",
                       .SD,
                       .SDcols = kolka
                       ]
                     ),
   by = jednostka,
   .SDcols = kolka
   ]

 D[1, jednostka := "kraj"]

 D[,
   do_pol := .SD/get(kolka,
                     D[1]
                     ),
   .SDcols = kolka
   ]
 
 #D[, wzg_woj := wsp_dz/D[jednostka]]
  
}

demo1 <- popraw_5p("plodnosc_5p_", 2015)

demo1 <- as.data.table(readxl::read_xls("plodnosc_5p_2015.xls", sheet = "Ogó³em"))

demo1 <- demo1[, .SD, .SDcols = kolki]
