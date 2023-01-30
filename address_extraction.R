library(data.table)
library(stringi)

#dane <- fread(...)

dane <- dane[, c(2, 3, 4)]

colnames(dane) <- c("OW", "nazwaObiektu", "lokalizacja")

dane[, kod := unlist(stri_extract_all_regex(nazwaObiektu, "\\d{2}-\\d{3}"))][is.na(kod)][,  kod :=  unlist(stri_extract_all_regex(lokalizacja, "\\d{2}-\\d{3}"))]


dane[, ulica :=  unlist(stri_extract_all_regex(nazwaObiektu, "(?<=ul. ).+(?= \\d+\\w*[$,])"))][is.na(ulica), ulica :=  unlist(stri_extract_all_regex(nazwaObiektu, "al\\. .+(?= \\d+\\w*[$,])"))]

dane[is.na(ulica), ulica :=  unlist(stri_extract_all_regex(lokalizacja, "(?<=ul. ).+(?= \\d+\\w*[$,])"))][is.na(ulica), ulica :=  unlist(stri_extract_all_regex(lokalizacja, "al\\. .+(?= \\d+\\w*[$,])"))]

dane[, nr:=  unlist(stri_extract_all_regex(nazwaObiektu, "\\d+\\w*(?=[$,])"))][is.na(nr), nr:=  unlist(stri_extract_all_regex(nazwaObiektu, "\\d+\\w*(?=[$,])"))]


dane[is.na(nr), nr:=  unlist(stri_extract_all_regex(lokalizacja, "\\d+\\w*(?=[$,])"))][is.na(nr),  unlist(nr:= stri_extract_all_regex(lokalizacja, "\\d+\\w*(?=[$,])"))]
dane[, miasto :=unlist(stri_extract_all_regex(nazwaObiektu, "(?<=\\d ).+(?=[$\\n])"))][is.na(miasto), miasto :=unlist(stri_extract_all_regex(lokalizacja, "(?<=\\d ).+(?=[$\\n])"))]


#adres miejsca
dane[,`Kod pocztowy` := stri_extract_first_regex(`Adres miejsca lokalizacji kwarantanny zbiorowej`, "\\d{2}\\s?(-|?|?)\\s?\\d{3}")]

dane[, Ulica := stri_extract_first_regex(`Adres miejsca lokalizacji kwarantanny zbiorowej`, 
                                         "(?<=ul\\.\\s?)([:upper:]|[:digit:]){1}.+(?=\\s\\d+\\w*($|,|\\s?-$|\\s*Miasto\\s{1}.*|(\\s{1}\\d{2}-\\d{3})))")][is.na(Ulica), 
                                                                                                                                                    Ulica := stri_extract_first_regex(`Adres miejsca lokalizacji kwarantanny zbiorowej`, 
                                                                                                                                                                                      "[ap]l\\.\\s?([:upper:]|[:digit:]){1}.+(?=\\s\\d+\\w*($|,|\\s?-$|\\s*Miasto\\s{1}.*|(\\s{1}\\d{2}-\\d{3})))")][is.na(Ulica), Ulica := stri_extract_first_regex(`Adres miejsca lokalizacji kwarantanny zbiorowej`, 
                                                                                                                                                                                                                                                                                            "^[:upper:]{1}([:alpha:]|\\s|[:alpha:]-[:upper:])*(?=\\s?\\d?\\w*,)")]

dane[, `Nr domu` :=  stri_extract_first_regex(`Adres miejsca lokalizacji kwarantanny zbiorowej`, "(?<=\\w\\s)\\d+\\w*(?=($|\\W))")]


     
dane[, `Miejscowo??` := stri_extract_first_regex(`Adres miejsca lokalizacji kwarantanny zbiorowej`, "(?<=\\d{3}\\s?)[:upper:]{1}([:alpha:]|\\s|[:alpha:]-[:upper:])*(?=$|,|-\\s+|\\n|[apu]l\\.|\\()")][is.na(`Miejscowo??`), `Miejscowo??` := stri_extract_first_regex(`Adres miejsca lokalizacji kwarantanny zbiorowej`, "(?<=^|m.\\s?)[:upper:]{1}([:alpha:]|\\s|[:alpha:]-[:upper:])*(?=$|,|-\\s+|\\n|[apu]l\\.|\\()")]    

#nazwa obiektu

dane[is.na(`Kod pocztowy`),`Kod pocztowy` := stri_extract_first_regex(`Nazwa obiektu`, "\\d{2}\\s?(-|?|?)\\s?\\d{3}")]

dane[is.na(Ulica), Ulica := stri_extract_first_regex(`Nazwa obiektu`, 
                                         "(?<=ul\\.\\s?)([:upper:]|[:digit:]){1}.+(?=\\s\\d+\\w*($|,|\\s?-$|\\s*Miasto\\s{1}.*|(\\s{1}\\d{2}-\\d{3})))")][is.na(Ulica), 
                                                                                                                                                          Ulica := stri_extract_first_regex(`Nazwa obiektu`, 
                                                                                                                                                                                            "[ap]l\\.\\s?([:upper:]|[:digit:]){1}.+(?=\\s\\d+\\w*($|,|\\s?-$|\\s*Miasto\\s{1}.*|(\\s{1}\\d{2}-\\d{3})))")][is.na(Ulica), Ulica := stri_extract_first_regex(`Nazwa obiektu`, 
                                                                                                                                                                                                                                                                                                                                                           "^[:upper:]{1}([:alpha:]|\\s|[:alpha:]-[:upper:])*(?=\\s?\\d?\\w*,)")]

dane[is.na(`Nr domu`), `Nr domu` :=  stri_extract_first_regex(`Nazwa obiektu`, "(?<=\\w\\s)\\d+\\w*(?=($|\\W))")]



dane[is.na(`Miejscowo??`), `Miejscowo??` := stri_extract_first_regex(`Nazwa obiektu`, "(?<=\\d{3}\\s?)[:upper:]{1}([:alpha:]|\\s|[:alpha:]-[:upper:])*(?=$|,|-\\s+|\\n|[apu]l\\.|\\()")][is.na(`Miejscowo??`), `Miejscowo??` := stri_extract_first_regex(`Nazwa obiektu`, "(?<=^|m.\\s?)[:upper:]{1}([:alpha:]|\\s|[:alpha:]-[:upper:])*(?=$|,|-\\s+|\\n|[apu]l\\.|\\()")]  
