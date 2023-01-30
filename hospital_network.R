library(data.table)
library(igraph)

setwd("C:/Users/miixaa2iil/Desktop/Studia/ICM/ONS")



agregaty <- dane[, .(ile =.N), by = c("medyk1", "medyk2")]

graf <- graph_from_data_frame(d=agregaty, directed=FALSE)


plot(graf)

agregaty <- dane[medyk1 != medyk2 & !is.na(medyk1) & !is.na(medyk2), .(ile =.N), by = c("medyk1", "medyk2", "ICD1", "ICD3")]

agregaty <- dane[, .(ile =.N), by = c("medyk1", "ICD3")]

graf <- graph_from_data_frame(d=agregaty, directed=FALSE)


plot(graf)

doc <- table(dane$medyk1)


ktorzy <- names(doc[doc >= 1721])

dane[, Hosp := uniqueN(ICD3), by = id]


agregaty <- dane[medyk1 != medyk2 & !is.na(medyk1) & !is.na(medyk2), .(ile =.N), by = c("medyk1", "medyk2", "ICD1", "ICD3")]#[medyk1 %in% ktorzy]

agregaty <- dane[, .(ile =.N), by = c("medyk", "dziedziny", "ICD3")]

dlugo <- dcast(agregaty, medyk+dziedziny~ICD3, value.var = "ile", fill = 0)


dlugo <- dlugo[!is.na(medyk)]

write.csv(dlugo, "doJulii.csv", row.names = FALSE)

setorder(dlugo, "medyk")

medycy <- dlugo[, 1:2]

dlugo <- data.matrix(dlugo[, -(1:2)])


M <- outer(1:100, 1:100, FUN = function(x, y) { sum(apply(rbind(dlugo[x, -1], dlugo[y, -1]), 2, min))}
  )

graf <- graph_from_data_frame(d=agregaty, directed=FALSE)

M <- matrix(ncol=10, nrow=10)
for (i in 1:10){
  w <- dlugo[i, -1]
  for (j in i:10){
    print(paste0("i = ", i, ", j = ", j))
    M[i,j] <- sum(apply(rbind(w, dlugo[j, -1]), 2, min))
    
    
  }}

D <- data.matrix(D[, -(1:2)])
z <- nrow(D)
M <- matrix(ncol=z, nrow=z)
for (i in 1:z){
  W <- D[i,]
  if (i+1 <= z){
    for (j in (i+1):z){
      print(paste0("i = ", i, ", j = ", j))
      M[i,j] <- sum(apply(rbind(W, D[j, ]), 2, min))
    }
  }
}

for (v in agregaty$medyk1){
 n <- agregaty[medyk1 == 1][1]$nr
 ch <- agregaty[medyk1 == v]$ICD3
 kto <- agregaty 
 
}


M[is.na(M)] <- 0

M <- M + t(M)

macierz <- data.matrix(fread("ONS1.csv"))

colnames(macierz) <- dlugo$medyk

rownames(macierz) <- dlugo$medyk

graf <- graph_from_adjacency_matrix(M, "undirected", weighted = TRUE)

plot(graf,
     vertex.color=medycy$barwa)

V(graf)$size <- log10(strength(graf))
V(graf)$label <- NA
E(graf)$width <- log(E(graf)$weight) + 1
plot(graf,layout=layout_as_star)


co <- "prodPelny"

agregaty <- dane[, .(ile =.N), by = c("medyk", "dziedziny", "VIII")]

dlugo <- dcast(agregaty, medyk+dziedziny~VIII, value.var = "ile", fill = 0)


dlugo <- dlugo[!is.na(medyk)]

write.csv(dlugo, "doJulii.csv", row.names = FALSE)

setorder(dlugo, "medyk")

medycy <- dlugo[, 1:2]

dlugo <- data.matrix(dlugo[, -(1:2)])

macierz <- data.matrix(fread("ONS4.csv"))

colnames(macierz) <- dlugo$medyk

rownames(macierz) <- dlugo$medyk

graf <- graph_from_adjacency_matrix(macierz, "undirected", weighted = TRUE)

plot(graf,
     vertex.color=medycy1$barwa)

V(graf)$size <- log10(strength(graf))
V(graf)$label <- NA
E(graf)$width <- log(E(graf)$weight) + 1
plot(graf,layout=layout_as_star)
