setwd("~/Documents/datathonlatam") ## setting dir

##############################################################
## Getting data about education ##############################

arc <- list.files("./datasets/educacion_superior", pattern = ".csv") ## creating list files csv

acreditacion <- read_csv(paste("./datasets/educacion_superior", arc[1], sep = "/"))
names(acreditacion) <- c("departamento", "anio", 
                         paste("acr", gsub("tecnica profesional", "tecpro", tolower(names(acreditacion)[3:8])), sep = "_") )

cobertura <- read_csv(paste("./datasets/educacion_superior", arc[2], sep = "/"))
names(cobertura) <- c("departamento", "anio", "cobertura" )

graduados <- read_csv(paste("./datasets/educacion_superior", arc[3], sep = "/"))
names(graduados) <- c("departamento", "anio", 
                      paste("gra", gsub("tecnica profesional", "tecpro", tolower(names(graduados)[3:8])), sep = "_") )

matricula <- read_csv(paste("./datasets/educacion_superior", arc[4], sep = "/"))
names(matricula) <- c("departamento", "anio", 
                      paste("matr", gsub("tecnica profesional", "tecpro", tolower(names(matricula)[3:8])), sep = "_") )

sector <- read_csv(paste("./datasets/educacion_superior", arc[5], sep = "/"))
names(sector) <- c("departamento", "anio", tolower(names(sector)[3:4]))

#############################################################
## Importing and transposing shaping PIB ####################


totalpib <- data.frame() ## empty data frame to fill
k <- 0 ## a counter for naming by department from departamentos var

library(readxl) ## library needed to read xls
for(i in seq(from = 2, to = 66, by = 2)) { ## We'll read even sheets (by number, not by name)

pib <- read_excel("~/Documents/datathonlatam/datasets/Copia de PIB_Departamentos_2015provisional.xls", 
                 sheet = i, skip = 5, range = "A7:Q59") ## read just a range of the sheet
pib <- pib[!is.na(pib[, 1]) & !is.na(pib[, 2]), c(1, 12:16)] ## delete NAs and keep 2010 to 2015
pib <- data.frame(t(pib)) ## transpose
k <- k + 1 ## increase 'iterator' of departamentos names
pib$departamento <- departamentos[k] ## namig the departamento
pib$anio <- rownames(pib) ## anio as value not just as rowname
pib <- pib[c(2:nrow(pib)), c(48:49, 1:47)] ## erasing first row and reordering cols-vars
totalpib <- rbind(totalpib, pib) ## stacking results in one data frame
}

rm(pib) ## erase inecessary data frame

totalpib[, 3:ncol(totalpib)] <- apply(totalpib[, 3:ncol(totalpib)], 2, as.integer) ## data as numeric

#############################################################
## Getting CENSO 2005 proyections ###########################

library(readr)
dane <- read_csv("./datasets/censo 2005/proyeccion poblacion DANE 2005.csv")

totaldane <- data.frame()
for( i in 1:nrow(dane)){
    temp <- data.frame(departamento = dane[i, 1], 
                                  anio = seq(2010, 2014), 
                                  proy = as.numeric(dane[i, 2:6]))
    totaldane <- rbind(totaldane, temp)
}

rm(list = c("dane", "temp"))


total <- merge(merge(totaldane, totalpib), df) ## merging all data till here


#################################################################
## Exploratory Data Analysis (a.k.a. EDA) #######################
correlations <- cor(total[, c(2:ncol(total))])
heatmap(correlations)

library(corrplot)
corrplot(correlations, order = "hclust", tl.cex = 0.6, tl.col = "black")
corrplot(correlations, order = "original", tl.cex = 0.6, tl.col = "black")

library(qgraph)
qgraph(cor(correlations), shape="circle", 
       posCol="darkblue", negCol="darkred", 
       layout="spring", vsize=8)

#################################################################
## Saving #######################################################
write.csv(total, "./alldataset.csv") ############################
write.csv(df, "./edudataset.csv")    ############################
write.csv(totaldane, "./danedataset.csv") #######################
write.csv(totalpib, "./pibdataset.csv") #########################
#################################################################