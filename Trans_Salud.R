library(dplyr)
library(readr)
library(readxl)
library(xlsx)
library(data.table)
library(stringr)
library(qdap)
## Modificación Datos Tranferencias a Salud.
setwd("~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/Giros Sistema General de Participaciones Salud 2019-2021")
## listzip <- dir(pattern = "*.zip") %>% lapply(unzip)
listxlsx <- dir(pattern = "*.xlsx")
dat <- lapply(listxlsx, read_excel, sheet = 1, skip = 4,
              col_type = c("text", "guess", "text", "text", "text","guess", "text", "text", "text"))
## Convertir fecha lista 6. 
dat[[6]][1] <- rep("2019-04-04",length(dat[[6]][[1]]))

listxls <- dir(pattern = "*.xls$") %>% lapply(read_xls, sheet = 1, range = "A5:I1138",
                                              col_type = c("text", "guess", "text", "text", "text","guess", "text", "text", "text"))
listdat <- append(dat, listxls)

## Identificar fechas mal leídas
lapply(listdat, function(x){
        print(unique(x[[1]]))
})

## Cambiar fechas mal leídas
listdat[[10]][1] <- rep("2019-02-08",length(listdat[[10]][[1]]))
listdat[[11]][1] <- rep("2019-03-06",length(listdat[[11]][[1]]))
## Juntar bases.
merge_dat <- do.call("rbind", listdat) %>% 
        mutate(`Fecha de pago` = as.Date(`Fecha de pago`, format = "%Y-%m-%d"))
## Valor en caracter.
merge_dat$Valor <- gsub(",", "", merge_dat$Valor)
merge_dat$Valor <- gsub("\\.", "", merge_dat$Valor)
merge_dat$Valor[str_detect(merge_dat$Valor, pattern = "![:digit:]{1,}")]
merge_dat <- merge_dat %>% mutate(Valor = as.numeric(Valor))
## Cambiar municipios
mun_transf <- merge_dat[str_detect(merge_dat$`Nombre Razon Social`, regex("municipio", ignore_case = T)),]
mun_transf$`Nombre Razon Social` <-  gsub("MUNICIPIO |MUNICIPIO DE | DPTO| DEPARTAMENTO", "",mun_transf$`Nombre Razon Social`)
mun_transf <- mun_transf[order(mun_transf$`Nombre Razon Social`),] %>% rename(Municipio = `Nombre Razon Social`)
## Eliminar municipios con nombres de departamentos 
dpto <- c("AMAZONAS",
          "ANTIOQUIA",
          "ATLANTICO",
          "BOLIVAR",
          "BOYACA",
          "CALDAS",
          "CAQUETA",
          "CASANARE",
          "CAUCA",
          "CESAR",
          "CHOCO",
          "CORDOBA",
          "CUNDINAMARCA",
          "GUAINIA",
          "GUAVIARE",
          "HUILA",
          "LA GUAJIRA",
          "MAGDALENA",
          "META",
          "NARIÑO",
          "NORTE DE SANTANDER",
          "PUTUMAYO",
          "QUINDÍO",
          "RISARALDA",
          "SAN ANDRÉS Y PROVIDENCIA",
          "SANTANDER",
          "SUCRE",
          "TOLIMA",
          "VALLE DEL CAUCA",
          "VAUPÉS",
          "VICHADA")
dpto <- paste0(dpto, "$")
#View(cbind(mgsub(dpto, "", mun_transf$Municipio[str_detect(mun_transf$Municipio, " ")], fixed = F),mun_transf$Municipio[str_detect(mun_transf$Municipio, " ")]))
mun_transf$Municipio <- replace(mun_transf$Municipio, which(str_detect(mun_transf$Municipio, " ")), mgsub(dpto, "", mun_transf$Municipio[str_detect(mun_transf$Municipio, " ")], fixed = F))
mun_transf$Municipio <- replace(mun_transf$Municipio, which(str_detect(mun_transf$Municipio, " ")), gsub(" DE$", "", mun_transf$Municipio[str_detect(mun_transf$Municipio, " ")], fixed = F))
## Identificar municipios con nombres repetidos. 
C <- read.csv("Mun_CodDane.csv", sep = ";", colClasses = "character") %>% select(1:2)
## Añadir cero a la izquierda a los códigos con cuatro dígitos
C <- transform(C, Cod = ifelse(nchar(CODIGO_MUNICIPIO)==4, paste0(0,CODIGO_MUNICIPIO),CODIGO_MUNICIPIO))
C <- transform(C, Cod = as.character(Cod))
cdup <- C[duplicated(C$NOMBRE_MPIO),]
length(unique(cdup$NOMBRE_MPIO))
unique(cdup$NOMBRE_MPIO)
## Eliminar municipios con nombres repetidos de la base de datos
mun_fin <- subset(mun_transf, !(mun_transf$Municipio %in% unique(cdup$NOMBRE_MPIO)))
## Resumir datos por municipio y agregar valor de transferencia. 
mun_fin <- mun_fin %>% group_by(Municipio) %>% summarise(transf = sum(Valor))
## Join con código DANE a través de municipios
mun_cod <- inner_join(mun_fin, C, by = c("Municipio" ="NOMBRE_MPIO"))
dim(mun_fin)[1] - dim(mun_cod)[1] ## Número de observaciones perdidas en el join. 
dim(listdat[[1]])[1]- dim(mun_cod)[1] ## Número de observaciones perdidas en todo el proceso.

## Probelma: municipios no tienen identificador. problema 1 Se agrupan los que tienen el mismo nombre, 
## Sesgo en la medición. Solución: identificación municipios repetidos, eliminación de dichas observaciones. 
## Pérdida de obs = 276. Minimizar pérdida.

write.csv(mun_cod, file = "Transferencias_Salud_Municipio_2019.csv")

## Análsis exploratorio
library(ggplot2)
View(mun_cod[order(mun_cod$transf, decreasing = T),])
ggplot(mun_cod, aes(x = log10(transf))) + geom_density()

