## Datos Covid Proyecto
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(ggplot2)
file <- c("~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/Casos_positivos_de_COVID-19_en_Colombia.csv")
x <- read_csv(file, col_types = c("ciccccciiffcficfccccffc"))
## Exploración básica bases de datos.
head(x)
dim(x)
str(x)
dim(x)[1]
## Variable que registre cada caso y cada fallecimiento 
count <- rep(1,dim(x)[1])
x <- cbind(x,count)
fallecidos <- x$Recuperado == "Fallecido" | x$Recuperado == "fallecido"
x <- cbind(x,fallecidos)
## Lo códigos municipiales está en formato numérico por lo algunos tienen cuatro dígitos y otros cinco.
## Para poder empalmar con otras bases de datos se tranforman los datos a formato númeor y se agrega un 0 en el primer dígito para los tienen cuatro dígitos. 
sum(nchar(x$`Código DIVIPOLA municipio`) == 4) + sum(nchar(x$`Código DIVIPOLA municipio`) == 5) == dim(x)[1]
## Ahora tomamos los de cuatro dígitos y añadimos un 0 al principio del dígito
df <- transform(x, Cod = ifelse(nchar(`Código DIVIPOLA municipio`)==4, paste0(0,`Código DIVIPOLA municipio`),`Código DIVIPOLA municipio`))
df <- transform(df, Cod = as.character(Cod))
## Agrupar datos por código de municipio
data <-df %>% group_by(Cod)
## Resumir casos y fallecidos a nivel municipal.
casos_municipio <- data %>% summarise(casos = sum(count), muertes = sum(fallecidos))
##identificación de capitales
file <- "~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/Capitales Colombia.xlsx"
cap <- read_excel(file, sheet = 1)
cap <- cap[,-2]
colnames(cap) <- c("Cod", "cap_dpto")
casos_municipio_cap <- full_join(casos_municipio, cap, by = c("Cod" = "Cod")) %>% 
        replace_na(list(cap_dpto = 0)) %>% arrange(desc(muertes), desc (casos))
file <- "~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/casos_muertes_municipio.csv"
write.csv(casos_municipio_cap, file)
## Importar índice de probeza multidimensional municipal. 
file <- "Datos/IPM_2018_FuenteCensal.csv"
IPM <- read_csv(file)
str(IPM)
## Empalme IPM con casos y fallecidos, identificador: código municipal.
data_base <- inner_join(IPM, casos_municipio_cap, by = c("MPIO_CCNCT" = "Cod"))