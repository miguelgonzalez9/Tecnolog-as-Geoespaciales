library(readr)
library(dplyr)
library(fBasics)
library(Hmisc)
library(gridExtra)
#Recopilación Datos. Empalme de datos usando código DANE municipal como identificador. 
casos_muert <- read_csv("casos_muertes_municipio.csv") %>% select(c(-1))
pop <- read_xls("Censo Poblacional 2018.xls") %>% select(c(1,4))
dist <- read_csv("DistanciaCap.csv") %>%  select(c(4,6))
dist <- dist[dist$RASTERVALU != -9999,]
ipm <- read_csv("IPM_2018_FuenteCensal.csv") %>% select(c("MPIO_CCNCT", "MPM"))
ips <- read_csv("IPS_Municipios.csv") %>% select(c(2,4))
tranf <- read_csv("~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/Giros Sistema General de Participaciones Salud 2019/Transferencias_Salud_Municipio_2019.csv") %>% 
        select(c(3,5))
data <- left_join(ipm, casos_muert, by = c("MPIO_CCNCT" = "Cod")) %>% 
        left_join(pop, by = c("MPIO_CCNCT" = "Cod")) %>% 
        left_join(dist, by = c("MPIO_CCNCT" = "Cod")) %>% 
        left_join(ips, by = c("MPIO_CCNCT" = "idmpio")) %>% 
        left_join(tranf, by = c("MPIO_CCNCT" = "Cod"))
colnames(data)[c(1,7,8)] <- c("COD_MUN", "DIST_CAP", "IPS")
## Normalizar fallecimientos y casos por población.
data <- data %>%  mutate(CasosR = (1000*casos) / Población, MuertesR = (1000*muertes) / Población, LogTransfP = log(transf/Población)) %>% 
        select(-c("casos", "muertes"))
## Exportación de estadísticas descriptivas datos.
descriptive <- basicStats(select(data, -c(1,3,4)))
descriptive <- descriptive[-c(10,11,12),]
dev.off(dev.list()["RStudioGD"])
png("descriptive.png", width = 70*nrow(descriptive), height = 100*ncol(descriptive))
grid.table(descriptive)
dev.off()
write.csv(data[complete.cases(data),], "data_base.csv")
## Modelo de regresión lineal múltiple. 
modelo_muert <- lm(data = data, formula = MuertesR~ MPM + LogTransfP + DIST_CAP + IPS )
modelo_casos <- lm(data = data, formula = CasosR~ MPM + LogTransfP + DIST_CAP + IPS )
summary(modelo_casos)
