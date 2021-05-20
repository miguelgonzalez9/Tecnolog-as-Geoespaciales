library(dplyr)
## Datos distancia. 
file <- "/Users/miguel/OneDrive\ -\ Universidad\ del\ rosario/Tec\ Geo/Proyecto/Datos/DistanciaCap.txt"
Dist <- read.table(file, header = T, sep = ";") %>% select(MPIO_CCNCT, MPIO_CNMBR, RASTERVALU, POLY_ID)
sum(nchar(Dist$MPIO_CCNCT) == 4) + sum(nchar(Dist$MPIO_CCNCT) == 5) == length(Dist$MPIO_CCNCT)
df <- transform(Dist, Cod = ifelse(nchar(MPIO_CCNCT)==4, paste0(0,MPIO_CCNCT),MPIO_CCNCT))
df <- transform(df, Cod = as.character(Cod))
df$RASTERVALU <- gsub(",", ".", df$RASTERVALU)
df$RASTERVALU <- as.numeric(df$RASTERVALU)
file <- "/Users/miguel/OneDrive\ -\ Universidad\ del\ rosario/Tec\ Geo/Proyecto/Datos/DistanciaCap.csv"
write.csv(df, file)

