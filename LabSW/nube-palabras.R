install.packages("tidyverse")
install.packages("wordcloud2")
install.packages("wordcloud")
install.packages("tidytext")
library(dplyr)
library(tidyverse)
library(tidytext)
library(tokenizers)
library(tm)
library(wordcloud)
library(wordcloud2)
library(reshape2)

library(ggplot2)
library(lubridate)

# Leer archivo csv con datos de SERCOP. Cargarlo en el objeto datos
datos<-read.csv("/home/liacco/NextCloud/FSG_Proyectos/RICHSL/DAB/BDD-CP-software.csv", 
                header = TRUE, sep = ";")
# Agregar una columna al final con el año extraído del campo Fecha_Adjudicacion
datos<-mutate(datos,anio = year(as.Date(datos$Fecha_Adjudicacion, format = "%d/%m/%Y")))

# Agrupar por CPC_N8, anio y sumarizar por Valor_adjudicado. Enviar el resultado al objeto cpcn8
cpcn8 <- datos %>%
  select(CPC_N8, anio, Valor_adjudicado) %>%
  group_by(CPC_N8, anio) %>%
  summarize(sum(Valor_adjudicado)) 

# Cambiar el nombre de la columna sumarizada por "total"
names(cpcn8)[3] <- "total"
names(cpcn8)

# Ordenar cpcn8 descendentemente por total y anio
cpcn8<-arrange(cpcn8,desc(total), desc(anio))
cpcn8[1,1]

# Crear objeto compra_df con el contenido de CPC_N8 y Descripcion_compra para procesar a posterior el texto, y matenerlo relacionado al CPC_N8
compra_df <- datos %>%
  select(CPC_N8, Descripcion_compra)

# Otra manera de crear el objeto compra_df
compra_df <- data_frame(cpcn8 = datos$CPC_N8, compratext = as.character(datos$Descripcion_compra))

# Segregar el texto del campo compratext en varios registros en una misma columna, de nombre "palabras"
# cada descrición de compra se separa en palabras y cada palabras se convierte en un registro.
palabras_df <- compra_df %>% unnest_tokens(palabras, compratext)
# Crear vector, con palabras que deben eliminarse (excluirse)
stop_words<-data_frame(palabras = 
  c("no", "o","al","e", "un","a", "n","y", "en", "de","año","ano","anos","mes","meses","sus","como","mil","millon","millones","el","los","la","las", "por","del","para","una","con","que"))
# Eliminar las palabras definidas en stop_words
palabras_df <- palabras_df %>% anti_join(stop_words)

# Reemplazar vocales con acento por vocales sin acento
palabras_df$palabras <- chartr("áéíóú", "aeiou", palabras_df$palabras)
palabras_df$palabras<-gsub("^licencias.*", "licencia", palabras_df$palabras)
palabras_df$palabras<-gsub("^licenciami.*", "licencia", palabras_df$palabras)

palabras_df

# Agrupar palabras_df por npcn8 y palabra,  y contar cuantas veces se repite cada palabra.
palabras_cpc_df <- palabras_df %>% 
  group_by(cpcn8) %>%
  count(palabras, sort = TRUE) %>%
  mutate(proporcion = n/sum(n)) 

# Agrupar palabras_df por palabras y contar por cada una cuántas veces se repite
palabras_todas_df <- palabras_df %>% 
  count(palabras, sort = TRUE) %>%
  mutate(proporcion = n/sum(n))

palabras_cpc_df 
palabras_todas_df

palabras_todas_df %>% filter(grepl("^licencia", palabras))


# Generar Nubes con todas las palabras sin importar CPC
png(file=paste("~/Descargas/NubeTodas", "01",".png",sep=""),height=600,width=1200)
plot.new()
wordcloud(palabras_todas_df$palabras,palabras_todas_df$n, colors=20:1, random.order = FALSE, random.color = FALSE, min.freq=(palabras_todas_df$n[1]*0.05))
dev.off()

# Generar Nubes por cada CPC 
for (i in cpcn8$CPC_N8) {
  print(i)
  dft <- palabras_cpc_df %>% filter(cpcn8 == i)
  png(file=paste("~/Descargas/Nube_", i,".png",sep=""),height=600,width=1200)
  plot.new()
  wordcloud(dft$palabras, dft$n, colors=20:1, random.order = FALSE, random.color = FALSE, min.freq=(dft$n[1]*0.05))
  dev.off()
}
cpcn8[1:10,]

# Enviar a un df temporal "dftempo" los 15 primeros registros de cpcn8, es decir, los 15 CPCs con valores más altos de compras.
dftempo<-cpcn8[1:15,] %>%
  select(CPC_N8, total) %>%
  group_by(CPC_N8) %>%
  summarize(sum(total))
names(dftempo)[2] <- "total"
# GRaficar en barras a dftempo
ggplot(data = dftempo) + 
  geom_bar(mapping = aes(x = CPC_N8, y = total, fill = CPC_N8, group = 1, size = CPC_N8),
  stat = "identity",
  show.legend = TRUE,
  width = 1)
cpcn8[1:5,]

ggplot(data = palabras_todas_df[1:10,,]) +
  geom_point(mapping = aes(y = palabras, x = n, size = n),
  stat = "identity",
  show.legend = TRUE,
  width = 1)


c(palabras_todas_df[,1])
write.csv(palabras_todas_df[,1], file = "/home/liacco/NextCloud/FSG_Proyectos/RICHSL/PROYECTOS/DAB/salida.csv")

# Agrupar por proveedor y anio, sumarizar por Valor_adjudicado
df_proveedor_anio <- datos %>%
  select(anio, Razon_Social_Proveedor, Valor_adjudicado) %>%
  group_by(Razon_Social_Proveedor, anio) %>%
  summarize(sum(Valor_adjudicado))
# Cambiar el nombre de la columna sumarizada por "total"
names(df_proveedor_anio)[3] <- "total"
df_proveedor_anio

df_proveedor_nodef <- datos %>% 
  select(Ruc_Proveedor, Razon_Social_Proveedor, Provincia_Proveedor) %>%
  group_by(Razon_Social_Proveedor)
df_proveedor_nodef

# Agrupar por razon social y sumarizar con base a df_proveedor_anio
df_proveedor <- df_proveedor_anio %>%
  select(Razon_Social_Proveedor, total) %>%
  group_by(Razon_Social_Proveedor) %>%
  summarize(sum(total))
# Cambiar nombre de columna sumarizada por "total"
names(df_proveedor)[2] <- "total"
# Ordernar por campo total en descendente.
df_proveedor<-arrange(df_proveedor,desc(total))

write.csv(df_proveedor_nodef, file = "/home/liacco/NextCloud/FSG_Proyectos/RICHSL/PROYECTOS/DAB/salida2.csv")

dftempo<-df_proveedor[1:10,]
ggplot(data = dftempo) + 
  geom_bar(mapping = aes(x = Razon_Social_Proveedor, y = total, fill = Razon_Social_Proveedor, group = 1, size = Razon_Social_Proveedor),
           stat = "identity",
           show.legend = TRUE,
           width = 1)
