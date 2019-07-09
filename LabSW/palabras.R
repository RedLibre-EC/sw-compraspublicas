library(tidyverse)
library(tokenizers)
library(tm)
library(wordcloud)
library(reshape2)
library(dplyr)
datos<-read.csv("/home/liacco/NextCloud/FSG_Proyectos/RICHSL/PROYECTOS/DAB/BDD-CP-software.csv", header = TRUE, sep = ";")
cpcn8 <- datos %>%
  select(CPC_N8, Valor_adjudicado) %>%
  group_by(CPC_N8) %>%
  summarize(sum(Valor_adjudicado)) 
cpcn8<-arrange(cpcn8,desc(`sum(Valor_adjudicado)`))
cpcn8[1,1]

datosf <- datos  %>%
  select(CPC_N8,Descripcion_compra) %>%
  filter(CPC_N8 == cpcn8$CPC_N8[1:4])

datosf<-datos

lista<-tokenize_words(as.character(datosf$Descripcion_compra))
corpus<-Corpus(VectorSource(lista))
d <- tm_map(corpus, removeWords, c("año","ano","anos","mes","meses","sus","como","mil","millon","millones","el","los","la","las", "por","del","para","una","con","que"))
tdm<-TermDocumentMatrix(d)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(palabras = names(v),freq=v)
print(df)
wordcloud(df$palabras,df$freq,colors=1:20, ramdon.order = FALSE, random.color = TRUE, min.freq=200)





#tabdin<-dcast(datos,CPC_N8~Provincia_Proveedor,sum,value.var="Valor_adjudicado")
#head(tabdin)
#palabras<-findFreqTerms(tdm,lowfreq = 1)
#tabla<-table(palabras[1])
#tabla<-data.frame(palabra = names(tabla), recuento = as.numeric(tabla))
#tabla

