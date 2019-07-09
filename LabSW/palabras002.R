library(tidyverse)
library(tokenizers)
library(tm)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(dplyr)
datos<-read.csv("/home/liacco/NextCloud/FSG_Proyectos/RICHSL/DAB/BDD-CP-software.csv", header = TRUE, sep = ";")
cpcn8 <- datos %>%
  select(CPC_N8, Valor_adjudicado) %>%
  group_by(CPC_N8) %>%
  summarize(sum(Valor_adjudicado)) 
cpcn8<-arrange(cpcn8,desc(`sum(Valor_adjudicado)`))
cpcn8[1,1]

for (i in cpcn8$CPC_N8) {
  datosf <- datos  %>%
    select(CPC_N8,Descripcion_compra) %>%
    filter(CPC_N8 == i)  
  print(i)
  
#datosf<-datos

  lista<-tokenize_words(as.character(datosf$Descripcion_compra))
  corpus<-Corpus(VectorSource(lista))
  d <- tm_map(corpus, removeWords, c("año","ano","anos","mes","meses","sus","como","mil","millon","millones","el","los","la","las", "por","del","para","una","con","que"))
  tdm<-TermDocumentMatrix(d)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  df <- data.frame(palabras = names(v),freq=v)
#  print(df)

 #print(df$freq[1]*0.15)

 png(file=paste("~/Descargas/Nube_", i,".png",sep=""),height=600,width=1200)
# text(x=0.5,y=0.5,i)
  plot.new()
  wordcloud(df$palabras,df$freq, colors=1:20, ramdon.order = FALSE, random.color = FALSE, min.freq=(df$freq[1]*0.10))
 dev.off()
}  





#tabdin<-dcast(datos,CPC_N8~Provincia_Proveedor,sum,value.var="Valor_adjudicado")
#head(tabdin)
#palabras<-findFreqTerms(tdm,lowfreq = 1)
#tabla<-table(palabras[1])
#tabla<-data.frame(palabra = names(tabla), recuento = as.numeric(tabla))
#tabla

