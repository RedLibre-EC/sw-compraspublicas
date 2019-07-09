#Investigación de Network Science
install.packages("igraph") 
library(igraph)          # load a package

data <- read.csv("data/base.csv")
str(data)

#Creo la matriz de incidencia de acuerdo a la Descripcion de la compra y la descripcion del CPCN8
link <-matrix(c(data$DescripcionCompra, data$DescripcionCPCN8), ncol=2)
head.matrix(link)

# creo el grafo
g<-graph_from_edgelist(link)

# elimino posibles loops
g <- simplify(g)
plot(g)

# funcion para obtener panoramica sobre la estructura de la red
function(grafo){
    wtc <- cluster_walktrap(grafo)
    g<-data.frame(
        Indicadores =c("nodos", "aristas", "densidad", "diametro", "long.med.cam",
                       "grado medio", "modularidad", "coefic.cluster"),
        valor=c(vcount(grafo),
                ecount(grafo),
                round(graph.density(grafo), 2),
                diameter(grafo),
                round(average.path.length(grafo),2),
                round(mean(degree(grafo)),2),
                round(modularity(grafo,membership(wtc)),2),
                round(transitivity(grafo),2)
        ))
    return(g)
}

# funcion para determinar numero maximo de relaciones
function(grafo){
    r<-data.frame(relaciones =c(
        "potenciales", "presentes",
        "% presentes"),
        valor =c((vcount(grafo)*(vcount(grafo)-1))/2,
                 ecount(grafo),
                 round(graph.density(grafo)*100,2)
        )
    )
    return(r)
}

par(bg="gray15",mar=c(1,1,1,1))
plot(g,vertex.label=NA, asp=FALSE,
     vertex.size =degree(g)/max(degree(g)),
     vertex.color= "gray90",
     vertex.frame.color= "gray90",
     edge.color="gray55",
     edge.arrow.size=0.6)

plot(g,layout=layout.auto, vertex.size=3,
     edge.width= 0.4, vertex.label.cex =0.78,
     edge.arrow.size=0.4, asp=FALSE,
     edge.color="black")