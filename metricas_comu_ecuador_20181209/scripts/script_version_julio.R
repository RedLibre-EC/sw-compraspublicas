#Investigación de Network Science
install.packages("janitor")
install.packages("visNetwork")
install.packages("here")
# importacion librerias ---------------------------------------------------

library(here) # para tema de ubicaciones de archivos # install.packages("here") 
library(readr) # para una version mejorada de lectura de archivos # install.packages("readr") 
library(dplyr) # para manejo mas " facil "de los los datos # install.packages("dplyr") 
library(igraph)# install.packages("igraph") 
library(janitor)# para limpieza de los nombres de la tabla # install.packages("janitor")
library(visNetwork) # para visualizacion interactiva de grafos # install.packages("visNetwork")


# exploracion datos inicial -----------------------------------------------

ubicacion_archivo <- here::here("data","base.csv")
data <- readr::read_csv(ubicacion_archivo) %>% 
    janitor::clean_names() # esto para que cambie los nombres del dataframe a snake_case

dplyr::glimpse(data) # para ver la estructura y demo de los datos que se encuentran


# armado del grafo --------------------------------------------------------

#Creo la matriz de incidencia de acuerdo a la Descripcion de la compra y la descripcion del CPCN8
# link <-matrix(c(data$DescripcionCompra, data$DescripcionCPCN8), ncol=2)
# head.matrix(link)
link <- data %>% 
    dplyr::select(descripcion_compra,descripcion_cpcn8) # selecciono las 2 columnas del dato.

# CUIDADO TENES MUCHOS NA ! 
link %>% 
    filter(!complete.cases(link)) %>%  # mostrame los datos incompletos
    count(descripcion_compra,descripcion_cpcn8) # conta cuantos grupos segun las 2 columnas

# para practicidad mia le saco los NA;
link_sin_na <- link %>% 
    filter(complete.cases(link))# dame solo los casos completos 
# esto lo tenes que revisar bien, porque son NA, o si aplicas esto decir 
# se excluyeron tantos casos que no tienen completas las descripciones de compra o de cpcn8

# cuidado cuando te basas en descripciones para armar los links , 
# asegurate que no existan errores en los factores ej: 
# "SOFTWARE PARA EL DPTO. INFORMATICO" 
# no es lo mismo que "SOFTWARE PARA EL DEPARTAMENTO INFORMATICO"
# eso te lo dejo a vos , aca voy a basarme en lo que hiciste.

# creo el grafo
g <- igraph::graph_from_data_frame(link_sin_na)
# consideracion: a la hora de armar el grafo considera si necesitas que sea dirigido o no.
# si no hace falta, no pongas dirigido, 
# dado que te puede duplicar la cantidad de aristas

# elimino posibles loops
g <- simplify(g)
# plot(g) # lo saque porque no se entiende

# esto para que quede numeros y le quede el nombre como propiedad
# igraph::vertex_attr_names(g)
igraph::V(g)$nombre <- igraph::V(g)$name

# agrego un indice 
# de 1 a N ; en vez de hacer 1:N porque puede traer problemas esa notacion.
n_index <- length(igraph::V(g)$name)
igraph::V(g)$name <- seq(n_index) 


# visualizacion del grafo inicial -----------------------------------------

plot(g) 


# vistazo a la estructura -------------------------------------------------

# definicion de la funcion
# funcion para obtener panoramica sobre la estructura de la red
estructura_red <- function(grafo){
    wtc <- cluster_walktrap(grafo)
    estructura_g <-data.frame(
        indicadores =c("nodos", "aristas", "densidad", "diametro", "long.med.cam",
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
    # no hace falta poner return, la funcion devuelve lo ultimo que pongas
    estructura_g 
}

# invocacion de la funcion
estructura_red(g)

# CUIDADO!: sale advertencia cuando ejecutas la parte de estructura: 
# Modularity is implemented for undirected graphs only.
# sale cuando ejecutas el codigo, eso significa, que la modularidad solo esta implementada para grafos no dirigidos ( sin las flechitas )

# maximo nro de relaciones ------------------------------------------------

# funcion para determinar numero maximo de relaciones
maximo_relaciones <- function(grafo){
    relaciones_resultado <-data.frame(relaciones =c(
        "potenciales", "presentes",
        "% presentes"),
        valor =c((vcount(grafo)*(vcount(grafo)-1))/2,
                 ecount(grafo),
                 round(graph.density(grafo)*100,2)
        )
    )
    relaciones_resultado
}

maximo_relaciones(g)

# plots  -------------------------------------------------------------


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


# Algunos extra (JULIO ) --------------------------------------------------

# aca lo armo no dirigido
grafo_no_dirigido <- igraph::graph_from_data_frame(link_sin_na,directed = FALSE)

# elimino posibles loops
grafo_no_dirigido <- simplify(grafo_no_dirigido)
# plot(g) # lo saque porque no se entiende

# esto para que quede numeros y le quede el nombre como propiedad
# igraph::vertex_attr_names(g)
igraph::V(grafo_no_dirigido)$nombre <- igraph::V(grafo_no_dirigido)$name
igraph::V(grafo_no_dirigido)$label <- igraph::V(grafo_no_dirigido)$nombre
igraph::V(grafo_no_dirigido)$title <- igraph::V(grafo_no_dirigido)$nombre

# head(igraph::V(grafo_no_dirigido)$name)
# head(igraph::V(grafo_no_dirigido)$nombre)
# head(igraph::V(grafo_no_dirigido)$label)
# head(igraph::V(grafo_no_dirigido)$title)

# agrego un indice 
# de 1 a N ; en vez de hacer 1:N porque puede traer problemas esa notacion.
n_index <- length(igraph::V(grafo_no_dirigido)$name)
igraph::V(grafo_no_dirigido)$name <- seq(n_index) 



# cuidado se puede poner MUY pesado para armar
# tal vez te conviene usar un subgrafo, 
# ej: por comunidaded que sale, o por componentes
# o algun criterio de filtrado al armar el conjunto de datos.
# no ver todo el grafo junto sino una parte del mismo

# visualizacion interactiva -- TODO ---------------------------------------

# para que siempre se muestre de la misma forma
semilla <- 12345
# CUIDADO AL EJECUTAR; SI LE MANDAS TODO ES PESADO!
visNetwork:::visIgraph(grafo_no_dirigido,
                       idToLabel = FALSE,
                       randomSeed = semilla) %>% 
    visNetwork:::visNodes(size = 10) %>%
    visNetwork:::visIgraphLayout(
        randomSeed = semilla, # para que se muestre de la misma forma
        layout="layout.auto") %>% 
    visNetwork:::visOptions( 
        selectedBy= list(variable = "label"), # esto hace aparecer combos en la red.
        highlightNearest = list(enabled = TRUE, hover = TRUE)
        
        #nodesIdSelection = list(useLabels=TRUE) 
    )

# vista de la estructura y maximo de relaciones en grafo no dirigi --------


estructura_red(grafo_no_dirigido)
maximo_relaciones(grafo_no_dirigido)

# version acotada por componentes  ---------------------------------------------------------

# lo restrinjo a un componente
# los componentes son los " islotes " que se arman de  nodos separados
componentes <- igraph::components(grafo_no_dirigido)
# cuantos hay por cada componente.
# analisis del componente mayor es una de las cosas que comunmente se hace.
# sin embargo depende de lo que analices. 
# ( ej te interes ael principal , o te interesa lo que no esta en el principal )
# o te interesa tratar de entender porque estan separados

# aca ves cuantos hay en cada componente
table(componentes$membership)


# aca ves que nodo esta en que componente
# componentes$membership
# aca le asigno el componente como propiedad del grafo
igraph::V(grafo_no_dirigido)$componente <- componentes$membership

# esto para validar que se hayan asignado bien
# agarre uno aleatorio para fijarme 
# uno que no este en el componente principal
igraph::V(grafo_no_dirigido)[igraph::V(grafo_no_dirigido)$name==986]$componente


# visualizacion un componente pequeño ------------------------------------

# descompongo el grafo en sus componentes
subgrafos_componentes <- igraph::decompose.graph(grafo_no_dirigido)

# agarro el componente 6, que vi antes que era uno pequeño
subgrafo_componente_chico <- subgrafos_componentes[[6]]

# me aseguro que el componente sea el mimsmo
# igraph::V(subgrafo_componente_chico)$componente

# para que siempre se muestre de la misma forma
semilla <- 12345
visNetwork:::visIgraph(subgrafo_componente_chico,
                       idToLabel = FALSE,
                       randomSeed = semilla) %>% 
    visNetwork:::visNodes(size = 10) %>%
    visNetwork:::visIgraphLayout(
        randomSeed = semilla, # para que se muestre de la misma forma
        layout="layout.auto") %>% 
    visNetwork:::visOptions( 
        selectedBy= list(variable = "label"), # esto hace aparecer combos en la red.
        highlightNearest = list(enabled = TRUE, hover = TRUE)
        
        #nodesIdSelection = list(useLabels=TRUE) 
    )


# extracto acotado a alguna comunidad -------------------------------------
# asi como sacas po rcomponentes podrias sacar por comunidades

# para que siempre se ejecute el algoritmo de la misma forma
semilla <- 12345
# divido en comunidades por el algoritmo cluster_walktrap
comunidades_actual_wt <- igraph::cluster_walktrap(grafo_no_dirigido)

# me fijo cuantos hay en cada comunidad
table(comunidades_actual_wt$membership)

# extraigo una comunidad especifica para visualizar
comunidad_especifica <- igraph::groups(comunidades_actual_wt)[[1]]


subgrafo_comunidad_especifica <- igraph::induced_subgraph(grafo_no_dirigido,comunidad_especifica) 
# valido que sea la comunidad 1
vcount(subgrafo_comunidad_especifica)

# para que siempre se muestre de la misma forma
semilla <- 12345
visNetwork:::visIgraph(subgrafo_comunidad_especifica,
                       idToLabel = FALSE,
                       randomSeed = semilla) %>% 
    visNetwork:::visNodes(size = 10) %>%
    visNetwork:::visIgraphLayout(
        randomSeed = semilla, # para que se muestre de la misma forma
        layout="layout.auto") %>% 
    visNetwork:::visOptions( 
        selectedBy= list(variable = "label"), # esto hace aparecer combos en la red.
        highlightNearest = list(enabled = TRUE, hover = TRUE)
        
        #nodesIdSelection = list(useLabels=TRUE) 
    )


estructura_red(subgrafo_comunidad_especifica)
maximo_relaciones(subgrafo_comunidad_especifica)