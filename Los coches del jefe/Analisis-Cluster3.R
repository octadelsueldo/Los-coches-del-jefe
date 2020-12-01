
'''
Introducción. Los métodos jerárquicos frente a los no jerárquicos.
 
 
Existen, como sabemos, dos grandes grupos en los que dividir los métodos de segmentación: jerárquicos y no jerárquicos. 
Los primeros agrupan las observaciones de acuerdo a una métrica preestablecida y generan los grupos en virtud de la similaridad hallada;
no existe un número predeterminado de grupos y es el científico de datos quien, en virtud de su experiencia y de ayudas tanto visuales
(como el dendrograma) como analísticas quien decida el número final con el que segmentar la población. En los segundos, en cambio, el 
científico de datos establece, de acuerdo en general con su experiencia, cuál es el número de grupos con el que proceder a la partición
de la población; exige un conocimiento inicial de la misma, o una decisión de tipo administrativo (los costes de segmentar la población 
en muchos grupos pueden ser elevados). Existen también pruebas que permiten establecer el número adecuado de grupos antes de proceder a 
la segmentación, que establecerá las observaciones en grupos de acuerdo con la minimización del indicador establecido.

Podemos efectuar una clasificación somera de acuerdo con el siguiente esquema:
  
  Métodos jerárquicos aglomerativos (procedimientos hclust, agnes entre otros).
+ Muestran cómo aglomerar en grupos mayores;
+ Las mayores diferencias provienen de la forma de cálculo de las distancias entre clusters (los encadenamientos “simple”, 
“completo” o “promedio”);
+ Es computacionalmente exigente en presencia de grandes volúmenes.

Métodos jerárquicos divisivos (procedimientos diana, mona entre otros).
+ Dividen grandes grupos en otros más pequeños;
+ Interesante cuando buscamos la presencia de algunos grupos mayores;
+ Muy exigente computacionalmente de cara a hallar el número óptimo de grupos.

Métodos no jerárquicos (procedimientos k medias, pam, clara, fanny entre otros).
+ Requieren la determinación a priori del número de grupos;
+ Necesitan de una semilla / grupo inicial;
+ Muchos criterios diferentes para optimizar los clusters, algunos basados en modelos;
+ Pueden construir diferentes grupos de ‘outliers’.


MÉTODOS JERÁRQUICOS
Necesitamos los packages cluster, factoextra (para visualización) y dendextend (para comparar dos dendrogramas), dplyr para 
recodificar niveles de los factores y corrplotpara hacer gráficos de correlaciones.

'''

library(cluster)
library(dendextend)
library(factoextra)
library(dplyr)
library(corrplot)

'''

El cluster jerárquico puede dividirse en aglomerativo (AGNES) y divisivo (DIANA); el primero parte de cada observación por separado y va agrupándolas de acuerdo con medidas de similaridad pre-definidas; el segundo trabaja del grupo globalmente considerado hacia abajo, dividiendo en virtud del grado de heterogeneidad. Aunque pueden emplearse otras, la medida de disimilaridad más empleada es la distancia euclídea. Recordemos las distintas formas de medir la disimilaridad entre observaciones:
  
  Método del centroide;
Método del vecino más próximo o del encadenamiento simple;
Método del vecino más alejado o del encadenamiento completo (1);
Método del encadenamiento promedio;
Método de Ward (2).
y (2) son los más empleados.
Cargamos el archivo churn.arff que contiene las caracerísticas completas de 20.000 usuarios de una compañía de telefonía, de los que algunos abandonaron y otros no. La descripción informal de las variables es la siguiente:
  
  COLLEGE: ¿Tiene educación secundaria?
  INCOME: Ingresos anuales
OVERAGE: Cargos adicionales mensuales (promedio)
LEFTOVER: porcentaje Promedio de minutos sobrantes / no consumidos por mes
HOUSE: Valor de la vivienda (datos del censo)
HANDSET_PRICE: Coste del terminal
OVER_15MINS_CALLS_PER_MONTH: Promedio de llamadas largas (más de 15 min) por mes
AVERAGE_CALL_DURATION: Promedio duración llamada
REPORTED_SATISFACTION: Nivel consignado de satisfacción
REPORTED_USAGE_LEVEL: Nivel de empleo auto-reportado
CONSIDERING_CHANGE_OF_PLAN: ¿Está el consumidor pensando en un cambio de plan?
  LEAVE : Variable grupo: el cliente abandonó (leave) o permaneció (stay)
(Datos provenientes del libro Data Science for Business, de Provost and Fawcett; de acuerdo con la licencia de distribución, este archivo sólo puede emplearse exclusivamente para fines educativos.)

# Para leer archivos tipo arff necesitamos instalar el paquete Rweka

'''
library(RWeka)
churn=read.arff("churn.arff") #siempre que esté el archivo en nuestro directorio de trabajo.

set.seed(123)
churn = churn[,2:10] # 20.000 observaciones
#Comprobamos que no haya valores perdidos
churn.compl= complete.cases(churn) 
head(churn.compl)
## [1] TRUE TRUE TRUE TRUE TRUE TRUE
#Hacemos un muestreo de 1.000 observaciones
churn_mas =churn[sample(1:nrow(churn), 1000,replace=FALSE),] 
summary(churn_mas)
##      INCOME          OVERAGE         LEFTOVER         HOUSE       
##  Min.   : 20013   Min.   :  0.0   Min.   : 0.00   Min.   :150027  
##  1st Qu.: 42185   1st Qu.:  0.0   1st Qu.: 0.00   1st Qu.:267416  
##  Median : 74412   Median : 57.0   Median :14.00   Median :453974  
##  Mean   : 79976   Mean   : 85.0   Mean   :23.14   Mean   :492340  
##  3rd Qu.:114435   3rd Qu.:179.2   3rd Qu.:38.00   3rd Qu.:697089  
##  Max.   :159926   Max.   :295.0   Max.   :89.00   Max.   :997956  
##  HANDSET_PRICE   OVER_15MINS_CALLS_PER_MONTH AVERAGE_CALL_DURATION
##  Min.   :130.0   Min.   : 0.000              Min.   : 1.000       
##  1st Qu.:216.0   1st Qu.: 1.000              1st Qu.: 2.000       
##  Median :321.0   Median : 4.000              Median : 5.000       
##  Mean   :383.2   Mean   : 7.797              Mean   : 6.163       
##  3rd Qu.:511.2   3rd Qu.:14.000              3rd Qu.:10.000       
##  Max.   :898.0   Max.   :29.000              Max.   :15.000       
##  REPORTED_SATISFACTION  REPORTED_USAGE_LEVEL
##  very_unsat:406        very_little:198      
##  unsat     :209        little     :379      
##  avg       : 95        avg        : 63      
##  sat       : 64        high       : 82      
##  very_sat  :226        very_high  :278      
## 

#Y generamos los estadísticos de posición y dispersión habituales:
  
  churn_mas_stats = data.frame(
    Min = apply(churn_mas[,1:7], 2, min), # mínimo
    Med = apply(churn_mas[,1:7], 2, median), # mediana
    Mean = apply(churn_mas[,1:7], 2, mean), # media
    SD = apply(churn_mas[,1:7], 2, sd), # desviación típica
    Max = apply(churn_mas[,1:7], 2, max) # máximo
  )
churn_mas_stats = round(churn_mas_stats, 1)
head(churn_mas_stats)



# Procedemos a la tipificación de las 7 variables en escala métrica.

churn_tip=scale(churn_mas[,1:7])

'''
# Podemos ya proceder a la segmentación mediante el procedimiento jerárquico. Empleamos las 7 primeras variables por ahora. Tradicionalmente, y debido a la característica general de todas las métricas, sólo se han empleado variables métricas para el jerárquico, pero luego veremos cómo podemos resolver esta cuestión.

# La función hclust() de stats tiene el siguiente formato:
  
hclust(dist, method = “complete”)


dist es la matriz de disimilaridades creada por la función dist(), por lo que las variables (por ahora) deben ser métricas;
method hace referencia al método, que puede ser complete, single, average, ward.D, ward.D2, mcquitty, centroid o median.
Empleamos dist() con métrica euclídea para generar la matriz de disimilaridades:
  
  (La versión original ward.D no implementa correctamente el criterio de Ward, salvo que se eleven al cuadrado las distancias euclídeas proporcionadas por dist() antes de introducirlas en hclust(); por esa razón es mejor emplear ward.D2)

'''
churn_tip=as.data.frame(churn_tip)
d = dist(churn_tip, method = "euclidean")
# Aplicamos el método de Ward para llevar a cabo la segmentación:
  
  churn.hc = hclust(d, method = "ward.D2" )

# Representamos ahora el dendrograma, y rodeamos los grupos creados con un rectángulo de color:
  
  plot(churn.hc, cex = 0.6, hang = -1, main="Dendrograma - hclust", xlab="Cluster jerárquico, método de Ward")
rect.hclust(churn.hc, k=3, border = 2:4)


# Dividimos en tres grupos a la vista del grafo anterior:
  
  grp = cutree(churn.hc, k = 3)
# Y observamos el número de elementos de cada segmento:
  
  table(grp)
## grp
##   1   2   3 
## 354 471 175

#En caso de tener menos observaciones y de querer identificar quiénes pertenecen a cada grupo (aquí, el 1), haríamos

# rownames(churn_tip)[grp==1]

# Finalmente visualizamos los grupos con factoextra:
  
  #library(factoextra) #cargamos si no lo extuviese
fviz_cluster(list(data = churn_tip, cluster = grp), stand = FALSE, geom = "point", pointsize = 1,
               main="Clusters generados - hclust")

'''
Podemos observar que los grupos 1 y 3 presentan bastante solapamiento.

Identificación de las diferencias entre los clusters

Lo aplicamos sobre el objeto churn_mas para poder efectuar una tabla sobre las variables categóricas, que nos devolverá, para cada uno de los integrantes del grupo 1, su nivel de satisfacción reportado

'''
churn_mas$REPORTED_SATISFACTION[grp == 1] 

# Las siguientes tablas muestran la relación de la satisfacción reportada con tener educación secundaria.

for(i in 1:3){
  print(table(churn_mas$REPORTED_SATISFACTION[grp == i], churn_mas$COLLEGE[grp == i]))
}

# Para conocer el grado de satisfacción reportado por cada inviduo de cada grupo (en churn_mas)

sapply(unique(grp),function(g)churn_mas$REPORTED_SATISFACTION[grp == g])

# Finalmente, construimos la distribución de frecuencias asociada, mediante un simple recuento:
  
RS.t=table(grp, churn_mas$REPORTED_SATISFACTION)
# de la que reordenamos el nivel de satisfacción en forma creciente en vez de alfabética,

levels(churn_mas$REPORTED_SATISFACTION)
## [1] "very_unsat" "unsat"      "avg"        "sat"        "very_sat"
churn_mas$REPORTED_SATISFACTION=factor(churn_mas$REPORTED_SATISFACTION,
                                       levels =  c("very_unsat", "unsat", 
                                                   "avg", "sat", "very_sat"))
# para así rehacer la tabla

RS.t=table(grp, churn_mas$REPORTED_SATISFACTION)
RS.t
##    
## grp very_unsat unsat avg sat very_sat
##   1        161    64  26  22       81
##   2        183   100  53  33      102
##   3         62    45  16   9       43
# La proporción de cada nivel de satisfacción reportado en función del grupo de pertenencia lo obtenemos así:
  
  round(prop.table(RS.t,1)*100,2)
##    
## grp very_unsat unsat   avg   sat very_sat
##   1      45.48 18.08  7.34  6.21    22.88
##   2      38.85 21.23 11.25  7.01    21.66
##   3      35.43 25.71  9.14  5.14    24.57
# Y la proporción del grupo dentro de cada nivel de satisfacción reportado, de este otro modo:
  
  round(prop.table(RS.t,2)*100 , 2)
##    
## grp very_unsat unsat   avg   sat very_sat
##   1      39.66 30.62 27.37 34.38    35.84
##   2      45.07 47.85 55.79 51.56    45.13
##   3      15.27 21.53 16.84 14.06    19.03
# Si quisiéramos dividir entre satisfechos e instasfechos, duplicamos la variable…

churn_mas$Satisf = churn_mas$REPORTED_SATISFACTION
# y agregamos niveles, creando los nuevos mediante agregación de los anteriores con el recode de dplyr:
  
  levels(churn_mas$REPORTED_SATISFACTION)
## [1] "very_unsat" "unsat"      "avg"        "sat"        "very_sat"
churn_mas = churn_mas %>%
  mutate(Satisf = recode(Satisf, 
                         very_unsat = "Insatisfecho",
                         unsat = "Insatisfecho",
                         avg = "Ni satisfecho ni insatisfecho",
                         sat = "Satisfecho",
                         very_sat="Satisfecho"))

levels(churn_mas$Satisf)
## [1] "Insatisfecho"                  "Ni satisfecho ni insatisfecho"
## [3] "Satisfecho"
# Rehacemos ahora la tabla,

RS.t2=table(grp, churn_mas$Satisf)
RS.t2
##    
## grp Insatisfecho Ni satisfecho ni insatisfecho Satisfecho
##   1          225                            26        103
##   2          283                            53        135
##   3          107                            16         52
#La proporción de cada nivel de satisfacción reportado en función del grupo de pertenencia 
# lo obtenemos así:
round(prop.table(RS.t2,1)*100,2)
##    
## grp Insatisfecho Ni satisfecho ni insatisfecho Satisfecho
##   1        63.56                          7.34      29.10
##   2        60.08                         11.25      28.66
##   3        61.14                          9.14      29.71
#Y la proporción del grupo dentro de cada nivel de satisfacción reportado de este modo:
round(prop.table(RS.t2,2)*100 , 2)
##    
## grp Insatisfecho Ni satisfecho ni insatisfecho Satisfecho
##   1        36.59                         27.37      35.52
##   2        46.02                         55.79      46.55
##   3        17.40                         16.84      17.93
# Nos centramos ahora en las diferencias entre las medianas y las medias de los grupos

aggregate(churn_tip,list(grp),median) #sobre los datos tipificados

aggregate(churn_mas[,2:7],list(grp),median) #sobre los datos sin tipificar

aggregate(churn_tip,list(grp),mean)

aggregate(churn_mas[,2:7],list(grp),mean)   

'''
En ocasiones, los valores extremos pueden hacer que tanto la media como la desviación típica se sesguen; en tales casos, conviene trabajar con los valores tipificados empleando como medida de posición la mediana y como medida de dispersión la desviación absoluta mediana, MAD por sus siglas en inglés. MAD no es sino la mediana de las desviaciones, en valor absoluto, respecto de la mediana:
  
  MAD=mediana(|xi−mediana(X)|)


Es un indicador de dispersión robusto, pues no se ve afectado por valores extremos.

Las funciones AGNES y DIANA
Las funciones agnes() y diana() del paquete cluster permiten llevar a cabo segmentaciones aglomerativas y divisivas, respectivamente.

Agglomerative Nesting (AGNES, anidación aglomerativa).

agnes(datos, metric = “euclidean”, stand = FALSE, method = “average”)
Frente al tradicional hclust(), agnes() ofrece ciertas características como son

devolver un coeficiente de aglomeración que mide la estructura de segmentación;
representar la segmentación de forma alternativa al dendrograma (banner).
DIvisive ANAlysis Clustering (DIANA; segmentación divisiva).

diana(datos, metric = “euclidean”, stand = FALSE)

Los procedimientos comparten las siguientes órdenes:
  
  datos es un data frame, una matriz de datos o incluso una matriz de disimilaridades; las observaciones en los dos primeros casos suelen venir dadas por filas, mientras que de ser una matriz de disimilaridades suele tratarse del resultado de aplicar un daisy() o un dist();
**metric*}** hace referencia, como es fácil de entender, a la métrica empleada en el cálculo de las distancias; puede ser “euclidean” o “mahattan”;
stand, valor lógico; si TRUE se estandarizan las variables antes de proceder al cálculo de las distancias;
method se refiere al método empleado en la segmentación aglomerativa jerárquica; pueden ser lo habituales “average”, “single”, “complete”, “ward”.
Las funciones agnes() y diana() devuelven un objeto de clase agnes y diana, respectivamente, que permiten operar con las funciones print(), summary(), plot(), pltree(), as.dendrogram(), as.hclust() y cutree().
'''

## AGNES
# library("cluster") # si no estuviese cargada
# Cálculo
churn.agnes = agnes(churn_tip, method = "ward")
# Coeficiente de aglomeración
churn.agnes$ac
## [1] 0.9853958
# Representamos el árbol mediante la función pltree():
  
  pltree(churn.agnes, cex = 0.6, hang = -1, main = "Dendrograma - AGNES") 
rect.hclust(churn.agnes, k = 3, border = 2:4)
# Como permite apreciar el gráfico, los perfiles son totalmente distintos, sugiriendo la posibilidad de grupos frente a la distribución aleatoria de los datos.

# Dividimos en tres grupos,

grp.ag = cutree(as.hclust(churn.agnes), k = 3)
# y determinamos el número de elementos de cada grupo:
  
  table(grp.ag)
## grp.ag
##   1   2   3 
## 354 471 175
# En caso de tener menos observaciones y de querer identificar quiénes pertenecen a cada grupo, tal y como vimos anteriormente, haríamos

rownames(churn_tip)[grp==1]
## DIANA
# library("cluster") # si no estuviese cargada
# Cálculo
churn.diana = diana(churn_tip)
#coeficiente divisivo
churn.diana$dc
## [1] 0.8797645
# Como anteriormente, representamos el árbol mediante la función pltree(), y rodeamos con un rectángulo de color:
  
  pltree(churn.diana, cex = 0.6, hang = -1, main = "Dendrograma - AGNES") 
rect.hclust(churn.diana, k = 3, border = 2:4)


# Dividimos de nuevo en tres grupos,

grp.diana = cutree(as.hclust(churn.diana), k = 3)
# y determinamos el número de elementos en cada grupo:
  
  table(grp.diana)
## grp.diana
##   1   2   3 
## 336 443 221
# Cluster jerárquico para distancias basadas en correlaciones.
# El análisis cluster jerárquico emplea fundamentalmente las métricas euclídea o euclídea al cuadrado; sin embargo, es posible emplear también medidas de distancia basadas en correlaciones.En primer lugar se calculará la matriz de correlaciones mediante cor(), pudiendo efectuarse para correlaciones de Pearson, Spearman o Kendall; a continuación se procederá a convertir esta matriz en una de distancias que servirá de base para un cluster jerárquico.

# calculamos la matriz de correlaciones
hc.cor = cor(t(churn_tip), method = "pearson") 
# la transformamos en matriz de distancias
d.cor = as.dist(1 - hc.cor) 
# la usamos como base para el hcluster
plot(hclust(d.cor, method = "ward.D2"), cex = 0.6) 


# Comparación de dos dendrogramas.
# El package dendextend permite efectuar comparaciones de dos dendrogramas a través de distintas funciones, como dend_diff(), tanglegram(), entanglement(), all.equal.dendrogram(), cor.dendlist(). A efectos de visualización emplearemos el subconjunto de las primeras 25 observaciones de nuestra muestra (ya eran aleatorias, así que no cambia nada).

churn_tip.m=churn_tip[1:25,]
# Llevaremos a cabo dos AC, uno empleando el método de Ward y otro el del encadenamiento promedio, y compararemos los resultados.

#library(dendextend) #si no estuviese cargada

# Cálculo de la matriz de distancias
churn.dist = dist(churn_tip.m, method = "euclidean")
# Cálculo del jerárquico por Ward y promedio:
  
  cj1 = hclust(churn.dist, method = "average")
cj2 = hclust(churn.dist, method = "ward.D2")
# Hacemos los dendrogramas,

dend1 = as.dendrogram (cj1)
dend2 = as.dendrogram (cj2)
# y creamos una lista con ellos:
  
  dend_list = dendlist(dend1, dend2)
# Tanglegrama.
# El tanglegrama o diagrama de enmarañamiento permite la comparación de grafos de árboles de forma sencilla. Presenta los dos dendrogramas uno frente al otro con las observaciones conectadas mediante líneas. Los nodos únicos, no presentes en el otro dendrograma / segmentación, se presentan mediante líneas punteadas.
tanglegram(dend1, dend2)


# Podemos modificar las opciones, y asimismo calcular la calidad de la alineación mediante la función de enmañaranamiento, entanglement(); un buen alineamiento se asocia a valores bajos del coeficiente, siempre entre 0 y 1.

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Omite las líneas punteadas
           common_subtrees_color_lines = FALSE, # Omite las líneas de colores
           common_subtrees_color_branches = TRUE, # Colorea las ramas comunes
           main = paste("Entanglement =", round(entanglement(dend_list), 2))
)


#Correlaciones entre dendrogramas.
# Podemos medir la asociación entre árboles de segmentación a través de los coeficientes de correlación de Baker (Gamma de Baker) o la cofenética. Ambos coeficientes de correlación se emplean para medir la asociación entre dos árboles procedentes de un cluster jerárquico; del primero puede obtenerse información aquí y del segundo aquí. La función cor.dendlist() de dendextend permite calcularlas.

require(dendextend)

# Matriz de correlación de Baker
cor.dendlist(dend_list, method = "baker")
##           [,1]      [,2]
## [1,] 1.0000000 0.9910372
## [2,] 0.9910372 1.0000000
# o para el coefieciente de correlación simplemente
cor_bakers_gamma(dend1, dend2)
## [1] 0.9910372
#  Matriz de correlación Cofenética
cor.dendlist(dend_list, method = "cophenetic")
##           [,1]      [,2]
## [1,] 1.0000000 0.9690459
## [2,] 0.9690459 1.0000000
# o para el coefieciente de correlación simplemente
cor_cophenetic(dend1, dend2)
## [1] 0.9690459
# Podemos asimismo comparar múltiples dendrogramas encadenándolos del modo siguiente:
  
  dend1 = churn_tip.m %>% dist %>% hclust("com") %>% as.dendrogram
dend2 = churn_tip.m %>% dist %>% hclust("single") %>% as.dendrogram
dend3 = churn_tip.m %>% dist %>% hclust("ave") %>% as.dendrogram
dend4 = churn_tip.m %>% dist %>% hclust("centroid") %>% as.dendrogram
#El cálculo de la matriz de correlaciones:
  
  dend_list = dendlist("Complete" = dend1, "Single" = dend2, "Average" = dend3, "Centroid" = dend4)
correl = cor.dendlist(dend_list)

round(correl, 2)
##          Complete Single Average Centroid
## Complete     1.00   0.63    0.90     0.42
## Single       0.63   1.00    0.80     0.59
## Average      0.90   0.80    1.00     0.44
## Centroid     0.42   0.59    0.44     1.00
# Como siempre, podemos vizualizarla mediante la función corrplot() del package corrplot:
  
  library(corrplot)
corrplot(correl, "pie", "lower")
