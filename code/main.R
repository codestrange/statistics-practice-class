library(rpart)

data <- read.csv("anorectic.csv", row.names = "number") #El ejercicio dice 55 en el .csv son 217, haremos el analisis para todos
#Un detalle es que eliminaremos la columna number pues en si esta es equivalente a un identificador del paciente y no aporta informacion real al problema

#Analicemos entonces la correlacion entre las diferentes variables

cor_data <- cor(data)

symnum(cor_data)

png(filename = "images/pairs.png")
pairs(data)
dev.off()

# Como se pudo observar casi todas las variables estan poco correlacionadas con el resto, debido a la abundancia
# de '.' y ' '. Existe solo un caso de variables muy correlacionadas, la variable "tidi" y "time". De cualquier forma
# la reduccion de dimension es factible.

acp <- prcomp(data, scale. = TRUE) #Analiaaremos todas las variables pues no usaremos esto para una regresion
summary(acp)

png(filename = "images/variances.png")
plot(acp)
dev.off()

# Despues de analisar los valores propios podemos notar que usando usando el criterio del porcentaje o el criterio
# de Kaiser(para valores propios mayores que 1) podemos usar entre 6 y 5 componentes principales. Por simplicidad
# utilizaremos 5.

acp$rotation[,1:5]

#Veamos que definen cada componente
# PC1 Para la primera componente encontramos que exite una precencia de las pacientes con alto peso, (aparece menstruacion pero no se explicarla)
# , que ademas tienen una alta restriccion de comida, que ademas purgan(viene de la traduccion de purge en este contxto qu no domino)
# despues de ingerir alimentos, con hiperactividad, buenas relaciones con familiares y amigos, alta emancipacion de sus familias,
# (desconozco school para que sera), buena actitud y comportamiento sexual, con buen estado de animo, muy preocupadas por su
# alimentacion y peso corporal, con mucha percepcion de su cuerpo, (por ultimo time y tidi tambien pero no logro interpretar a que se refieren).
#
# PC2 La segunda componente esta conformada por un grupo de pacientes con caracteristicas como pocos atracones, poca
# precensia de vomitos, que purgan(ver anterior referencia) poco las comidas, con buenas relaciones familiares, pero
# que fueron diagnosticadas con la enfermedad(asumimos q alto es q sea mas grave), (tambien esta tidi pero no tengo interpretacion).
#
# PC3 La tercera componente presenta pacientes con caracteristicas como buenas relaciones con amigos, (alto school pero sin interpretacion),
# buena actitud sexual, (poco time y poco tidi).
#
# PC4 La cuarta componente muestra pacientes con alta hiperactividad, con emancipacion de sus familias, sin buenas realciones
# con sus amigos, (school alto sin interpretacion), desanimadas, con poco preocupacion por su peso y alimentacion,
# poca percepcion de su cuerpo, (altos time y tidi sin interpretacion), y con poca precensia de la enfermedad.
#
# PC5 La quinta componenete se caracterisa por tener presencia de pacientes con alto peso corporal, pocos atracones,
# malas relaciones familiares, malas relaciones con amistades, buen estado de animo y sin presencia de la enfermedad.
#

# Ahora veamos como se agrupan estos datos usando cluster jerarquicos, kmeans y arbol de decision

data.norm <- as.data.frame(scale(data)) # Normalizamos los datos, aunque podemos obviar esta transformacion. 

clusters <- function(data, k, method = "euclidean"){
  d <- dist(data, method = method)
  fit <- hclust(d, method = "complete")
  
  filename <- paste("images/hier_cluster","_",k,"_",method,".png",sep="")
  png(filename = filename)
  plot(fit)
  rect.hclust(fit, k=k, border = "red")
  dev.off()
}

clusters(data.norm, 4)# Con 4 podemos observar que se forman grandes grupos pero que aun pudiera seguir particionandoce.
clusters(data.norm, 5)# Lo haremos para 5 grupos de datos. Esto a partir de la observacion del dendrograma con 4. Ademas intuitivamente por la cantidad de componentes principales que tomamos.
clusters(data.norm, 6)# Con 6 no se nota un gran cambio con respecto a los grupos formados para 5 por eso trabajaremos con 5 como principal cantidad de conjuntos.
# Lo que si es cierto es que existen una gran cantidad de subtipos y por ende muchos grupos peque�os


k_means <- function(data, k){
  km <- kmeans(data, k)
  
  print(km)
  
  filename <- paste("images/kmeans_cluster","_",k,".png",sep="")
  png(filename = filename)
  plot(data, col=km$cluster)
  dev.off()
}

k_means(data.norm, 4) # Comprobando para 4 la similitude entre elementos del mismo grupo es del 36%, la cual es 
                      # bien baja. Las cantidades en los grupos son de 67, 19, 88 y 43.
k_means(data.norm, 5) # De igual forma usaremos 5 grupos como medidor principal. La similitud es del 41%, la cual 
                      # igualmente no es tan buena. Las cantidades son 28, 85, 47, 19 y 38.
k_means(data.norm, 6) # Para 6 esta similitud alcanza el 44.3%. La diferencia no es substancial con respecto a 5. 
                      # Las cantidades son 37, 71, 27, 18, 36 y 28. De esas cantidades si se puede observar que al
                      # menos los elementos estan mejor distribuidos que con las cantidades de clusteres anteriores.


d_tree <- function(data, var, method, depth){
  n <- length(data[,1])
  
  model <- paste(var, "~ .")
  
  sub <- sample(1:n, 2*n/3)
  
  diag.tree <- rpart(model, data = data[sub,], method = method, cp = 0.01, maxdepth = depth)
  
  summary(diag.tree)
  
  filename <- paste("images/d_tree_cluster","_",var,"_",method,"_",depth,".png",sep="")
  png(filename = filename)
  plot(diag.tree)
  text(diag.tree, use.n = TRUE, pretty = 1, xpd = TRUE)
  dev.off()
  
  plotcp(diag.tree)
  printcp(diag.tree)
  
  diag.pred <- predict(diag.tree, newdata = data[-sub,], type = "vector")
  
  tb <- table(diag.pred, data[-sub,][[var]])
  
  error.rpart <- 1 - (sum(diag(tb))/sum(tb))
  print(tb)
  print(error.rpart)
}

d_tree(data, "diag", "class", 8)# Analisaremos el arbol de decision formado para la variable diag(Diagnostico)
                                # utilizando un metodo CART de clasificacion. En este caso alcanzamos un 40% de
                                # error aproximadamente. El arbol es posible de construir pero no tienve mucho
                                # valor predictivo debido al alto error en un contexto en el cual este no es 
                                # permisible.



