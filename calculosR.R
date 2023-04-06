4 +8

20 - 8

4^2

x <- 86
x

Oficina <- 7
Platzi <- 1
Transporte <- 1.5
Tiempo_al_dia <- Oficina + Platzi + Transporte
Tiempo_al_dia

Corte1 <- 0.3
Corte2 <- 0.3
Corte3 <- 0.4

Nota1<- 4.0
Nota2<-4.6
Nota3<-3.0

Notac1 <- Nota1 *Corte1
Notac1

Notac2 <- Nota2 *Corte2
Notac2

Notac3 <- Nota3 *Corte3
Notac3

Nota_Final <- Notac1 + Notac2 + Notac3
Nota_Final

str(mtcars)
class(mtcars$vs) #lo que nos dice aqui no es cierto osea no es numerico

mtcars$vs = as.logical(mtcars$vs) #El igual es la tranformacion DE UNA VARIABLE QUE LA QUEREMOS VER COMO LOGICA
mtcars$am = as.logical(mtcars$am)

str(mtcars)

str(orangeec)

summary(orangeec)

summary(mtcars)

wt <- (mtcars$wt*1000)/2
wt

mtcars.new <- transform(mtcars,wt=wt*1000/2)
mtcars.new

summary(mtcars.new)

tiempo_platzi <- c(25,5,10,15,10)
tiempo_lecturas <- c(30,10,5,10,15)
tiempo_aprendizaje <- tiempo_platzi + tiempo_lecturas
tiempo_aprendizaje

#Vector con caracteres

dias_aprendizaje <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")
dias_aprendizaje

dias_mas_20min <- c(TRUE,FALSE,FALSE,TRUE,TRUE)
dias_mas_20min

total_tiempo_platzi <- sum(tiempo_platzi)
total_tiempo_platzi

total_tiempo_lecturas <- sum(tiempo_lecturas)
total_tiempo_lecturas

total_tiempo_adicional <- total_tiempo_platzi + total_tiempo_lecturas
total_tiempo_adicional

#Matriz
tiempo_matrix <- matrix(c(tiempo_platzi, tiempo_lecturas), 
                        nrow = 2, byrow=TRUE)

dias <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")
Tiempo <- c("Tiempo platzi", "tiempo lecturas")

colnames(tiempo_matrix) <- dias
rownames(tiempo_aprendizaje) < Tiempo

tiempo_matrix
colSums(tiempo_matrix)

final_matrix <- rbind(tiempo_matrix, c(10,15,30,5,0))
final_matrix

colSums(final_matrix)

final_matrix[1,5] #Aqui seleccionamos la fila 1 y la columna 5, que es Viernes 10



#Agregar una columna sabado con sus respectivos minutos

final_matrix <- cbind(final_matrix, Sabado = c(1,2,3))
final_matrix

mtcars[mtcars$cyl<6,]

#Esto es para saber quienes tiene esta variable
orangeec[orangeec$`GDP PC`>= 15000,]

#Para saber quienes tiene menor o igual que 2 PIB per capita desde su economia naranja
orangeec[orangeec$`Creat Ind % GDP`<=2,]

#Identificar datos dentro del dataset
neworangeec <- subset(orangeec, 'Internet penetration % population' > 80
                      & 'Education invest % GDP' >= 4.5,
                      select = 'Creat Ind % GDP')
neworangeec

#Cambiar el nombre a una variable

rename(orangeec,c("Creat.Ind...GDP" = "AporteEcNja"))

#Crear un factor 
Nivel_Curso <- c("Basico", "Intermedio", "Avanzado")
Nivel_Curso

head(mtcars)
head(orangeec)

tail(mtcars)
tail(orangeec)

#Forta de realizar un vistazo del dataset
glimpse(orangeec)



my_vector <- 1:8
my_vector

my_matrix <- matrix(1:9, ncol=3)
my_matrix

#Solamente aparecen los primeros carros
my_df <- mtcars[1:4,]
my_df

#Listas, esta lista contiene todo
my_list <- list(my_vector, my_matrix, my_df)
my_list


numeros <- c(1, 2, 3, 4) 
suma_numeros <- sum(numeros) 
suma_numeros



