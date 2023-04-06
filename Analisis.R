#EDA Scatter plot mtcars. Ejemplo de un scatter plo
plot(mtcars$mpg ~ mtcars$cyl,
     xlab= "cilindros", ylab = "millas por galon",
     main= "Relacion cilindro y millas por galon")

plot(mtcars$mpg ~ mtcars$hp,
     xlab= "caballos de fuerza ", ylab = "millas por galon",
     main= "Relacion caballos de fuerza y millas por galon")

#EDA orangeec
plot(orangeec$Unemployment ~ orangeec$`Education invest % GDP`,
     xlab= "Inversion educacion (%PIB)",
     ylab= "Desempleo",
     main= "Relacion inversion en educacion y desempleo")

plot(orangeec$`GDP PC` ~ orangeec$`Creat Ind % GDP`,
     xlab= "Aporte economia naranja al PIB(%))",
     ylab= "PIB per capita",
     main= "Relacion economia naranja y pib per capita")

#histogramas mtcars qplot
hist(mtcars$hp,
      geom="histogram",
      xlab="caballos de fuerza",
      main = " Carros segun caballos de fuerza")
#Son los mismos
ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="Caballos de fuerza", y = "Cantidad de carros",
       tittle= "Caballos de fuerza en carros selecionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())


ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)+
  labs(x="Caballos de fuerza", y = "Cantidad de carros",
       tittle= "Caballos de fuerza en carros selecionados")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#histograma con colores
ggplot()+ geom_histogram(data=mtcars,
                         aes(x=hp),fill="blue",color="red",
                         binwidth = 20)+
  labs(x="Caballos de fuerza", y = "Cantidad de carros",
       tittle= "Caballos de fuerza en carros selecionados")+
  xlim(c(80,280))+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), panel.grid.major =  element_blank(),
        panel.grid.minor = element_blank())

#hist orangeec

ggplot()+geom_histogram(data=orangeec,
                         aes(x=orangeec$`GDP PC`),fill="blue",color="red",
                         binwidth = 2000)+
  labs(x="pib per capita", y = "Cantidad de paises",
       tittle= "PIB per capita en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#-----------------------------------------------------------------------
ggplot()+geom_histogram(data=orangeec,
                        aes(x=orangeec$`Creat Ind % GDP`),fill="blue",color="red",
                        binwidth = 1)+
  labs(x="Aporte economia naranja al pib (%)", y = "Cantidad de paises",
       tittle= "Contribucion economia naranja al pib en paises en latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#----------------------------------------------------------------------
ggplot()+geom_histogram(data=orangeec,
                        aes(x=`Internet penetration % population`),fill="red",color="yellow",
                        binwidth = 5)+
  labs(x="Penetracion internet (%) poblacion ", y = "Cantidad de paises",
       tittle= "Penetracion de internet en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#BOXPLOTS
boxplot(mtcars$hp,
        ylab="Caballos de fuerza",
        main="Caballos de fuerza en carros mtcars")

#-------------------------------------------------------------------
#Se puso as.factor para que lo tome como etiqueta o factor
ggplot(mtcars, aes(x=as.factor(cyl), y=hp, fill=cyl))+
  #Se puso el alpha para aclarar los colores
  geom_boxplot(alpha=0.6)+
  labs(x="cilindros", y= "caballos de fuerza",
       tittle = "Caballos de fuerza segun cilindros en mtcars")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#----------------------------------------------------------------------------
ggplot(mtcars,aes(x=am, y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="Tipo de caja", y="millas por galon",
       tittle= "Millas por galon segun tipo de caja")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Se realiza un cambio en la grafica para poder ver que es Manual(Falso) Y Automatico(Verdadero)
mtcars$am <- factor(mtcars$am, levels=c(TRUE,FALSE),
                    labels=c("Manual", "Automatico"))

#
economy <- mean(orangeec$`GDP PC`)
economy

#pip(%) es del paquete dplyr, significa que ese dataset va pasar a mutate
#mutate es una funcion que hace que mute o cambie el dataset. Va agregar una nueva columna en el dataset
orangeec <- orangeec %>%
  #nombre de la variable 
  #El = en R es como el == en otro lenguaje
  #Esto significa que se va llamar depende de que el pib per capita
  mutate(Strong_economy = ifelse(`GDP PC`< economy,
                    "Por debajo promedio pib per capita",
                    #De lo contrario dice esto
                    "Sobre-Arriba promedio pib per capita ") )

#------------------------------------------------------------------------------
ggplot(orangeec, aes(x=Strong_economy, y=orangeec$`Creat Ind % GDP`,
                     fill= Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pais", y= "Aporte economia naranja al pib",
       tittle= "Aporte economia naranja en pib paises latam con
       alto y bajo pib per capita")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#--------------------------------------------------------------------

ggplot(orangeec, aes(x=Strong_economy, y=orangeec$`Internet penetration % population`,
                     fill= Strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pais", y= "Penetracion de internet(%)",
       tittle= "Penetracion de internet en paises latam con
       alto y bajo pib per capita")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#SCATTER PLOTS con ggplot en mtcars - dos variables
ggplot(mtcars, aes(hp, mpg))+
  geom_point()+
  labs(x="caballos de fuerza", y= "millas por galon",
       tittle = "Relacion caballos de fuerza y millas por galon")
theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#------------------------------------------------------
ggplot(mtcars, aes(wt, hp))+
  geom_point()+
  labs(x="peso", y= "potencia",
       tittle = "Relacion peso-potencia")
theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#---------------------------------------------------------------------------
#*Relacion de cuatro variables en el mismo scatter plot o grafica de dispersion*
#Aqui hubo un error que se tiene que volver a importa el dataset

ggplot(mtcars, aes(hp,qsec))+
  #Se quiere que el color de las burbjujas este segun la caja del carro(am)
  #Tambien se quiere que se size este segun los cilindros 
  geom_point(aes(color=am, size=cyl))+
  labs(s= "caballos de fuerza", y= "tiempo en 1/4 milla",
       title = "caballos-velocidad segun cilindraje y 
       tipo de caja")

# Scatter plot con ggplot orangeec
ggplot(orangeec, aes(orangeec$`Internet penetration % population`, orangeec$`Creat Ind % GDP`))+
  geom_point(aes(color=factor(Strong_economy), size= orangeec$`GDP Growth %`))+
  labs(x='Penetración Internet', y='Aporte economía naranja PIB',
       title='Internet y aporte economía naranja según economía y crecimiento PIB')

#RETO:Es el mismo scatterplot de arriba solo que cruzando 4 variables pero las vamos a cambiar
#En el ejex quiere que se ubique la inversion de educacion
#En el ejey el desemplo
#El color de las bubujas sigue siendo factor de la nueva variable
#El tamaño de la burbuja sera el porcentaje de la poblacio que esten por debajo de la linea de pobreza

ggplot(orangeec, aes(Education.invest...GDP,Unemployment))+
  geom_point(aes(color=factor(Strong_economy), size = `X..pop.below.poverty.line`))+
  labs(x = "Inversión de eduación como %PIB", y="Desempleo",
       title ="Inversión en educación, población debajo de la línea de pobreza, desempleo y crecimiento PIB")

#Scatter plots interactivos
my_graph = ggplot(orangeec, aes(Internet.penetration...population,
                                 Creat.Ind...GDP, label= Country))+
  geom_point()+
  labs(x="Penetracion internet", y= "Aporte economia naranja",
       title= "Penetracion Internte y aporte ecnomia naranja")
  
my_graph

p = ggplotly(my_graph)
p

#
pairs(mtcars)

#
newdata <- subset(mtcars, select = c(2,7:8,11,12))
pairs(newdata)

#Aqui se pone lo que no se quiere por eso se pone el (-)
pairs(mtcars[, -c(1,3,4,5,6,9,10)])

#Filtar datos de los carros eficientes
Eficientes<- filter(mtcars, mpg >= 30)
Eficientes

#Queremos ver una relacion pero solo con la selecion de carros eficientes
pairs(Eficientes[,2:6])

#Selccion de modelos por un tipo de nombre. 
#Hace un filtrado de puros modelos merc
# %>% se llama funcion pipe y le pasa un filtrado
merc <- mtcars %>%
  filter(str_detect(model, "Merc"))
merc

#Relacion entre variables pero solo de los modelos merc
pairs(merc[,2:6])

#Aplicar correlacion, esto nos apoya a verficar lo visualizamos en las graficas
cor(merc[,2:6])

# 
cor(newdata)

cor(merc[,2:6])

#23. Buscando correlaciones con pairs en dataset proyecto
pairs(orangeec[,2:6])

#
pairs(orangeec[,5:10])

#Se pone <- porque es una variable
newdata <- subset(orangeec, select=c(5,6,10,11,12,13))
newdata

pairs(newdata)

#24. Buscando correlaciones con la función cor en dataset proyecto

cor(orangeec[,2:6])

#Para que en la observaciones no haya N/A
cor(orangeec[,2:6],use="complete.obs")

#Para que en la observaciones no haya N/A
cor(orangeec[,5:10], use= "complete.obs")

#
cor(newdata, use ="complete.obs")

#25.Protegiendonos de los peligros del promedio

summary(mtcars)

#
sd(mtcars$mpg)
desv <- sd(mtcars$mpg)
mean(mtcars$mpg)

#
prom <- mean(mtcars$mpg)
prom

#Hacer el coeficiente
CoefVar <- (desv/prom)*100
CoefVar

#26.Eliminado los NA's para hacer los calculos

summary(orangeec)

#
sd(orangeec$`Internet penetration % population`)
desv <- sd(orangeec$`Internet penetration % population`)
desv

mean(orangeec$`Internet penetration % population`)
prom <- sd(orangeec$`Internet penetration % population`)
prom

#
CoefVar<- (desv/prom)*100
CoefVar

summary(orangeec)

#
mean(orangeec$`Creat Ind % GDP`)

#Esto indica que en promedio lo paises de latam estan aportando en promedio 3.2909
mean(orangeec$`Creat Ind % GDP`, na.rm=TRUE)

#
sd(orangeec$`Creat Ind % GDP`)
sd(orangeec$`Creat Ind % GDP`, na.rm=TRUE)
desv <- sd(orangeec$`Creat Ind % GDP`, na.rm=TRUE)

#
CoefVar <- (desv/prom)*100
CoefVar

#27.Estadistica y visualizacion aplicada a analisis de datos de marcadeo


#28.Generando tablas, filtrando y seleccionado datos – dplyr – Parte 1
#Estos editan el datset, osea agregan algo a ala tabla

eficientes <- mean(mtcars$mpg)
eficientes

mtcars <- mtcars %>%
  mutate(Mas_eficientes=ifelse(mpg<eficientes,
                               "bajo promedio", "en o sobre promedio"))

Mas_veloces <- mtcars[mtcars$qsec<16,]
Mas_veloces

        #Esto significa que se va ir a algo
mtcars <- mtcars %>%
  mutate(Velocidad_Cuarto_milla=ifelse(qsec < 16,
                                       "Menos 16 segs",
                                       "Mas 16 segs"))

#
mtcars <- mtcars %>%
  mutate(Peso_kilos = (wt/2)*1000)

mtcars <- mtcars %>%
  mutate(Peso=ifelse(Peso_kilos <= 1500,
                     "Livianos", "Pesados"))

#28.Generando tablas, filtrando y seleccionado datos – dplyr – Parte 2
orangeec <- orangeec %>%
  mutate(Crecimiento_GDP = ifelse(`GDP Growth %` <= 2.5,
                            "2.5% o mas", "Menos de 2.5%"))

#En la segunda coma siempre va lo contrario de la afirmacion
orangeec <- orangeec %>%
  mutate(Anaranjados=ifelse(`Creat Ind % GDP` >= 2.5,
                            "Mas anaranjados", "Menos anarajandos"))

#Realizar un ranking. %>% = pipe
orangeec %>%
  arrange(desc(`Creat Ind % GDP`))

# El %>% nos va a buscar lo que este en tal cosa.Aqui hace el ranking
TopNaranjas <- orangeec %>%
  filter(Country %in% c("Mexico", "Panama", "Argentina",
                        "Colombia", "Brazil","Paraguay" ))

TopNaranjas

TopNaranjas %>%
  arrange(desc(`Creat Ind % GDP`))

#29.Viendo mas información con facet wrap – Parte 1
mtcars %>%
  arrange(desc(Peso_kilos))

Mas_pesados <- mtcars %>%
  filter(model %in% c("Lincoln Continental", "Chrysler Imperial", 
                      "Cadillac Fleetwood", "Merc 450SE"))

Mas_pesados

#
ggplot(Mas_pesados, aes(x=`hp`, y=`mpg`))+
  geom_point()+
  #Se pone esto porque se quiere que se muestre por modelos
  facet_wrap(~model)

#
ggplot(mtcars, aes(x=`cyl`, y=`mpg`, size=Peso))+
  geom_point()+
  facet_wrap(~ am)

#Reto: Cambiar el ajuste de peso a peso en kilos
ggplot(mtcars, aes(x=`cyl`, y=`mpg`, size=Peso_kilos))+
  geom_point()+
  facet_wrap(~ am)

#30.Viendo mas informacion con facet wrap - Parte 2

ggplot(TopNaranjas, aes(x=`Internet penetration % population`,
                        y = `Services % GDP`, size =`GDP PC`))+
  geom_point()+
  facet_wrap(~Country)

#
ggplot(TopNaranjas, aes(x=`Education invest % GDP`,
                        y = `Creat Ind % GDP`, size =Unemployment))+
  geom_point()+
  facet_wrap(~Country)

#Cuando es una variable se pone <-

myColors <- brewer.pal(9,"Reds")

ggplot(TopNaranjas, aes(x=`Internet penetration % population`,
                        y = `GDP PC`, 
                        fill = `Creat Ind % GDP`))+
  geom_tile()+
  facet_wrap(~Country)+
  scale_fill_gradientn(colors=myColors)

#31.Conociendo R Marckdown y organizando los hallazgos del análisis en un documento PDF

#cierre

cajas <- c(1,2,3,4,5,6,7,8)
tiempo <- c(10,9,8,5,8,6,3,1,8,1)

plot(tiempo-cajas)

#--------------------------------------

plot(`Services % GDP`~`Education.invest...GDP`)

plot(mtcars$`mpg`~mtcars$`am`)

pairs(ventas[,2:5])
#-------------------------------------------
ggplot(ventas, aes(x=ciudad)) + geom_histogram() + labs(x=“Ciudades”, y=“Unidad comercial”, title=“Ventas por unidad comercial”)