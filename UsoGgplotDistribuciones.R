library(ggplot2)

#Distribucion Binomial
#Problema
# Suponga que hay diez preguntas de opci√≥n m√∫ltiple en una prueba de clase de 
#ingles.
# Cada pregunta tiene cuatro posibles respuestas, y solo una de ellas es correcta.
#
# Q1. Encuentre la probabilidad de tener exactamente 4 respuestas correctas al azar
# Soluci√≥n
dbinom(x=4, size = 10, prob = 0.25)
#x = n√∫mero de exitos
#n = size = numero de intentos
#p = prob = probabilidad de exito
# Q2. ¬øCu√°l es la probabilidad de tener cuatro o menos respuestas correctas si 
# un estudiante intenta responder todas las preguntas al azar?
pbinom(q=4, size = 10, prob = 0.25)
?pbinom
k <- seq(0,10,1)
proTable <- data.frame(answer = k, prob = dbinom(k,10,0.25))

ggplot(data=proTable, aes(x=answer, y = prob)) + 
  geom_bar(stat="identity", fill="blue") +
  scale_x_continuous(breaks=k) + 
  theme_bw()
# ------------------------------------------------- ------------------------------
# Distribucion de Poisson

# Problema
# Si hay diez autos cruzando un puente por minuto en promedio, encuentre el
# probabilidad de tener diecisiete o m√°s autos cruzando el puente en un
# minuto particular.
ppois(16, lambda = 10) #P(x<16)
#P(x > 17) = 1 - P(x<16)
probabilidad = 1 - ppois(16, lambda = 10) 
probabilidad
#oTRA FORMA
ppois(16, lambda = 10, lower = FALSE)

cars <- seq(10,20,1)
cars
prob <- dpois(cars,lambda = 10)
prob

#Grafica
ggplot() + geom_step(aes(x=cars, y=prob)) +
  scale_x_continuous(breaks=cars) +
  theme_bw()

# Distribucion normal
# Si queremos hallar el valor de z usamos qnorm
qnorm(0.01) #para 98%
qnorm(0.025) #para 95%
# Problema
# El conjunto de datos de Iris en R contiene especificaciones de flores de iris.
# La columna Sepal Width sigue una distribuci√≥n normal.
#
# Q1. øCual es el porcentaje de flores con un ancho de s√©palo de 3 cm o menos?
# Solucion
meanIris <- mean(iris$Sepal.Width)
sdIris <- sd(iris$Sepal.Width)
pnorm(3, mean=meanIris, sd=sdIris)

# Q2. øQuÈ fracciion de las flores donde el ancho del sepalo se encuentra entre 
#3 cm y 4 cm?
pnorm(4, mean=meanIris, sd=sdIris) - pnorm(3, mean=meanIris, sd=sdIris)

#GRAFICO
sepal_width <- seq(1,5,0.1)
sepal_width
?dnorm
prob <- dnorm(sepal_width, mean=meanIris, sd=sdIris)
prob

ggplot() + geom_step(aes(x=sepal_width, y=prob))
theme_bw()

# DistribuciÛn exponencial
#
# Q1. Suponga que la hora media de llegada de un cliente al cajero de un supermercado 
# es cuatro minutos. Encuentre la probabilidad de que un cliente llegue al 
# cajero en menos de tres minutos.
# SoluciÛn
pexp(3, rate=1/4) #rate es beta

# Q2. El n˙mero de dÌas antes de que los viajeros compren sus billetes de aviÛn sigue un
# distribuciÛn exponencial con el n˙mero medio de dÌas = 15.
# Calcule la probabilidad de que un viajero compre un boleto m·s de
# 10 dÌas antes.
# SoluciÛn
1 - pexp(10, rate=1/15)

#Grafico
days <- seq(1,100,1)
prob <- pexp(days,rate=1/15)
ggplot() + geom_point(aes(x=days, y=prob)) + 
  theme_bw()

# DistribuciÛn chi-cuadrado

# Planteamiento del problema
# Digamos que, en promedio, una baterÌa dura 50 minutos con una sola carga. El estandar
# la desviaciÛn es de 3 minutos. Suponga que el fabricante de la baterÌa tiene una calidad
# prueba de control. Seleccionan aleatoriamente 9 baterÌas. La desviaciÛn est·ndar de
# las baterÌas seleccionadas son 5 minutos.
# Q1. øCu·l serÌa el estadÌstico chi-cuadrado representado por esta prueba?
# SoluciÛn

dof <- 8
chi <- (dof * 5^2)/3^2
chi
# Q2. Suponga que repitieron la prueba con una nueva muestra aleatoria de 9 baterÌas.
# øCu·l es la probabilidad de que la desviaciÛn est·ndar en la nueva prueba sea
# øm·s de 5 minutos?
# SoluciÛn

1 - pchisq(chi, df = dof)
chi_square <- seq(1,40,1)
prob <- pchisq(chi_square, df = dof)
ggplot() + geom_point(aes(x=chi_square, y=prob)) +
  theme_bw()