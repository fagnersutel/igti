######### Pratica Dois ########## 
airquality = read.csv("meusdados.csv", header = TRUE, sep = ",")
head(airquality)
prod = airquality$Temp
plot(prod)
head(prod)
#Criamos o histograma de Temperatura
histograma <- hist(prod, freq=FALSE, col="orange", xlab = "Temperatura", ylab = "densidade", main = "Histograma de temperaturas")
#Calculamosa densidade
densidade <- density(prod)
#Plotamos a linha de densodade
lines(densidade, col=4, lwd=4)
#Plotamos uma curva normal para a mesma média e desvio padrão
lines(seq(40, 110, by=.5), dnorm(seq(40, 110, by=.5), mean(airquality$Temp), sd(airquality$Temp)), col="red", lwd=4)
#Plotamosd as legendas
legend(55, 0.04, legend=c("Curva Normal", paste("Variabilidade Total: ", round(sd(prod), 2), sep = "")),
       col=c("red", "blue"), lty=1, cex=1)
#criamos uma borda
box()

variabilidade_total = sd(prod)
variabilidade_total
media_total = mean(prod)
media_total
plot(prod)
temp = prod[50:100]
temp = temp[temp < 100]
length(temp)
plot(temp)
variabilidade_capaz = sd(temp)
variabilidade_capaz
prod[2:153] - prod[1:152]
(prod[2:153] - prod[1:152])^2
sum((prod[2:153] - prod[1:152])^2)
passo_um = sum((prod[2:153] - prod[1:152])^2)
passo_um
passo_dois = (passo_um/(2*(length(prod)-1)))
passo_dois
medias_sucessivas =  sqrt(passo_dois)
medias_sucessivas
#Variavel explicativa candidata 1 "Concentracao de Ozonio"
cor.test(airquality$Temp, airquality$Ozone,method = "pearson")
#Variavel explicativa candidata 2 "MES"
cor.test(airquality$Temp, airquality$Month,method = "pearson")
#Variavel explicativa candidata pouco relevante
cor.test(airquality$Temp, airquality$Solar.R,method = "pearson")
#Variaveis explicativas candidatas pouco relevantes
cor.test(airquality$Temp, airquality$Ozone + airquality$Month,method = "pearson")
#Variaveis explicativas candidatas pouco relevantes
cor.test(airquality$Temp, airquality$Ozone + airquality$Month + airquality$Solar.R,method = "pearson")



library(qcc)
airquality <- matrix(unlist(airquality$Temp), ncol = 9, byrow = TRUE)
obj <- qcc(airquality, type="xbar")
q <- qcc(airquality, type="R")
obj <- qcc(airquality, type="xbar")
obj <- qcc(airquality[c(4:9),], type="xbar")
obj <- qcc(airquality[c(4:9),], type="xbar",newdata=airquality[1:3,])
obj <- qcc(airquality[c(4:9, 13, 15),], type="xbar",newdata=airquality[c(1:3,10:12 ),])
