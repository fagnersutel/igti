## Exercício 1

#realizamos a carga de dados
data(airquality)
#verificamos os promeiros dados
head(airquality)
#nos certificamos de que há mais de 100 registros
dim(airquality)

#como os dados são built-in vamos exportar para uma tabela
write.csv(airquality, "meusdados.csv", row.names=FALSE)
#removemos o bando original
rm(airquality)
#realizamos a carga de dados
airquality = read.csv("meusdados.csv", header = TRUE, sep = ",")
#Plotamos os dados na ordem plot(eixo_x, eixo_y), com pontos perder e solidos => pch=16
plot(airquality$Solar.R, airquality$Temp, col="green", pch=16)
#ploramos a linha de regressão linear em vermelho
abline(lm(airquality$Temp~airquality$Solar.R), col="red")
#Armazenamos o modelo de regressão linear
modelo_lm <- lm(Temp ~ Solar.R, data = airquality)
#imprimimos o modelo com o intercepto e coeficente angular
modelo_lm
#imprimimos as medidas resumo
summary(modelo_lm)
#Retiramos as notacoes coentificas para melhor interpretacao dos numeros
options(scipen = 999)
#e tornamos a verificar as medidas resumo
summary(modelo_lm)
#criamos tres novos valores de radiacao solar
radiacao <- data.frame(Solar.R = c(184, 242, 168))
#Como Lm é nativo do R utilizamnos predict para restgatar o modelo e predizer a tamperatira frente aos tres valores de radiacao
predict(modelo_lm, newdata = radiacao)

#obtemos o coefiente de determinacao R2, ou R ajustado, que nos diz que o modelo explica apenas 6% da variaçao
summary(modelo_lm)$adj.r.squared