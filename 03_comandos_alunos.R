#trabalharemos com medidas de dimensões do corpo. Este conjunto de dados contém medidas
#de 247 homens e 260 mulheres, a maioria dos quais foram considerados adultos jovens saudáveis.
download.file("http://www.openintro.org/stat/data/bdims.RData", destfile = "bdims.RData")
load("bdims.RData")

#primeiras linhas dos dados
head(bdims)
#temos 25 medidas, muitas das quais são diâmetros ou circunferências
#focaremos em: peso em kg (wgt), altura em cm (hgt), e sex (sexo, 1 masculino, 0 feminino)

#será útil criar dois conjuntos de dados adicionais: um com os dados dos homens e outro com os dados das mulheres
mdims <- subset(bdims, bdims$sex == 1)
fdims <- subset(bdims, bdims$sex == 0)

#Exercício 1 Elabore um histograma da altura dos homens e um histograma das alturas das
#mulheres. Como você descreveria os diferentes aspectos das duas distribuições?
hist(mdims$hgt)
hist(fdims$hgt)
#R. Homens com distribuição de altura centrada em 175-185 e mulheres entre 160-170
# Distribuição aproximadamente simétrica, em forma de sino (com uma moda central)

#média e desvio padrão das alturas das mulheres
fhgtmean <- mean(fdims$hgt)
fhgtsd <- sd(fdims$hgt)
summary(fdims$hgt)

#histograma de densidade (áreas das barras somadas resultam em 1) - só muda o eixo y
hist(fdims$hgt, probability = TRUE)
x <- 140:190
y <- dnorm(x = x, mean = fhgtmean, sd = fhgtsd)
lines(x = x, y = y, col = "blue")
?dnorm

# Exercício 2 Baseado neste gráfico, parece que os dados seguem aproximadamente uma distribuição normal?
# R. Sim, bastante parecido.


#gráfico normal Q-Q, de "quantil-quantil"
qqnorm(fdims$hgt)
qqline(fdims$hgt) #se for normal, espera-se todos os pontos perto da linha


#simulando dados a partir de uma distribuição normal - para ver como fica o q-q plot
sim_norm <- rnorm(n = length(fdims$hgt), mean = fhgtmean, sd = fhgtsd)

#Exercício 3 
#Faça um gráfico de probabilidade normal do vetor sim_norm. Os pontos caem todos em 
#cima da linha? Como este gráfico se compara ao gráfico de probabilidade dos dados reais?
hist(sim_norm, probability = TRUE)
lines(x = x, y = y, col = "blue")
qqnorm(sim_norm)
qqline(sim_norm)
# R. Os gráficos se parecem muito, com alguns pontos errantes na direção das caudas. 


#comprarar vários outros gráficos utilizando os dados originais e amostras simuladas
qqnormsim(fdims$hgt)


# Exercício 4 O gráfico de probabilidade normal para fdims$hgt parece similar aos gráficos criados para os dados simulados? 
# R. São muito similares
# Quer dizer, os gráficos fornecem evidência de que as alturas de mulheres são aproximadamente normais?
# R. As evidências são de que as alturas das mulheres são aproximadamente normais.


#####################################

#Exercício 5 
#Usando a mesma técnica, determine se os pesos de mulheres parecem ser provenientes de uma distribuição normal.
hist(fdims$wgt)

fwgtmean <- mean(fdims$wgt)
fwgtsd <- sd(fdims$wgt)

hist(fdims$wgt, probability = TRUE)
x <- 20:120
y <- dnorm(x = x, mean = fwgtmean, sd = fwgtsd)
lines(x = x, y = y, col = "blue")

qqnorm(fdims$wgt)
qqline(fdims$wgt)

sim_norm <- rnorm(n = length(fdims$wgt), mean = fwgtmean, sd = fwgtsd)
hist(sim_norm, probability = TRUE)
lines(x = x, y = y, col = "blue")
qqnorm(sim_norm)
qqline(sim_norm)

qqnormsim(fdims$wgt)

########################################

#Qual é a probabilidade de que uma mulher adulta jovem escolhida por acaso 
# é maior do que 6 pés (cerca de 182 cm - os dados estão em cm)?
1 - pnorm(q = 182, mean = fhgtmean, sd = fhgtsd)
# R. 0.004434387

#calcular a mesma probabilidade anterior empiricamente
sum(fdims$hgt > 182) / length(fdims$hgt)
# R. 0.003846154

########################################

#Exercício 6 
#Elabore duas questões de probabilidade que você gostaria de responder; uma
#com relação à altura de mulheres e outra com relação ao peso de mulheres. Calcule essas
#probabilidades usando tanto o método teórico da distribuição normal quanto a distribuição
#empírica (quatro probabilidade no total). 


#Qual é a probabilidade de uma mulher ser menor do que 168 cm?
pnorm(q = 168, mean = fhgtmean, sd = fhgtsd)
# R. 0.6836408
sum(fdims$hgt < 168) / length(fdims$hgt)
# R. 0.6846154

#Qual é a probabilidade de uma mulher ter peso maior que 60 kg?
1 - pnorm(q = 60, mean = fwgtmean, sd = fwgtsd)
# R. 0.524893
sum(fdims$wgt > 60) / length(fdims$wgt)
# 0.4384615

# Qual variável, altura ou peso, teve uma concordância maior entre os dois métodos?
# R. altura

########################################

# Se diz o valor da altura e tem a probabilidade
pnorm(175.889, mean = 178, sd = 8.38, lower.tail = T)
# Se diz o valor da area e tem a altura do fulano
qnorm(0.40, mean = 178, sd = 8.38, lower.tail = TRUE, log.p = FALSE)

# Qual é a probabilidade de um homem adulto estar entre 175cm e 188cm?
pnorm(188, mean = 178, sd = 8.38, lower.tail = T)-pnorm(175, mean = 178, sd = 8.38, lower.tail = T)
# R. 52%

pnorm(170.0, mean = 178, sd = 8.38, lower.tail = T)
# Calculo de Z
(170-178)/8.38 
pnorm(-0.9546, mean = 0, sd = 1, lower.tail = T)


# SUA VEZ
#Usando a mesma técnica, determine se os bii.di das mulheres parecem ser provenientes de uma distribuição normal.
# (a)
fbii.dimean <- mean(fdims$bii.di)
fbii.disd <- sd(fdims$bii.di)

fdims$Zbii.di <- (fdims$bii.di - fbii.dimean) / fbii.disd

hist(fdims$bii.di)
hist(fdims$Zbii.di)

hist(fdims$Zbii.di, probability = TRUE)
x <- -5:3
y <- dnorm(x = x, mean = fbii.dimean, sd = fbii.disd)
lines(x = x, y = y, col = "blue")

qqnorm(fdims$Zbii.di)
qqline(fdims$Zbii.di)

sim_norm <- rnorm(n = length(fdims$Zbii.di), mean = fbii.dimean, sd = fbii.disd)
hist(sim_norm, probability = TRUE)
lines(x = x, y = y, col = "blue")
qqnorm(sim_norm)
qqline(sim_norm)

qqnormsim(fdims$Zbii.di)

# (b)
felb.dimean <- mean(fdims$elb.di)
felb.disd <- sd(fdims$elb.di)

fdims$Zelb.di <- (fdims$elb.di - felb.dimean) / felb.disd

qqnorm(fdims$Zelb.di)
qqline(fdims$Zelb.di)

# (c)
fagemean <- mean(fdims$age)
fagesd <- sd(fdims$age)

fdims$Zage <- (fdims$age - fagemean) / fagesd

qqnorm(fdims$Zage)
qqline(fdims$Zage)

# (d)
fche.demean <- mean(fdims$che.de)
fche.desd <- sd(fdims$che.de)

fdims$Zche.de <- (fdims$che.de - fche.demean) / fche.desd

qqnorm(fdims$Zche.de)
qqline(fdims$Zche.de)

#Como você pode ver, gráficos de probabilidade normal podem ser utilizados tanto para avaliar a
#normalidade quanto visualizar a assimetria. Crie um gráfico de probabilidade normal para o diâ-
#  metro do joelho feminino (kne.di). Baseado neste gráfico de probabilidade normal, você diria que
#essa variável é simétrica, assimétrica à direita ou assimétrica à esquerda? Utiliza um histograma para
#confirmar seu resultado.

fkne.dimean <- mean(fdims$kne.di)
fkne.disd <- sd(fdims$kne.di)

fdims$Zkne.di <- (fdims$kne.di - fkne.dimean) / fkne.disd

hist(fdims$kne.di)
hist(fdims$Zkne.di)

hist(fdims$kne.di, probability = TRUE)
x <- 10:30
y <- dnorm(x = x, mean = fkne.dimean, sd = fkne.disd)
lines(x = x, y = y, col = "blue")

qqnorm(fdims$kne.di)
qqline(fdims$kne.di)

sim_norm <- rnorm(n = length(fdims$kne.di), mean = fkne.dimean, sd = fkne.disd)
hist(sim_norm, probability = TRUE)
lines(x = x, y = y, col = "blue")
qqnorm(sim_norm)
qqline(sim_norm)

qqnormsim(fdims$kne.di)
