##Sua Vez

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
dim(ames)
names(ames)

# 1. Utilizando a seguinte função (que foi carregada junto com o conjunto de dados), crie gráficos de todos
# os intervalos. Que proporção dos intervalos de confiança contém a verdadeira média populacional?
# Essa proporção é exatamente igual ao nível de confiança? Se não, explique por quê.

population <- ames$Gr.Liv.Area
samp <- sample(population, 60)

samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60
for(i in 1:50){
  samp <- sample(population, n) # obtém uma amostra de n = 60 elementos da população
  samp_mean[i] <- mean(samp) # salva a média amostral no i-ésimo elemento de samp_mean
  samp_sd[i] <- sd(samp) # salva o dp da amostra como o i-ésimo elemento de samp_sd
}

#construir os intervalos de confiança.
lower_vector <- samp_mean - 1.96 * samp_sd / sqrt(n)
upper_vector <- samp_mean + 1.96 * samp_sd / sqrt(n)

#Visualizar o primeiro intervalo.
c(lower_vector[1],upper_vector[1])

plot_ci(lower_vector, upper_vector, mean(population))

#Que proporção dos intervalos de confiança contém a verdadeira média populacional?
# R. São 50 intevalos somente 4 ficaram sem conter a verdadeira média populacional.
4/50
# Essa proporção é exatamente igual ao nível de confiança? Se não, explique por quê.
# Não é igual, é menor que o nível de confiança.

# 2. Escolha um intervalo de confiança de sua preferência, desde que não seja de 95%. Qual é o valor
# crítico apropriado?

#99%
qnorm(0.995, mean =0 , sd= 1, lower.tail = T)
qnorm(0.005, mean =0 , sd= 1, lower.tail = F)

# 3. Calcule 50 intervalos de confiança utilizando o nível de confiança que você escolheu na questão
# anterior. Você não precisa obter novas amostras: simplesmente calcule os novos intervalos baseado nas
# médias amostrais e desvios padrão que você já coletou. Utilizando a função plot_ci, crie gráficos de
# todos os intervalos e calcule a proporção de intervalos que contém a verdadeira média populacional.
# Compare essa proporção com o nível de confiança escolhido para os intervalos

samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60
for(i in 1:50){
  samp <- sample(population, n) 
  samp_mean[i] <- mean(samp) 
  samp_sd[i] <- sd(samp) 
}

#construir os intervalos de confiança.
lower_vector <- samp_mean - 2.57 * samp_sd / sqrt(n)
upper_vector <- samp_mean + 2.57 * samp_sd / sqrt(n)

#Visualizar o primeiro intervalo.
c(lower_vector[1],upper_vector[1])

plot_ci(lower_vector, upper_vector, mean(population))

# R. Com o nível de 99% de confiança os intervalos ficaram mais próximos ao Mu.
