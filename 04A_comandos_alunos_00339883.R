##Sua Vez
# Até agora, nós nos ocupamos em estimar a média da área habitável nas casas do município de Ames.
# Agora você tentará estimar a média dos preços das casas.

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
dim(ames)
names(ames)

# 1. Retire uma amostra aleatória de 50 elementos da variável price (preço). Com essa amostra, qual é
# sua melhor estimativa pontual para a média populacional?

# o preço da venda (SalePrice).
price <- ames$SalePrice
#retirando uma amostra n=50 de price
samp1 <- sample(price, 50)
# Com essa amostra, qual é sua melhor estimativa pontual para a média populacional?
summary(samp1)
hist(samp1)
mean(samp1)
# R. com essa amostra a melhor estimativa é a média da amostra que é 166637.7

# 2. Já que você tem acesso à população, simule a distribuição amostral de ̄xprice retirando 5000 amostras
# de 50 elementos da população e calculando 5000 médias amostrais. Armazene essas médias em um
# vetor com o nome sample_means50. Crie um gráfico com os resultados, e então descreva a forma
# dessa distribuição amostral. Baseado nessa distribuição amostral, qual seria seu palpite para a média
# dos preços das casas na população? Por fim, calcule e informe a média populacional.
sample_means50 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(price, 50)
  sample_means50[i] <- mean(samp)
}
hist(sample_means50)
# então descreva a forma dessa distribuição amostral.
# R. Esse histograma apresenta a distribuição amostral de forma parecida com a distribuição normal.
# Baseado nessa distribuição amostral, qual seria seu palpite para a média dos preços das casas na população?
summary(sample_means50)
mean(sample_means50)
# R. 180827
# Por fim, calcule e informe a média populacional.
mean(price)
# R. 180796.1

# 3. Mude o tamanho da sua amostra de 50 para 150, e então calcule a distribuição amostral utilizando o
# mesmo método descrito acima, e guarde as médias em um novo vetor com o nome sample_means150.
# Descreva a forma dessa distribuição amostral e compare-a com a distribuição amostral para a amostra
# de 50 elementos. Com base nessa distribuição amostral, qual seria seu palpite sobre a média dos
# preços de vendas de casas no município de Ames?
sample_means150 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(price, 150)
  sample_means150[i] <- mean(samp)
}
# Descreva a forma dessa distribuição amostral e compare-a com a distribuição amostral para a amostra
# de 50 elementos.
hist(sample_means150)

# R. As duas distribuições amostrais se parecem.
# Com base nessa distribuição amostral, qual seria seu palpite sobre a média dos
# preços de vendas de casas no município de Ames?
summary(sample_means150)
mean(sample_means150)
# R. 180985.4


# 4. Das distribuições amostrais calculadas nos exercícios 2 e 3, qual tem menor dispersão?
# R. O centro está sempre em torno de 180796, e a dispersão vai diminuindo 
# com o aumento do tamanho da amostra. O sample_means150 tem a menor dispersão.

# Se estamos interessados em estimativas que estão mais próximas do valor verdadeiro, preferiríamos uma
# distribuição com uma dispersão pequena ou grande?
# R. Com uma dispersão pequena.