source("http://www.openintro.org/stat/data/cdc.R")


# SUA VEZ

# - Exercicio:
# Repostas: item 1
#  Crie um gráfico de dispersão da variável peso em relação ao peso desejado. Defina a relação entre
#  essas duas variáveis.
plot(x = cdc$weight, y = cdc$wtdesire)
# R. Pessoas com maior peso tendem a ter um peso desejado diferente do seu peso.
summary(cdc$weight)
summary(cdc$wtdesire)

# Repostas: item 2
# Vamos considerar uma nova variável: a diferença entre o peso desejado (wtdesire) e o peso atual
# (weight). Crie esta nova variável subtraindo as duas colunas na base de dados e atribuindo-as a um
# novo objeto chamado wdiff.
# R.
cdc$wdiff <- (cdc$wtdesire - cdc$weight)

# Repostas: item 3
# Que tipo de dado está contido na variável wdiff? 
# R. Tipo númerico
# Se uma observação de wdiff é 0, o que isso implica com relação ao peso atual e desejado de uma pessoas? 
# R. Isso indica que não existe diferença peso atual e desejado de uma pessoa
# E se o valor de wdiff for positivo ou negativo?
# Se for negativo a pessoa está insatisfeita com o seu peso quer emagrecer, 
# se for positivo a pessoa está insatisfeita com o seu peso quer engordar.

# Repostas: item 4
# Descreva a distribuição de wdiff em termos de seu centro, forma e variação, incluindo qualquer
# gráfico que você usar. O que isso nos diz sobre como as pessoas se sentem a respeito do seu peso
# atual?
# R. A maioria das pessoas estão insatisfeitas.
summary(cdc$wdiff)
boxplot(cdc$wdiff)

# Repostas: item 5
# Utilizando sumários numéricos e um gráfico de caixas lado-a-lado, determine se homens tendem a
# ver seu peso diferentemente das mulheres.
h_wtdesire_cdc <- subset(cdc$wtdesire, cdc$gender == 'm')
f_wtdesire_cdc <- subset(cdc$wtdesire, cdc$gender == 'f')
summary(h_wtdesire_cdc)
summary(f_wtdesire_cdc)
boxplot(cdc$wtdesire ~ cdc$gender)
#R.As mulheres tendem a ver o seu peso acima do desejado mais que os homens.

# Repostas: item 6
# Agora chegou a hora de usar a criatividade. Encontre a média e o desvio padrão de weight e
# determine qual a proporção de pesos que estão a um desvio padrão da média.
#medidas descritivas
# Media
mean(cdc$weight)
# Desvio Padrao
sd(cdc$weight)
hist(cdc$weight)
#extrair apenas os pesos a um desvio padrao acima ou abaixo
mdata <- subset(cdc, cdc$weight > mean(cdc$weight) + sd(cdc$weight) | cdc$weight < mean(cdc$weight) - sd(cdc$weight) )
head(mdata)
dim(mdata)
5848/20000
#R. 29% dos pesos estão acima ou abaixo de um desvio padrão.
