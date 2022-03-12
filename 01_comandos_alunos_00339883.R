source("http://www.openintro.org/stat/data/cdc.R")


# SUA VEZ

# - Exercicio:
# Repostas: item 1
#  Crie um gráfico de dispersão da variável peso em relação ao peso desejado. Defina a relação entre
#  essas duas variáveis.
plot(x = cdc$weight, y = cdc$wtdesire)
# R. O peso desejado é em geral menor que o peso.
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
h_weight_cdc <- subset(cdc$weight, cdc$gender == 'm')
f_weight_cdc <- subset(cdc$weight, cdc$gender == 'f')
summary(h_weight_cdc)
summary(f_weight_cdc)
boxplot(cdc$weight ~ cdc$gender)
#R.As mulheres tendem a ver o seu peso acima do desejado mais que os homens.