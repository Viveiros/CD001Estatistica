source("http://www.openintro.org/stat/data/present.R")
source("http://www.openintro.org/stat/data/arbuthnot.R")

# - Exercicio:
# Repostas: item 1
#Quais anos estão incluídos neste conjunto de dados? 

#somente dados de anos
present$year

# Quais são as dimensões da base de dados e
dim(present)

#quais são os nomes das colunas ou variáveis?
#nomes das variáveis
names(present)

# Repostas: item 2
#Como estas contagens se comparam aos dados de Arbuthnot?
#Eles estão numa escala similar?

# Repostas: item 3
#A observação de Arbuthnot de que os meninos nascem numa proporção maior que as meninas se
#mantém nos EUA?
#gráfico das proporções dos meninos com relação ao tempo
plot(x = arbuthnot$year, y = arbuthnot$boys / (arbuthnot$boys + arbuthnot$girls), type = "l")
plot(x = present$year, y = present$boys / (present$boys + present$girls), type = "l")

# Sim a proporção de nascimento de meninos em relação ao tempo se mantém no EUA assim como em Arbuthnot.

# Repostas: item 4
#Crie um gráfico que mostre a razão de meninos para meninas para cada ano do conjunto de dados.
#O que você pode verificar?
plot(x = present$year, y = present$boys / present$girls, type = "l")
