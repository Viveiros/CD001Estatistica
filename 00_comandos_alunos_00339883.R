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
plot(x = arbuthnot$year, y = arbuthnot$girls, type = "l")
plot(x = present$year, y = present$girls, type = "l")
