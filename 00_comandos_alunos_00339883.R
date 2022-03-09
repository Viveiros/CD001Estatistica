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
# R. Contagem das amostras do conjunto arbuthnot é de 82 observações
# já o conjunto present tem 63 observações, comparando temos menos observações no 
# conjunto present.

#Eles estão numa escala similar?
# R. Sim as variáveis estão na escala similar
# R. 63 casos e 3 variáveis
# R. year - numérica discreta
# R. boys - numérica contínua
# R. girls - numérica contínua


# Repostas: item 3
#A observação de Arbuthnot de que os meninos nascem numa proporção maior que as meninas se
#mantém nos EUA?
#gráfico das proporções dos meninos com relação ao tempo
plot(x = arbuthnot$year, y = arbuthnot$boys / (arbuthnot$boys + arbuthnot$girls), type = "l")
plot(x = present$year, y = present$boys / (present$boys + present$girls), type = "l")
# Sim a proporção de nascimento de meninos em relação ao tempo se mantém no EUA assim como em Arbuthnot.

# Repostas: item 4
#Crie um gráfico que mostre a razão de meninos para meninas para cada ano do conjunto de dados.
plot(x = present$year, y = present$boys / present$girls, type = "l")
#O que você pode verificar?
# R. existe um tendência de queda da razão de meninos em relação a meninas entre os anos de 1940 a 2000.

# Repostas: item 5
# Em qual ano se verifica o maior número de nascimentos nos EUA? Você pode utilizar os arquivos de
# ajuda ou o cartão de referência do R (http://cran.r-project.org/doc/contrib/Short-refcard.pdf ) para encontrar
# comandos úteis.
# Criar uma coluna somando boys e girls
present$total<-present$boys + present$girls
# Valor máximo da coluna somada
max(present$total)
# qual a linha que contem esse valor máximo
which.max(present$total)
# Recuperar o year baseado na linha queestá o valor máximo
present$year[which.max(present$total)]

