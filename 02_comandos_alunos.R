#Mãos Quentes
#artigo de 1985 escrito por Gilovich, Vallone e Tversky coletou evidência que contradiz 
#essa crença e mostrou que lances sucessivos são eventos independentes.

#Os objetivos deste laboratório são (1) refletir sobre o efeito de eventos independentes e dependentes,
#(2) aprender como simular sequências de lances no R, e
#(3) comparar a simulação com os dados efetivos para determinar se o fenômeno das mãos quentes parece ser real.

#performance de um jogador: Kobe Bryant do Los Angeles Lakers. Sua
#performance contra o Orlando Magic nas finais de 2009 da NBA lhe deram o título de "Jogador Mais
#Valioso" e vários espectadores comentaram como ele parecia demonstrar uma mão quente.

download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")
load("kobe.RData")

#analisar as primeiras linhas
head(kobe)

#Neste banco de dados, cada linha registra um lance feito por Kobe Bryant. Se ele acertou o lance 
#(fez uma cesta), um acerto, H (de Hit), é registrado na coluna denominada basket (cesta); 
#caso contrário um erro, M (de Miss), é registrado.

#sequência de acertos e erros de suas nove tentativas no primeiro quarto do primeiro jogo
kobe$basket[1:9]

# H M | M | H H M | M | M | M
# comprimentos são um, zero, dois, zero, zero, zero

# Exercício 1 O que uma sequência de comprimento 1 significa, ou seja, quantos acertos e erros
# existem dentro de um sequência de 1? E de uma sequência de comprimento 0?
# R. Comprimeiro 1 significa que teve um acerto em sequencia. No comprimento zero não houve acerto.


#A função personalizada calc_streak, que foi carregada com os dados, pode ser utilizada para calcular os
#comprimentos de todas as sequências de acertos e então conferir sua distribuição.
kobe_streak <- calc_streak(kobe$basket)
table(kobe_streak)
barplot(table(kobe_streak))


# Exercício 2 Descreva a distribuição do comprimento das sequências de Kobe nas finais de 2009 da NBA. 
# R. Distribuição assimétrica positiva, de 0 a 4.
# Qual foi seu tamanho de sequência típico? 
# R. zero
# Quão longa foi sua maior sequência de cestas?
# R. quatro

# Mostramos que Kobe teve algumas sequências de arremesso longas, 
# mas elas são longas o suficiente para apoiar a crença de que ele 
# tinha mãos quentes? Com o que podemos compará-las?
# ...

#simular um lance de uma moeda honesta
outcomes <- c("heads", "tails")
sample(outcomes, size = 1, replace = TRUE)

#simular o lançamento de uma moeda honesta 100 vezes
sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)
sim_fair_coin
table(sim_fair_coin)

#simular uma moeda viciada que sabemos que dá cara somente 20% das vezes
sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob = c(0.2, 0.8))
sim_unfair_coin
table(sim_unfair_coin)

# Exercício 3 Em sua simulação de lançar uma moeda viciada 100 vezes, quantos lances deram cara?
# R. 49 lances na moeda honesta e 24 lances na moeda viciada


#Se você quiser saber mais sobre a função sample
?sample


#simular um jogador de basquete que arremessa de forma independente
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 1, replace = TRUE)
sim_basket #um único arremesso

#
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))
sim_basket
sim_streak <- calc_streak(sim_basket)
table(sim_streak)
barplot(table(sim_streak))
#

#Exercício 4 
#Qual mudança precisa ser feita para que a função sample reflita o percentual
#de arremessos de 45%? Faça esse ajuste, e então rode a simulação para uma amostra de 133
sim_basket_kobe <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))
sim_basket_kobe
table(sim_basket_kobe)/133 # percentual de acertos simulado
table(kobe$basket)/133 # percentual de acertos observado no dataset kobe


#SUA VEZ

#Comparando Kobe Bryant ao Arremessador Independente

#Utilizando a função calc_streak, calcule o comprimento das sequências do vetor sim_basket.
#1. Descreva a distribuição das sequências de arremessos. Qual é o comprimento de sequência típico
#para o arremessador independente simulado com um percentual de arremesso de 45%? Quão longa
#é a sequência mais longa de cestas em 133 arremessos?

#2. Se você rodasse a simulação do arremessador independente uma segunda vez, como você acha que
#seria a distribuição de sequências em relação à distribuição da questão acima? Exatamente a mesma?
#Mais ou menos parecida? Completamente diferente? Explique seu raciocínio.

#3. Como a distribuição dos comprimentos de sequência de Kobe Bryant, analisada na página 2, se
#comparam à distribuição de comprimentos de sequência do arremessador simulado? Utilizando
#essa comparação, você tem evidência de que o modelo das mãos quentes se ajusta aos padrões de
#arremessos de Kobe? Explique.