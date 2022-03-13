
#SUA VEZ

#Comparando Kobe Bryant ao Arremessador Independente

#Utilizando a função calc_streak, calcule o comprimento das sequências do vetor sim_basket.

#1. Descreva a distribuição das sequências de arremessos. Qual é o comprimento de sequência típico
#para o arremessador independente simulado com um percentual de arremesso de 45%? 
#R. zero
#Quão longa é a sequência mais longa de cestas em 133 arremessos?
# R. seis

outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))
sim_basket
sim_streak <- calc_streak(sim_basket)
table(sim_streak)
barplot(table(sim_streak))

#2. Se você rodasse a simulação do arremessador independente uma segunda vez, como você acha que
#seria a distribuição de sequências em relação à distribuição da questão acima? 
#Exatamente a mesma? #Mais ou menos parecida? Completamente diferente? Explique seu raciocínio.
# R. Não é exatamente as mesmas observações de acertos e erros a semente não está fixada, 
# então a função sample irá calcular dentro da mesma probabilidade pedida de 45% novos arremessos.
# Os números de acertos e erros podem ser completamente diferentes.
# Porém o gráfico será parecido assimétrico com maior frequencia de zeros e menor frequencia 
# de maiores acertos.

#3. Como a distribuição dos comprimentos de sequência de Kobe Bryant, analisada na página 2, se
#comparam à distribuição de comprimentos de sequência do arremessador simulado? 
# R. Considerando a independencia como foi simulado, a simulação é comparável a sequencia de Kobe Bryant
# Utilizando essa comparação, você tem evidência de que o modelo das mãos quentes se ajusta aos padrões de
#arremessos de Kobe? Explique.
# O modelo das mãos quentes não se ajusta aos padrões de arremessos de kobe, porque vimos que
# há pouca frequencia de acertos repetitivos.


