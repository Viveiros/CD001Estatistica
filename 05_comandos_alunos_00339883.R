#Nascimentos na Carolina do Norte em 2004

download.file("http://www.openintro.org/stat/data/nc.RData", destfile = "nc.RData")
load("nc.RData")

#sumário dos dados de todas as variáveis do banco de dados
summary(nc)

#1.Calcule o intervalo de confiança de 95% para a duração média das gravidezes (weeks) e o interprete
#no contexto do conjunto de dados. Perceba que, uma vez que você está realizando uma inferência
#sobre um único parâmetro populacional, não há nenhuma variáveis explanatória, e portanto você
#pode omitir a variável x da função.

#gráfico para verificar presença de outlier - semanas de gestação
boxplot(nc$weeks)

inference(y = nc$weeks, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical")
# R. IC é: (38.1528 ; 38.5165)


#2. Calcule um novo intervalo de confiança para o mesmo parâmetro com nível de confiança de 90%.
#Você pode mudar o nível de confiança adicionando um novo argumento à função: conflevel =0.90.
inference(y = nc$weeks, est = "mean", type = "ci", null = 0, conflevel = 0.90,
          alternative = "twosided", method = "theoretical")
# R. 90 % Confidence interval = ( 38.182 , 38.4873 )

#3. Realize um teste de hipótese para avaliar se a média do peso ganho pelas mães mais jovens é
#diferente da média de peso ganho pelas mães mais velhas.
# gained - peso ganho pela mãe durante a gravidez, em libras.
# mature - maioridade da mãe.

boxplot(nc$gained ~ nc$mature)


inference(y = nc$gained, x = nc$mature, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")

inference(y = nc$gained, x = nc$mature, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical")

# R. p-valor = 0.1686
# R. Como o p-valor é considerável não podemos rejeitar a hipótese nula.
# R. Conclusao Maes maduras e jovens podem ganhar peso da mesma forma com nivel de 
# significancia de 5% ou p-valor = 16,86%

#4. Agora, um tarefa não-inferencial: determine o ponto de corte da idade das mães jovens e maduras.
#Utilize um método da sua escolha, e explique como seu método funciona.

# R. Até 34 anos as mães são consideradas jovens com 35 anos em diante são consideradas maduras.
# Para dividir o intervalo usei o comando by, dividindo a variável mage nos grupos da variável mature, 
# e fazendo um summary
by(nc$mature, nc$mage, summary)
# Outra forma de verificar é com boxplot
boxplot(nc$mage ~ nc$mature)


#5. Escolha um par de variáveis, sendo uma numérica e outra categorial, e desenvolva um pergunta de
#pesquisa para avaliar a relação entre essas variáveis. Formule a questão de maneira que ela possa ser
#respondida utilizando um teste de hipótese e/ou um intervalo de confiança.

#R. Realize um teste de hipótese para avaliar a diferença média dos pesos dos bebês prematuros
# e a termo. Normalmente os bebes prematuros são menores que os bebes a termo?
# weight - peso do bebê no nascimento, em libras.
# premie - se o nascimento é classificado como prematuro ou a termo.

# Responda a sua questão utilizando a função inference, informe os resultados estatísticos, 
# e também elabora uma explicação em linguagem simples.

boxplot(nc$weight ~ nc$premie)
inference(y = nc$weight, x = nc$premie, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")

# R. p-valor = 0
# R. Como o p-valor é zero podemos rejeitar a hipótese nula.
# R. Conclusao bebes prematuros geralmente tem peso menor que bebes a termo.
# significancia de 5% ou p-valor = 0.