# Sua Vez - Laboratório 07

# Vamos carregar os dados da temporada de 2011.
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")

# 1. Escolha outra variável tradicional contida no banco de dados mlb11 que você acha que poderia ser
# um bom preditor da variável runs (pontos). Crie um gráfico de dispersão das duas variáveis e ajuste
# um modelo linear. Visualmente, parece haver uma relação linear?
# R. Criar gráfico de dispersão
plot(mlb11$hits, mlb11$runs)
cor(mlb11$runs, mlb11$hits)
# R. Ajustar a um modelo linear
m1 <- lm(mlb11$runs ~ mlb11$hits)
summary(m1)
abline(m1)
plot(m1,1)
plot(m1,2)
# R. Sim parece haver uma boa relação linear
shapiro.test(residuals.lm(m1))
  
########################################################

# 2. Compare essa relação com a relação entre runs (pontos) e at_bats (vez ao taco). Utilize os valores
# R2 do sumário dos dois modelos para compará-los. A variável que vocês escolheu parece predizer
# runs (pontos) melhor do que at_bats (vez ao taco)? Como você justificaria sua resposta?
plot(mlb11$at_bats, mlb11$runs)
cor(mlb11$runs, mlb11$at_bats)
# R. Ajustar a um modelo linear
m2 <- lm(mlb11$runs ~ mlb11$at_bats)
summary(m2)
abline(m2)
plot(m2,1)
plot(m2,2)
# R. Sim parece haver uma boa relação linear
shapiro.test(residuals.lm(m2))

# R. Para a variavel hits encontramos Multiple R-squared:  0.6419,
# para a variavel at_bats encontramos Multiple R-squared:  0.3729, sendo assim 
# A variável hits apresenta 64% da variabilidade nos resultados de pontos enquanto a variável at_bats
# representa 37% da variabilidade de pontos. Portanto a variável hits consegue predizer melhor do que at_bats.

########################################################

# 3. Agora que você pode resumir a relação linear entre duas variáveis, investigue a relação entre runs
# (pontos) e cada uma das outras cinco variáveis tradicionalmente utilizadas no beisebol. Qual variável
# prediz melhor o valor de runs? Justifique sua conclusão utilizando métodos gráficos e numéricos
# que já discutimos (para ser conciso, inclua apenas os resultados da melhor variável, não de todas as cinco).

cor(mlb11$runs, mlb11$at_bats)
m3 <- lm(mlb11$runs ~ mlb11$at_bats)
summary(m3) #Multiple R-squared:  0.3729

cor(mlb11$runs, mlb11$homeruns)
m3 <- lm(mlb11$runs ~ mlb11$homeruns)
summary(m3)#Multiple R-squared:  0.6266

cor(mlb11$runs, mlb11$stolen_bases)
m3 <- lm(mlb11$runs ~ mlb11$stolen_bases)
summary(m3)# Multiple R-squared:  0.002914

cor(mlb11$runs, mlb11$hits)
m3 <- lm(mlb11$runs ~ mlb11$hits)
summary(m3) #Multiple R-squared:  0.6419 

cor(mlb11$runs, mlb11$bat_avg)
m3 <- lm(mlb11$runs ~ mlb11$bat_avg)
summary(m3)#Multiple R-squared:  0.6561
plot(mlb11$bat_avg, mlb11$runs)
abline(m3)
plot(m3,1)
plot(m3,2)
plot(m3$residuals ~ mlb11$bat_avg)
abline(h = 0, lty = 3)
hist(m3$residuals)
qqnorm(m3$residuals)
qqline(m3$residuals)
# R. Sim parece haver uma boa relação linear p-value = 0.6035
shapiro.test(residuals.lm(m3))

# R. A melhor variável prediz o valor de runs, é bat_avg com 65,61% da variabilidade nos resultados de pontos
# pode ser devida as bat_avg (média de rebatidas), ou seja, 34,39% da variabilidade total é devido a outros fatores que não foram investigados.

########################################################

# 4. Agora examine as três variáveis mais recentes. Essas são as estatísticas utilizadas pelo autor do
# filme O Homem que Mudou o Jogo para predizer o sucesso de um time. De modo geral, elas são
# mais ou menos eficazes para predizer os pontos do que as variáveis mais tradicionais? Explique
# utilizando evidências gráficas e numéricas. De todas as dez variáveis que nós analisamos, qual
# parece ser o melhor preditor da variável runs (pontos)? Utilizando as informações limitadas (ou não
# tão limitadas) que você conhece sobre estas estatísticas do beisebol, seu resultado faz sentido?

cor(mlb11$runs, mlb11$new_slug)
m4 <- lm(mlb11$runs ~ mlb11$new_slug)
summary(m4) #Multiple R-squared:  0.8969

cor(mlb11$runs, mlb11$new_onbase)
m4 <- lm(mlb11$runs ~ mlb11$new_onbase)
summary(m4) #Multiple R-squared:  0.8491

cor(mlb11$runs, mlb11$new_obs)
m4 <- lm(mlb11$runs ~ mlb11$new_obs)
summary(m4) #Multiple R-squared:  0.9349 
plot(mlb11$new_obs, mlb11$runs)
abline(m4)
plot(m4,1)
plot(m4,2)
plot(m4$residuals ~ mlb11$new_obs)
abline(h = 0, lty = 3)
hist(m4$residuals)
qqnorm(m4$residuals)
qqline(m4$residuals)
# R. Sim parece haver uma boa relação linear p-value = 0.8434
shapiro.test(residuals.lm(m4))

#R. Elas são mais eficazes para predizer os pontos do que as variáveis mais tradicionais.
# As novas variáveis tem acima de 80% da variabilidade nos resultados de pontos enquanto hits ficou com 64%.
# A melhor preditora ficou com new_obs com 93,49% da variabilidade nos resultados de pontos.
# Essa variável new_obs nas estatísticas do beisebol, é o calculada como a soma das outras duas outras variáveis.
# então imagino que traga mais variabilidade nos resultados de pontos.

########################################################

# 5. Verifique os diagnósticos do modelo para o modelo de regressão com a variável que você escolheu
# como o melhor preditor dos pontos (runs).

# Para avaliar se um modelo linear é confiável, precisamos verificar (1) a linearidade, 
# (2) resíduos normalmente distribuídos, e (3) variância constante.

m5 <- lm(mlb11$runs ~ mlb11$new_obs)
summary(m5) #Multiple R-squared:  0.9349 

# (1) a linearidade
plot(mlb11$new_obs, mlb11$runs)
plot(m5$residuals ~ mlb11$new_obs)
abline(h = 0, lty = 3) 
# (2) resíduos normalmente distribuídos
hist(m5$residuals)
qqnorm(m5$residuals)
qqline(m5$residuals) # adiciona uma linha diagonal ao gráfico de probabilidade normal     
# (3) variância constante
# R. Sim é atentida


# FINALIZANDO...
# Encontrar um modelo de Regressão Linear Múltipla para os dados do LAB07.

# matriz de correlação para escolher a primeira variável
library(DataExplorer)
plot_correlation(mlb11)

# Modelo completo
m6 <- lm(mlb11$runs ~ mlb11$at_bats+mlb11$hits+mlb11$homeruns+mlb11$bat_avg+mlb11$strikeouts+mlb11$stolen_bases+mlb11$wins+mlb11$new_obs)
summary(m6)
stepwise <-step(m6,direction="both")
summary(stepwise)
stepwise$formula
# R. Escolheu fazendo uma simulação retirando e adicionando vairiáveis.
# lm(formula = mlb11$runs ~ mlb11$stolen_bases + mlb11$new_obs)