###
# Exercícios Aula 4
# Fazer exercícios 1 e 2 dos slides 15, 18 e 22.
# Utilize os três exemplos feitos em aula como referência (para resolver e interpretar).
###

# Exercício 1: Para testar a performance em termos de consumo de combustível
# de um novo carro compacto, o fabricante sorteou seis motoristas profissionais
# que dirigiram o automóvel de Pelotas a Porto Alegre. O consumo do carro (em
# litros) para cada um dos seis motoristas foi de
#           27,2; 29,3; 31,5; 28,7; 30,2; 29,6.
# Baseado nesses dados e utilizando nível de significância de 5%, o fabricante
# pode indicar que o consumo médio do novo carro é de 30 litros para viagens
# nesse percurso?
#           t0,025 = 2,571 tc = -0,9894 p-valor = 0,3679

mi0 <- 30 
amostra <- c(27.2,29.3,31.5,28.7,30.2,29.6)
summary(amostra)
shapiro.test(amostra)
t.test(amostra, alternative = "two.sided", mu = mi0, conf.level = 0.95) 
qt(0.025,df=5,lower.tail = T)
qt(0.025,df=5,lower.tail = F)
inference(amostra, est = "mean", type = "ht", null = mi0,
          alternative = "twosided", method = "theoretical")

# Como a amostra é pequena fizemos o shapiro.test e aceitamos H zero para normalidade.
# O intervalo de 95% de confiança (27.90109 ; 30.93224) inclui o consumo médio do fabricante.
# que é de 30 litros
# Mesmo resultado pelo teste t, comparando o t-calculado = -0.9894 com o
# t-crítico = ( -2.570582, 2.570582) está no intervalo e aceitamos H zero
# Ou ainda comprando o p-valor = 0.3679 com alfa = 0.05 (p > que alfa aceita H0).

#####################################

# Exercício 2: O Instituto de Nutrição da América Central e Panamá fez um estudo
# intensivo de resultados de dietas publicados em revistas científicas. Uma dieta
# aplicada a 15 pessoas produziu os seguintes níveis de colesterol (em mg/l):
#          204; 108; 140; 152; 158; 129; 175; 146; 157; 174; 192; 194; 144; 152; 135.
# Sabendo-se que o nível médio normal de colesterol é de 190 mg/l, verifique se a
# redução no teor médio de colesterol das pessoas submetidas a essa dieta foi
# significativa, com alfa=0,05.
#          t0,05 = -1,761 tc = - 4,803 (teste unilateral) p-valor = 0,0001404

mi0 <- 190
amostra <- c(204,108,140,152,158,129,175,146,157,174,192,194,144,152,135)
summary(amostra)
shapiro.test(amostra)
t.test(amostra, alternative = "greater", mu = mi0, conf.level = 0.95) 
?t.test
qt(0.05,df=14,lower.tail = T)

inference(amostra, est = "mean", type = "ht", null = mi0,
          alternative = "greater", method = "theoretical")

# Como a amostra é pequena fizemos o shapiro.test e aceitamos H zero para normalidade. p-value = 0.8541
# O intervalo de 95% de confiança de colesterol 145.3546 Inferior a 190, rejeita-se H0.
# Mesmo resultado pelo teste t, comparando o t-calculado = -4.8032 com o
# t-crítico = -1.76131 (valor crítico cai em uma das caudas, rejeita-se H0)
# Ou ainda comparando o p-valor = 0,0001404 com alfa = 0.05 (p < que alfa rejeita-se H0).

#####################################

# Exercício 1: Os dados abaixo dão os acertos obtidos por oito soldados num
# experimento destinado a determinar se a precisão do tiro é afetada pela maneira
# de dispor os olhos: com o olho direito aberto ou com o olho esquerdo aberto.
# Que conclusão você poderia tirar, com alfa = 0,05?
# Soldado 1 2 3 4 5 6 7 8
# Direito 44 39 33 56 43 56 47 58
# Esquerdo 40 37 28 53 48 51 45 60

# t0,025 = 2,365 tc = 1,4 p-valor = 0,2042

soldado_dir <- c(44,39,33,56,43,56,47,58) 
soldado_esq <- c(40,37,28,53,48,51,45,60) 
summary(soldado_dir)
summary(soldado_esq)
shapiro.test(soldado_dir) 
shapiro.test(soldado_esq) 
t.test(soldado_dir, soldado_esq, paired=T, alternative="two.sided") 
qt(0.025,df=7,lower.tail = T)
qt(0.025,df=7,lower.tail = F)
dif<-soldado_dir-soldado_esq #calculando as diferenças entre os pares
t.test(dif) #mesmo resultado do teste pareado

# Normalidade dos dados soldados olho esquerdo foi aceita (p = 0.9922 > alfa = 0.01)
# Normalidade dos dados soldados olho direito foi aceita (p = 0.4914 > alfa = 0.01)

# O intervalo de 95% de confiança (-1.20578 ; 4.70578) inclui o valor 0,
# não podemos rejeitar H0.
# Mesmo resultado pelo teste t, comparando o t-calculado = 1.4 com o
# t-crítico = ( -2.364624, 2.364624) está no intervalo, aceitamos H zero
# Ou ainda comparando o p-valor = 0,2042 com alfa = 0.05 (p > alfa aceitamos H0).

#####################################

# Exercício 2: Tendo interesse em estudar os efeitos de determinada dieta
# alimentar sobre o aumento do peso corporal em cobaias adultas, um
# investigador tomou uma amostra de 9 cobaias. Determinou seus pesos antes e
# três meses após a administração da nova dieta. Com os dados a seguir,
# analise o efeito da nova dieta, para alfa = 0,05.
# Antes 54 61 50 74 79 58 55 49 63
# Depois 57 66 53 73 82 58 56 53 63

# t0,05 = -1,8595 tc = -2,9104 p-valor = 0,009788

cobaia_antes <- c(54,61,50,74,79,58,55,49,63) 
cobaia_depois <- c(57,66,53,73,82,58,56,53,63) 
summary(cobaia_antes)
summary(cobaia_depois)
shapiro.test(cobaia_antes) 
shapiro.test(cobaia_depois) 
t.test(cobaia_antes, cobaia_depois, paired=T, alternative="two.sided")
qt(0.05,df=8,lower.tail = T)
dif<-cobaia_antes-cobaia_depois #calculando as diferenças entre os pares
t.test(dif) #mesmo resultado do teste pareado

# Normalidade dos dados cobaias antes foi aceita (p = 0.2947 > alfa = 0.01)
# Normalidade dos dados cobaias depois foi aceita (p = 0.1513 > alfa = 0.01)

# O intervalo de 95% de confiança (-3.5846498 ; -0.4153502) não inclui o valor 0,
# rejeitamos H0.
# Mesmo resultado pelo teste t, comparando o t-calculado = -2.9104 com o
# t-crítico = -1.859548 (valor crítico cai em uma das caudas então rejeita-se H0)
# Ou ainda comparando o p-valor = 0.01958 com alfa = 0.05 (p < alfa rejeitamos H0).

#####################################

# Exercício 1: Para verificar o grau de adesão de uma nova cola para vidros,
# preparam-se dois tipos de montagem; cruzado (A), onde a cola é posta em forma
# de X, e quadrado (B), onde a fórmula é posta nas 4 bordas. O resultado para a
# resistência das duas amostras está abaixo. Para um nível de 5% de significância
# que tipo de conclusão poderia ser tirada?

# Método A 16 14 19 18 19 20 15 18 17 18
# Método B 13 19 14 17 21 24 10 14 13 15

# f0,025 = 4,03   fc = 5       p-valor = 0,02507
# t0,025 = 2,169  tc = 0,953   p-valor = 0,3534

# fc = 0.2

A <- c(16,14,19,18,19,20,15,18,17,18)
B <- c(13,19,14,17,21,24,10,14,13,15)
summary(A)
summary(B)
# Primeiro teste, Testando a normalidade porque a amostra é pequena
shapiro.test(A) 
shapiro.test(B) 
# Normalidade dos dados colas A foi aceita (p = 0.5733 > alfa = 0.01)
# Normalidade dos dados colas B foi aceita (p = 0.5842 > alfa = 0.01)

# Segundo teste é verificar a variancia
var.test(A,B,alternative = c("two.sided"),conf.level = 0.95)
qf(0.025, df1=9, df2=9, lower.tail = TRUE)
qf(0.025, df1=9, df2=9, lower.tail = FALSE)

# a) fc = 0.2 < fc0.025 = 0.2483859
# p-valor = 0.02507 < alfa = 0.05
# Rejeitar H zero, Deve considerar a desigualdade de variâncias

t.test(A,B,alternative = c("two.sided"), var.equal=F, conf.level = 0.95) 
qt(0.025, df=12.462, lower.tail = T)
qt(0.025, df=12.462, lower.tail = F)

# a) tc = 0.95258 está no intervalo (-2.169888, 2.169888)
# p-valor = 0.3589 > alfa = 0.05
# Aceitar H zero

#Não se rejeita H0. Com significância de 5%, não foi possível verificar
# se existe efeito significativo na forma de usar a cola.


#####################################

# Exercício 2: A fim de comparar a eficácia de dois operários, foram tomadas, para
# cada um, sete medidas do tempo gasto, em segundos, para realizar certa
# operação. Os resultados obtidos são dados a seguir. Pergunta-se se, ao nível de
# 5% de significância, os operários devem ser considerados igualmente eficazes ou não.

# Operário A 35 32 40 36 35 32 33
# Operário B 29 35 36 34 30 33 31

# f0,025 = 5,82  fc = 1,137  p-valor = 0,8801
# t0,025 = 2,179 tc = 1,471  p-valor = 0,1671

A <- c(35,32,40,36,35,32,33)
B <- c(29,35,36,34,30,33,31)
summary(A)
summary(B)
# Primeiro teste, Testando a normalidade porque a amostra é pequena
shapiro.test(A) 
shapiro.test(B) 
# Normalidade dos dados operários A foi aceita (p = 0.2502 > alfa = 0.01)
# Normalidade dos dados operários B foi aceita (p = 0.7437 > alfa = 0.01)

# Segundo teste é verificar a variancia
var.test(A,B,alternative = c("two.sided"),conf.level = 0.95)
qf(0.025, df1=6, df2=6, lower.tail = TRUE)
qf(0.025, df1=6, df2=6, lower.tail = FALSE)

# a) fc = 1.137 < fc0.025 = 5.819757
# p-valor = 0.8801 > alfa = 0.05
# Aceitar H zero

t.test(A,B,alternative = c("two.sided"), var.equal=F, conf.level = 0.95) 
qt(0.025, df=11.951, lower.tail = T)
qt(0.025, df=11.951, lower.tail = F)
# a) tc = 1.4709 está no intervalo (-2.179804, 2.179804)
# p-valor = 0.1672 > alfa = 0.05
# Aceitar H zero

#Não se rejeita H0. Com significância de 5%, não foi possível verificar
# se existe efeito significativo na eficiencia entre os dois funcionários.