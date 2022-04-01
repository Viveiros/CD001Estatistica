# A questão sobre o ateísmo foi também feita pelo WIN-Gallup International numa pesquisa de opinião
# parecida realizada em 2005. A Tabela 4 na página 13 do relatório resume os resultados da pesquisa de
# 2005 a 2012 em 29 países.

# Os Dados
download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")

# 1. Responda às duas perguntas seguintes utilizando a função inference. Como sempre, descreva as
# hipóteses para qualquer teste que você realizar e esboce sobre as condições para inferência.

# (a) Há evidência convincente de que a Espanha teve uma mudança em seu índice de ateísmo entre
# 2005 e 2012?
esp05 <- subset(atheism, atheism$nationality == "Spain" & atheism$year == "2005")
table(esp05$response)
115+1031
table(esp05$response)/1146
# R. 10,03%
esp12 <- subset(atheism, atheism$nationality == "Spain" & atheism$year == "2012")
table(esp12$response)
103+1042
table(esp12$response)/1145
# R. 8,99%
#   Dica: Crie um novo conjunto de dados para os respondentes da Espanha. Depois, utilize suas
# respostas como a primeira entrada na função inference, e utilize a variável year (ano) para
# definir os grupos.
spain_2005_2012 <- subset(atheism, atheism$nationality == "Spain")
table(spain_2005_2012$response)
table(spain_2005_2012$year)
# inference(y = spain_2005_2012$response ,  x = spain_2005_2012$year, est = "proportion", type = "ci", conflevel = 0.95, method = "theoretical", success = "atheist")
inference(y = spain_2005_2012$response ,  x = spain_2005_2012$year, est = "proportion", type = "ht", alternative="twosided", conflevel = 0.95, method = "theoretical", success = "atheist")
# R. 95% de intervalo de confiança e com o p-value =  0.3966, não podemos rejeitar H0,
# sendo assim não é possível afirmar que na Espanha tiveram mudança em seu indice de ateísmo entre 2005 e 2012.

# (b) Há evidência convincente de que os Estados Unidos tiveram uma mudança em seu índice de
# ateísmo entre 2005 e 2012?
eua_2005_2012 <- subset(atheism, atheism$nationality == "United States")
table(eua_2005_2012$response)
table(eua_2005_2012$year)
inference(y = eua_2005_2012$response ,  x = eua_2005_2012$year, est = "proportion", type = "ht", alternative="twosided", conflevel = 0.95, method = "theoretical", success = "atheist")
# R. 95% de intervalo de confiança e com o p-value =  0, não podemos rejeitar H0,
# sendo assim não é possível afirmar que na Espanha tiveram mudança em seu indice de ateísmo entre 2005 e 2012.

#   2. Se de fato não houve nenhuma mudança no índice de ateísmo nos países listados na Tabela 4, em
# quantos países você esperar detectar uma mudança (com um nível de significância de 0,05) simplesmente
# por acaso?
#   Dica: Procure no índice do livro sobre erros do Tipo 1.
# R. Erro do tipo 1 é rejeitar H0 verdadeira, 0,05 * 39.
0.05 * 39
# R. 1.95, aproximadamente 2 países

# 3. Suponha que você foi contratado pelo governo local para estimar a proporção de residentes que
# participam de cultos religiosos semanalmente. De acordo com diretrizes, a estimativa deve ter uma
# margem de erro inferior a 1% com nível de confiança de 95%. Você não tem nenhuma noção de que
# valor supor para p. Quanto pessoas você teria que amostrar para garantir que você está dentro das
# diretrizes?
#   Dica: Retome seu gráfico da relação entre p e a margem de erro. Não use o conjunto de dados para
# responder a essa questão.
1.96^2 * ( (0.5*(1-0.5)) / 0.01^2)
# R. Precisamos de 9605 participantes ou mais, para garantir que a proporção da amostra esteja dentro 0.01
# da proporção rela com 95% de confiança.

# Exercícios Aula 5
# Fazer exercícios do slide 17.
# Utilize os três exemplos feitos em aula como referência (para resolver e interpretar).

# Exercício 1: Um fabricante garante que 90% dos equipamentos que fornece a uma
# fábrica estão de acordo com as especificações exigidas. O exame de uma amostra
# de 200 peças desse equipamento revelou 25 defeituosas. Obtenha o intervalo de
# confiança de 95% para a proporção de equipamentos defeituosos e teste a garantia
# do fabricante. 
# IC=(0.08342289 0.16657711) z_calculado= 1.178511 z_critico=1.959964 p-valor=0.2385928

n = 200
x = 25
phat = x/n
p0 = 0.10
SE = sqrt(p0*(1-p0)/n)
# esse 
phat + c(-1, 1)*qnorm(0.025,lower.tail = F)*SE #IC
# ou esse da na mesma
phat + c(-1, 1)*qnorm(.975)*SE #IC

(phat-p0)/SE # z_calculado
qnorm(.975) # z_critico
2*pnorm((phat-p0)/SE,lower.tail=F) # p-valor

prop.test(25,200,p=0.10,alternative="two.sided",correct = F,conf.level = 0.95) #aproximado

# Para uma proporções a ser comparada.
binom.test(25,200,p=0.10,alternative="two.sided",conf.level = 0.95) #exato

# R. 95% de intervalo de confiança e com o p-value = 0.2385928, não podemos rejeitar H0,
# sendo assim não é possível afirmar que o fabricante extrapola a margem da garantia.

# Exercício 2: Em uma pesquisa de opinião, 32 dentre 80 homens declararam
# apreciar certa revista, acontecendo o mesmo com 26 dentre 50 mulheres. Ao nível
# de 5% de significância os homens e as mulheres apreciam igualmente a revista?
# IC=(-0.29521683 0.05521683) z_calculado=-1.342312 z_critico=1.959964 p-valor=0.1794948

n1 = 80
x1 = 32
n2 = 50
x2 = 26
phat1 = x1/n1
phat2 = x2/n2
SE = sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2))
(phat1-phat2) + c(-1, +1)*qnorm(.975)*SE #IC
(phat1-phat2)/SE # z_calculado
qnorm(.975) # z_critico
2*pnorm((phat1-phat2)/SE,lower.tail=T) # p-valor

prop.test(x=c(32,26),n=c(80,50),alternative="two.sided",correct = F,conf.level = 0.95) #aproximado

# Para duas proporções a serem comparadas
fisher.test(matrix(c(32,48,26,24),nrow=2),alternative="two.sided",conf.level = 0.95) #exato

# R. 95% de intervalo de confiança e com o p-value = 0.1794948, não podemos rejeitar H0,
# sendo assim não é possível afirmar que existe diferença no aprecio da revista entre homens e mulheres.

# Exercício 3: A tabela abaixo resume os resultados de uma pesquisa da Pew
# Research. Gostaríamos de determinar se há realmente diferenças nas
# classificações de aprovação de Obama, democratas no Congresso e republicanos
# no Congresso. Utilizando nível de significância de 5%, teste as hipóteses:
#  H0: Não há diferença nas classificações de aprovação entre os três grupos.
#  H1: Existe alguma diferença nas classificações de aprovação entre os três grupos.
#             Obama  Democratas  Replublicanos   Total
# Aprovam      842    736         541             2119
# Desaprovam   616    646         842             2104
# Total        1458   1382        1382            4223

# X-squared = 106.35
# p-value < 2.2e-16
# valor crítico=5.991465
# Encontrar resíduos ajustados maiores que 1,96

# teste qui-quadrado de independencia
dados=matrix(c(842,616,736,646,541,842),nrow=2)
dados
chisq.test(dados)

# valor crítico
# Sempre do lado direito com o total de 5%
qchisq(0.05,df=2,lower.tail=FALSE)

# análise de resíduos
chisq.test(dados)$stdres

# R. Como p-value < 2.2e-16 < 5%, rejeitamos H0 e concluímos que existe diferenças nas classificações,
# sendo que Obama e Democratas (resíduo ajustado respectivamente 7.147065 e 2.790668 > 1.96) de melhor classificação.
# verificacao de resíduos, como o x2 calculado = 106.35 > x2 crítico 5.991465, rejeitamos H0.


