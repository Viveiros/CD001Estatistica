### Arquivo de ajuda na prova

# ------ Verdades
# - Amostra é o conjunto formado por uma parcela dos elementos ou resultados sob investigação
# - A abordagem estatística envolvida ao generalizarmos resultados de uma amostra para toda 
#   uma população é chamada inferência estatística
# ------ Mentiras
# - Parâmetro é a medida usada para descrever uma característica numérica da população.
# - Temos o interesse na altura média dos jogadores de futebol profissional no Brasil. 
#   As alturas de todos os jogadores profissionais dos diversos clubes nacionais são medidas. 
#   Então, a média e o desvio padrão das alturas são calculadas. Dessa forma, se observou 
#   o censo dos jogadores profissionais
# - A abordagem estatística envolvida no processo usado para se obter uma parte representativa 
#   da população é chamada amostragem.
# - Suponha que todos os alunos de uma escola foram investigados com o objetivo de saber o seu
#   desempenho (bom, ruim e regular) nas aulas assíncronas de matemática. O resultado desta 
#   proporção foi de 36% para desempenho bom, o que representa um parâmetro



#################
# Obter a média, a mediana e o desvio-padrão
media.mediana.sd <- c(21,15,16,18,15,20,23,28,16,28,18)
summary(media.mediana.sd)
sd(media.mediana.sd)

#################
# Uso do histograma

dados <- c(18.7,19.8,19.9,20.7,21.0,21.0,21.2,22.1,22.1,22.5,
              23.3,23.3,23.4,23.7,23.8,23.9,24.0,24.1,24.2,25.2,
              25.4,25.5,25.6,26.4,26.4,27.0,27.3,27.4,28.6,29.1)
h <- hist(dados, freq = TRUE, breaks = c(15, 18, 21, 24, 27, 30) , xlim = c(15,30), ylim = c(0,15))
h <- hist(dados, freq = TRUE, breaks = c(15, 17.5, 20, 22.5, 25, 27.5, 30) , xlim = c(15,30), ylim = c(0,15))
h <- hist(dados, freq = TRUE, breaks = c(15, 17.14, 19.29, 21.43, 23.57, 25.71, 27.86, 30) , xlim = c(15,30), ylim = c(0,15))
h <- hist(dados, freq = TRUE, breaks = c(15, 16.88, 18.75, 20.62, 22.5, 24.38, 26.25, 28.12,30) , xlim = c(15,30), ylim = c(0,15))
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

##################################

X <- c(15,22,23,19,21,22,18,20)
Y <- c(25,33,38,40,33,45,30,28)
m1 <- lm(Y ~ X)
summary(m1)

1.5+1.6250*20
37
