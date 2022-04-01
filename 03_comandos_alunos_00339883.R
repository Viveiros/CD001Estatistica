# SUA VEZ

download.file("http://www.openintro.org/stat/data/bdims.RData", destfile = "bdims.RData")
load("bdims.RData")

#1. Agora vamos analisar outras variáveis no conjunto de dados das dimensões corporais. Utilizando
# as figuras na próxima página, combine os histogramas com seus gráficos de probabilidade normal.
# Todas as variáveis foram estandardizadas (primeiro subtraindo a média, e em seguida dividindo pelo
# desvio padrão), de tal forma que as unidades não serão de qualquer ajuda. Se você estiver incerto
# com base nessas figuras, gere um gráfico no R para verificar.
#  R. (a) O histograma do diâmetro bi-ilíaco (pélvico) feminino (bii.di) pertence ao gráfico de probabili-
#  dade normal de letra B.
fbii.dimean <- mean(fdims$bii.di)
fbii.disd <- sd(fdims$bii.di)
fdims$Zbii.di <- (fdims$bii.di - fbii.dimean) / fbii.disd
qqnorm(fdims$Zbii.di)
qqline(fdims$Zbii.di)
#  R. (b) O histograma do diâmetro do cotovelo feminino (elb.di) pertence ao gráfico de probabilidade
#  normal de letra C.
felb.dimean <- mean(fdims$elb.di)
felb.disd <- sd(fdims$elb.di)
fdims$Zelb.di <- (fdims$elb.di - felb.dimean) / felb.disd
qqnorm(fdims$Zelb.di)
qqline(fdims$Zelb.di)
#  R. (c) O histograma de idade geral (age) pertence ao gráfico de probabilidade normal de letra D.
fagemean <- mean(fdims$age)
fagesd <- sd(fdims$age)
fdims$Zage <- (fdims$age - fagemean) / fagesd
qqnorm(fdims$Zage)
qqline(fdims$Zage)
#  R. (d) O histograma de profundidade do peito feminino (che.de) pertence ao gráfico de probabilidade
#  normal de letra A.
fche.demean <- mean(fdims$che.de)
fche.desd <- sd(fdims$che.de)
fdims$Zche.de <- (fdims$che.de - fche.demean) / fche.desd
qqnorm(fdims$Zche.de)
qqline(fdims$Zche.de)

#2. Perceba que os gráficos de probabilidade normal C e D tem um pequeno padrão passo a passo. Por
#que você acha que eles são assim?
# R. Porque todas as variáveis foram estandardizadas.

#3. Como você pode ver, gráficos de probabilidade normal podem ser utilizados tanto para avaliar a
#normalidade quanto visualizar a assimetria. Crie um gráfico de probabilidade normal para o diâ-
#  metro do joelho feminino (kne.di). Baseado neste gráfico de probabilidade normal, você diria que
#essa variável é simétrica, assimétrica à direita ou assimétrica à esquerda? Utiliza um histograma para
#confirmar seu resultado.
# R.  assimétrica à direita.
hist(fdims$kne.di)


# Média – Moda = 0 → Assimetria nula ou distribuição simétrica;
# Média – Moda < 0 → Assimetria negativa ou à esquerda;
# Média – Moda > 0 → Assimetria positiva ou à direita.