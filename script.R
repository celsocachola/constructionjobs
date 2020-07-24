#Média de ocupação dos útlimos anos na Construção Civil, em todos os tipos de empresas
mean(EmpCC$T)

#Encontrando todos oo resultados descritivos da Matriz
summary(EmpCC)

#gráfico da evolução do empregos
plot(EmpCC$Y, EmpCC$T, xlab="Ano", ylab="Quantidade de Empregos", main="Evolução dos empregos", col="blue");
lines(EmpCC$Y, EmpCC$T, col="blue")

#gráfico da evolução dos PIBs
ggplot(EmpCC, aes(Y)) +                    # basic graphical object
  geom_line(aes(y=PIBBr), colour="red", size=1) +  # first layer
  geom_area(aes(y=PIBCC), colour="green", fill="green") + # second layer
  labs (y="Value",
        x="Year")

#correlação entre quantidades de emprego e valor do PIB, setor da construção civil e nacioal
cor(EmpCC$T,EmpCC$PIBCC)
cor(EmpCC$T,EmpCC$PIBBr)
cor(EmpCC$PIBBr,EmpCC$PIBCC)

#análise regressional
regPIBCC <- lm(EmpCC$T~EmpCC$PIBCC)
regPIBBr <- lm(EmpCC$T~EmpCC$PIBBr)

#valores méidos da regressão
summary(regPIBCC)
summary(regPIBBr)

#gráfico de dispersão entre PIBBr e empregos
ggplot(EmpCC, aes(y = T, x = PIBBr)) + 
  geom_point() + 
  scale_x_continuous() +
  geom_text(aes(label = Y), nudge_x = 1, nudge_y = 1, check_overlap = TRUE, vjust=1.2) +
  geom_smooth() +
  labs (y="Number of jobs (civil construction)",
        x="GDP (Brazil)"
  )

#Test-T
t.test(EmpCC$T,EmpCC$PIBBr, paired = TRUE)
t.test(EmpCC$T,EmpCC$PIBCC, paired = TRUE)
t.test(EmpCC$PIBBr,EmpCC$PIBCC, paired = TRUE)

#Criando uma tabela só com os dados dos empregos
Tabela1 <- EmpCC[c("Y","A","B","C","T")]

#renomeando as colunas
library('plyr')
Tabela1 <- rename(Tabela1,c("Y" = "Year", "A" = "1 to 4 busy people",
                            "B" = "5 to 29 busy people",
                            "C" = "30 or more people busy",
                            "T" = "Total"))
Tabela1 %>%
  kable() %>%
  kable_styling()

#gráfico de empregos por tipo de empresa
ggplot(EmpCC, aes(Y)) +                    # basic graphical object
  geom_line(aes(y=A), colour="yellow", size=1) +  # first layer
  geom_line(aes(y=B), colour="orange", size=1) + # second layer
  geom_line(aes(y=C), colour="red", size=1) +
  geom_line(aes(y=T), colour="blue", size=1) +
  labs (y="Amount employees",
        x="Year")