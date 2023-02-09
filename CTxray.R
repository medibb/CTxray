library(readxl)
library(DescTools)
library(moments)
library(tidyverse)
library(gridExtra)

Calcification <- data.frame(0,0,0,0,0,0,0)
NormalTendon <- data.frame(0,0,0,0,0,0,0)
colnames(Calcification)<- c('mean', 'skewness', 'kurtosis', 'entropy', 'complexity', 'Q1', 'Q3')
colnames(NormalTendon)<- c('mean', 'skewness', 'kurtosis', 'entropy', 'complexity', 'Q1', 'Q3')

for (i in 1:57){
S1 <- read_excel(path = "./histogram.xlsx",sheet = i,col_names=c('index','calc','nort'))

#Healthy nort, patients calc
H1 <- S1 %>%
  filter(nort > 0 ) %>%
  select(index,nort)

P1 <- S1 %>%
  filter(calc > 0 ) %>%
  select(index,calc) 

S1 <- S1 %>%
  mutate(normalcalc = (calc-mean(calc))/sd(calc), normalnort = (nort-mean(nort))/sd(nort)) %>%
  mutate(cumcalc = cumsum(calc), cumnort = cumsum(nort))

PQ1 <- S1 %>%  filter(cumcalc > sum(P1$calc)/4)
PQ3 <- S1 %>%  filter(cumcalc < sum(P1$calc)*3/4)
HQ1 <- S1 %>%  filter(cumnort > sum(H1$nort)/4)
HQ3 <- S1 %>%  filter(cumnort < sum(H1$nort)*3/4)

# #complexity
# Entropy(S1$nort)*(Entropy(S1$nort) - 1)/4
# Entropy(S1$calc)*(Entropy(S1$calc) - 1)/4

# variables ( mean, skewness, kurtosis, entropy, Q1, Q3)
df1 = c(sum(P1$index*P1$calc)/sum(P1$calc), skewness(P1$calc), kurtosis(P1$calc), Entropy(P1$calc), Entropy(S1$nort)*(Entropy(S1$nort) - 1)/4, as.numeric(PQ1[1,1]), as.numeric(PQ3[nrow(PQ3),1]))
df2 = c(sum(H1$index*H1$nort)/sum(H1$nort), skewness(H1$nort), kurtosis(H1$nort), Entropy(H1$nort), Entropy(S1$calc)*(Entropy(S1$calc) - 1)/4, as.numeric(HQ1[1,1]), as.numeric(HQ3[nrow(HQ3),1]))

Calcification <- rbind(Calcification, df1)
NormalTendon <- rbind(NormalTendon, df2)

# graph

a = ggplot(data = as.data.frame(P1), aes(x=index, y=calc)) +
  geom_col(fill = "red", alpha = 0.5) +
  geom_col(data= as.data.frame(H1), aes(x=index, y=nort), fill = "blue", alpha = 0.5) +
  ylab("Counts of bins") +
  xlab("Greyscale (0-255)")


# normalized graph

b = ggplot(data = as.data.frame(S1), aes(x=index, y=normalcalc)) +
  geom_col(fill = "red", alpha = 0.5) +
  geom_col(aes(x=index, y=normalnort), fill = "blue", alpha = 0.5) +
  ylim(c(0,max(c(S1$normalcalc, S1$normalnort)))) +
  ylab("Nomalized counts of bins") + 
  xlab("Greyscale (0-255)")

grid.arrange(a,b, nrow=1, ncol=2)

}

Calcification <- Calcification[-1,]
NormalTendon <- NormalTendon[-1,]

write.csv(Calcification, file = 'calcification.csv')
write.csv(NormalTendon, file = 'NormalTendon.csv')
