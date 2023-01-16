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
S1 <- read_excel(path = "C:/Users/Jaehyun/OneDrive/project/CTxray/220705DATA.xlsx",sheet = i,col_names=c('V1','V2','V3'))

H1 <- S1 %>%
  filter(V3 > 0 ) %>%
  select(V1,V3)

P1 <- S1 %>%
  filter(V2 > 0 ) %>%
  select(V1,V2) 

S1 <- S1 %>%
  mutate(normalV2 = (V2-mean(V2))/sd(V2), normalV3 = (V3-mean(V3))/sd(V3)) %>%
  mutate(cumV2 = cumsum(V2), cumV3 = cumsum(V3))

PQ1 <- S1 %>%  filter(cumV2 > sum(P1$V2)/4)
PQ3 <- S1 %>%  filter(cumV2 < sum(P1$V2)*3/4)
HQ1 <- S1 %>%  filter(cumV3 > sum(H1$V3)/4)
HQ3 <- S1 %>%  filter(cumV3 < sum(H1$V3)*3/4)

# #complexity
# Entropy(S1$V3)*(Entropy(S1$V3) - 1)/4
# Entropy(S1$V2)*(Entropy(S1$V2) - 1)/4

# variables ( mean, skewness, kurtosis, entropy, Q1, Q3)
df1 = c(sum(P1$V1*P1$V2)/sum(P1$V2), skewness(P1$V2), kurtosis(P1$V2), Entropy(P1$V2), Entropy(S1$V3)*(Entropy(S1$V3) - 1)/4, as.numeric(PQ1[1,1]), as.numeric(PQ3[nrow(PQ3),1]))
df2 = c(sum(H1$V1*H1$V3)/sum(H1$V3), skewness(H1$V3), kurtosis(H1$V3), Entropy(H1$V3), Entropy(S1$V2)*(Entropy(S1$V2) - 1)/4, as.numeric(HQ1[1,1]), as.numeric(HQ3[nrow(HQ3),1]))

Calcification <- rbind(Calcification, df1)
NormalTendon <- rbind(NormalTendon, df2)

# graph

a = ggplot(data = as.data.frame(P1), aes(x=V1, y=V2)) +
  geom_col(fill = "red", alpha = 0.5) +
  geom_col(data= as.data.frame(H1), aes(x=V1, y=V3), fill = "blue", alpha = 0.5) +
  ylab("Counts of bins") +
  xlab("Greyscale (0-255)")


# normalized graph

b = ggplot(data = as.data.frame(S1), aes(x=V1, y=normalV2)) +
  geom_col(fill = "red", alpha = 0.5) +
  geom_col(aes(x=V1, y=normalV3), fill = "blue", alpha = 0.5) +
  ylim(c(0,max(c(S1$normalV2, S1$normalV3)))) +
  ylab("Nomalized counts of bins") + 
  xlab("Greyscale (0-255)")

grid.arrange(a,b, nrow=1, ncol=2)

}
Calcification <- Calcification[-1,]
NormalTendon <- NormalTendon[-1,]

write.csv(Calcification, file = 'calcification.csv')
write.csv(NormalTendon, file = 'NormalTendon.csv')
