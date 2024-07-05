#####################################################################################################
#carregando os dados
f <- file.choose("fish_catches.csv")
dados <- read.csv(f)
summary(dados)
dados
#alterando as colunas de Species, Area e Country para fatores
dados$Species <- as.factor(dados$Species)
dados$Area <- as.factor(dados$Area)
dados$Country <- as.factor(dados$Country)

#retirando as colunas de NA
library(tidyverse)
dados <- mutate(dados,
      X = NULL,
      X.1 = NULL,
      X.2 = NULL,
      X.3 = NULL,
      X.4 = NULL,
      X.5 = NULL,
      X.6 = NULL,
      X.7 = NULL,
      X.8 = NULL,
      X.9 = NULL,
      X.10 = NULL,
      X.11 = NULL,
      Units = NULL)

#retirando a última linha, que está vazia
dados <- dados %>% filter(row_number() <= n()-1)

names(dados) <- janitor::make_clean_names(names(dados))

write.csv(dados[1:5000,], 'dados_para_envio.csv')

dados2 <- dados %>% 
  pivot_longer(!c(species,area,country),names_to = "Ano",values_to = "Valores") %>% 
  mutate(Ano = str_sub(Ano,start = 2)) %>% 
  mutate(across(species:Ano,as.factor))

top_paises <- dados2 %>% 
  group_by(country) %>% 
  summarise(Total = sum(Valores)) %>% 
  arrange(desc(Total)) %>% 
  head(5) %>% 
  dplyr::select(country) %>% 
  unlist(use.names = F)

top20_paises <- dados2 %>% 
  group_by(country) %>% 
  summarise(Total = sum(Valores)) %>% 
  arrange(desc(Total)) %>% 
  head(20) %>% 
  dplyr::select(country) %>% 
  unlist(use.names = F)

top_especies <- dados2 %>% 
  group_by(species) %>% 
  summarise(Total = sum(Valores)) %>% 
  arrange(desc(Total)) %>% 
  head(10) %>% 
  dplyr::select(species) %>% 
  unlist(use.names = F)

top5_especies <- dados2 %>% 
  group_by(species) %>% 
  summarise(Total = sum(Valores)) %>% 
  arrange(desc(Total)) %>% 
  head(5) %>% 
  dplyr::select(species) %>% 
  unlist(use.names = F)

top20_especies <- dados2 %>% 
  group_by(species) %>% 
  summarise(Total = sum(Valores)) %>% 
  arrange(desc(Total)) %>% 
  head(20) %>% 
  dplyr::select(species) %>% 
  unlist(use.names = F)

#plot por ano e país
dados2 %>% 
  group_by(country, Ano) %>% 
  summarise(Total = sum(Valores)) %>%
  filter(country %in% top5paises) %>% 
  ggplot(aes(x = Ano, y = Total))+
  geom_line(aes(group = country,
                colour = country))+
  lims(y = c(0,NA))

#plot por ano e espécie
dados2 %>% 
  group_by(species,Ano) %>% 
  summarise(Total = sum(Valores)) %>%
  filter(species %in% top5especies) %>% 
  ggplot(aes(x = Ano, y = Total))+
  geom_line(aes(group = species,
                colour = species))+
  lims(y = c(0,NA))+theme(axis.text=element_text(size=10),
                          axis.title=element_text(size=14,face="bold"))

#plot por ano e valores totais
library(GGally)

dados2 %>% 
  group_by(Ano) %>% 
  summarise(Total = sum(Valores)) %>%
  ggplot(aes(x = Ano, y = Total, group = 1))+
  geom_line()+theme(axis.text=element_text(size=12),
                    axis.title=element_text(size=14,face="bold"))+
  scale_y_continuous(labels = scales::number)+
  geom_point()+
  stat_smooth(method = "lm")

x11()
dados2 %>% 
  group_by(species,country) %>% 
  summarise(Total = sum(Valores)) %>% 
  pivot_wider(names_from = country, values_from = Total, values_fill = 0) %>% 
  ggpairs()

tab <- dados2 %>% 
  group_by(species) %>% 
  summarise(Total = sum(Valores)) %>% 
  arrange(desc(Total)) %>%
  as.data.frame() %>% 
  mutate(Total = prettyNum(Total, big.mark = ".", decimal.mark = ",")) %>% 
  head(10)

