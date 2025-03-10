## call packages

library(ggplot2) 
library(viridis)
library(dplyr)
library(data.table)
library(tidyr)
library(readr)
library(tidyverse)
require(RColorBrewer)
library(ggExtra)
library(ggtext)
library (gridExtra)
#library(showtext)

# Abrir diretório automaticamente: ctrl + shift + h
############## pinhão per year
pino <- read.csv("ibge_pinhao_kg_year.csv", header=T,sep=";")
 
head(pino)
View(pino)

pino <- pino %>% 
  rename(
    #State  = ï..State,
    Production_kg = Production..kg.
  )
#pino$State = factor(pino$State,
#                             levels = c("Minas Gerais","SÃ£o Paulo", "ParanÃ¡","Santa Catarina",
#                                       "Rio Grande do Sul"),
#                           labels = c("Minas Gerais","São Paulo","Paraná","Santa Catarina",
#                                     "Rio Grande do Sul"))
summary(pino$Production_kg)
#Create tibble with labels
viridis_pal(option = "C")(5)
viridis_pal(option = "D")(12)

tibble(Year = 2023,
       Production_kg = c(12120, 8130, 4140, 3190, 150),
       State = c("MG", "PR", "RS", "SC", "SP"),
       colores = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF", "#B4DE2CFF")) -> labels

#tibble for geom_richtext
tibble(x = 2014.5, 
       y = 12500,
       text = "<span style='color:#252031;font-size:30px;'>(a) Pinhão gathering (tonnes)</span><br><span style='color:#5C4D7D;'>Between 2010-2023 – According to major producers</span>"
) -> title

pino %>% 
  filter(State %in% c("Minas Gerais", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul")) %>% 
  mutate(State = factor(State, levels = c("MG", "SP", "PR", "SC", "RS"))) -> pinos  


go <- ggplot() +
  geom_area(pino, 
            mapping = aes(Year, Production_kg, fill = State),
            #extra_span = 1,
            #type = "ridge",
            show.legend = F) +
#  scale_y_continuous(labels = scale_cut()) +
  geom_text(labels, 
            mapping = aes(Year, Production_kg, 
                          label = State, 
                          color = colores), 
            hjust = 0,
            vjust = 1,
            family = "IBM Plex Sans") +
  geom_richtext(title, mapping = aes(x = x, y = y, label = text),
                hjust = .5,
                family = "IBM Plex Sans",
                color = NA,
                fill = NA,
                size = 5) +
  # Adding the arrow and annotation for peak production
  geom_segment(aes(x = 2022, xend = 2022, y = 12000, yend = 13300),
               arrow = arrow(length = unit(.6, "cm")), 
               linewidth = 1.5, color = "white") +
  annotate("text", x = 2022, y = 13700, label = "13.377 tonnes (2022)", 
           hjust = .5, size = 5, color = "black", family = "IBM Plex Sans") +
  
  scale_y_continuous(breaks = seq(0, 14000, 4000), limits = c(0, 14000)) +
  scale_x_continuous(breaks = seq(2010, 2023, 1), limits = c(2010, 2023)) +
  scale_fill_manual(values = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF", "#F0F921FF")) +
  scale_color_identity() +
  labs(x = NULL, y = "Pinhão gathering (x 1000)") +
  coord_cartesian(expand = TRUE, clip = "off") +
  theme(plot.margin = margin(30, 30, 25, 30),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#291600",
                                                family = "IBM Plex Sans"),
        axis.text = element_text(family = "IBM Plex Sans"))+
  theme_bw(base_size = 15)

ggsave(file=paste0("./outputs/pinhão_prodc.png"),
       plot=go,dpi=600, units = "cm")


# Em português

#tibble for geom_richtext
tibble(x = 2014.5, 
       y = 12500,
       text = "<span style='color:#252031;font-size:25px;'>(a) Colheita de Pinhão (toneladas)</span><br><span style='color:#5C4D7D;'>Entre 2010-2023 – Conforme estados produtores</span>"
) -> title

pino %>% 
  filter(State %in% c("Minas Gerais", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul")) %>% 
  mutate(State = factor(State, levels = c("MG", "SP", "PR", "SC", "RS"))) -> pinos  


go <- ggplot() +
  geom_area(pino, 
            mapping = aes(Year, Production_kg, fill = State),
            show.legend = F) +
  geom_text(labels, 
            mapping = aes(Year, Production_kg, 
                          label = State, 
                          color = colores), 
            hjust = 0,
            vjust = 1,
            family = "sans") +
  geom_richtext(title, mapping = aes(x = x, y = y, label = text),
                hjust = .5,
                family = "sans",
                color = NA,
                fill = NA,
                size = 5) +
  # Adding the arrow and annotation for peak production
  geom_segment(aes(x = 2022, xend = 2022, y = 12000, yend = 13300),
               arrow = arrow(length = unit(.6, "cm")), 
               linewidth = 1.5, color = "white") +
  annotate("text", x = 2022, y = 13700, label = "13.377 toneladas", 
           hjust = .5, size = 5, color = "black", family = "sans") +
  scale_y_continuous(breaks = seq(0, 14000, 4000), limits = c(0, 14000)) +
  scale_x_continuous(breaks = seq(2010, 2023, 1), limits = c(2010, 2023)) +
  scale_fill_manual(values = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF", "#F0F921FF")) +
  scale_color_identity() +
  labs(x = NULL, y = "Venda de pinhão - toneladas por ano") +
  coord_cartesian(expand = TRUE, clip = "off") +
  theme(plot.margin = margin(30, 30, 25, 30),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#291600",
                                                family = "sans"),
        axis.text = element_text(family = "sans"))+
  theme_bw(base_size = 16)

ggsave(file = paste0("./outputs/pinhão_prodc_port.png"),
       plot = go,
       dpi = 600,        # Very high DPI for journal quality
       width = 25,       # Increase width
       height = 18,      # Increase height
       units = "cm")     # Use centimeters for precise sizing


###################### aumentar qualidade

go <- ggplot() +
  geom_area(pino, 
            mapping = aes(Year, Production_kg, fill = State),
            show.legend = F) +
  geom_text(labels, 
            mapping = aes(Year, Production_kg, 
                          label = State, 
                          color = colores), 
            hjust = 0,
            vjust = 1,
            family = "IBM Plex Sans", size = 6) + # Increase size of the state labels
  geom_richtext(title, mapping = aes(x = x, y = y, label = text),
                hjust = .5,
                family = "IBM Plex Sans",
                color = NA,
                fill = NA,
                size = 7) + # Increase size of the title
  # Adding a wider arrow for peak production
  geom_segment(aes(x = 2022, xend = 2022, y = 12000, yend = 13377),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"), 
               size = 1.5, # Keep arrow thick
               color = "black") +
  annotate("text", x = 2022, y = 13700, label = "Peak Production = 13.377 tonnes", 
           hjust = 0.5, size = 6, color = "black", family = "IBM Plex Sans") + # Increase annotation size
  scale_y_continuous(breaks = seq(0, 14000, 4000), limits = c(0, 14000)) +
  scale_x_continuous(breaks = seq(2010, 2023, 1), limits = c(2010, 2023)) +
  scale_fill_manual(values = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF", "#F0F921FF")) +
  scale_color_identity() +
  labs(x = NULL, y = "Pinhão gathering (x 1000)") +
  coord_cartesian(expand = TRUE, clip = "off") +
  theme(plot.margin = margin(25, 25, 20, 25),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#291600",
                                                family = "IBM Plex Sans"),
        axis.text = element_text(size = 14, family = "IBM Plex Sans"), # Increase axis text size
        axis.title = element_text(size = 16, family = "IBM Plex Sans"), # Increase axis title size
        axis.text.x = element_text(angle = 45, hjust = 1), # Tilt x-axis labels for better fit
        plot.title = element_text(size = 20, face = "bold"), # Increase title size
        plot.subtitle = element_text(size = 18)) + # Increase subtitle size
  theme_bw(base_size = 18) # General size for text in plot

# Plot the modified figure
go

################ $$$$ per yerar pinhao


viridis_pal(option = "D")(5)

tibble(Year = 2023,
       Extraction_value = c(61800, 46600, 19800, 13500, 1000),
       State = c("MG", "PR", "RS", "SC", "SP"),
       colores = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "black")) -> labels


#tibble for geom_richtext
tibble(x = 2014.5, 
       y = 60000,
       text = "<span style='color:#252031;font-size:25px;'>(b) Pinhão trade (R$ *10³)</span><br><span style='color:#5C4D7D;'>Between 2010-2023 - According to major producers</span>"
) -> title

pino %>% 
  filter(State %in% c("Minas Gerais", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul")) %>% 
  mutate(State = factor(State, levels = c("MG", "SP", "PR", "SC", "RS"))) -> pinos  

gole <- ggplot() +
  geom_area(pino,
            mapping = aes(Year, Extraction_value, fill = State),
            #extra_span = 1,
            #type = "ridge",
            show.legend = FALSE) +
  #scale_y_continuous(labels = label_number_si())+
  geom_text(labels, 
            mapping = aes(Year, Extraction_value, 
                          label = State, 
                          color = colores), 
            hjust = 0,
            vjust = 1,
            family = "IBM Plex Sans") +
  geom_richtext(title, mapping = aes(x = x, y = y, label = text),
                hjust = .5,
                family = "IBM Plex Sans",
                color = NA,
                fill = NA,
                size = 5) +
  # Adding the arrow and annotation for peak production
  #geom_segment(aes(x = 2020.5, xend = 2021.5, y = 61000, yend = 62000),
   #            arrow = arrow(length = unit(.6, "cm")), 
    #           linewidth = 1.5, color = "black") +
  annotate("text", x = 2020.5,  y = 64000, label = "Year 2023 = R$6.189.990,00", 
           hjust = .5, size = 5, color = "black", family = "sans") +
  scale_x_continuous(breaks = seq(2010, 2023, 1), limits = c(2010, 2023)) +
  scale_fill_manual(values = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "black")) +
  scale_color_identity() +
  labs(x = NULL, y = "Pinhão trade (R$)") +
  coord_cartesian(expand = TRUE, clip = "off") +
  theme(plot.margin = margin(30, 30, 25, 30),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#291600",
                                                family = "IBM Plex Sans"),
        axis.text = element_text(family = "IBM Plex Sans"))+
  theme_bw(base_size = 15)

ggsave(file=paste0("./outputs/pinhão_money.png"),
       plot=gole,dpi=640, units = "cm")


### em português

#tibble for geom_richtext
tibble(x = 2014.5, 
       y = 60000,
       text = "<span style='color:#252031;font-size:25px;'>(b) Comércio Pinhão (R$ *10³)</span><br><span style='color:#5C4D7D;'>Entre 2010-2023 - Conforme extração por estado</span>"
) -> title

tibble(Year = 2023,
       Extraction_value = c(61800, 46600, 19800, 13500, 1000),
       State = c("MG", "PR", "RS", "SC", "SP"),
       colores = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "black")) -> labels


pino %>% 
  filter(State %in% c("Minas Gerais", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul")) %>% 
  mutate(State = factor(State, levels = c("MG", "SP", "PR", "SC", "RS"))) -> pinos  

gole <- ggplot() +
  geom_area(pino,
            mapping = aes(Year, Extraction_value, fill = State),
            #extra_span = 1,
            #type = "ridge",
            show.legend = FALSE) +
  #scale_y_continuous(labels = label_number_si())+
  geom_text(labels, 
            mapping = aes(Year, Extraction_value, 
                          label = State, 
                          color = colores), 
            hjust = 0,
            vjust = 1,
            family = "IBM Plex Sans") +
  geom_richtext(title, mapping = aes(x = x, y = y, label = text),
                hjust = .5,
                family = "IBM Plex Sans",
                color = NA,
                fill = NA,
                size = 5) +
  # Adding the arrow and annotation for peak production
  #geom_segment(aes(x = 2020.5, xend = 2021.5, y = 61000, yend = 62000),
  #            arrow = arrow(length = unit(.6, "cm")), 
  #           linewidth = 1.5, color = "black") +
  annotate("text", x = 2020.5,  y = 64000, label = "Ano 2023 = R$61.899.000,00", 
           hjust = .5, size = 5, color = "black", family = "sans") +
  scale_x_continuous(breaks = seq(2010, 2023, 1), limits = c(2010, 2023)) +
  scale_fill_manual(values = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "black")) +
  scale_color_identity() +
  labs(x = NULL, y = "Comércio Pinhão (R$ *10³)") +
  coord_cartesian(expand = TRUE, clip = "off") +
  theme(plot.margin = margin(30, 30, 25, 30),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#291600",
                                                family = "IBM Plex Sans"),
        axis.text = element_text(family = "IBM Plex Sans"))+
  theme_bw(base_size = 15)

#ggsave(file=paste0("./outputs/pinhão_money_português.png"),
 #      plot=gole,dpi=640, units = "cm")

ggsave(file = paste0("./outputs/pinhão_prodc_port_valor.png"),
       plot = gole,
       dpi = 600,        # Very high DPI for journal quality
       width = 25,       # Increase width
       height = 18,      # Increase height
       units = "cm")     # Use centimeters for precise sizing


### todos juntos
g5 <- grid.arrange(go, gole, ncol=2)

ggsave(file = paste0("./outputs/pinhão_prod_valor_juntos.png"),
       plot = g5,
       dpi = 600,        # Very high DPI for journal quality
       width = 40,       # Increase width
       height = 18,      # Increase height
       units = "cm")     # Use centimeters for precise sizing


ggsave(file=paste0("./outputs/production_cost_pinhao.png"),
       plot=g5,width=20,height=10,dpi="print")

############## gráfico 2 da Sabrina

evadir <- read.csv("evasao.csv", header=T,sep=";")

head(evadir)



trago <- evadir %>%
  group_by(Ano,Mês) %>%
  summarize(n=n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
View(trago)

trago2 <- trago %>%
  mutate(Mês = fct_relevel(Mês, 'Abril','Maio', 'Junho', 
                                        'Julho'))
compar_all <- ggplot(trago, 
                     aes(x = factor(Ano,
                                    levels = c("2019", "2020","2021", 
                                               "2022","2023","2024"),
                                    labels = c("2019 N=56", "2020 N=0", "2021 N=102",
                                               "2022 N=61", "2023 N=66","2024 N=31")),
                         y = pct,
                         fill = factor(Mês,
                                       levels = c("Abril","Maio","Junho","Julho"),
                                       labels = c("Abril N=94","Maio N=83",
                                                  "Junho N=100","Julho N=39")))) + 
  geom_bar(stat="identity", color="black", 
           position = "fill") +
  geom_text(aes(label = lbl), 
            size = 4.5, 
            position = position_stack(vjust = 0.5)) +
  #scale_fill_viridis(discrete=T,option='C', direction = -1) +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(y = "Percentagem dos registros", 
       fill = "Registros de evasão\nem meses de safra do Pinhão\n(N=316 registros)",
       title = "Evasão escolar em Clevelândia (2019-2024)",
       x = "Percentagem de evasão conforme meses de safra (2019-2024)") +
  theme_bw(base_size = 15)


ggsave(file = paste0("./output/evasão_cleve_pinhão.png"),
       plot = compar_all,
       dpi = 600,        # Very high DPI for journal quality
       width = 25,       # Increase width
       height = 18,      # Increase height
       units = "cm")     # Use centimeters for precise sizing

#################################

######## causas da evasão

trago2 <- evadir %>%
  group_by(Ano,Motivos) %>%
  summarize(n=n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
View(trago2)


compar_all2 <- ggplot(trago2, 
                     aes(x = factor(Ano,
                                    levels = c("2019", "2020","2021", 
                                               "2022","2023","2024"),
                                    labels = c("2019 N=56", "2020 N=0", "2021 N=102",
                                               "2022 N=61", "2023 N=66","2024 N=31")),
                         y = pct,
                         fill = factor(Motivos,
                                       levels = c("Casamento","Colheita_pinhão","Condição_climática",
                                                  "Desmotivação (pais ou aluno)","Gestação",
                                                  "Idade_ultrapassada","Mudança cidade","Sem_justificativa",
                                                  "Sem_transporte","Trabalho","Violência","Vulnerabilidade_social"),
                                       labels = c("Casamento N=8","Colheita pihão N=10","Condição climática N=3",
                                                  "Desmotivação (pais ou aluno) n=142","Gestação N=17",
                                                  "Idade ultrapassada N=7","Mudança cidade N=26","Sem justificativa N=17",
                                                  "Sem transporte N=1","Trabalho N=42","Violência N=3",
                                                  "Vulnerabilidade social N=41")))) + 
  geom_bar(stat="identity", color="black", 
           position = "fill") +
  geom_text(aes(label = lbl), 
            size = 4.5, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_viridis(discrete=T,option='D') +
  #scale_fill_brewer(palette = "GnBu") +
  labs(y = "Percentagem dos registros", 
       fill = "Motivos de evasão\nem meses de safra do Pinhão\n(N=316 registros)",
       title = "Evasão escolar em Clevelândia (2019-2024)",
       subtitle= "Estratificada pelos motivos de evasão entre abril e julho",
       x = "Evasões entre 2019-2024") +
  theme_bw(base_size = 15)


ggsave(file = paste0("./output/evasão_cleve_pinhão_motivos.png"),
       plot = compar_all2,
       dpi = 600,        # Very high DPI for journal quality
       width = 25,       # Increase width
       height = 18,      # Increase height
       units = "cm")     # Use centimeters for precise sizing


######### Estratificado pelo sexo


######## causas da evasão por sexto

trago3 <- evadir %>%
  filter(Sexo == "Masculino") %>%
  #filter(SEXO == "Masculino") %>%
  select(Ano,Motivos)


trago4 <- trago3 %>%
  group_by(Ano,Motivos) %>%
  summarize(n=n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
summary(trago4)

View(trago4)


compar_all4 <- ggplot(trago4, 
                      aes(x = factor(Ano,
                                     levels = c("2019", "2020","2021", 
                                                "2022","2023","2024"),
                                     labels = c("2019 N=32", "2020 N=0", "2021 N=61",
                                                "2022 N=31", "2023 N=36","2024 N=22")),
                          y = pct,
                          fill = factor(Motivos,
                                        levels = c("Colheita_pinhão","Condição_climática",
                                                   "Desmotivação (pais ou aluno)","Gestação",
                                                   "Idade_ultrapassada","Sem_justificativa",
                                                   "Trabalho","Vulnerabilidade_social"),
                                        labels = c("Colheita pihão N=10","Condição climática N=3",
                                                   "Desmotivação (pais ou aluno) N=120","Gestação N=9",
                                                   "Idade ultrapassada N=7","Sem justificativa N=9",
                                                   "Trabalho N=1",
                                                   "Vulnerabilidade social N=14")))) + 
  geom_bar(stat="identity", color="black", 
           position = "fill") +
  geom_text(aes(label = lbl), 
            size = 4.5, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_viridis(discrete=T,option='D') +
  #scale_fill_brewer(palette = "GnBu") +
  labs(y = "Percentagem dos registros", 
       fill = "Motivos de evasão\nem meses de safra do Pinhão\n(N=180 registros | 57.6%)",
       title = "Evasão escolar em Clevelândia (2019-2024)",
       subtitle= "Estratificada pelo sexo (masculino)",
       x = "Evasões entre 2019-2024") +
  theme_bw(base_size = 15)


ggsave(file = paste0("./output/evasão_cleve_pinhão_motivos_sexo_masculino.png"),
       plot = compar_all4,
       dpi = 600,        # Very high DPI for journal quality
       width = 25,       # Increase width
       height = 18,      # Increase height
       units = "cm")     # Use centimeters for precise sizing


##############


######## causas da evasão por sexto

trago5 <- evadir %>%
  filter(Sexo == "Feminino") %>%
  #filter(SEXO == "Masculino") %>%
  select(Ano,Motivos)


trago6 <- trago5 %>%
  group_by(Ano,Motivos) %>%
  summarize(n=n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
summary(trago4)

View(trago6)


compar_all6 <- ggplot(trago6, 
                      aes(x = factor(Ano,
                                     levels = c("2019", "2020","2021", 
                                                "2022","2023","2024"),
                                     labels = c("2019 N=24", "2020 N=0", "2021 N=41",
                                                "2022 N=30", "2023 N=30","2024 N=9")),
                          y = pct,
                          fill = factor(Motivos,
                                        levels = c("Casamento",
                                                   "Desmotivação (pais ou aluno)","Gestação","Mudança cidade",
                                                  "Sem_justificativa",
                                                   "Trabalho","Violência","Vulnerabilidade_social"),
                                        labels = c("Casamento N=8","Desmotivação (pais ou aluno) N=13",
                                                   "Gestação N=8","Mudança cidade N=23",
                                                   "Sem justificativa N=7",
                                                   "Trabalho N=41","Violência N=3",
                                                   "Vulnerabilidade social N=27")))) + 
  geom_bar(stat="identity", color="black", 
           position = "fill") +
  geom_text(aes(label = lbl), 
            size = 4.5, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_viridis(discrete=T,option='E') +
  #scale_fill_brewer(palette = "GnBu") +
  labs(y = "Percentagem dos registros", 
       fill = "Motivos de evasão\nem meses de safra do Pinhão\n(N=136 registros | 42.4%)",
       title = "Evasão escolar em Clevelândia (2019-2024)",
       subtitle= "Estratificada pelo sexo (feminino)",
       x = "Evasões entre 2019-2024") +
  theme_bw(base_size = 15)


ggsave(file = paste0("./output/evasão_cleve_pinhão_motivos_sexo_feminino.png"),
       plot = compar_all6,
       dpi = 600,        # Very high DPI for journal quality
       width = 25,       # Increase width
       height = 18,      # Increase height
       units = "cm")     # Use centimeters for precise sizing

#### End of script
