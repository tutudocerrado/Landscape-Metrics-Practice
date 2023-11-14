# Carregando pacotes ------------------------------------------------------

# https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c('terra', 'landscapemetrics', 'ggplot2'))

# Reclassificando a paisagem ----------------------------------------------

dir <- list.dirs("./Dados")
files <- list.files(dir[2], pattern = ".tif")

BnoC <- list()

for (i in 1:length(files)) {
  BnoC[[i]] <- rast(paste(dir[2],files[i], sep = "/"))
}; rm(i)

# Visualisando as classes para o ano de 1985

plot(BnoC[[1]])

# Novas classes

# classes: (1) natural; (2) antrópico; (3) água

classes_uso <- c(0,3,15,21,4,33,11,12,24,39,25,41,9,20)
classes_paisagem <- c(NA,1,2,2,1,3,1,1,2,2,2,2,2,2)

classes <- cbind(classes_uso, classes_paisagem); rm(classes_uso,classes_paisagem)

# Reclassificando

for (i in 1:length(BnoC)) {
  BnoC[[i]] <- terra::classify(BnoC[[i]],classes)
}; rm(i)

# Visualisando as novas classes para o ano de 1985

plot(BnoC[[1]])

# Visualizando as diferenças na estrutura da paisagem em 1985 e 2022

par(mfrow = c(1,2))
plot(BnoC[[1]])
plot(BnoC[[38]])

# Conferindo o status da paisagem -----------------------------------------

check_landscape(BnoC[[38]])

# Conferindo o sistema de coordenadas do arquivo

BnoC[[38]]

# Conferindo em qual zona UTM nossos dados estão situados

# floor((longitude + 180) / 6) + 1
# Aqui substiuiremos os valores mínimos e máximos de longitude na função

floor((-48.83323 + 180) / 6) + 1
floor((-48.64108 + 180) / 6) + 1

# Atribuindo uma nova projeção

newcrs <- "+proj=utm +south +zone=22 +datum=WGS84 +units=m +no_defs"
w = project(BnoC[[1]], newcrs)
w

# Conferindo o status da paisagem

check_landscape(w)

# Usando a funcao round()

for (i in 1:length(BnoC)) {
  w <- project(BnoC[[i]], newcrs)
  BnoC[[i]] <- round(w)
}

# Conferindo novamente o status da paisagem

check_landscape(BnoC[[1]])

# Analisando metricas de paisagem -----------------------------------------

# Análise em nível de mancha ----------------------------------------------

# Área da mancha (lsm_p_area)

area <- lsm_p_area(BnoC[[38]], directions = 8)
area

#Visualizando os dados

area <- area[-931,] # removendo ponto discrepante

ggplot(data =  area, 
       aes(x = as.factor(class), 
           y = value, 
           colour = as.factor(class))) +
  geom_boxplot() +
  scale_x_discrete(breaks = c(1,2,3),
                   labels = c("Natural", "Antrópico", "Água")) +
  xlab("Classes") +
  ylab("Área (m²)") +
  theme_bw() + theme(legend.position = "none")

# Porcentagem da área do interior (lsm_p_cai)

cai <- lsm_p_cai(BnoC[[38]], 
                 directions = 8, 
                 consider_boundary = FALSE, 
                 edge_depth = 1)
cai

# Área do interior (lsm_p_core)

core <- lsm_p_core(BnoC[[38]],
                   directions = 8,
                   consider_boundary = FALSE,
                   edge_depth = 1)
core # hectare

# Circularidade da mancha (lsm_p_circle)

circle <- lsm_p_circle(BnoC[[38]], directions = 8)
circle

# Distância Euclidiana do Vizinho mais Próximo (lsm_p_enn)

enm <- lsm_p_enn(BnoC[[38]], directions = 8, verbose = TRUE)
enm # metros

# Razão Perímetro-Área (lsm_p_para)

para <- lsm_p_para(BnoC[[38]], directions = 8)
para # porcentagem

# Análise em nível de classe ----------------------------------------------

# Índide de agregação (lsm_c_ai)

ai <- lsm_c_ai(BnoC[[38]])
ai

# Média do tamanho da área (lsm_c_area_mn)

area_mn <- lsm_c_area_mn(BnoC[[38]], directions = 8)
area_mn # hectare

# Desvio padrão do tamanho da área (lsm_c_area_sd)

area_sd <- lsm_c_area_sd(BnoC[[38]], directions = 8)
area_sd # hectare

# Número de manchas (lsm_c_np)

np <- lsm_c_np(BnoC[[38]], directions = 8)
np

## Observando o número de fragmentos ao longo do tempo

dado <- as.data.frame(matrix(data = NA,
                             nrow = length(BnoC), 
                             ncol = 2))

for (i in 1:length(BnoC)) {
  anos <- seq(from = 1985, to = 2022, by = 1)
  np <- lsm_c_np(BnoC[[i]], directions = 8)
  dado[i,1] <- anos[i]
  dado[i,2] <- np[1,6]
}

colnames(dado) <- c("ano","fragmentos")
head(dado)


ggplot(data = dado, aes(x = ano, y = fragmentos)) +
  geom_line()

# Porcentagem da paisagem (lsm_c_pland)

pland <- lsm_c_pland(BnoC[[38]], directions = 8)
pland

# Análise em nível de paisagem --------------------------------------------

# Número de manchas (lsm_l_np)

np <- lsm_l_np(BnoC[[38]], directions = 8)
np

# Índice de diversidade de Simpson (lsm_l_sidi)

sidi <- lsm_l_sidi(BnoC[[38]], directions = 8)
sidi

# Área total da paisagem (lsm_l_ta)

ta <- lsm_l_ta(BnoC[[38]], directions = 8)
ta
