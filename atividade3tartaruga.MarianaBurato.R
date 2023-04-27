#####atividade 3. Mariana Burato. ESPECIE ESCOLHIDA: CHelonia mydas####

##instalando o pacote rbibif

install.packages("rgbif") 
install.packages("tidyverse")
library(rgbif)
library(tidyverse)
library(rgbif)
library(dplyr)
# checar funcoes
?occ_data

# baixar ocorrencias
chel_gbif <- occ_data(scientificName = "Chelonia mydas", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)


# dimensoes
dim(chel_gbif)

dim(chel_gbif$data)

# checar campos
chel_gbif$data %>% names
chel_gbif


gbif_issues()

chel_gbif1 <- chel_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues,sex,occurrenceRemarks,lifeStage,identificationID,country,iucnRedListCategory, occurrenceStatus,datasetName, recordedBy, locality)
chel_gbif1 <- chel_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(chel_gbif1, unique)

install.packages("bdc")
install.packages("CoordinateCleaner")

library(bdc)
library(CoordinateCleaner)
install.packages("Rcpp")
library(Rcpp)


# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = chel_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- chel_gbif1 %>%
  select(acceptedScientificName, decimalLatitude, decimalLongitude) %>%
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude,
         scientificName = acceptedScientificName) %>% 
  as_tibble() %>% 
  mutate(val = cc_val(., value = "flagged"),
         sea = cc_sea(., value = "flagged"),
         capital = cc_cap(., value = "flagged"))

# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "capital") 

cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "sea") 

##Nessa parte eu escolhi a variavel "country" pra saber os locais de ocorrencia mencionados.

# investigar niveis suspeitos
chel_gbif1 %>% 
  distinct(country) %>% 
  pull()

# waterBody
chel_gbif1 %>%
  group_by(country) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=country)) +
  geom_bar(stat = 'identity') 

###Nao utilizei os filtros nos proximos comandos, eles deram erro aqui######
# fonte das regioes erradas
chel_gbif1 %>% 
  filter(country %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)

# 25 ocorrencias
chel_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science"))

# filtrar todas do dataset suspeito


chel_gbif_ok <- chel_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))

## baixando os pacotes para criar mapas e graficos mais dinamicos

install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")

library(ggmap)
library(maps)
library(mapdata)

ggplot2::map_data('world')
world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = chel_gbif1, aes(x = decimalLongitude, y = decimalLatitude), color = "green") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chelonia mydas")))


# checar profundidade - NAO TEM DADOS DE PROFUNDIDADE PARA MINHA ESPECIE > CHELONIA MYDAS
chel_gbif1 %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 



##### BANCO DE DADOS OBIS. FIZ OS COMANDOS, MAS NAO DERAM CERTO, MAS MESMO ASSIM DEIXEI AQUI####

install.packages("robis")
library(robis)

## OBIS
chel_obis <- robis::occurrence("Chelonia mydas")

# checar dados
names(chel_obis)


chel_obis1 <- chel_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude,
                sex,occurrenceStatus,lifeStage,identificationID,country, occurrenceStatus,datasetName, recordedBy, locality) %>% 
  distinct()

# check problemas reportados (flags)
chel_obis1 %>% 
  distinct(flags)


# check NA em datasetName
chel_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)

# depth ok
chel_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 


# checar niveis
chel_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  lapply(., unique)

# ok
chel_obis_ok <- chel_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique", NA)) 


# check
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = chel_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chelonia mydas")))


##unir GBIF e OBIS

# ver diferencas

setdiff(names(chel_gbif_ok), names(chel_obis_ok))


setdiff(names(chel_obis_ok), names(chel_gbif_ok))

all_data <- bind_rows(chel_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      chel_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Paracanthurus hepatus") %>% 
  dplyr::select(-rn)

# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Chelonia mydas")))


write.csv(all_data, "data/occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)


# funcao para classificar ocorrencias suspeitas
flag_outlier <- function(df, species){
  
  # funcao para classificar ocorrencias suspeitas
  # baseada no calculo do centroide de todas ocorrencias
  # indica como 'check' as ocorrencias que tem distancias até o centroide
  # acima do 90th quantil (default) das distancias calculadas
  
  dados <- df %>% 
    dplyr::filter(scientificName == species); 
  
  dados2 <- geosphere::distVincentyEllipsoid(
    dados %>%
      summarise(centr_lon = median(decimalLongitude),
                centr_lat = median(decimalLatitude)),
    dados %>% 
      dplyr::select(decimalLongitude, decimalLatitude)
  ) %>% 
    bind_cols(dados) %>% 
    rename(dist_centroid = '...1') %>% 
    mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.9), "OK",
                         ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.90) & dist_centroid < quantile(dist_centroid, probs = 0.95), "check > Q90",
                                ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.95), "check > Q95", "OK"))))
  
  # mutate(flag = ifelse(dist_centroid > quantile(dist_centroid, probs = prob), "check", "OK"))
  
  print(dados2)
  
}


# classificar ocorrências
marcados <- chel_gbif$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., "Chelonia mydas (Linnaeus, 1766)")


# mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados, 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = flag)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Chelonia mydas")))


devtools::install_github('iobis/obistools')

install.packages('obistools')

library(obistools)


# dori_obis %>% 
#   dplyr::select(decimalLongitude, decimalLatitude, scientificNameID) %>% 
#   distinct() %>% 
#   check_outliers_species(., report=TRUE)


# usando essa configuração chegamos a valores próximos aos da limpeza manual
chel_obis %>% 
  dplyr::select(decimalLongitude, decimalLatitude, scientificNameID) %>% 
  distinct() %>% 
  check_outliers_dataset(., report = FALSE, iqr_coef = 1, mad_coef = 5) %>% 
  dim()


ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = datasetName)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Chelonia mydas")))


flags <-
  clean_coordinates(
    x = marcados,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("equal", "gbif",
              "zeros", "seas")
  )


