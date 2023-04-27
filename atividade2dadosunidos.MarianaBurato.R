require(dplyr)
require(ggplot2)
require(tidyr)
require(taxize)
require(validate)

iris <- read.csv("iris_mod.csv", header = T)
iris2 <- read.csv("dados_unidosmari.csv", sep = ";", header = T)
iris2$X<-NULL

lapply(iris2, unique)

iris2 %>%
  select(spp, tamanho_petala:largura_sepala) %>%
  pivot_longer(cols = -spp, names_to = "variavel", values_to = "valores") %>%
  ggplot(aes(x = valores, fill = spp)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (mm)") +
  scale_fill_discrete(
    expression(bold("spp:")),
    labels = c(expression(italic("Iris setosa")),
               expression(italic("Iris versicolor")),
               expression(italic("Iris virginica"))))

rules <- validator(in_range(latitude, min = -90, max = 90),
                   in_range(latitude, min = -180, max = 180),
                   is.character(site),
                   is.numeric(data),
                   all_complete(iris2))

out   <- confront(iris2, rules)
summary(out)

plot(out)

# check taxa
spp <- iris2 %>%
  distinct(spp) %>%
  pull() %>%
  c("Iris murchosa", .) %>% # inserimos uma espécie fictícia para teste
  get_tsn() %>%
  data.frame() %>%
  bind_cols(spp = iris2 %>%
              distinct(spp) %>%
              pull() %>%
              c("Iris murchosa", .))

iris_1 <- iris2 %>%
  dplyr::mutate(eventID = paste(site, data, sep = "_"), # create indexing fields
                occurrenceID = paste(site, data, amostra, sep = "_")) %>%
  left_join(spp %>%
              select(spp, uri)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = longitude, # rename fields according to DwC
                decimalLatitude = latitude,
                eventDate = data,
                scientificName = spp,
                scientificNameID = uri) %>%
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "spp",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")



eventCore <- iris_1 %>%
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>%
  distinct()



occurrences <- iris_1 %>%
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct()

eMOF <- iris_1 %>%
  select(eventID, occurrenceID, recordedBy, tamanho_petala:largura_sepala) %>%
  pivot_longer(cols = tamanho_petala:largura_sepala,
               names_to = "measurementType",
               values_to = "measurementValue") %>%
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala"),
                                           to = c("tamanho petala", "largura petala", "tamanho sepala", "largura sepala")))

#check if all eventID matches

setdiff(eventCore$eventID, occurrences$eventID)
eventCore
# check NA values
eMOF %>%filter(is.na(eventID))
occurrences %>%
  filter(is.na(eventID))

rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF)
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))}
