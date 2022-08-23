## Replication data for "Limiting aggressive policing can reduce police and 
## civilian violence"
## Making Crossfire (Fogo Cruzado) databases 
## This file: 
## 1) Downloads all shootings data from the Fogo Cruzado API for Rio de Janeiro and Recife
## 2) Geolocates shootings data in the police precinct in the state of Rio de Janeiro
## 3) Cleans fogo cruzado data to show daily totals by precinct and daily subtotals by police presence

## 
library(crossfire)
library(tidyverse)
library(rdrobust)
library(rdd)
library(sf)

##### DOWNLOADING DATA #####

## Read shape file of police precincts for entire state of Rio de Janeiro
delegacias <- st_read("shape limites seg/lm_dp_2019.shp")

## Get Fogo Cruzado data (Have to query API in sections) 
## Get data for 2020
fogocruzado_signin(email = "[INSERT YOUR EMAIL HERE]", password = "[INSERT YOUR PASSWORD HERE]")
fogocruzado_rj <- get_fogocruzado(state="RJ", initial_date = as.Date("2020-01-01"), 
                                  final_date = as.Date("2020-06-30"))
fogocruzado_rj2 <- get_fogocruzado(state="RJ", initial_date = as.Date("2020-07-01"), final_date = as.Date("2020-12-20"))
fogocruzado_rj <- rbind(fogocruzado_rj, fogocruzado_rj2)
# And for 2019
fogocruzado_rj_19 <- get_fogocruzado(state="RJ", initial_date = as.Date("2019-01-01"), 
                                  final_date = as.Date("2019-06-30"))
fogocruzado_rj2_19 <- get_fogocruzado(state="RJ", initial_date = as.Date("2019-07-01"), 
                                   final_date = as.Date("2019-12-20"))
fogocruzado_rj_19 <- rbind(fogocruzado_rj_19, fogocruzado_rj2_19)
## Rbind 2019 to 2020
fogocruzado_rj <- rbind(fogocruzado_rj, fogocruzado_rj_19)

## Get Pernambuco data for 2020
fogocruzado_pe <- get_fogocruzado(state="PE", initial_date = as.Date("2020-01-01"), 
                                  final_date = as.Date("2020-06-30"))
fogocruzado_pe2 <- get_fogocruzado(state="PE", initial_date = as.Date("2020-07-01"),  final_date = as.Date("2019-12-20"))
fogocruzado_pe <- rbind(fogocruzado_pe, fogocruzado_pe2)


## Save raw data for when API is down 
save(fogocruzado_rj, fogocruzado_pe, file = "fogo_RJ_PE_rawdata.RData")

##### GEOLOCATING RIO DATA #####

## Identify which CISP (precinct) the shooting was in 
delegacias <- st_transform(delegacias, "+proj=longlat +datum=WGS84 +no_defs")
fogo_shape <- fogocruzado_rj[,c("latitude_ocorrencia", "longitude_ocorrencia", "id_ocorrencia")]

## Add precinct to 2020 fogo data 
fogo_sf <- fogo_shape %>%
  mutate_at(vars(longitude_ocorrencia, latitude_ocorrencia), as.numeric) %>%
  st_as_sf(
    coords = c("longitude_ocorrencia","latitude_ocorrencia"),
    agr = "identity", 
    crs = "+proj=longlat +datum=WGS84 +no_defs",
    stringsAsFactors = F,
    remove = T
  )
fogo_in_delegacia <- st_join(fogo_sf, delegacias)
## add delegacia and aisp to dataframe 
fogocruzado_rj$dp <- fogo_in_delegacia$dp
fogocruzado_rj$aisp <- fogo_in_delegacia$aisp


##### CLEANING DATA #####
## Aggregate data at the daily level for Rio and Recife
## Aggregate data by precinct for Rio
## Aggregate data by police presence or police raid
## ---

## Label shootings if they were classified as raids
fogocruzado_rj$operation <- 0
fogocruzado_rj$operation[which(fogocruzado_rj$motivo_principal=="Operação Policial" | fogocruzado_rj$motivo_complementar =="Operação Policial")] <- 1

## Construct margin variable
fogocruzado_rj$margin <- as.numeric(as.Date(fogocruzado_rj$data_ocorrencia)) - as.numeric(as.Date("2020-05-19"))
fogocruzado_pe$margin <- as.numeric(as.Date(fogocruzado_pe$data_ocorrencia)) - as.numeric(as.Date("2020-05-19"))

## Aggregate RJ data by day for the entire metro area (all data points in FC database) 
fogo_city <- fogocruzado_rj %>%
  group_by(data_ocorrencia, margin) %>%
  summarise(n = as.numeric(length(id_ocorrencia)),
            n_police = sum(presen_agen_segur_ocorrencia),
            n_deaths = sum(qtd_morto_civil_ocorrencia),
            n_poldeaths = sum(qtd_morto_agen_segur_ocorrencia),
            n_injuries = sum(qtd_ferido_civil_ocorrencia),
            n_polinjuries = sum(qtd_ferido_agen_segur_ocorrencia))
## Construct treatment variable
fogo_city$FederalBan <- 0
fogo_city$FederalBan[which(fogo_city$data_ocorrencia > "2020-05-19")] <- 1

## Aggregate RJ data by day per precinct 
fogo_dp <- fogocruzado_rj %>%
  group_by(data_ocorrencia, dp, aisp) %>%
  summarise(margin = unique(margin),
            n = as.numeric(length(id_ocorrencia)),
            n_police = sum(presen_agen_segur_ocorrencia),
            n_deaths = sum(qtd_morto_civil_ocorrencia),
            n_poldeaths = sum(qtd_morto_agen_segur_ocorrencia),
            n_injuries = sum(qtd_ferido_civil_ocorrencia),
            n_polinjuries = sum(qtd_ferido_agen_segur_ocorrencia))
## Make sure dataframe includes all precinct units at every day even if there was no shooting: 
ndays <- length(unique(fogo_dp$data_ocorrencia))
dp <- rep(unique(fogo_dp$dp), each = (ndays))
data_ocorrencia <- rep(unique(fogo_dp$data_ocorrencia), length(unique(fogo_dp$dp)))
margin <- rep(unique(fogo_dp$margin), length(unique(fogo_dp$dp)))
fogo_dp_all <- data.frame(dp, data_ocorrencia, margin)
fogo_dp <- left_join(fogo_dp_all, fogo_dp, by = c("dp", "data_ocorrencia", "margin")) %>%
  mutate_all(list(~replace_na(.,0)))
## Drop observations with precinct (DP) listed as zero 
fogo_dp <- subset(fogo_dp, dp > 0)
## Assign all AISPs their value
for (i in 1:length(unique(fogo_dp$dp))){
  dps <- subset(fogo_dp, dp == unique(fogo_dp$dp)[i] & aisp > 0)
  fogo_dp$aisp[which(fogo_dp$dp==unique(fogo_dp$dp)[i])] <- unique(dps$aisp)
}
## Construct treatment variable
fogo_dp$FederalBan <- 0
fogo_dp$FederalBan[which(fogo_dp$data_ocorrencia > "2020-05-19")] <- 1


## Aggregate RJ data by day and by police presence for the entire metro area
fogocruzado_rj$polpresent <- 0
fogocruzado_rj$polpresent[which(fogocruzado_rj$presen_agen_segur_ocorrencia> 0)] <- 1
fogo_city_pol <- fogocruzado_rj %>%
  group_by(data_ocorrencia, margin, polpresent) %>%
  summarise(n = as.numeric(length(id_ocorrencia)),
            n_deaths = sum(qtd_morto_civil_ocorrencia),
            n_poldeaths = sum(qtd_morto_agen_segur_ocorrencia),
            n_injuries = sum(qtd_ferido_civil_ocorrencia),
            n_polinjuries = sum(qtd_ferido_agen_segur_ocorrencia))
## Construct treatment variable
fogo_city_pol$FederalBan <- 0
fogo_city_pol$FederalBan[which(fogo_city_pol$data_ocorrencia > "2020-05-19")] <- 1
## Separate into two dataframes
fogo_cops <- subset(fogo_city_pol, polpresent==1)
fogo_nocops <- subset(fogo_city_pol, polpresent==0)


# Aggregate RJ data by day, precinct, and police presence
fogo_city_dp_pol <- fogocruzado_rj %>%
  group_by(data_ocorrencia, dp, aisp, polpresent) %>%
  summarise(margin = unique(margin),
            n = as.numeric(length(id_ocorrencia)),
            n_deaths = sum(qtd_morto_civil_ocorrencia),
            n_poldeaths = sum(qtd_morto_agen_segur_ocorrencia),
            n_injuries = sum(qtd_ferido_civil_ocorrencia),
            n_polinjuries = sum(qtd_ferido_agen_segur_ocorrencia),
            share_operations = sum(operation)/length(operation))
# Drop observations with DP listed as zero 
fogo_city_dp_pol <- subset(fogo_city_dp_pol, dp > 0)
## Make sure DF includes all units at every day: 
ndays <- length(unique(fogo_city_dp_pol$data_ocorrencia))
dp <- rep(unique(fogo_city_dp_pol$dp), each = (2*ndays))
data_ocorrencia <- rep(unique(fogo_dp$data_ocorrencia), 2*length(unique(fogo_dp$dp)))
margin <- rep(unique(fogo_dp$margin), 2*length(unique(fogo_dp$dp)))
polpresent <- c(rep(1, each = ndays), rep(0, each = ndays))
fogo_cop_all <- data.frame(dp, data_ocorrencia, margin, polpresent)
fogo_city_dp_pol <- left_join(fogo_cop_all, fogo_city_dp_pol, by = c("dp", "data_ocorrencia", "margin", "polpresent")) %>%
  mutate_all(list(~replace_na(.,0)))
# assign all AISPs their value
for (i in 1:length(unique(fogo_city_dp_pol$dp))){
  dps <- subset(fogo_city_dp_pol, dp == unique(fogo_city_dp_pol$dp)[i] & aisp > 0)
  fogo_city_dp_pol$aisp[which(fogo_city_dp_pol$dp==unique(fogo_city_dp_pol$dp)[i])] <- unique(dps$aisp)
}
## Construct treatment variable
fogo_city_dp_pol$FederalBan <- 0
fogo_city_dp_pol$FederalBan[which(fogo_city_dp_pol$data_ocorrencia > "2020-05-19")] <- 1
## Write new dataframes for precinct-level daily shootings involving police (fogo_dp_cops) and not involving police (fogo_dp_nocops)
fogo_dp_cops <- subset(fogo_city_dp_pol, polpresent==1)
fogo_dp_nocops <- subset(fogo_city_dp_pol, polpresent==0)


## Aggregate RJ data by day, precinct, and police presence, dropping police raids (operations) from shootings
fogoops <- subset(fogocruzado_rj, operation ==0)
fogo_city_polop <- fogoops %>%
  group_by(data_ocorrencia, dp, aisp, polpresent) %>%
  summarise(margin = unique(margin),
            n = as.numeric(length(id_ocorrencia)),
            n_deaths = sum(qtd_morto_civil_ocorrencia),
            n_poldeaths = sum(qtd_morto_agen_segur_ocorrencia),
            n_injuries = sum(qtd_ferido_civil_ocorrencia),
            n_polinjuries = sum(qtd_ferido_agen_segur_ocorrencia))
# DP listed as zero 
fogo_city_polop <- subset(fogo_city_polop, dp > 0)
## Make sure DF includes all units at every day: 
ndays <- length(unique(fogo_city_polop$data_ocorrencia))
dp <- rep(unique(fogo_city_polop$dp), each = (2*ndays))
data_ocorrencia <- rep(unique(fogo_city_polop$data_ocorrencia), 2*length(unique(fogo_city_polop$dp)))
margin <- rep(unique(fogo_city_polop$margin), 2*length(unique(fogo_city_polop$dp)))
polpresent <- c(rep(1, each = ndays), rep(0, each = ndays))
fogo_op_all <- data.frame(dp, data_ocorrencia, polpresent, margin)
fogo_op_all <- left_join(fogo_op_all, fogo_city_polop, by = c("dp", "data_ocorrencia", "margin", "polpresent")) %>%
  mutate_all(list(~replace_na(.,0)))
#assign all AISPs their value
for (i in 1:length(unique(fogo_op_all$dp))){
  dps <- subset(fogo_op_all, dp == unique(fogo_op_all$dp)[i] & aisp > 0)
  fogo_op_all$aisp[which(fogo_op_all$dp==unique(fogo_op_all$dp)[i])] <- unique(dps$aisp)
}
## Construct treatment variable
fogo_op_all$FederalBan <- 0
fogo_op_all$FederalBan[which(fogo_op_all$data_ocorrencia > "2020-05-19")] <- 1


## Aggregate Recife data by day and police presence
fogocruzado_pe$polpresent <- 0
fogocruzado_pe$polpresent[which(fogocruzado_pe$presen_agen_segur_ocorrencia> 0)] <- 1
fogo_pe <- fogocruzado_pe %>%
  group_by(data_ocorrencia, nome_cidade, cidade_id, polpresent) %>%
  summarise(margin = unique(margin),
            n = as.numeric(length(id_ocorrencia)),
            n_police = sum(presen_agen_segur_ocorrencia),
            n_deaths = sum(qtd_morto_civil_ocorrencia),
            n_poldeaths = sum(qtd_morto_agen_segur_ocorrencia),
            n_injuries = sum(qtd_ferido_civil_ocorrencia),
            n_polinjuries = sum(qtd_ferido_agen_segur_ocorrencia))
## Make sure DF includes all units at every day: 
ndays <- length(unique(fogo_pe$data_ocorrencia))
cidade_id <- rep(unique(fogo_pe$cidade_id), each = (2*ndays))
data_ocorrencia <- rep(unique(fogo_pe$data_ocorrencia), 2*length(unique(fogo_pe$cidade_id)))
margin <- rep(unique(fogo_pe$margin), 2*length(unique(fogo_pe$cidade_id)))
polpresent <- c(rep(1, each = ndays), rep(0, each = ndays))
fogo_pe_all <- data.frame(cidade_id, data_ocorrencia,margin, polpresent)
fogo_pe_all <- left_join(fogo_pe_all, fogo_pe, by = c("cidade_id", "data_ocorrencia", "margin", "polpresent")) %>%
  mutate_all(list(~replace_na(.,0)))
## Construct treatment variable
fogo_pe_all$FederalBan <- 0
fogo_pe_all$FederalBan[which(fogo_pe_all$data_ocorrencia > "2020-05-19")] <- 1
fogo_pe <- fogo_pe_all


##### SAVE FILES #####
save(fogo_city, fogo_dp, fogo_dp_cops, fogo_dp_nocops, fogo_cops, fogo_nocops, fogo_op_all, fogo_pe,
     file = "fogo_rd.RData")
#load("fogo_rd.RData")
