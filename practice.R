
library(easypackages)
my_packages <- c("tidyverse")
libraries(my_packages)


denue_data <- read_csv("content/post/denue_inegi_02_.csv", guess_max = 10000)

scian <- read_excel("content/post/scian.xlsx") %>%
  filter(!is.na(`Código`)) %>%
  mutate(`codigo_industria` = factor(str_sub(`Código`,1,2))) %>%
  filter(codigo_industria %in% c("31", "32", "33"))


db <- denue_data %>%
  select(latitud, longitud, municipio, id, codigo_act, per_ocu, ageb, manzana, fecha_alta, nombre_act) %>%
  mutate(codigo_industria = factor(str_sub(codigo_act, 1,3))) %>%
  filter(municipio %in% c("Tijuana") & codigo_industria %in% c("311", "312", "313", 
                                                               "314", "315", "316", 
                                                               "321", "322", "323", 
                                                               "324", "325", "326", 
                                                               "327", "331", "332", 
                                                               "333", "334", "335", 
                                                               "336", "337", "339")) %>%
  droplevels()





dat <- db %>% 
  group_by(nombre_act,codigo_act, per_ocu) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo)) %>%
  ungroup()



psych::describeBy(dat, group = dat$per_ocu)





dat %>%
  group_by(codigo_act) %>%
  summarise(desv = sd(conteo)) %>%
  arrange(desc(desv))






# Cálculo del Rango

range(dat$conteo)


# Cálculo del Rango Interquartil

quantile(dat$conteo)



# Cálculo de la desviación estándar

sd(dat$conteo)

# Cálculo de la kurtosis

psych::kurtosi(dat$conteo)


# Cálculo de skewness

psych::skew(dat$conteo)







View(dat)

distance <- dist(dat, method = "euclidean")

empresas.hclust <- hclust(d = distance, method = "ward.D")

plot(empresas.hclust)

rect.hclust(empresas.hclust, k = 8, border = "blue")


dat$clust <- cutree(empresas.hclust, k = 8)

dat %>%
  filter(clust == 6)



plot(dat$conteo, dat$clust)

   db %>%
    mutate(per_ocu = as_factor(per_ocu)) %>%
    group_by(codigo_industria, per_ocu) %>%
    summarise(conteo = n()) %>%
    arrange(desc(conteo)) %>%
    ggplot() + 
    geom_point(aes(x = per_ocu, y = conteo, color = codigo_industria)) + 
    geom_boxplot(aes(x = per_ocu, y = conteo))

  
  
  

db %>%
  mutate(per_ocu = as_factor(per_ocu)) %>%
  filter(codigo_industria %in% c("311")) %>%
  group_by(codigo_industria, per_ocu) %>%
  summarise(conteo = n()) %>%
  droplevels() %>%
  ggplot() + 
  geom_density(aes(x = conteo, fill = per_ocu))
  

db %>%
  mutate(per_ocu = as_factor(per_ocu)) %>%
  filter(codigo_industria %in% c("311") & per_ocu %in% c("0 a 5 personas", "6 a 10 personas", "11 a 30 personas", "31 a 50 personas", 
                                                         "51 a 100 personas", "101 a 250 personas", "251 y más personas")) %>%
  group_by(codigo_industria) %>%
  
  summarise(conteo = n())



db %>%
  group_by(codigo_industria, per_ocu) %>%
  summarise(conteo = n()) %>%
  droplevels() %>%
  mutate(per_ocu = factor(per_ocu))

hist(dat$conteo)


  ggplot(aes(x = codigo_industria, y = conteo, fill = per_ocu)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~factor(per_ocu))


# Histograma de conteo de unidades por personal ocupado


db %>%
  group_by(per_ocu) %>%
  summarise(n()) %>%
  ggplot(aes(x = per_ocu, y = `n()`)) + 
  geom_point()


##################################################################################################################
#################################################################################################################

library(easypackages)
my_packages <- c("rvest", "tidyverse",  "rgeos", "sf")
libraries(my_packages)

url <- "https://www.bloomberg.com/billionaires/"


position <- read_html(url) %>%
  html_nodes(".t-rank") %>%
  html_text()


nombre <- read_html(url) %>%
  html_nodes(".t-name") %>%
  html_text()


net_worth <- read_html(url) %>%
  html_nodes(".t-nw") %>%
  html_text()


last_change <- read_html(url) %>%
  html_nodes(".t-lcd") %>%
  html_text()

YTD_change <- read_html(url) %>%
  html_nodes(".t-ycd") %>%
  html_text()


SOVEREIGNT <- read_html(url) %>%
  html_nodes(".t-country") %>%
  html_text()

industry <- read_html(url) %>%
  html_nodes(".t-industry") %>%
  html_text()



df <- tibble::tibble(position, nombre, SOVEREIGNT, industry, net_worth, last_change, YTD_change) [-1,] %>%
  mutate(net_worth = as.numeric(str_extract(net_worth, pattern = "(\\d+\\.?\\d+)")),
         last_change = as.numeric(str_extract(last_change, pattern = "(\\d+\\.?\\d?+)")),
         YTD_change = as.numeric(str_extract(YTD_change, pattern = "(\\d+\\.?\\d?+)")),
         SOVEREIGNT = fct_recode(SOVEREIGNT,
                                 "United States of America" = "United States",
                                 "Russia" = "Russian Federation"))


countries <- df %>%
  distinct(SOVEREIGNT) %>%
  rename(.,  "Country" = SOVEREIGNT) %>%
  mutate(Country = as.character(Country)) %>%
  mutate_geocode(., `Country`)



worldmap <- rnaturalearth::ne_download(scale = 110,
type = "countries",
category = "cultural",
destdir = tempdir(),
load = TRUE,
returnclass = "sf")


map <- worldmap %>% 
  filter(TYPE %in% c("Sovereign country","Country"))


world_data <- full_join(df, countries, by = c("SOVEREIGNT" = "Country" )) %>%
  filter(!is.na(lat))



world_data_map <- full_join(map, world_data, by = "SOVEREIGNT") %>%
  arrange(as.numeric(position)) %>%
  select(position, nombre, SOVEREIGNT, industry, net_worth, last_change, YTD_change, lat, lon)

library(ggrepel)

ggplot(world_data_map) + 
  geom_sf() + 
  geom_point(aes(lon, lat))


library(ggmaps)

dato <- df %>%
  mutate(locs = geocode(location = SOVEREIGNT))



pr <- left_join(df, worldmap, by = "SOVEREIGNT")



mapa <- worldmap %>%
  filter(TYPE %in% c("Sovereign country","Country")) %>%
  select(SOVEREIGNT, SOV_A3) %>%
  filter(distinct(SOVEREIGNT))




df$SOVEREIGNT[!df$SOVEREIGNT %in% mapa$SOVEREIGNT]


mapa_geometria <- worldmap %>%
  filter(TYPE %in% c("Sovereign country", "Country")) %>%
  left_join(df, by = "SOVEREIGNT")




mapa_puntos <- worldmap %>%
  filter(TYPE %in% c("Sovereign country", "Country")) %>%
  fuzzyjoin::stringdist_left_join(df, by = "SOVEREIGNT") %>%
  select(SOVEREIGNT.x, `geometry`, industry, net_worth, nombre, posision) %>%
  st_centroid() %>%
  mutate(x_coords = map_dbl(geometry, 1),
         y_coords = map_dbl(geometry, 2),
         position = as.numeric(posision)) %>%
  arrange(position)








library(ggrepel)


ggplot(mapa_geometria) + 
  geom_sf(mapa_geometria) + 
  geom_sf(data = mapa_puntos, inherit.aes = FALSE, aes(size = ))
  



mapa <- fuzzyjoin::stringdist_left_join(mapa,df, by = "SOVEREIGNT")


ggplot(prueba) + 
  geom_sf(aes(fill = SOVEREIGNT.x)) +
  theme(legend.position = "none")



df_map <- left_join(df, worldmap, join = "SOVEREIGNT")


unique_paises <- df %>%
  distinct(country, .keep_all = FALSE)

map_data <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country")






class(map_data)


View(df)


nodos <- list(".t-rank", ".t-name", ".t-nw", ".t-lcd", ".t-ycd", ".t-country", ".t-industry")


html_nodes(webpage,".t-rank")


i <- html_nodes(webpage,c(".t-rank", ".t-industry"))




tbl <- webpage %>%
  html_nodes(".t-industry") %>%
  html_text()

head(tbl)















