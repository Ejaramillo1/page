
library(easypackages)
my_packages <- c("tidyverse")
libraries(my_packages)


denue_data <- read_csv("content/post/denue_inegi_02_.csv", guess_max = 10000)


db <- denue_data %>%
  select(latitud, longitud, municipio, id, codigo_act, per_ocu, ageb, manzana, fecha_alta) %>%
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
  mutate(per_ocu = as_factor(per_ocu)) %>%
  group_by(codigo_industria, per_ocu) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

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
