library(ggplot2)
library(tidyr)
library(dplyr)

jame <- read.csv('hrosci_sobivanje_vaja.csv', sep = ';', encoding = "UTF-8")

jame <- jame %>% 
  mutate(
    Jama.oznacba = substr(Jama.tip, 0, 3),
    Ndm_vis = as.integer(Ndm_vis),
    stevilo_vrst = ifelse(
      Chol_STEVILO + Cara_STEVILO <= 1,
      'ena vrsta',
      'dve ali vec vrst'
    )
  )

jame %>% group_by(stevilo_vrst) %>% summarise(n = n())

write.csv(
  jame %>% 
    group_by(stevilo_vrst, Jama.oznacba) %>% 
    summarise(n = n()) %>%
    pivot_wider(names_from = stevilo_vrst, values_from = n, values_fill = 0)
, 'po_tipu_jame.csv', row.names = FALSE)
  

ggplot(jame, aes(y = Jama.oznacba, fill = stevilo_vrst)) + 
  geom_bar(position = 'dodge') +
  facet_wrap(vars(stevilo_vrst), scales = 'free_x') +
  xlab('Stevilo jam') +
  guides(fill=FALSE)



write.csv(
  jame %>% 
    mutate(dolzina_bin = cut(Dolžina, breaks = 20)) %>%
    group_by(stevilo_vrst, dolzina_bin) %>% 
    summarise(n = n()) %>%
    pivot_wider(names_from = stevilo_vrst, values_from = n, values_fill = 0)
  , 'po_dolzini.csv', row.names = FALSE)

ggplot(jame, aes(y = Dolžina, fill = stevilo_vrst)) + 
  geom_histogram(position = 'dodge', bins = 20) +
  facet_wrap(vars(stevilo_vrst), scales = 'free_x') +
  xlab('Stevilo jam') +
  guides(fill=FALSE)

ggplot(jame, aes(y = Globina, fill = stevilo_vrst)) + 
  geom_histogram(position = 'dodge', bins = 20) +
  facet_wrap(vars(stevilo_vrst), scales = 'free_x') +
  xlab('Stevilo jam') +
  guides(fill=FALSE)

ggplot(jame, aes(y = Ndm_vis, fill = stevilo_vrst)) + 
  geom_histogram(position = 'dodge', bins = 20) +
  facet_wrap(vars(stevilo_vrst), scales = 'free_x') +
  xlab('Stevilo jam') + ylab('Nadmorska visina') + 
  guides(fill=FALSE)
