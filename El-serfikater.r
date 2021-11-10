## El serfikat
library(tidyverse)
library(lubridate)
library(readr)
sertifikater <- read_delim("Elsertifikater.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
View(sertifikater)
names(sertifikater)

sertifikater$ESerfikatProdMWH <- as.numeric(sertifikater$ESerfikatProdMWH)
sertifikater$SerfikatProdmndMWH <- as.numeric(sertifikater$SerfikatProdmndMWH)
sertifikater$YtelseMW <- as.numeric(sertifikater$YtelseMW)
sertifikater$Startdato <-  dmy(sertifikater$Startdato)
sertifikater$Sluttdato <- dmy(sertifikater$Sluttdato)
sertifikater
df
df <- sertifikater[,c(2,5:7,14:19)]
df <- df %>% drop_na(Sluttdato) %>% drop_na(Startdato)
## Bruker årssnittfor registerpris. VELDIG forenklet
priser <- tibble( Year = c(2012:2020),
                  pris = c(181,185,172,181,149,123,116, 108,68))

df$mndsertifikat <- interval(df$Startdato,df$Sluttdato) %/% months(1)
df$startaar <- year(df$Startdato)
df$startmnd <- month(df$Startdato)
df$sluttaar <- year(df$Sluttdato)
df$sluttmnd <- month(df$Sluttdato)
df$id <- c(1:nrow(df))
View(df)
## Trekk ut ID, StartDato og Sluttdato for å lage sekvens
df1 <- df %>% select(id,Startdato,Sluttdato)
df1 <- df1 %>% gather(Type,Dato, Startdato:Sluttdato)
df1$Year <- year(df1$Dato)
df1$Month <- month(df1$Dato)
df1$date <- format(df1$Dato, "%Y-%m") 
df1
## Kode for å lage sekvenser
## https://stackoverflow.com/questions/62943371/repeat-rows-based-on-time-values-split-across-multiple-columns-r

df2 <- df1 %>% 
  select(id,Year,Month) %>% 
  mutate(Month2 = Month) %>% 
  group_by(id) %>% 
  complete(Year = min(Year):max(Year), Month = first(Month):12) %>% 
  fill(Month2) %>%
  filter(Year == max(Year) & Month <= last(Month2)| Year != max(Year)) %>%
  select(-Month2)

df3 <- left_join(df2,df, by = "id")
df4 <- left_join(df3,priser, by = "Year")
df4 <- df4[,c(1:14,19)]
df4$serfikater <- df4$SerfikatProdmndMWH*df4$pris

unique(df$TypeAnlegg)
sum(na.omit(df4 %>% filter(TypeAnlegg == "Vannkraft") %>% pull(serfikater)))
print(c("Utbetalt vannkraft i mill",2.2139e+10/1000000 ))

sum(na.omit(df4 %>% filter(TypeAnlegg == "Vindkraft") %>% pull(serfikater)))
print(c("Utbetalt Vindkraft i mill", round(3351442926/1000000,0)))



