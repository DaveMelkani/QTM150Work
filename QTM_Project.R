library(anyflights)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggmap)
library(maps)


airports <- get_airports()
airlines <- get_airlines()


may2019 <- read.csv("flights_may2019.csv")
may2020 <- read.csv("flights_may2020.csv")
may2021 <- read.csv("flights_may2021.csv")

sel_airlines <- c("DL", "UA", "AA", "B6")

may2019mod <- may2019 %>%
  filter(carrier %in% sel_airlines) %>% 
  select(year, month, day, carrier, origin, dest, time_hour)

may2020mod <- may2020 %>% 
  filter(carrier %in% sel_airlines) %>% 
  select(year, month, day, carrier, origin, dest, time_hour)

may2021mod <- may2021 %>% 
  filter(carrier %in% sel_airlines) %>% 
  select(year, month, day, carrier, origin, dest, time_hour)

##########################################################
ex <- may2019mod %>% 
  group_by(carrier, dest) %>% 
  mutate(count = n()) %>% 
  summarize(carrier, dest, count)

ex2 <- ex[!duplicated(ex),]  

ex3 <- ex2 %>% 
  pivot_wider(names_from="dest",values_from="count")

sel_airports = c("SEA", "SAT", "LAS", "MCI", "MCO", "ATL", "BOS","SLC", "JFK" , "DTW", "ORD", "DEN", "SFO", "IAH", "MSP" )  

final2019 <- ex3 %>% 
  select(carrier, sel_airports)%>% 
  replace(is.na(.), 0)

#final2019
###############################################################################
ex <- may2020mod %>% 
  group_by(carrier, dest) %>% 
  mutate(count = n()) %>% 
  summarize(carrier, dest, count)

ex2 <- ex[!duplicated(ex),]  

ex3 <- ex2 %>% 
  pivot_wider(names_from="dest",values_from="count")

final2020 <- ex3 %>% 
  select(carrier, SEA, LAS, MCO, ATL, BOS, SLC, DTW, IAH, MSP, JFK, ORD, DEN, SFO)

final2020 <- final2020 %>% 
  mutate(SAT = 0,
         MCI = 0)

#final2020
###############################################################################

ex <- may2021mod %>% 
  group_by(carrier, dest) %>% 
  mutate(count = n()) %>% 
  summarize(carrier, dest, count)

ex2 <- ex[!duplicated(ex),]  

ex3 <- ex2 %>% 
  pivot_wider(names_from="dest",values_from="count")


sel_airports = c("SEA", "SAT", "LAS", "MCO", "ATL", "BOS","SLC", "JFK" , "DTW" , "ORD" , "DEN" , "SFO", "IAH" , "MSP")  

final2021 <- ex3 %>% 
  select(carrier, sel_airports)%>%
  replace(is.na(.),0)

#final2021
##########################################################################

final2019 <- final2019 %>% 
  replace(is.na(.), 0)

final2020 <- final2020 %>% 
  replace(is.na(.), 0)

final2021 <- final2021 %>% 
  replace(is.na(.),0)

##########################################################################

change2019_2020 <- left_join(final2019, final2020, by="carrier")

change2019_2020 <- change2019_2020 %>% 
  mutate(SEA = SEA.y - SEA.x,
         LAS = LAS.y - LAS.x,
         SAT = SAT.y - SAT.x,
         MCO = MCO.y - MCO.x,
         MCI = MCI.y - MCI.x,
         ATL = ATL.y - ATL.x,
         BOS = BOS.y - BOS.x,
         SLC = SLC.y - SLC.x,
         DTW = DTW.y - DTW.x,
         IAH = IAH.y - IAH.x,
         MSP = MSP.y - MSP.x,
         JFK = JFK.y - JFK.x,
         ORD = ORD.y - ORD.x,
         DEN = DEN.y - DEN.x,
         SFO = SFO.y - SFO.x) %>% 
  select(carrier, SEA, LAS, SAT, MCO, MCI, ATL, BOS, SLC, DTW, IAH, MSP, JFK, ORD, DEN, SFO)

TotalChange2019_2020 <- t(change2019_2020)

TotalChange2019_2020 <- as.data.frame(TotalChange2019_2020)

colnames(TotalChange2019_2020) <- TotalChange2019_2020[1,]

TotalChange2019_2020 <- TotalChange2019_2020[-1,]

TotalChange2019_2020 <- TotalChange2019_2020 %>% 
  mutate(faa = c("SEA", "LAS", "SAT", "MCO", "MCI", "ATL", "BOS", "SLC", "DTW", "IAH","MSP", "JFK", "ORD", "DEN", "SFO"))


rownames(TotalChange2019_2020) <- c(1:15)

airports <- airports %>% 
  filter(faa %in% sel_airports) %>% 
  select(faa, name, lat, lon)

TotalChange2019_2020 <- TotalChange2019_2020 %>% 
  left_join(airports, by="faa")

TotalChange2019_2020 <- TotalChange2019_2020 %>% 
  transform(AA = as.numeric(AA),
            B6 = as.numeric(B6),
            DL = as.numeric(DL),
            UA = as.numeric(UA)) %>%
 mutate(time_frame= "2019-2020")


#TotalChange2019_2020
###############################################################################
#fix MCI data

MCIdata<- c(0,0,0,0)
final2021<-final2021 
final2021<- cbind(final2021, MCI = MCIdata)


final2021
###############################################################################
# CHANGE FROM 2019-2021

  TotalChange19_21 <- left_join(final2019, final2021 , by= "carrier") %>%
  mutate( SEA= SEA.x - SEA.y, 
          SAT = SAT.x - SAT.y, 
          LAS = LAS.x - LAS.y,
          MCI= MCI.x- MCI.y, 
          MCO= MCO.x - MCO.y,
          ATL = ATL.x - ATL.y,
          BOS= BOS.x - BOS.y,
          SLC= SLC.x - SLC.y,
          JFK= JFK.x - JFK.y,
          DTW= DTW.x- DTW.y,
          ORD= ORD.x - ORD.y,
          DEN= DEN.x - DEN.y,
          SFO= SFO.x - SFO.y,
          IAH= IAH.x - IAH.y,
          MSP= MSP.x- MSP.y
          )%>%
  select(carrier, SEA, SAT, LAS, MCI, MCO , ATL, BOS, SLC, JFK, DTW , ORD, DEN, SFO, IAH, MSP)%>%
  t()%>%
  data.frame()
  
  colnames(TotalChange19_21) <- TotalChange19_21[1,]
  
 TotalChange19_21 <- TotalChange19_21[-1,]
  
 TotalChange19_21 <- TotalChange19_21 %>% 
    mutate(faa = c("SEA", "SAT", "LAS", "MCI", "MCO", "ATL", "BOS","SLC", "JFK" , "DTW" , "ORD" , "DEN" , "SFO", "IAH" , "MSP"))
 
  rownames(TotalChange19_21) <- c(1, 2, 3, 4 ,5 , 6, 7, 8, 9 ,10, 11, 12,13, 14, 15)
   
  Final_sel_airports = c("SEA", "SAT", "LAS", "MCO", "ATL", "BOS","SLC", "JFK" , "DTW" , "ORD" , "DEN" , "SFO", "IAH" , "MSP","MCI")  
  
 # joining airport data 
  coordinates <- airports %>%
  filter(faa %in% Final_sel_airports)%>%
  select(faa,name,lat,lon)

TotalChange19_21 <- left_join(TotalChange19_21 , coordinates , by="faa" )

TotalChange19_21 <- TotalChange19_21 %>% 
  transform(AA = as.numeric(AA),
            B6 = as.numeric(B6),
            DL = as.numeric(DL),
            UA = as.numeric(UA)) %>%
     mutate(time_frame= "2019-2021")

#TotalChange19_21
###############################################################################

change2020_2021 <- left_join(final2020, final2021, by="carrier")

change2020_2021 <- change2020_2021 %>% 
  mutate(SEA = SEA.y - SEA.x,
         LAS = LAS.y - LAS.x,
         SAT = SAT.y - SAT.x,
         MCO = MCO.y - MCO.x,
         MCI = MCI.y - MCI.x,
         ATL = ATL.y - ATL.x,
         BOS = BOS.y - BOS.x,
         SLC = SLC.y - SLC.x,
         DTW = DTW.y - DTW.x,
         MSP = MSP.y - MSP.x,
         JFK = JFK.y - JFK.x,
         ORD = ORD.y - ORD.x,
         IAH = IAH.y - IAH.x,
         DEN = DEN.y - DEN.x,
         SFO = SFO.y - SFO.x,
  ) %>% 
  select(carrier, 
         SEA, LAS, SAT, IAH, DEN,
         MCO, MCI, ATL, BOS, SLC,
         DTW, MSP, JFK, ORD, SFO)


TotalChange2020_2021 <- t(change2020_2021)


TotalChange2020_2021 <- as.data.frame(TotalChange2020_2021)


colnames(TotalChange2020_2021) <- TotalChange2020_2021[1,]


TotalChange2020_2021 <- TotalChange2020_2021[-1,]

TotalChange2020_2021 <- TotalChange2020_2021 %>% 
  mutate(faa = c("SEA", "LAS", "SAT", "MCO", "MCI", 
                 "ATL", "BOS", "SLC", "DTW", "MSP", 
                 "JFK", "ORD", "IAH", "DEN", "SFO"))



rownames(TotalChange2020_2021) <- c(1:15)

airports <- airports %>% 
  filter(faa %in% Final_sel_airports) %>% 
  select(faa, name, lat, lon)


TotalChange2020_2021 <- TotalChange2020_2021 %>% 
  left_join(airports, by="faa")


TotalChange2020_2021 <- TotalChange2020_2021 %>% 
  transform(AA = as.numeric(AA),
            B6 = as.numeric(B6),
            DL = as.numeric(DL),
            UA = as.numeric(UA)) %>% 
  mutate(time_frame = "2020-2021")

#
###############################################################################


 BIG <- full_join(TotalChange2019_2020,TotalChange2020_2021, by= c("name" , "faa", "lat", "lon", "time_frame", "AA", "B6", "UA", "DL")) %>%
  full_join(TotalChange19_21 , by= c("name" , "faa", "lat", "lon", "time_frame", "AA", "B6", "UA", "DL") )



  BIGDelta <- BIG %>%
   select(name, lat, lon, faa, time_frame, DL)


us <- c(left = -125, bottom = 24, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")


DLplot <- ggmap(map) +
  geom_point(data = BIGDelta, aes(x=lon, y=lat, color = DL, ids = name , size=1.5, frame=time_frame)) +
  scale_color_gradient(low = "red", high="green") + ggtitle(" Delta's Resillience in Flight Volume Over the Pandemic")

ggplotly(DLplot)

##################################################################################################

BIGAmerican <- BIG %>%
  select(name, lat, lon, faa, time_frame, AA)

AAplot <- ggmap(map) +
  geom_point(data = BIGAmerican, aes(x=lon, y=lat, color = AA, ids = name , size=1.5 , frame= time_frame)) +
  scale_color_gradient(low = "red", high="green") + ggtitle(" American Airlines's Resillience in Flight Volume Over the Pandemic")

ggplotly(AAplot)
################################################################################################
BIGJet <- BIG %>%
  select(name, lat, lon, faa, time_frame, B6)

B6plot <- ggmap(map) +
  geom_point(data = BIGJet, aes(x=lon, y=lat, color = B6, ids = name , size=1.5 , frame= time_frame)) +
  scale_color_gradient(low = "red", high="green") + ggtitle(" Jet Blue's Resillience in Flight Volume Over the Pandemic")

ggplotly(B6plot)
################################################################################################
BIGUnited <- BIG %>%
  select(name, lat, lon, faa, time_frame, UA)


UAplot <- ggmap(map) +
  geom_point(data= BIGUnited, aes(x=lon, y=lat, color = UA, ids = name , size=1.5, frame=time_frame)) +
  scale_color_gradient(low = "red", high="green") + ggtitle(" United Airlines's Resillience in Flight Volume Over the Pandemic")

ggplotly(UAplot)

###############################################################################