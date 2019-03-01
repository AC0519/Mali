library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)

acled <- ACLED_WAfrica2018

#convert event date column to date type
acled$event_date <- dmy(acled$event_date)

unique(acled$country)

#grab only events from 2018 that took place in Mali
mali <- filter(acled, acled$country == "Mali")

#unique groups in Mali
unique(mali$actor1)

#rename groups of interest to more common and shorter spellings
mali$actor1[mali$actor1 == "MINUSMA: United Nations Multidimensional Integrated Stabilization Mission in Mali (2013-)"] <- "MINUSMA"
mali$actor1[mali$actor1 == "JNIM: Group for Support of Islam and Muslims"] <- "JNIM"
mali$actor1[mali$actor1 == "Islamic State (Greater Sahara)"] <- "Islamic State Greater Sahara"
mali$actor1[mali$actor1=="Fulani Ethnic Militia (Mali)"] <- "Fulani Militia"
mali$actor1[mali$actor1=="Military Forces of Mali (2013-)"] <- "Malian Military"



#combine JNIM affiliates into JNIM
JNIM_All <- function(group){
  if(group == "Al Mourabitoune Battalion" | group == "AQIM: Al Qaeda in the Islamic Maghreb" | group == "Ansar Dine"){
    return("JNIM")
  }else{
    return(group)
  }
}

mali$actor1 <- sapply(mali$actor1, JNIM_All)


#combine GATIAN groups
GATIA_All <- function(group){
  if(group=="GATIA: Imghad Tuareg and Allies Self-Defense Group; Military Forces of Mali (2013-)" | group=="GATIA: Imghad Tuareg and Allies Self-Defense Group"){
    return("GATIA")
  }else{
    return(group)
  }
}

mali$actor1 <- sapply(mali$actor1, GATIA_All)

#unique groups in Mali
unique(mali$actor1)

#filter out groups of interest
JNIM <- filter(mali, mali$actor1=="JNIM")
ARI <- filter(mali, mali$actor1=="Ansaroul Islam")
ISGS <- filter(mali, mali$actor1=="Islamic State Greater Sahara")
Dozo <- filter(mali, mali$actor1=="Dozo Militia")
Fulani <- filter(mali, mali$actor1=="Fulani Militia")
KM <- filter(mali, mali$actor1=="Katiba Macina")
MaliGov <- filter(mali, mali$actor1=="Malian Military")
France <- filter(mali, mali$actor1=="Military Forces of France")


#Seperate out IEDs from remote violence and filter both

mali$event_type[mali$event_type == "Remote violence"] <- "Remote Violence"
Remote <- filter(mali, mali$event_type=="Remote Violence")

mali$event_type <- ifelse(grepl("IED", mali$notes),'IED', mali$event_type) 
#the above says that if IED is found anywhere in the notes, then change the event type to IED, otherwise leave it alone
                                                                           
IED <- filter(mali, mali$event_type == "IED")





#make flag and symbol icons
french.flag <- makeIcon(
                  iconUrl = "http://flags.fmcdn.net/data/flags/w580/fr.png",
                  iconWidth = 16,
                  iconHeight = 16
)

mali.flag <- makeIcon(
                iconUrl = "http://flags.fmcdn.net/data/flags/w580/ml.png",
                iconWidth = 16,
                iconHeight = 16
)

IED.icon <- makeIcon(
                iconUrl = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAHkAAAB5CAMAAAAqJH57AAAAaVBMVEX///8AAABJSUn7+/u3t7fHx8fo6Ojz8/P4+PjS0tKurq7V1dXv7+++vr7s7Oy6urqJiYk8PDyoqKgfHx8tLS1zc3OCgoKUlJTb29skJCShoaEUFBR5eXllZWVdXV2amppSUlI1NTULCwvLo5b/AAAEUElEQVRoge2b2ZqiMBBGSZC1lQYFGwRUfP+HHLaEoCypbH0x/d+M8A0ei1QqVZW0ZamV2+Cj4q/k1Bmh0++Qbwg5RoH0DT8QCkyC4yIcP/0g9DV+jAwYf0R0dCdygNBDPzpGZHivhOy0t676yVZF0C057/49tDeeBsCWVaPBtaJrP+Lt5EK1EbBl3RHC9KIdY1QZAlv2nXEox8wYE7TLXJx+KZD9T3JuWZqYDds9NkODhvFugxkqixTnmrF2mCKi3mibXD/1gp2GctFrsPmMcZI0RXHQyfUqir0kT7JI2r7nHd1IJ9jFBHt1XFsn6U35yG2+DUJZ8Iyb3/X/jGjg3udv+YlQuPKAKvnD5PmZfknvYN0iqdWp+0Sg1Y1e28WwYsVIc/br9OB4upGThIAmKXrkJ/0YM3dacjZ86pIUfeSwj1lssAgoufMybWC7N3nmxGwycvC0kftRTt9vxSv/W6XqD5OtEzvDtMkr0IcX5UZKyf5lZ283AxP1nFNnWWY+9/mTKTkVniv+2n9Iha7oU7f9x+R1XgCbaY68FskGimV3EaxzRfwj/5E76S5rosdzhfx86CzegmwFOyjTFU6CNXMZw3Wwo3qX26lW/s6Xo/WSzkq50Z0b3NYcCs120n0eo1RZcvYN4nZSVLeHYLCiun3mW0l44vM1BX7Ggsq+fvALI1azY1yNsdnhslqyysmZ3Ic2PtwLD7mU6ngOTYFe6RQY3ZLL6ERm+ZpWiITp2PPGM4mdjGlAGwYccIJlUmH6Vl/TmPnVFmuuUhT8s/DjD/xcJLw9eKRfMNUuAIN7iTVpYvI43QHz93ODNwk1TSI6lYl32XgLsqiLyNb/jTxN20yQRZpIIH77xMCS/OyHABhheDihs5aYnG8B1gWPocSLSzLKjRgZ3BK0ScQmhbFIftAJb2IW9EWeHHMqX9BkNvzxiZhYjInkSRAMX6fJKkVeNl+evyToFjjJc8cgxLkkLymBgT2SdozDzJcALaqAkWkWNM6ppR4Yp0p3G/UmYmMxBjDwUsEI1iAkroz94TrZ/O5twWodkgCMu35eI0GGTStKHi6PsJJuLliRRcjj23a5yooVwTYoaWkxOKZ4BAPbTFH9+hrJgIHjPEWO1+MQb3wth2C+nS/3kkUEXKw8rqKNSxdg5iszjeZK92Ezia+K74J2+0WTn09Bmwf8BeOeoDtKvswawSrxgWRru63Lr/fN2n1JZCEzwfuBUgvjpESgjpWMmaNEyti1DQuYYEnYKBU+JtYeUmG0kMlSqe4o0bMlnjQZHEWIZIO3RL8X3pJhdd8HrEoqAXtJ/UmITNIpuY9y2yesSPrQg2gQVXBkSmxWKzklJoJWdDwN/sKVnU6DupnC861fkAbNRekxdZu/pa78BKLDl4xiDcem7PN+xdNoOsHsnbftxqG+s6W+U68VmZcs0Hyk5HiqPh39Up30mcvKdcK4xmmJyhTXcegILYf/ACTqMcnIed+MAAAAAElFTkSuQmCC",
                iconWidth = 16,
                iconHeight = 16
)



#Map data

mali.map <- leaflet() %>%
            addTiles() %>%
                addMarkers(data = MaliGov,
                                 icon = mali.flag,
                                 clusterOptions = markerClusterOptions(),
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "Malian Military") %>%
                addMarkers(data = France,
                                 icon = french.flag,
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "Military Forces of France") %>%
                addMarkers(data = IED,
                           icon = IED.icon,
                           popup = ~paste("Event: ", event_type, "<br/>",
                                          "Date: ", event_date, "<br/>",
                                          "Location: ", location, "<br/>",
                                          "Report: ", notes, "<br/>",
                                          "Group Responsible: ", actor1),
                           group  = "IED") %>%
                addCircleMarkers(data = JNIM,
                                 color = "black",
                                 radius = 2,
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "JNIM") %>% 
                addCircleMarkers(data = ISGS,
                                 color = "orange",
                                 radius = 2,
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "Islamic State Greater Sahara") %>% 
                addCircleMarkers(data = ARI,
                                 color = "blue",
                                 radius = 2,
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "Ansaroul Islam") %>%
                addCircleMarkers(data = Fulani,
                                 color = "purple",
                                 radius = 2,
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "Fulani Militia") %>%
                addCircleMarkers(data = Dozo,
                                 color = "yellow",
                                 radius = 2,
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "Dozo Militia") %>%
                addCircleMarkers(data = KM,
                                 color = "cyan",
                                 radius = 2,
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "Katiba Macina") %>%
                addCircleMarkers(data = Remote,
                                 color = "red",
                                 radius = 2,
                                 popup = ~paste("Event: ", event_type, "<br/>",
                                                "Date: ", event_date, "<br/>",
                                                "Location: ", location, "<br/>",
                                                "Report: ", notes, "<br/>",
                                                "Group Responsible: ", actor1),
                                 group  = "Remote Violence") %>% 
                
                addLayersControl(overlayGroups = c("Ansaroul Islam","IED", "Dozo Militia", "Fulani Militia", "Islamic State Greater Sahara","JNIM","Katiba Macina","Malian Military","Military Forces of France","Remote Violence"), 
                                 options=layersControlOptions(collapsed=FALSE))

mali.map




##########
#Explore Violent events
#########

mali.violent <- filter(mali, fatalities > 0)

max(mali.violent$fatalities)
mean(mali.violent$fatalities)
sum(mali.violent$fatalities)



#########Plot violent incidents by group
ggplot(mali.violent) +
geom_bar(aes(x = actor1)) + 
ylab("Number of Incidents") + 
xlab("Perpetrator")+
theme(axis.text.x = element_text(angle = 90, hjust=1))


#######Who is responsible for the IEDs
mali.IEDs <- filter(mali, mali$event_type=="IED")
fatal.IEDs <- filter(mali.IEDs, fatalities > 0)
nonfatal.IEDs <- filter(mali.IEDs, fatalities == 0)

#all IEDs
ggplot(mali.IEDs) +
geom_bar(aes(x = actor1)) +
theme(axis.text.x = element_text(angle = 90, hjust=1))

#fatal IEDs
ggplot(fatal.IEDs) +
geom_bar(aes(x = actor1)) +
theme(axis.text.x = element_text(angle = 90, hjust=1)) +
ggtitle("Fatal IED Attacks")

#Non fatal IEDs
ggplot(nonfatal.IEDs) +
geom_bar(aes(x = actor1)) +
theme(axis.text.x = element_text(angle = 90, hjust=1)) +
ggtitle("Non-Fatal IED Attacks")


