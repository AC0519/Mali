####Libraries----
library(tidyverse)
library(ggplot2)
library(lubridate)
library(leaflet)
library(sf)
library(ggthemes)
library(scales)
library(MASS)
library(sp)
library(viridis)
library(raster)
library(urltools)
library(MASS)
library(KernSmooth)
library(leaflet.extras)


#########Import data---- 
gdelt_mali <- read_csv("/home/dsp.student01/Gdelt_Mali-data.csv")

acled <- read_csv("2017-01-01-2019-02-27-Mali.csv")
acled$event_date <- dmy(acled$event_date)
 


####Idetify missing values----

get_missing_values_col <- function(col){
  col %>%
    is.na() %>%
    sum()
}

get_missing_values_col(gdelt_mali)


mali_missing <- gdelt_mali %>% 
  map_df(get_missing_values_col)

mali_missing

mali_missing <- mali_missing %>%
  gather() %>%
  filter(value > 0)

#map missing data as bar chart
mali_missing %>%
  ggplot(aes(x = fct_reorder(key, value), y = value)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Missing Values in Mali Data Set",
       x = "Column Name",
       y = "Count of Missing Values")


######

kidnapping_hits <- gdelt_mali %>% 
  filter(EventCode == '181')

#Turn $MonthYear into date so I can get a time series look at kidnapping events
kidnapping_hits$MonthYear <- ymd(kidnapping_hits$MonthYear, truncated = 1)


#kidnappings by month
ggplot(kidnapping_hits)+
geom_bar(aes(x = kidnapping_hits$MonthYear))


impose_blockade <- gdelt_mali %>% 
  filter(EventCode == '191')
impose_blockade$MonthYear <- ymd(impose_blockade$MonthYear, truncated = 1)


#######Gdelt sources of info


#Extract URL names
sources <- kidnapping_hits %>% 
  mutate(source = map_chr(SOURCEURL, function(x) suffix_extract(domain(x))[[3]])) %>%
  filter(source != "NA") %>% 
  group_by(source) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total))


#check number of hits is correct
kidnapping_hits %>% 
  dplyr::select(SOURCEURL) %>%
  pull() %>%  #turns this column into a vector from df
  str_detect("yahoo") %>%
  sum()
  
sourceGraph <- sources %>% 
  filter(total >= 7) %>% 
  mutate(source = factor(source) %>% fct_reorder(total)) %>% 
  ggplot()+
  geom_col(aes(x = source, y = total), fill= "dark green")+
  coord_flip()

sourceGraph #Sources of GDELT information


#####################Gdelt Maps----

#Map gdelt Kidnapping Data by season 

kidnapping_hits %>% 
                group_by(MonthYear) %>% 
                summarize(total = n()) %>% 
                ggplot()+
                geom_line(aes(x = MonthYear, y = total))+
                geom_point(aes(x = MonthYear, y = total))+
                scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")+
                theme(axis.text.x = element_text(angle = 90, hjust=1))

#Heat Map of gdelt kidnapping reports

mali_shapefile <- getData("GADM", country='MLI', level=1) %>% 
                                                 st_as_sf()

maliMap <- ggplot(mali_shapefile)+
           geom_sf(fill = "white")+
           labs(title = 'Mali')+
           theme(plot.title = element_text(hjust = 0.5)) %>% 
maliMap
    

maliMap+
 geom_point(data = kidnapping_hits, aes(x = ActionGeo_Long, y = ActionGeo_Lat))


maliMap +
  stat_density2d(data = kidnapping_hits,
                 mapping = aes(x = ActionGeo_Long, y = ActionGeo_Lat,
                               fill = ..level.., alpha = ..level..),
                 geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") +
  guides(alpha="none") +
  labs(title = "Malian Hostage Related Reporting\nJan 2017 - Feb 2019", x = "", y = "") + 
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5))+
  theme_economist()+
  guides(fill=F)




##################
#ACLED violence against civilians



#Removing unnecessary columns, filtering on civ violence, and creating JNIM grouping
acled_civ_violence <- acled %>% 
                        select(-data_id, -iso, -event_id_cnty, -year, -time_precision,
                               -assoc_actor_1, -assoc_actor_2, -region, -geo_precision,
                               -timestamp, -iso3) %>% 
                        filter(event_type == "Violence against civilians") %>% 
                        mutate(actor1=str_replace_all(actor1,"Al Mourabitoune Battalion|AQIM: Al Qaeda in the Islamic Maghreb|Ansar Dine|JNIM: Group for Support of Islam and Muslims", "JNIM"))


#How many attacks is each group committing.  I filtered by groups with 10 or more attacks
civ_attack <- acled_civ_violence %>% 
                    group_by(actor1) %>% 
                    summarize(total = n()) %>%
                    filter(total >= 10) %>% 
                    arrange(desc(total)) %>% 
                    mutate(actor1 = factor(actor1) %>% fct_reorder(total)) 
                    
                    

ggplot(civ_attack)+
  geom_col(aes(x = actor1, y = total))+
  coord_flip()
  

#ACLED Civillian attacks Heat Density Map


maliMap +
  stat_density2d(data = acled_civ_violence,
                 mapping = aes(x = longitude, y = latitude,
                               fill = ..level.., alpha = ..level..),
                 geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") +
  guides(alpha="none") +
  labs(title = "Violence Against Civillian Incidents\nJan 2017 - Feb 2019", x = "", y = "") + 
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5))+
  theme_economist()+
  guides(fill=F)



#######leaflet combining gdelt heatmap And ACLED incidents----

#Top Five Groups

JNIM <- acled_civ_violence %>% 
            filter(actor1 == "JNIM")

Unidentified <- acled_civ_violence %>% 
                  filter(actor1 == "Unidentified Armed Group (Mali)")

Malian_Mil <- acled_civ_violence %>% 
                filter(actor1 == "Military Forces of Mali (2013-)")

Dozo <- acled_civ_violence %>% 
                filter(actor1 == "Dozo Militia")

Fulani <- acled_civ_violence %>% 
                filter(actor1 == "Fulani Ethnic Militia (Mali)")

ISGS <- acled_civ_violence %>% 
            filter(actor1 == "Islamic State (Greater Sahara)")


mali.flag <- makeIcon(
  
  iconUrl = "http://flags.fmcdn.net/data/flags/w580/ml.png",
  
  iconWidth = 16,
  
  iconHeight = 16
  
)

mali.flag



########For leaflet heat map of GDELT data

# Compute 2-D Gaussian kernel density estimate.
kde <- bkde2D(as.data.frame(kidnapping_hits[ , c("ActionGeo_Long",
                                           "ActionGeo_Lat")]),
              bandwidth=c(bandwidth.nrd(kidnapping_hits$ActionGeo_Long),
                          bandwidth.nrd(kidnapping_hits$ActionGeo_Lat)), gridsize = c(500,500))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
# Extract contour line levels.
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))
# Convert contour lines to polygons.
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)





##############Map by gdelt heat and ACLED group activity
leaflet(kidnapping_hits) %>% 

addProviderTiles("CartoDB.Positron") %>% 
  
addPolygons(data = spgons,
            color = heat.colors(NLEV, NULL)[LEVS],
            weight = 1, opacity = .1, fillOpacity = .075,
            group = "Heatmap") %>% 

addCircleMarkers(data = JNIM,
                   
                 color = "black",
                   
                radius = 2,
                   
                popup = ~paste(
                                  "Date: ", event_date, "<br/>",
                                  
                                  "Location: ", location, "<br/>",
                                  
                                  "Report: ", notes),
                   
                  group  = "JNIM") %>% 
  
addCircleMarkers(data = Unidentified,
                   
                   color = "white",
                   
                   radius = 2,
                   
                   popup = ~paste(
                     "Date: ", event_date, "<br/>",
                     
                     "Location: ", location, "<br/>",
                     
                     "Report: ", notes),
                   
                   group  = "Unidentified")%>% 
  
  addMarkers(data = Malian_Mil,
             
                    icon = mali.flag,
    

                   
                   popup = ~paste(
                     "Date: ", event_date, "<br/>",
                     
                     "Location: ", location, "<br/>",
                     
                     "Report: ", notes),
                   
                   group  = "Malians")%>% 
  
  addCircleMarkers(data = Dozo,
                   
                   color = "yellow",
                   
                   radius = 2,
                   
                   popup = ~paste(
                     "Date: ", event_date, "<br/>",
                     
                     "Location: ", location, "<br/>",
                     
                     "Report: ", notes),
                   
                   group  = "Dozo Militia")%>% 
  
  addCircleMarkers(data = Fulani,
                   
                   color = "blue",
                   
                   radius = 2,
                   
                   popup = ~paste(
                     "Date: ", event_date, "<br/>",
                     
                     "Location: ", location, "<br/>",
                     
                     "Report: ", notes),
                   
                   group  = "Fulani Militia")%>% 
  
  addCircleMarkers(data = ISGS,
                   
                   color = "red",
                   
                   radius = 2,
                   
                   popup = ~paste(
                     "Date: ", event_date, "<br/>",
                     
                     "Location: ", location, "<br/>",
                     
                     "Report: ", notes),
                   
                   group  = "Islamic State") %>% 
  
addLayersControl(overlayGroups = c("Malians","Dozo Militia", "Fulani Militia", "JNIM", "Islamic State","Unidentified"), 
                   
                  options=layersControlOptions(collapsed=FALSE))

  








