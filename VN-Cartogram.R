require(plotly)
require(dplyr)
require(cartogram)
require(rgdal)

VNproj<- read.csv("VN-Proj.csv")
VNweigths <- read.csv("VN-Weigths.csv")

g <- list(
  fitbounds = "locations",
  visible = FALSE
)

load("srd")
srd@data$okrug <- gsub(as.character(srd@data$QNAME), pattern = " okrug", replacement = '')
srd@data$okrug[2] <- "Beogradski"
srd@data$ok <- gsub(srd@data$okrug, pattern = "-", replacement = "")
srd@data$ST2020 <- left_join(srd@data,VNproj %>% filter(Godina==2020), by=c("ok"="region"))$BrojStanovnika
srd@data$ST2050 <- left_join(srd@data,VNproj %>% filter(Godina==2050), by=c("ok"="region"))$BrojStanovnika

srd$Smanjenje <- left_join(srd@data,VNweigths, by=c("okrug"="Oblast"))$Weights*(-1)
srd$Weights<- 100-left_join(srd@data,VNweigths, by=c("okrug"="Oblast"))$Weights*(-1)

srd@data$area <- raster::area(srd)/1000000
srd$Weights<- (srd$Weights/100)^2 * srd@data$area 


serb_proj <- "+proj=tmerc +lat_0=0 +lon_0=21 +k=0.9999 +x_0=7500000 +y_0=0 +ellps=bessel +towgs84=574.027,170.175,401.545,4.88786,-0.66524,-13.24673,6.89 +units=m"
srd<- spTransform( srd,serb_proj ) 
srd_ncount <- cartogram_ncont(srd, "Weights",k = 0.8)

srd_sf<- sf::st_as_sf(srd)
srd_ncount_sf<- sf::st_as_sf(srd_ncount)
load("koMunicipalities")
kosovosf<- sf::st_as_sf(spTransform(raster::aggregate(koMunicipalities),serb_proj))

VNplotly<- 
plot_ly(stroke = I("#bdbdbd"), span = I(1),colors = "YlOrRd") %>% 
  add_sf(
    data = srd_sf, 
    color = I("gray95"),name="2020",
    hoverinfo = "none",showlegend =T
  ) %>%
  add_lines(x=7375992, y=4822605,linetype = I("dot"), line = list(color= '#525252', width=1, opacity= 0),showlegend=T, name="2050") %>% 
  add_sf(
    span = I(0.5),
    data = srd_ncount_sf, linetype = I("dot"),
    color = ~Smanjenje,
    colors="magma",
    fillscale="magma",
    split = ~okrug,
    line = list(
      color= '#525252', width=1,
      opacity= 1),showlegend =F,
    text = ~paste0("<b>",gsub(okrug,pattern = "ki", replacement = "ka oblast"),"</b>  \nProjektovano smanjenje: <b>", format(Smanjenje, nsmall=1, decimal.mark=","), "%</b>", "\nBr. stanovnika 2020: <b>", format(ST2020, nsmall=1, big.mark=".") ,"</b>\nBr. stanovnika 2050: <b>", format(ST2050, nsmall=1, big.mark="."), "</b>" ), 
    hoverinfo = "text", 
    hoveron = "fills" ) %>% colorbar(title="Smanjenje\npopulacije(%)", orientation="h",colorscale="magma") %>% layout(showlegend = T)  %>% add_sf(
      span = I(0.5),
      data = kosovosf)

VNplotly


