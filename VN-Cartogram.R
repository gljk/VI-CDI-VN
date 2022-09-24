require(plotly)
require(dplyr)
require(cartogram)
require(plotly)
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

plot_ly(stroke = I("#bdbdbd"), span = I(1)) %>% 
  add_sf(
    data = srd_sf, 
    color = I("gray95"),
    hoverinfo = "none"
  ) %>%
  add_sf(
    span = I(0.5),
    data = srd_ncount_sf, 
    color = ~Smanjenje,
    split = ~okrug, linetype="dash",
    colors="RdBu",
    text = ~paste0("<b>",gsub(okrug,pattern = "ki", replacement = "ka oblast"),"</b>  \nProjektovano smanjenje: <b>", format(Smanjenje, nsmall=1, decimal.mark=","), "%</b>", "\nBr. stanovnika 2020: <b>", format(ST2020, nsmall=1, big.mark=".") ,"</b>\nBr. stanovnika 2050: <b>", format(ST2050, nsmall=1, big.mark="."), "</b>" ), 
    hoverinfo = "text", 
    hoveron = "fills" ) %>% colorbar(len = 0.5) %>% layout(showlegend = FALSE) 


