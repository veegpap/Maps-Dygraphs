
library(shiny)
library(dygraphs)
library(tmap)
library(leaflet)


server <- function(input, output, session) {
  
  load("pandata_dekaeties.RData")
  load("countries22.RData")
  #load("pandata_for.RData")
  
  
  lut <- data.frame(iso2 = c("DK","FI","FR","IS","IT","NLD","NO","SE","CH","AT","BE","CZ","IE","EE","LV","LT","DE","GR","PL","PT","ES","UK"),
                    name = c("Denmark", "Finland", "France", "Island", "Italy","Netherlands", "Norway", "Sweden", "Switzerland",
                             "Austria","Belgium", "Czechia", "Ireland","Estonia","Latvia","Lithuania","Germany", "Greece","Poland",
                             "Portugal","Spain","United Kingdom"))
  
  
  ksi_doat <- reactive({
    mika <- lut$iso2[match(input$country, lut$name)]
    nb <- which(males_dat[[1]][,1] == mika)
    country_ksi_dat <- data.frame(year = c(1960,1970,1980,1990,2000,2010,2015,2020,2025,2030,2035,2040,2044,2045), 
                                  male = as.vector(t(males_dat[[1]][nb,-1])), 
                         female =  as.vector(t(females_dat[[1]][nb,-1])),
                         mean_male = as.vector(t(males_stat[[1]][1,-1])),
                         mean_female = as.vector(t(females_stat[[1]][1,-1])))
    
#    a1 <- data.frame(year = 2014:2045, male = as.vector(t(males_for_dat[[1]][nb,-c(1,2)])),
#                     female = as.vector(t(females_for_dat[[1]][nb,-c(1,2)])),
#                     mean_male = as.vector(t(males_for_stat[[1]][1,-c(1,2)])),
#                     mean_female = as.vector(t(females_for_stat[[1]][1,-c(1,2)]))
#                     )
#    
#    
#    rbind(country_ksi_dat, a1)
    country_ksi_dat
  })
  
  
  output$ksi_graf <- renderDygraph({
    cc <- ksi_doat()
    
    dygraph(cc, main = paste0("ξ - ", input$country), group = "param") %>%
      dySeries("male", label = "Male", fillGraph = TRUE) %>%
      dySeries("female", label = "Female", fillGraph = TRUE) %>%
      dySeries("mean_male", label = "EU mean(m)", strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries("mean_female", label = "EU mean(f)", strokeWidth = 2, strokePattern = "dashed") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyAxis("y", label = "", valueRange = 
               c(min(c(min(cc[,2],min(cc[,3]),min(cc[,4]),min(cc[,5])))),
                 max(c(max(cc[,2],max(cc[,3]),max(cc[,4]),max(cc[,5]))))+0.2*max(c(max(cc[,2],max(cc[,3]),max(cc[,4]),max(cc[,5])))))) %>%
      dyRangeSelector(height = 20) %>%
      dyEvent("2013", "Forecast", labelLoc = "bottom")
    
  })
  
  
  
  kapa_doat <- reactive({
    mikav <- lut$iso2[match(input$country, lut$name)]
    nv <- which(males_dat[[1]][,1] == mikav)
    country_kapa_dat <- data.frame(year = c(1960,1970,1980,1990,2000,2010,2015,2020,2025,2030,2035,2040,2044,2045),
                                   male = as.vector(t(males_dat[[2]][nv,-1])), 
                              female =  as.vector(t(females_dat[[2]][nv,-1])),
                              mean_male = as.vector(t(males_stat[[2]][1,-1])),
                              mean_female = as.vector(t(females_stat[[2]][1,-1])))
    
#    a2 <- data.frame(year = 2014:2045, male = as.vector(t(males_for_dat[[2]][nv,-c(1,2)])),
#                     female = as.vector(t(females_for_dat[[2]][nv,-c(1,2)])),
#                     mean_male = as.vector(t(males_for_stat[[2]][1,-c(1,2)])),
#                     mean_female = as.vector(t(females_for_stat[[2]][1,-c(1,2)]))
#    )
#    
#    rbind(country_kapa_dat,a2)
    country_kapa_dat
    
  })
  
  
  output$kapa_graf <- renderDygraph({
    ck <- kapa_doat()
    
    dygraph(ck, main = paste0("κ - ", input$country), group = "param") %>%
      dySeries("male", label = "Male", fillGraph = TRUE) %>%
      dySeries("female", label = "Female", fillGraph = TRUE) %>%
      dySeries("mean_male", label = "EU mean(m)", strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries("mean_female", label = "EU mean(f)", strokeWidth = 2, strokePattern = "dashed") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyAxis("y", label = "", valueRange = 
               c(min(c(min(ck[,2],min(ck[,3]),min(ck[,4]),min(ck[,5])))),
                 max(c(max(ck[,2],max(ck[,3]),max(ck[,4]),max(ck[,5]))))+0.2*max(c(max(ck[,2],max(ck[,3]),max(ck[,4]),max(ck[,5])))))) %>%
      dyRangeSelector(height = 20) %>%
      dyEvent("2013", "Forecast", labelLoc = "bottom")
    
  })
  
  
  lamda_doat <- reactive({
    mikax <- lut$iso2[match(input$country, lut$name)]
    nx <- which(males_dat[[1]][,1] == mikax)
    country_lamda_dat <- data.frame(year = c(1960,1970,1980,1990,2000,2010,2015,2020,2025,2030,2035,2040,2044,2045),
                                    male = as.vector(t(males_dat[[3]][nx,-1])), 
                                   female =  as.vector(t(females_dat[[3]][nx,-1])),
                                   mean_male = as.vector(t(males_stat[[3]][1,-1])),
                                   mean_female = as.vector(t(females_stat[[3]][1,-1])))
    
#    a3 <- data.frame(year = 2014:2045, male = as.vector(t(males_for_dat[[3]][nx,-c(1,2)])),
#                     female = as.vector(t(females_for_dat[[3]][nx,-c(1,2)])),
#                     mean_male = as.vector(t(males_for_stat[[3]][1,-c(1,2)])),
#                     mean_female = as.vector(t(females_for_stat[[3]][1,-c(1,2)]))
#    )
#    
#    rbind(country_lamda_dat,a3)
    
    country_lamda_dat
  })
  
  
  output$lamda_graf <- renderDygraph({
    cl <- lamda_doat()
    
    dygraph(cl, main = paste0("λ - ", input$country), group = "param") %>%
      dySeries("male", label = "Male", fillGraph = TRUE) %>%
      dySeries("female", label = "Female", fillGraph = TRUE) %>%
      dySeries("mean_male", label = "EU mean(m)", strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries("mean_female", label = "EU mean(f)", strokeWidth = 2, strokePattern = "dashed") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyAxis("y", label = "", valueRange = 
               c(min(c(min(cl[,2],min(cl[,3]),min(cl[,4]),min(cl[,5])))),
                 max(c(max(cl[,2],max(cl[,3]),max(cl[,4]),max(cl[,5]))))+0.2*max(c(max(cl[,2],max(cl[,3]),max(cl[,4]),max(cl[,5])))))) %>%
      dyRangeSelector(height = 20) %>%
      dyEvent("2013", "Forecast", labelLoc = "bottom")
    
  })
  
  
  
  theta_doat <- reactive({
    mikae <- lut$iso2[match(input$country, lut$name)]
    ne <- which(males_dat[[1]][,1] == mikae)
    country_theta_dat <- data.frame(year = c(1960,1970,1980,1990,2000,2010,2015,2020,2025,2030,2035,2040,2044,2045),
                                    male = as.vector(t(males_dat[[4]][ne,-1])), 
                                    female =  as.vector(t(females_dat[[4]][ne,-1])),
                                    mean_male = as.vector(t(males_stat[[4]][1,-1])),
                                    mean_female = as.vector(t(females_stat[[4]][1,-1])))
    
#    a4 <- data.frame(year = 2014:2045, male = as.vector(t(males_for_dat[[4]][ne,-c(1,2)])),
#                     female = as.vector(t(females_for_dat[[4]][ne,-c(1,2)])),
#                     mean_male = as.vector(t(males_for_stat[[4]][1,-c(1,2)])),
#                     mean_female = as.vector(t(females_for_stat[[4]][1,-c(1,2)]))
#    )
#    
#    rbind(country_theta_dat, a4)
    country_theta_dat    
  })
  
  
  output$theta_graf <- renderDygraph({
    ce <- theta_doat()
    
    dygraph(ce, main = paste0("θ - ", input$country), group = "param") %>%
      dySeries("male", label = "Male", fillGraph = TRUE) %>%
      dySeries("female", label = "Female", fillGraph = TRUE) %>%
      dySeries("mean_male", label = "EU mean(m)", strokeWidth = 2, strokePattern = "dashed") %>%
      dySeries("mean_female", label = "EU mean(f)", strokeWidth = 2, strokePattern = "dashed") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyLegend(width = 400) %>%
      dyAxis("y", label = "", valueRange = 
               c(min(c(min(ce[,2],min(ce[,3]),min(ce[,4]),min(ce[,5])))),
                 max(c(max(ce[,2],max(ce[,3]),max(ce[,4]),max(ce[,5]))))+0.2*max(c(max(ce[,2],max(ce[,3]),max(ce[,4]),max(ce[,5])))))) %>%
      dyRangeSelector(height = 20) %>%
      dyEvent("2013", "Forecast", labelLoc = "bottom")
    
  })
  
  
  #--------Maps------------------------------
  bi <- c(100000, 100000, 6000000, 7000000)
  names(bi) <- c("xmin","ymin","xmax","ymax")
  
  tmap_mode("view")
  
  output$malemap <- renderLeaflet({
    ioc <- paramos()
    #maldate <- cbind(males_dat[[ioc]], males_for_dat[[ioc]][-c(1,2)])
    maldate <- males_dat[[ioc]]
    mal_df <- merge(pan_22_sf, maldate, by.x = "pancode", by.y = colnames(maldate)[1])
  if(input$mode == "palette") {  
  tmm <-  tm_shape(mal_df, bbox = bi) +
      tm_fill(col = paste0("X", input$time), style = "jenks", palette = "YlOrBr", showNA = T) +
      tm_borders(lwd = 0.5) }
    else if(input$mode == "mean") {
      brikm <- c(min(maldate[,paste0("X",input$time)], na.rm = T), mean(maldate[,paste0("X",input$time)], na.rm = T), max(maldate[,paste0("X",input$time)], na.rm = T))
      tmm <-  tm_shape(mal_df, bbox = bi) +
      tm_fill(col = paste0("X", input$time), style = "fixed", palette = "YlOrBr", breaks = brikm, labels = c("below", "above"), showNA = T, title = paste0(input$time, " (mean)")) +
        tm_borders(lwd = 0.5)
    }
     # tm_scale_bar(position = c("left", "bottom"))
  tmap_leaflet(tmm) %>% setView(15,60,3)
    
  })
  
  output$femalemap <- renderLeaflet({
    ioc2 <- paramos()
    #femaldate <- cbind(females_dat[[ioc2]], females_for_dat[[ioc2]][-c(1,2)])
    femaldate <- females_dat[[ioc2]]
    femal_df <- merge(pan_22_sf, femaldate, by.x = "pancode", by.y = colnames(femaldate)[1])
    
    #midpoint = mean(femaldate[,paste0("X",input$time)], na.rm = T)
    if(input$mode == "palette") {   
  tmf <-  tm_shape(femal_df, bbox = bi) +
      tm_fill(col = paste0("X", input$time), palette = "YlOrBr", style = "jenks", showNA = T) +
      tm_borders(lwd = 0.5) }
    else if(input$mode == "mean") {
      brikf <- c(min(femaldate[,paste0("X",input$time)], na.rm = T), mean(femaldate[,paste0("X",input$time)], na.rm = T), max(femaldate[,paste0("X",input$time)], na.rm = T))
  tmf <-  tm_shape(femal_df, bbox = bi) +
     tm_fill(col = paste0("X", input$time), style = "fixed", palette = "YlOrBr", breaks = brikf, labels = c("below", "above"), showNA = T, title = paste0(input$time, " (mean)")) +
     tm_borders(lwd = 0.5)    
    }
  tmap_leaflet(tmf) %>% setView(15,60,3)
     
  })
  
  
  #----------histograms--------------------------
  
  
  paramos <- reactive({
    switch(input$paramo,
               "ksi" = 1,
               "kapa" = 2,
               "lamda" = 3,
               "theta" = 4)
  })  
  
  output$malehist <- renderPlot({
    indi <- paramos()
    tm <- paste0("X", input$time)
    #maldates <- cbind(males_dat[[indi]], males_for_dat[[indi]][-c(1,2)])
    maldates <- males_dat[[indi]]
    
    hist(maldates[,tm], prob = T, n = 10, col="lightblue", main=NULL, ylab = NULL, xlab = tm, yaxt="n")
    lines(density(maldates[,tm],  na.rm = T), col="red", lwd=2) 
    abline(v=mean(maldates[,tm], na.rm = T), col="blue", lwd=3)
    legend("topright", legend=c("mean"), col=c("blue"), lty=1, lwd = 3, cex=1, box.lty=0)
  })
  
  output$femalehist <- renderPlot({
    indi2 <- paramos()
    tm2 <- paste0("X", input$time)
    #femaldates <- cbind(females_dat[[indi2]], females_for_dat[[indi2]][-c(1,2)])
    femaldates <- females_dat[[indi2]]
    
    hist(femaldates[,tm2], prob = T, n = 10, col="lightblue", main=NULL, ylab = NULL, xlab = tm2, yaxt="n")
    lines(density(femaldates[,tm2],  na.rm = T), col="red", lwd=2) 
    abline(v=mean(femaldates[,tm2], na.rm = T), col="blue", lwd=3)
    
  })
  
  
  
  #----------Tables--------------------------  
  
#  statmal <- reactive({
#    
#    indi <- paramos()
#    tm <- paste0("X", input$time)
#    #tb1 <- males_stat[[indi]][tm]
#    DF1 <- data.frame(stat = males_stat[[indi]][1], value = males_stat[[indi]][tm])
#    
#  })
##  
#  output$malestat <-  renderTable({
#    statmal()
#  },  
#  striped = TRUE, bordered = TRUE,  
#  hover = TRUE, spacing = 'xs',  
#  digits = 6
#  )
#  
#  
##  statfemal <- reactive({
#    
#    indi2 <- paramos()
##    tm2 <- paste0("X", input$time)
#    #tb1 <- males_stat[[indi]][tm]
#    DF1 <- data.frame(stat = females_stat[[indi2]][1], value = males_stat[[indi2]][tm2])
#    
#  })
#  
#  output$femalestat <-  renderTable({
#    statfemal()
#  },  
##  striped = TRUE, bordered = TRUE,  
#  hover = TRUE, spacing = 'xs',  
#  digits = 6
#  )
  
  
  
  
  
}