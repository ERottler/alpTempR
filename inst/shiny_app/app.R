###

#R-code
#Interactive web app to explore Elevation-dependent warming in the Swiss Alps
#Erwin Rottler 2017/2018

###

#options(shiny.error = browser)

library("devtools")
library("alptempr")
library("shiny")
library("shinythemes")
library("leaflet")
library("zoo")
library("RColorBrewer")
library("zyp")
library("shape")

# baseDir  <- "/home/erwin/ownCloud/RhineFlow/Elevation/R/alpTempR/inst/shiny_app/"
# countDir <- "/home/erwin/ownCloud/RhineFlow/Elevation/R/alpTempR/inst/shiny_app/data/"
baseDir  <- "/srv/shiny-server/alpTemp/"
countDir <- "/home/rottler/alpTemp/"
setwd(baseDir)

load(paste0(baseDir,"data/results_30DMA.RData"))

#leaflat map----
pal <- colorFactor(palette = c("blue", "red", "black"), domain = stat_meta$category)
map <- leaflet(stat_meta) %>%
  # Base groups
  #addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
  addProviderTiles(providers$Esri.WorldImagery,        group = "World Imagery") %>%
  # Overlay groups
  addCircleMarkers(~lon, ~lat, label = as.character(stat_meta$name),
                   popup = ~paste0(as.character(name)," (",stat_meta$stn,"): ",alt," m"),
                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "bottom"),
                   stroke = F, group = "Stations", col = pal(stat_meta$category), fillOpacity = 0.5) %>%

  # Layers control
  addLayersControl(
    baseGroups = c("Terrain Background", "World Imagery"),
    overlayGroups = c("Stations"),
    options = layersControlOptions(collapsed = FALSE)
  )


#UserInterface ----
ui <- fluidPage(theme = shinytheme("superhero"),

        hr(),
        tags$h4("Interactive web app to explore:"),
        hr(),
        tags$h3("Elevation-dependet temperature changes in the Swiss Alps 1981-2017:
                 features, forcings and feedbacks"),
        tags$h5("Erwin Rottler, Christoph Kormann, Till Franke and Axel Bronstert"),
        hr(),

        wellPanel(
          checkboxInput(inputId = "condi_variable_1", label = strong("Select climate variable:")),

          conditionalPanel(
            condition = "input.condi_variable_1 == true",
            selectInput(inputId = "variable_1",
            label   = NULL,
            choices = c("Temperature", "Global radiation","Sunshine duration",
                        "Clouds", "Absolute humidity", "Snow depth"),
            selected = "Temperature")
          ),#conditional Panel

          checkboxInput(inputId = "condi_tool_1", label = strong("Select analytical method:")),

          conditionalPanel(
            condition = "input.condi_tool_1 == true",
            selectInput(inputId = "tool_1",
            label   = NULL,
            choices = c("Mean value", "Linear trend: Sen`s slope"),
            selected= "Mean value")
          ),#conditional Panel

          checkboxInput(inputId = "condi_window_1", label = strong("Select window width:")),

          conditionalPanel(
            condition = "input.condi_window_1 == true",
            selectInput(inputId = "window_1",
            label   = NULL,
            choices = c("30", "60", "90"),
            selected= "30")
          ),#conditional Panel

          checkboxInput(inputId = "condi_plot_type", label = strong("Select plot type:")),

          conditionalPanel(
            condition = "input.condi_plot_type == true",
            selectInput(inputId = "plot_type",
            label   = NULL,
            choices = c("Lines", "Image"),
            selected= "Lines")
          ),#conditional Panel

          actionButton("plot_elev_button", "Calculate", class = "btn-primary")

        ),#wellPanel

        wellPanel(plotOutput("plot_elev", height=220)),
        
        wellPanel(
          
          checkboxInput(inputId = "condi_weather", label = strong("Show weather type classification:")),
          
          conditionalPanel(
            condition = "input.condi_weather == true",
            
            plotOutput("plot_weather", height=420)
            
          )#conditional Panel
          
          ),

        wellPanel(leafletOutput("map", height=220)),

        wellPanel(
          tags$p("If you have questions, suggestions or ideas you want to share,
                   please do not hesitate to contact us: rottler(at)uni-potsdam.de"),
          tags$p("We thank the national weather and climate service of Switzerland
                  (MeteoSwiss) for providing climatological data. This research
                  was funded by Deutsche Forschungsgemeinschaft (DFG) within the
                  graduate research training group NatRiskChange (GRK 2043/1)
                  at the University of Potsdam."),
          tags$b(textOutput("counter"))
        )

      )#fluidpage


#Server----
server <- function(input, output) {


  #Plot yearly cycle and elevation dependency
  f_plot_elev <- function(){

    input$plot_elev_button

    isolate({

      #Accoring to which window width for moving average, data loaded
      if(input$window_1 == "30"){load(paste0(baseDir,"data/results_30DMA.RData"))}
      if(input$window_1 == "60"){load(paste0(baseDir,"data/results_60DMA.RData"))}
      if(input$window_1 == "90"){load(paste0(baseDir,"data/results_90DMA.RData"))}

      #Calculations for image plot
      zero_iso <- iso_days(data_in = tem0_me, isotherm =  0, meta_stat = stat_meta)
      five_iso <- iso_days(data_in = tem0_me, isotherm =  5, meta_stat = stat_meta)
      tens_iso <- iso_days(data_in = tem0_me, isotherm = 10, meta_stat = stat_meta)
      fift_iso <- iso_days(data_in = tem0_me, isotherm = 15, meta_stat = stat_meta)

      isotherm_data <- cbind(zero_iso, five_iso$iso_out, tens_iso$iso_out, fift_iso$iso_out)
      colnames(isotherm_data) <- c("stat_num", "0", "5", "10", "15")

      pos_iso_text <- matrix(ncol = ncol(isotherm_data)-1, nrow = 2)
      pos_iso_text[1, ] <- c(77, 104, 125, 158)
      pos_iso_text[2, ] <- c(67, 52, 34, 12)

      my_col <- colorRampPalette(brewer.pal(11,"RdYlBu"))(100); my_col <- my_col[length(my_col):1]

      if(input$plot_type == "Lines"){

         par(mfrow = c(1, 2))
         par(bg = 'grey85')

         if(input$tool_1 == "Mean value"){

         if(input$variable_1 == "Temperature"){

          plot_cycl_elev(data_in = tem0_me, data_mk = tem0_mk, data_in_me = tem0_me_an,
                         data_meta = stat_meta, main_text = "Temperature [°C]",
                         margins_1 = c(2, 2.5, 2.5, 0.2), margins_2 = c(2, 0.2, 2.5, 4.5),
                         no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = T,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

         if(input$variable_1 == "Global radiation"){
          plot_cycl_elev(data_in = radi_me, data_mk = radi_mk, data_in_me = radi_me_an,
                         data_meta = stat_meta, main_text = "b) Radiation [W/m²] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

         if(input$variable_1 == "Sunshine duration"){
          plot_cycl_elev(data_in = suns_me, data_mk = suns_mk, data_in_me = suns_me_an,
                         data_meta = stat_meta, main_text = "c) Sunshine duration [min] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }
 
         if(input$variable_1 == "Clouds"){
          plot_cycl_elev(data_in = clou_me, data_mk = clou_mk, data_in_me = clou_me_an,
                         data_meta = stat_meta, main_text = "d) Clouds [%] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

         if(input$variable_1 == "Absolute humidity"){
          plot_cycl_elev(data_in = ahum_me, data_mk = ahum_mk, data_in_me = ahum_me_an,
                         data_meta = stat_meta, main_text = "e) Humidity [g/cm³] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

         if(input$variable_1 == "Snow depth"){
          plot_cycl_elev(data_in = snow_me, data_mk = radi_mk, data_in_me = snow_me_an,
                         data_meta = stat_meta, main_text = "f) Snow depth [cm] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }
 
      }

         if(input$tool_1 == "Linear trend: Sen`s slope"){

        if(input$variable_1 == "Temperature"){
          plot_cycl_elev(data_in = tem0_sl, data_mk = tem0_mk, data_in_me = tem0_sl_an,
                         data_meta = stat_meta, main_text = "a) Temperature [°C/dec] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = T,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

        if(input$variable_1 == "Global radiation"){
          plot_cycl_elev(data_in = radi_sl, data_mk = radi_mk, data_in_me = radi_sl_an,
                         data_meta = stat_meta, main_text = "b) Radiation [(W/m²)/dec] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

        }

        if(input$variable_1 == "Sunshine duration"){
          plot_cycl_elev(data_in = suns_sl, data_mk = suns_mk, data_in_me = suns_sl_an,
                         data_meta = stat_meta, main_text = "c) Sunshine duration [min/dec] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

        if(input$variable_1 == "Clouds"){
          plot_cycl_elev(data_in = clou_sl, data_mk = clou_mk, data_in_me = clou_sl_an,
                         data_meta = stat_meta, main_text = "d) Clouds [%/dec] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

        if(input$variable_1 == "Absolute humidity"){
          plot_cycl_elev(data_in = ahum_sr, data_mk = ahum_mk, data_in_me = ahum_sr_an,
                         data_meta = stat_meta, main_text = "e) Humidity [(g/cm³)/dec/(g/cm³)] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

        if(input$variable_1 == "Snow depth"){
          plot_cycl_elev(data_in = snow_sl, data_mk = snow_mk, data_in_me = snow_sl_an,
                         data_meta = stat_meta, main_text = "f) Snow window likelihood [%/dec] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = F, show_mk = T, aggr_cat_mean = T, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }

        if(input$variable_1 == "Air pressure"){

          plot_cycl_elev(data_in = airp_sl, data_mk = airp_mk, data_in_me = airp_sl_an,
                         data_meta = stat_meta, main_text = "g) Air pressure [hPa/dec] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = T, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
                         smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
        }
        }

      }


      if(input$plot_type == "Image"){

        layout(matrix(c(1,1,1,1,1,1,1,2), 1, 8, byrow = TRUE))
        par(bg = 'grey85')

        if(input$tool_1 == "Mean value"){

          if(input$variable_1 == "Temperature"){

            image_cycle_elev(data_in = tem0_me,
                             stat_meta = stat_meta,
                             main="Temperature [°C]",
                             # colors = my_col,
                             # breaks=c(seq(min_na(tem0_sl), max_na(tem0_sl),length.out=101)),
                             colors = c(my_col, rep(my_col[length(my_col)], 15)),
                             breaks=c(seq(min_na(tem0_me), max_na(tem0_me),length.out=116)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)

          }

          if(input$variable_1 == "Global radiation"){
            image_cycle_elev(data_in = radi_me,
                             stat_meta = stat_meta,
                             main = "Radiation [(W/m²)]",
                             # colors = c(my_col, rep(my_col[length(my_col)], 14)),
                             # breaks=c(seq(min_na(radi_sl)-4, max_na(radi_sl),length.out=115)),
                             colors = c(my_col, rep(my_col[length(my_col)], 14)),
                             breaks=c(seq(min_na(radi_me)-5, max_na(radi_me),length.out=115)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Sunshine duration"){
            image_cycle_elev(data_in = suns_me,
                             stat_meta = stat_meta,
                             main = "Sunshine [min]",
                             colors = c(my_col, rep(my_col[length(my_col)], 29)),
                             breaks=c(seq(min_na(suns_me), max_na(suns_me),length.out=130)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Clouds"){
            image_cycle_elev(data_in = clou_me,
                             stat_meta = stat_meta,
                             main = "Clouds [%]",
                             # colors=c(my_col, rep(my_col[length(my_col)], 9)),
                             # breaks=c(seq(min_na(clou_sl), max_na(clou_sl), length.out=110)),
                             colors=c(my_col, rep(my_col[length(my_col)], 9)),
                             breaks=c(seq(min_na(clou_me), max_na(clou_me), length.out=110)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Absolute humidity"){
            image_cycle_elev(data_in = ahum_me,
                             stat_meta = stat_meta,
                             main = "Humidity [g/cm³]", unit="",
                             # colors = c(rep(my_col[1], 9), my_col),
                             # breaks = c(seq(min_na(ahum_sr), max_na(ahum_sr), length.out=110)),
                             colors = c(rep(my_col[1], 0), my_col, rep(my_col[length(my_col)], 5)),
                             breaks = c(seq(min_na(ahum_me), max_na(ahum_me), length.out=106)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Snow depth"){
            image_cycle_elev(data_in = snow_li,
                             stat_meta = stat_meta,
                             main = "Snow probability [%]", unit="",
                             colors = colorRampPalette(c("grey80",brewer.pal(11,"RdYlBu"),"darkblue"))(100),
                             breaks = c(seq(0, 100, length.out = 101)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

        }

        if(input$tool_1 == "Linear trend: Sen`s slope"){

          if(input$variable_1 == "Temperature"){
            image_cycle_elev(data_in = tem0_sl,
                             stat_meta = stat_meta,
                             main="Temperature [°C/dec]",
                             # colors = my_col,
                             # breaks=c(seq(min_na(tem0_sl), max_na(tem0_sl),length.out=101)),
                             colors = c(my_col, rep(my_col[length(my_col)], 15)),
                             breaks=c(seq(min_na(tem0_sl), max_na(tem0_sl),length.out=116)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Global radiation"){
            image_cycle_elev(data_in = radi_sl,
                             stat_meta = stat_meta,
                             main = "Radiation [(W/m²)/dec]",
                             # colors = c(my_col, rep(my_col[length(my_col)], 14)),
                             # breaks=c(seq(min_na(radi_sl)-4, max_na(radi_sl),length.out=115)),
                             colors = c(my_col, rep(my_col[length(my_col)], 14)),
                             breaks=c(seq(min_na(radi_sl)-5, max_na(radi_sl),length.out=115)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Sunshine duration"){
            image_cycle_elev(data_in = suns_sl,
                             stat_meta = stat_meta,
                             main = "Sunshine [min/dec]",
                             colors = c(my_col, rep(my_col[length(my_col)], 29)),
                             breaks=c(seq(min_na(suns_sl), max_na(suns_sl),length.out=130)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Clouds"){
            image_cycle_elev(data_in = clou_sl,
                             stat_meta = stat_meta,
                             main = "Clouds [%/dec]",
                             # colors=c(my_col, rep(my_col[length(my_col)], 9)),
                             # breaks=c(seq(min_na(clou_sl), max_na(clou_sl), length.out=110)),
                             colors=c(my_col, rep(my_col[length(my_col)], 9)),
                             breaks=c(seq(min_na(clou_sl), max_na(clou_sl), length.out=110)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Absolute humidity"){
            image_cycle_elev(data_in = ahum_sr,
                             stat_meta = stat_meta,
                             main = "Humidity [(g/cm³)/dec/(g/cm³)]", unit="",
                             # colors = c(rep(my_col[1], 9), my_col),
                             # breaks = c(seq(min_na(ahum_sr), max_na(ahum_sr), length.out=110)),
                             colors = c(rep(my_col[1], 0), my_col, rep(my_col[length(my_col)], 5)),
                             breaks = c(seq(min_na(ahum_sr), max_na(ahum_sr), length.out=106)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Snow depth"){
            image_cycle_elev(data_in = snow_sl,
                             stat_meta = stat_meta,
                             main = "Snow window probability [%/dec]",
                             # colors = c(my_col, rep(my_col[length(my_col)], 4)),
                             # breaks=c(seq(min_na(snow_sl), max_na(snow_sl),length.out=105)),
                             colors = c(rep(my_col[1], 30), my_col, rep(my_col[length(my_col)], 0)),
                             breaks=c(seq(min_na(snow_sl), max_na(snow_sl),length.out=131)),
                             isotherm_data = isotherm_data,
                             add_isotherms = TRUE,
                             pos_iso_text = pos_iso_text)
          }

          if(input$variable_1 == "Air pressure"){
            plot(1,1, type=F)
            text(1,1, "Plot not available.")
          }
        }

      }


    })

  }

  f_plot_weat <- function(){
    
    input$plot_elev_button
    
    isolate({
      
      #Accoring to which window width for moving average, data loaded
      if(input$window_1 == "30"){load(paste0(baseDir,"data/results_30DMA.RData"))}
      if(input$window_1 == "60"){load(paste0(baseDir,"data/results_60DMA.RData"))}
      if(input$window_1 == "90"){load(paste0(baseDir,"data/results_90DMA.RData"))}
      
      
      gwt_max <- max_na(c(loess_NA_restore(gwt_elev_low),
                          loess_NA_restore(gwt_elev_high),
                          (loess_NA_restore(gwt_elev_high) - loess_NA_restore(gwt_tem0_low)),
                          loess_NA_restore(gwt_tem0_low),
                          loess_NA_restore(gwt_tem0_high),
                          (loess_NA_restore(gwt_tem0_high) - loess_NA_restore(gwt_tem0_low))))+2
      
      gwt_min <- min_na(c(loess_NA_restore(gwt_elev_low),
                          loess_NA_restore(gwt_elev_high),
                          (loess_NA_restore(gwt_elev_high) - loess_NA_restore(gwt_tem0_low)),
                          loess_NA_restore(gwt_tem0_low),
                          loess_NA_restore(gwt_tem0_high),
                          (loess_NA_restore(gwt_tem0_high) - loess_NA_restore(gwt_tem0_low))))-1
      
      par(oma = c(0,0,0,0))
      par(family = "serif")
      layout(matrix(c(1, 2, 3,
                      1, 2, 3), 3, 2), widths=c(1, 1), heights=c(1.4, 1, 1.4))
      par(mar = c(2.2, 3.2, 1.5, 0.5))
      gap_lenght <- 2
      lwd_bar <- 2.5
      gaps_wtc_plot <- 0:25 * gap_lenght
      y_lim <- c(min_na(wtc_score_regis_elev) - 30, max_na(wtc_score_regis_elev) + 30)
      x_lim <- c(-0.5,(3 * 26 + gap_lenght*25) + gap_lenght - 0.5)
      #col2rgb("blue3")
      my_blu     <- rgb(0, 0, 205, max=255, alpha = 255)
      my_blu_bar <- rgb(0, 0, 205, max=255, alpha = 255)
      my_blu_rec <- rgb(0, 0, 205, max=255, alpha = 40)
      #col2rgb("red3")
      my_red     <- rgb(205, 0, 0, max=255, alpha = 255)
      my_red_bar <- rgb(205, 0, 0, max=255, alpha = 255)
      my_red_rec <- rgb(205, 0, 0, max=255, alpha = 40)
      #col2rgb("grey20")
      my_bla     <- rgb(50, 50, 50,   max=255, alpha = 220)
      my_bla_bar <- rgb(0, 0, 0, max=255, alpha = 255)
      
      #WTE score: Climatological subregions
      
      plot(((1:26) * 3 - 3) + gaps_wtc_plot, wtc_score_regis_elev[, 1], type = "n", col = my_blu, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      
      rect(xleft = -0.5, ybottom = min_na(wtc_score_regis_elev) - 30,
           xright = (8 * 3) + gaps_wtc_plot[8] + 1.5,
           ytop =  max_na(wtc_score_regis_elev) + 60, col = my_blu_rec, border = NA, lwd = 1)
      
      rect(xleft = (9 * 3) + gaps_wtc_plot[8] - 1.5, ybottom = min_na(wtc_score_regis_elev) - 30,
           xright = (16 * 3) + gaps_wtc_plot[16] + 1.5,
           ytop =  max_na(wtc_score_regis_elev) + 60, col = my_red_rec, border = NA, lwd = 1)
      
      rect(xleft = (25 * 3) + gaps_wtc_plot[24] - 1.5, ybottom = min_na(wtc_score_regis_elev) - 30,
           xright = (25 * 3) + gaps_wtc_plot[25] + 1.5,
           ytop =  max_na(wtc_score_regis_elev) + 60, col = my_blu_rec, border = NA, lwd = 1)
      
      rect(xleft = (26 * 3) + gaps_wtc_plot[25] - 1.5, ybottom = min_na(wtc_score_regis_elev) - 30,
           xright = (26 * 3) + gaps_wtc_plot[26] + 1.5,
           ytop =  max_na(wtc_score_regis_elev) + 60, col = my_red_rec, border = NA, lwd = 1)
      
      par(new = T)
      #Low GWTs
      plot(((1:26) * 3 - 2) + gaps_wtc_plot, wtc_score_regis_elev[, 1], type = "h", col = my_blu_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 3 - 1) + gaps_wtc_plot, wtc_score_regis_elev[, 4], type = "h", col = my_blu_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 3 - 0) + gaps_wtc_plot, wtc_score_regis_elev[, 7], type = "h", col = my_blu_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      #High GWTs
      plot(((1:26) * 3 - 2) + gaps_wtc_plot, wtc_score_regis_elev[, 2], type = "h", col = my_red_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 3 - 1) + gaps_wtc_plot, wtc_score_regis_elev[, 5], type = "h", col = my_red_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 3 - 0) + gaps_wtc_plot, wtc_score_regis_elev[, 8], type = "h", col = my_red_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      #Net GWTs
      plot(((1:26) * 3 - 2) + gaps_wtc_plot, wtc_score_regis_elev[, 3], type = "h", col = my_bla_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 3 - 1) + gaps_wtc_plot, wtc_score_regis_elev[, 6], type = "h", col = my_bla_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 3 - 0) + gaps_wtc_plot, wtc_score_regis_elev[, 9], type = "h", col = my_bla_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      
      axis(1, at = c(-0.5, ((1:26) * 3 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
           col="black", col.axis="black", tck=-0.04)#plot ticks
      axis(1, at = ((1:26) * 3) + gaps_wtc_plot -1.5, labels = 1:26, tick = FALSE,
           col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
      axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
      abline(h = 0, lty = "dashed", lwd = 0.7)
      abline(v = c(8, 16, 24) * 3 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
      mtext("a) WTE score: Elevation categories",    side = 3, line = 0.1,           cex = 1.0)
      mtext("GWT26 weather type",        side = 1, line = 0.4,        padj = 1, cex = 0.8)
      mtext("WTE score",    side = 2, line = 2.8,              padj = 1, cex = 0.8)
      mtext("cold",         side = 2, line = 1.8,    adj = 0.15, padj = 1, cex = 0.8, col = my_blu_bar)
      mtext("warm",         side = 2, line = 1.8,    adj = 0.85, padj = 1, cex = 0.8, col = my_red_bar)
      mtext("net",          side = 2, line = 1.8,    adj = 0.50, padj = 1, cex = 0.8, col = my_bla_bar)
      mtext("cyclonic",                  side = 3, line = -1.8, adj = 0.12, padj = 1, cex = 0.8)
      mtext("anticyclonic",              side = 1, line = -3.3, adj = 0.45, padj = 1, cex = 0.8)
      mtext("indifferent",               side = 3, line = -1.8, adj = 0.79, padj = 1, cex = 0.8)
      mtext("low pressure",              side = 4, line = -4.2, adj = 0.94, padj = 1, cex = 0.8)
      mtext("high press.",               side = 4, line = -2.3, adj = 0.05, padj = 1, cex = 0.8)
      
      mtext("Low",         side = 3, line = -3.0+0.8, adj = 0.008, padj = 1, cex = 0.6)
      mtext("Middle",      side = 3, line = -3.6+0.8, adj = 0.016, padj = 1, cex = 0.6)
      mtext("High",        side = 3, line = -4.2+0.8, adj = 0.025,  padj = 1, cex = 0.6)
      
      lines(c(1,1), c(125+0, 220+0), type = "l", lwd = 0.5)
      lines(c(2,2), c(125+0, 190+0), type = "l", lwd = 0.5)
      lines(c(3,3), c(125+0, 160+0), type = "l", lwd = 0.5)
      box(lwd = 1.2)
      
      box(lwd = 1.2)
      
      directs <- rep(c("W", "SW", "NW", "N", "NE", "E", "SE", "S"), 3)
      pos_labs <- ((1:26) * 3 - 1.5) + gaps_wtc_plot
      for (i in 1:8){
        mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
      }
      for (i in 9:16){
        mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 1, line = - 1.2)
      }
      for (i in 17:24){
        mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
      }
      
      
      #WTE index
      
      par(mar = c(1.5, 15, 2.1, 13))
      
      x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
      x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15
      
      plot(gwt_tem0_high, type = "n", main ="",
           ylim = c(gwt_min, gwt_max),
           ylab = "", xlab = "", axes = F)
      lines(loess_NA_restore(gwt_tem0_low),  col = my_blu, lwd = 2)
      lines(loess_NA_restore(gwt_tem0_high), col = my_red, lwd = 2)
      lines(loess_NA_restore(gwt_tem0_high) - loess_NA_restore(gwt_tem0_low), col = "black", lwd = 2)
      axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
           col="black", col.axis="black", tck=-0.04)#plot ticks
      axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
           col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
      axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
      abline(h = 0, lty = "dashed", lwd = 0.9)
      abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
      mtext("b) WTE index",               side = 3, line = 0.1, cex = 1.0)
      mtext("Window prob. [%/dec]", side = 2, line = 2,                padj = 1, cex = 0.8)
      legend("topleft", c("                    ","                    "), cex = 0.8, box.col = "white", bg = "white", adj = 0.2)
      mtext("warm GWTs", side = 3, line = -0.4, padj = 1, adj = 0.02, cex = 0.7, col = my_red)
      mtext("cold GWTs", side = 3, line = -1.1, padj = 1, adj = 0.02, cex = 0.7, col = my_blu)
      mtext("WTE index",  side = 3, line = -1.8, padj = 1, adj = 0.02, cex = 0.7, col = "black")
      box()
      
      
      #WTE Index: Climatological subregions WTE
      
      par(mar = c(2.2, 3.2, 1.5, 0.5))
      
      y_lim <- c(min_na(wtc_score_regis_tem0) - 30, max_na(wtc_score_regis_tem0) + 30)
      x_lim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)
      
      plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_tem0[, 1], type = "n", col = my_blu, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      
      rect(xleft = -0.5, ybottom = min_na(wtc_score_regis_tem0) - 30,
           xright = (8 * 4) + gaps_wtc_plot[8] + 1.5,
           ytop =  max_na(wtc_score_regis_tem0) + 60, col = my_blu_rec, border = NA, lwd = 1)
      
      rect(xleft = (9 * 4) + gaps_wtc_plot[8] - 2.5, ybottom = min_na(wtc_score_regis_tem0) - 30,
           xright = (16 * 4) + gaps_wtc_plot[16] + 1.5,
           ytop =  max_na(wtc_score_regis_tem0) + 60, col = my_red_rec, border = NA, lwd = 1)
      
      rect(xleft = (25 * 4) + gaps_wtc_plot[24] - 2.5, ybottom = min_na(wtc_score_regis_tem0) - 30,
           xright = (25 * 4) + gaps_wtc_plot[25] + 1.5,
           ytop =  max_na(wtc_score_regis_tem0) + 60, col = my_blu_rec, border = NA, lwd = 1)
      
      rect(xleft = (26 * 4) + gaps_wtc_plot[25] - 2.5, ybottom = min_na(wtc_score_regis_tem0) - 30,
           xright = (26 * 4) + gaps_wtc_plot[26] + 1.5,
           ytop =  max_na(wtc_score_regis_tem0) + 60, col = my_red_rec, border = NA, lwd = 1)
      
      par(new = T)
      #Low GWTs
      plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_tem0[, 1], type = "h", col = my_blu_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_tem0[, 4], type = "h", col = my_blu_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_tem0[, 7], type = "h", col = my_blu_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_tem0[, 10], type = "h", col = my_blu_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      #High GWTs
      plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_tem0[, 2], type = "h", col = my_red_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_tem0[, 5], type = "h", col = my_red_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_tem0[, 8], type = "h", col = my_red_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_tem0[, 11], type = "h", col = my_red_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      #Net GWTs
      plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_tem0[, 3], type = "h", col = my_bla_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_tem0[, 6], type = "h", col = my_bla_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_tem0[, 9], type = "h", col = my_bla_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      par(new = T)
      plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_tem0[, 12], type = "h", col = my_bla_bar, lwd = lwd_bar, lend = 2,
           xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
           ylim = y_lim, xlim = x_lim)
      
      axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
           col="black", col.axis="black", tck=-0.04)#plot ticks
      axis(1, at = ((1:26) * 4) + gaps_wtc_plot -1.5, labels = 1:26, tick = FALSE,
           col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
      axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
      abline(h = 0, lty = "dashed", lwd = 0.7)
      abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
      mtext("c) WTE score: Climatol. subregions",    side = 3, line = 0.1,                   cex = 1.0)
      mtext("GWT26 weather type",        side = 1, line = 0.4,        padj = 1, cex = 0.8)
      mtext("WTE score",    side = 2, line = 2.8,              padj = 1, cex = 0.8)
      mtext("cold",         side = 2, line = 1.8,    adj = 0.15, padj = 1, cex = 0.8, col = my_blu_bar)
      mtext("warm",         side = 2, line = 1.8,    adj = 0.85, padj = 1, cex = 0.8, col = my_red_bar)
      mtext("net",          side = 2, line = 1.8,    adj = 0.50, padj = 1, cex = 0.8, col = my_bla_bar)
      mtext("cyclonic",     side = 3, line = -1.8, adj = 0.12, padj = 1, cex = 0.8)
      mtext("anticyclonic", side = 1, line = -3.3, adj = 0.45, padj = 1, cex = 0.8)
      mtext("indifferent",  side = 3, line = -1.8, adj = 0.79, padj = 1, cex = 0.8)
      mtext("low pressure", side = 4, line = -4.2, adj = 0.94, padj = 1, cex = 0.8)
      mtext("high press.",  side = 4, line = -2.3, adj = 0.05, padj = 1, cex = 0.8)
      
      mtext("Jura",         side = 3, line = -3.0+0.8, adj = 0.005, padj = 1, cex = 0.6)
      mtext("Plateau",      side = 3, line = -3.6+0.8, adj = 0.016, padj = 1, cex = 0.6)
      mtext("Alps",         side = 3, line = -4.2+0.8, adj = 0.022,  padj = 1, cex = 0.6)
      mtext("S. Alps",      side = 3, line = -4.8+0.8, adj = 0.029, padj = 1, cex = 0.6)
      
      lines(c(1,1), c(120+0, 225+0), type = "l", lwd = 0.5)
      lines(c(2,2), c(120+0, 200+0), type = "l", lwd = 0.5)
      lines(c(3,3), c(120+0, 168+0), type = "l", lwd = 0.5)
      lines(c(4,4), c(120+0, 140+0), type = "l", lwd = 0.5)
      box(lwd = 1.2)
      
      directs <- rep(c("W", "SW", "NW", "N", "NE", "E", "SE", "S"), 3)
      pos_labs <- ((1:26) * 4 - 1.5) + gaps_wtc_plot
      for (i in 1:8){
        mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
      }
      for (i in 9:16){
        mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 1, line = - 1.2)
      }
      for (i in 17:24){
        mtext(text = directs[i], at = pos_labs[i], cex = 0.7, side = 3, line = - 1.2)
      }
      
      
    })
    
  }
  
  #Leaflet map
  output$map <- renderLeaflet({map})

  #Plot output
  output$plot_elev <- renderPlot({f_plot_elev()})

  #Plot output
  output$plot_weather <- renderPlot({f_plot_weat()})
    

  
  #Count app viewings
  output$counter <-
    renderText({
      if(!file.exists(paste0(countDir,"view_count.RData"))){
        view_count <- 0
        view_count_start <- Sys.Date()
        viewings <- Sys.time()
        save(view_count,       file = paste0(countDir, "view_count.RData"))
        save(view_count_start, file = paste0(countDir, "view_count_start.RData"))
        save(viewings, file = paste0(countDir, "viewings.RData"))
      }else{
        load(file = paste0(countDir, "view_count.RData"))
        load(file = paste0(countDir, "view_count_start.RData"))
        load(file = paste0(countDir, "viewings.RData"))
        view_count <- view_count +1
        viewings <- c(viewings, Sys.time())
        save(view_count, file = paste0(countDir, "view_count.RData"))
        save(viewings, file = paste0(countDir, "viewings.RData"))
        paste0("Number of app views since ", view_count_start, " : ", view_count)
      }
    })
}

#Shiny app
shinyApp(ui = ui, server = server)
