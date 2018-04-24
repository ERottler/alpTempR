###

#Interactive Web App: Temperature changes Swiss Alps

###


library("devtools")
#install_github("ERottler/alptempr")
library("alptempr")
library("shiny")
library("shinythemes")
library("leaflet")
library("zoo")
library("RColorBrewer")
library("zyp")

baseDir    <- "u:/RhineFlow/Elevation/R/alpTempR/inst/shiny_app/"
setwd(baseDir)

load(paste0(baseDir,"data/data_ana_30.RData"))

#leaflat map----
map <- leaflet(stat_meta) %>%
  # Base groups
  #addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.TerrainBackground, group = "Terrain Background") %>%
  addProviderTiles(providers$Esri.WorldImagery,        group = "World Imagery") %>%
  # Overlay groups
  addCircleMarkers(~lon, ~lat, label = as.character(stat_meta$name),
                   popup = ~paste0(as.character(name)," (",stat_meta$stn,"): ",alt," m"),
                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "bottom"),
                   stroke = F, group = "Stations", col="red") %>%

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
        tags$h3("Temperature changes Swiss Alps 1981-2017:
                 features, forcings and feedbacks"),
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

        wellPanel(leafletOutput("map", height=220)),

        wellPanel(
          tags$p("+++Under construction+++"),
          tags$p("If you have questions, suggestions or ideas you want to share,
                   please do not hesitate to contact us:"),
          tags$address("rottler(at)uni-potsdam.de", style="color:darkblue")
        )

      )#fluidpage


#Server----
server <- function(input, output) {


  #Plot yearly cycle and elevation dependency
  f_plot_elev <- function(){

    input$plot_elev_button

    isolate({

      #Accoring to which window width for moving average, data loaded
      if(input$window_1 == "30"){load(paste0(baseDir,"data/data_ana_30.RData"))}
      if(input$window_1 == "60"){load(paste0(baseDir,"data/data_ana_60.RData"))}
      if(input$window_1 == "90"){load(paste0(baseDir,"data/data_ana_90.RData"))}

      # if(input$window_1 == "30"){load(paste0("/srv/shiny-server/AlpTempApp/","data/data_ana_30.RData"))}
      # if(input$window_1 == "60"){load(paste0("/srv/shiny-server/AlpTempApp/","data/data_ana_60.RData"))}
      # if(input$window_1 == "90"){load(paste0("/srv/shiny-server/AlpTempApp/","data/data_ana_90.RData"))}

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

        if(input$variable_1 == "Air pressure"){
          plot_cycl_elev(data_in = airp_me, data_mk = airp_mk, data_in_me = airp_me_an,
                         data_meta = stat_meta, main_text = "g) Air pressure [hPa] ",
                         margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                         no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
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
                             main = "Clouds [%/dec]",
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

          if(input$variable_1 == "Air pressure"){
            plot(1,1, type=F)
            text(1,1, "Plot not available.")
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

  #Leaflet map
  output$map <- renderLeaflet({map})

  output$plot_elev <- renderPlot({f_plot_elev()})

}

#Shiny app
shinyApp(ui = ui, server = server)
