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

baseDir  <- "u:/RhineFlow/Elevation/R/alpTempR/inst/shiny_app/"
countDir <- "u:/RhineFlow/Elevation/R/alpTempR/inst/shiny_app/data/"
# baseDir  <- "/srv/shiny-server/AlpTempApp/"
# countDir <- "/home/shiny/AlpTempApp/"
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
                        "Clouds", "Absolute humidity", "Snow depth", "Weather type classification"),
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

           if(input$variable_1 == "Weather type classification"){
             par(oma = c(0,0,0,0))
             par(family = "serif")
             par(mfrow = c(1,2))

             y <- 1:26
             x <- 1:365

             #Plot 1: Temperature - Weather type ranking high / low
             par(mar = c(1, 2.2, 1.5, 0.6))

             col_lows  <- "darkblue"
             col_highs <- "darkorange3" #firebrick
             col_hig_im <- "darkorange3"
             col_low_im <- "darkblue"
             col_net <- "black"

             gwt_max <- max_na(c(loess_NA_restore(gwt_ahum_low),
                                 loess_NA_restore(gwt_ahum_high),
                                 (loess_NA_restore(gwt_ahum_high) - loess_NA_restore(gwt_tem0_low)),
                                 loess_NA_restore(gwt_tem0_low),
                                 loess_NA_restore(gwt_tem0_high),
                                 (loess_NA_restore(gwt_tem0_high) - loess_NA_restore(gwt_tem0_low))))+2

             gwt_min <- min_na(c(loess_NA_restore(gwt_ahum_low),
                                 loess_NA_restore(gwt_ahum_high),
                                 (loess_NA_restore(gwt_ahum_high) - loess_NA_restore(gwt_tem0_low)),
                                 loess_NA_restore(gwt_tem0_low),
                                 loess_NA_restore(gwt_tem0_high),
                                 (loess_NA_restore(gwt_tem0_high) - loess_NA_restore(gwt_tem0_low))))-1

             image(x, y, as.matrix(gwt_rank_tem0), col = c(col_low_im, col_hig_im), breaks = c(-2, 0, 2), ylab = "",
                   xlab = "", main = "", axes = F)

             x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
             x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,380)-15

             axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
                  col="black", col.axis="black", tck=-0.04)#plot ticks
             axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
                  col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
             axis(2, mgp = c(3, 0.2, 0), tck=-0.04, cex.axis = 0.7)
             mtext("GWT26 weather type", side = 2, line = 1.5, padj = 1, cex = 0.8)
             box()
             mtext("Weather types: Temperature", side = 3, line = 0.8, padj = 1, at = 385, cex = 1)

             #Add markers for selected weather types
             par(xpd = T)

             points_x <- rep(375, length(gwt_low_tem0))
             points_y_low  <- c(gwt_low_tem0)
             points_y_high <- c(gwt_high_tem0)

             Arrows(points_x, points_y_low, points_x+5,  points_y_low, col = col_lows,
                    arr.type = "triangle", arr.adj = 1, code = 2, arr.length = 0.15)

             Arrows(points_x, points_y_high, points_x+5,  points_y_high, col = col_highs,
                    arr.type = "triangle", arr.adj = 1, code = 2, arr.length = 0.15)

             par(xpd = F)

             #Plot 2: Temperature - Window trends frequencies
             par(mar = c(1, 0.2, 1.5, 2))

             plot(gwt_tem0_high, type = "n", main ="",
                  ylim = c(gwt_min, gwt_max),
                  ylab = "", xlab = "", axes = F)
             lines(loess_NA_restore(gwt_tem0_low), col = col_lows, lwd = 2)
             lines(loess_NA_restore(gwt_tem0_high), col = col_highs, lwd = 2)
             lines(loess_NA_restore(gwt_tem0_high) - loess_NA_restore(gwt_tem0_low), col = col_net, lwd = 2)
             axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
                  col="black", col.axis="black", tck=-0.04)#plot ticks
             axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
                  col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
             axis(4, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
             abline(h = 0, lty = "dashed", lwd = 0.9)
             abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
             legend("topleft", c("            ","            "), cex = 0.8, box.col = "white", bg = "white", adj = 0.2)
             mtext("warm GWTs", side = 3, line = -0.4, padj = 1, adj = 0.02, cex = 0.7, col = col_highs)
             mtext("cold GWTs", side = 3, line = -1.1, padj = 1, adj = 0.02, cex = 0.7, col = col_lows)
             mtext("WTE index",  side = 3, line = -1.8, padj = 1, adj = 0.02, cex = 0.7, col = col_net)
             box()

             mtext("Trend window prob. [%/dec]", side = 4, line = 0.3, padj = 1, cex = 0.8)

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

  #Plot output
  output$plot_elev <- renderPlot({f_plot_elev()})

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
