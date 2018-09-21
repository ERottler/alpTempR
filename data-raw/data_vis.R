###

#Data visualization

##

#cycl_elev_mea----

# pdf(paste0("u:/RhineFlow/Elevation/cycl_elev_mea.pdf"), width = 7.09, height = 7)
# png(paste0("u:/RhineFlow/Elevation/cycl_elev_mea.png"), width = 6.7, height = 8,
#     units = "in", res = 100)
tiff(paste0("u:/RhineFlow/Elevation/cycl_elev_mea.tiff"), width = 7.09, height = 7,
    units = "in", res = 800)

par(oma=c(0,0,0,0))
par(family="serif")

# layout(matrix(c(1,3,5,7,9,11,13,2,4,6,8,10,12,14), 7, 2), widths=c(1, 1), heights=c(1,1,1,1,1,1,1))
layout(matrix(c(1,3,5,7,9,11,2,4,6,8,10,12), 6, 2), widths=c(1, 1), heights=c(1,1,1,1,1,1))
#layout.show(m)

plot_cycl_elev(data_in = tem0_me, data_mk = tem0_mk, data_in_me = tem0_me_an,
               data_meta = stat_meta, main_text = "a) Temperature [°C] ",
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = T,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = radi_me, data_mk = radi_mk, data_in_me = radi_me_an,
               data_meta = stat_meta, main_text = "b) Radiation [W/m²] ",
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = suns_me, data_mk = suns_mk, data_in_me = suns_me_an,
               data_meta = stat_meta, main_text = "c) Daily sunshine duration [min] ",
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = clou_me, data_mk = clou_mk, data_in_me = clou_me_an,
               data_meta = stat_meta, main_text = "d) Cloud coverage [%] ",
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = ahum_me, data_mk = ahum_mk, data_in_me = ahum_me_an,
               data_meta = stat_meta, main_text = "e) Humidity [g/cm³] ",
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = snow_me, data_mk = radi_mk, data_in_me = snow_me_an,
               data_meta = stat_meta, main_text = "f) Snow depth [cm] ",
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

# plot_cycl_elev(data_in = airp_me, data_mk = airp_mk, data_in_me = airp_me_an,
#                data_meta = stat_meta, main_text = "g) Air pressure [hPa] ",
#                margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
#                no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
#                smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)
dev.off()




#cycl_elev_slo----

# pdf(paste0("u:/RhineFlow/Elevation/cycl_elev_slo.pdf"), width = 7.09, height = 7)
# png(paste0("u:/RhineFlow/Elevation/cycl_elev_slo.png"), width = 6.7, height = 8,
#     units = "in", res = 100)
tiff(paste0("u:/RhineFlow/Elevation/cycl_elev_slo.tiff"), width = 7.09, height = 7,
     units = "in", res = 800)

par(oma=c(0,0,0,0))
par(family="serif")

# layout(matrix(c(1,3,5,7,9,11,13,2,4,6,8,10,12,14), 7, 2), widths=c(1, 1), heights=c(1,1,1,1,1,1,1))
layout(matrix(c(1,3,5,7,9,11,2,4,6,8,10,12), 6, 2), widths=c(1, 1), heights=c(1,1,1,1,1,1))
#layout.show(m)

plot_cycl_elev(data_in = tem0_sl, data_mk = tem0_mk, data_in_me = tem0_sl_an,
               data_meta = stat_meta, main_text = "a) Temperature [°C/dec] ",
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = T,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = radi_sl, data_mk = radi_mk, data_in_me = radi_sl_an,
               data_meta = stat_meta, main_text = "b) Radiation [(W/m²)/dec] ",
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = suns_sl, data_mk = suns_mk, data_in_me = suns_sl_an,
               data_meta = stat_meta, main_text = "c) Daily sunshine duration [min/dec] ",
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = clou_sl, data_mk = clou_mk, data_in_me = clou_sl_an,
               data_meta = stat_meta, main_text = "d) Cloud coverage [%/dec] ",
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = ahum_sr, data_mk = ahum_mk, data_in_me = ahum_sr_an,
               data_meta = stat_meta, main_text = "e) Humidity [(g/cm³)/dec/(g/cm³)] ",
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = snow_sl, data_mk = snow_mk, data_in_me = snow_sl_an,
               data_meta = stat_meta, main_text = "f) Snow window probability [%/dec] ",
               no_col = F, show_mk = T, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)
# 
# plot_cycl_elev(data_in = airp_sl, data_mk = airp_mk, data_in_me = airp_sl_an,
#                data_meta = stat_meta, main_text = "g) Air pressure [hPa/dec] ",
#                margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
#                no_col = T, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
#                smooth_val = 0.1, mk_sig_level = 0.05, add_st_num = T)
dev.off()


#imag_elev_slo----

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


# pdf("u:/RhineFlow/Elevation/imag_elev_slo.pdf", width = 7.09, height = 7)
# png(paste0("u:/RhineFlow/Elevation/imag_elev_slo.png"), width = 6.7, height = 7,
#     units = "in", res = 1200)
tiff(paste0("u:/RhineFlow/Elevation/imag_elev_slo.tiff"), width = 7.09, height = 7,
     units = "in", res = 800)

par(oma=c(0,0,0,0))
par(family="serif")

layout(matrix(c(1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13, 1,5,9,13,
                2,6,10,14,
                3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15, 3,7,11,15,
                4,8,12,16),
              4, 16), widths=c(), heights=c(1,1,1,1))

image_cycle_elev(data_in = tem0_sl,
                 stat_meta = stat_meta,
                 main="a) Temperature [°C/dec]",
                 # colors = my_col,
                 # breaks=c(seq(min_na(tem0_sl), max_na(tem0_sl),length.out=101)),
                 colors = c(my_col, rep(my_col[length(my_col)], 15)),
                 breaks=c(seq(min_na(tem0_sl), max_na(tem0_sl),length.out=116)),
                 isotherm_data = isotherm_data,
                 add_isotherms = TRUE,
                 pos_iso_text = pos_iso_text)

image_cycle_elev(data_in = diff_max_min,
                 stat_meta = stat_meta,
                 main="b) Tmax - Tmin [°C/dec]",
                 # colors=c(rep(my_col[1], 4), my_col),
                 # breaks=c(seq(min_na(diff_max_min), max_na(diff_max_min), length.out = 105)),
                 colors=c(rep(my_col[1], 5), my_col, rep(my_col[length(my_col)], 0)),
                 breaks=c(seq(min_na(diff_max_min), max_na(diff_max_min), length.out = 106)),
                 isotherm_data = isotherm_data,
                 add_isotherms = TRUE,
                 pos_iso_text = pos_iso_text)

image_cycle_elev(data_in = radi_sl,
                 stat_meta = stat_meta,
                 main = "c) Radiation [(W/m²)/dec]",
                 # colors = c(my_col, rep(my_col[length(my_col)], 14)),
                 # breaks=c(seq(min_na(radi_sl)-4, max_na(radi_sl),length.out=115)),
                 colors = c(my_col, rep(my_col[length(my_col)], 14)),
                 breaks=c(seq(min_na(radi_sl)-5, max_na(radi_sl),length.out=115)),
                 isotherm_data = isotherm_data,
                 add_isotherms = TRUE,
                 pos_iso_text = pos_iso_text)

image_cycle_elev(data_in = clou_sl,
                 stat_meta = stat_meta,
                 main = "d) Cloud coverage [%/dec]",
                 # colors=c(my_col, rep(my_col[length(my_col)], 9)),
                 # breaks=c(seq(min_na(clou_sl), max_na(clou_sl), length.out=110)),
                 colors=c(my_col, rep(my_col[length(my_col)], 9)),
                 breaks=c(seq(min_na(clou_sl), max_na(clou_sl), length.out=110)),
                 isotherm_data = isotherm_data,
                 add_isotherms = TRUE,
                 pos_iso_text = pos_iso_text)

image_cycle_elev(data_in = ahum_sr,
                 stat_meta = stat_meta,
                 main = "e) Humidity [(g/cm³)/dec/(g/cm³)]", unit="",
                 # colors = c(rep(my_col[1], 9), my_col),
                 # breaks = c(seq(min_na(ahum_sr), max_na(ahum_sr), length.out=110)),
                 colors = c(rep(my_col[1], 0), my_col, rep(my_col[length(my_col)], 5)),
                 breaks = c(seq(min_na(ahum_sr), max_na(ahum_sr), length.out=106)),
                 isotherm_data = isotherm_data,
                 add_isotherms = TRUE,
                 pos_iso_text = pos_iso_text)

image_cycle_elev(data_in = snow_sl,
                 stat_meta = stat_meta,
                 main = "f) Snow window probability [%/dec]",
                 # colors = c(my_col, rep(my_col[length(my_col)], 4)),
                 # breaks=c(seq(min_na(snow_sl), max_na(snow_sl),length.out=105)),
                 colors = c(rep(my_col[1], 30), my_col, rep(my_col[length(my_col)], 0)),
                 breaks=c(seq(min_na(snow_sl), max_na(snow_sl),length.out=131)),
                 isotherm_data = isotherm_data,
                 add_isotherms = TRUE,
                 pos_iso_text = pos_iso_text)

image_cycle_elev(data_in = suns_sl,
                 stat_meta = stat_meta,
                 main = "g) Sunshine duration [min/dec]",
                 colors = c(my_col, rep(my_col[length(my_col)], 29)),
                 breaks=c(seq(min_na(suns_sl), max_na(suns_sl),length.out=130)),
                 isotherm_data = isotherm_data,
                 add_isotherms = TRUE,
                 pos_iso_text = pos_iso_text)

image_cycle_elev(data_in = snow_li,
                 stat_meta = stat_meta,
                 main = "h) Snow probability [%]", unit="",
                 colors = colorRampPalette(c("grey80",brewer.pal(11,"RdYlBu"),"darkblue"))(100),
                 breaks = c(seq(0, 100, length.out = 101)),
                 isotherm_data = isotherm_data,
                 add_isotherms = TRUE,
                 pos_iso_text = pos_iso_text)

dev.off()


#wtc_gwt_26####

# pdf(paste0("u:/RhineFlow/Elevation/gwt_26.pdf"), width = 7.09, height = 4)
tiff(paste0("u:/RhineFlow/Elevation/gwt_26.tiff"), width = 7.09, height = 4,
     units = "in", res = 800)

par(oma = c(0,0,0,0))
par(family = "serif")
par(mfrow = c(2,2))

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
mtext("a) Weather types: Temperature", side = 3, line = 0.8, padj = 1, at = 385, cex = 1)

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


#Plot3: Humidity: Weather type ranking high / low
par(mar = c(1, 2.2, 1.5, 0.6))

image(x, y, as.matrix(gwt_rank_ahum), col = c(col_low_im, col_hig_im), breaks = c(-2, 0, 2), ylab = "",
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
mtext("b) Weather types: Humidity", side = 3, line = 0.8, padj = 1, at = 385, cex = 1)

#Add markers for selected weather types
par(xpd = T)

points_x <- rep(375, length(gwt_low_ahum))
points_y_low  <- c(gwt_low_ahum)
points_y_high <- c(gwt_high_ahum)

Arrows(points_x, points_y_low, points_x+5,  points_y_low, col = col_lows,
       arr.type = "triangle", arr.adj = 1, code = 2, arr.length = 0.15)

Arrows(points_x, points_y_high, points_x+5,  points_y_high, col = col_highs,
       arr.type = "triangle", arr.adj = 1, code = 2, arr.length = 0.15)

par(xpd = F)

#Plot 4: Humidity - Window trends frequencies
par(mar = c(1, 0.2, 1.5, 2))

plot(gwt_ahum_high, type = "n", main ="",
     ylim = c(gwt_min, gwt_max),
     ylab = "", xlab = "", axes = F)
lines(loess_NA_restore(gwt_ahum_low), col = col_lows, lwd = 2)
lines(loess_NA_restore(gwt_ahum_high), col = col_highs, lwd = 2)
lines(loess_NA_restore(gwt_ahum_high) - loess_NA_restore(gwt_ahum_low), col = col_net,
      lwd = 2, lty = "solid")

axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
axis(4, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
abline(h = 0, lty = "dashed", lwd = 0.9, col = "grey40")
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
legend("topleft", c("            ","            "), cex = 0.8, box.col = "white", bg = "white", adj = 0.2)
mtext("moist GWTs", side = 3, line = -0.4, padj = 1, adj = 0.02, cex = 0.7, col = col_highs)
mtext("dry GWTs", side = 3, line = -1.1, padj = 1, adj = 0.02, cex = 0.7, col = col_lows)
mtext("WTE index",  side = 3, line = -1.8, padj = 1, adj = 0.02, cex = 0.7, col = col_net)
box()

mtext("Trend window prob. [%/dec]", side = 4, line = 0.3, padj = 1, cex = 0.8)


dev.off()

#wtc_gwt_26_new----

# pdf(paste0("u:/RhineFlow/Elevation/gwt_26_reg.pdf"), width = 7.09, height = 7)
tiff(paste0("u:/RhineFlow/Elevation/gwt_26_reg.tiff"), width = 7.09, height = 7,
     units = "in", res = 800)

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

par(oma = c(0,0,0,0))
par(family = "serif")
par(mar = c(1.8, 2.5, 1.5, 0.5))
layout(matrix(c(1, 2, 3, 
                1, 4, 3), 3, 2), widths=c(1, 1), heights=c(1.35, 1, 1.35))
gap_lenght <- 2
lwd_bar <- 2.5
gaps_wtc_plot <- 0:25 * gap_lenght
y_lim <- c(min_na(wtc_score_regis_tem0) - 30, max_na(wtc_score_regis_tem0) + 30)
x_lim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)
#col2rgb("blue3")
my_blu     <- rgb(0, 0, 205, max=255, alpha = 255)
my_blu_rec <- rgb(0, 0, 205, max=255, alpha = 40)
#col2rgb("red3")
my_red     <- rgb(205, 0, 0, max=255, alpha = 255)
my_red_rec <- rgb(205, 0, 0, max=255, alpha = 40)
#col2rgb("black")
col2rgb("grey20")
my_bla <- rgb(50, 50, 50,   max=255, alpha = 220)

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
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_tem0[, 1], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_tem0[, 4], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_tem0[, 7], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_tem0[, 10], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
#High GWTs
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_tem0[, 2], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_tem0[, 5], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_tem0[, 8], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_tem0[, 11], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
#Net GWTs
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_tem0[, 3], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_tem0[, 6], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_tem0[, 9], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_tem0[, 12], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)

axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = ((1:26) * 4) + gaps_wtc_plot -1.5, labels = 1:26, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("a) Temperature: WTE score",    side = 3, line = 0.2,                   cex = 1.0)
mtext("WTE score",    side = 2, line = 2,                padj = 1, cex = 0.8)
mtext("cyclonic",     side = 3, line = -1.8, adj = 0.12, padj = 1, cex = 0.8)
mtext("anticyclonic", side = 1, line = -3.3, adj = 0.45, padj = 1, cex = 0.8)
mtext("indifferent",  side = 3, line = -1.8, adj = 0.79, padj = 1, cex = 0.8)
mtext("low pressure", side = 4, line = -4.2, adj = 0.94, padj = 1, cex = 0.8)
mtext("high press.",  side = 4, line = -2.3, adj = 0.05, padj = 1, cex = 0.8)
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

#Plot b: Temperature WTE index

par(mar = c(1, 2.5, 1.5, 0.2))

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
mtext("b) Temperature: WTE index",               side = 3, line = 0.2, cex = 1.0)
mtext("Trend window prob. [%/dec]", side = 2, line = 2,                padj = 1, cex = 0.8)
legend("topleft", c("                    ","                    "), cex = 0.8, box.col = "white", bg = "white", adj = 0.2)
mtext("warm GWTs", side = 3, line = -0.4, padj = 1, adj = 0.02, cex = 0.7, col = my_red)
mtext("cold GWTs", side = 3, line = -1.1, padj = 1, adj = 0.02, cex = 0.7, col = my_blu)
mtext("WTE index",  side = 3, line = -1.8, padj = 1, adj = 0.02, cex = 0.7, col = "black")
box()


#Plot d: Humidity WTE index

par(mar = c(1.2, 2.5, 2.1, 0.5))
gap_lenght <- 2
lwd_bar <- 2.5
gaps_wtc_plot <- 0:25 * gap_lenght
y_lim <- c(min_na(wtc_score_regis_ahum) - 30, max_na(wtc_score_regis_ahum) + 30)
x_lim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_ahum[, 1], type = "n", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)

rect(xleft = -0.5, ybottom = min_na(wtc_score_regis_ahum) - 30, 
     xright = (6 * 4) + gaps_wtc_plot[6] + 1.5,
     ytop =  max_na(wtc_score_regis_ahum) + 60, col = my_blu_rec, border = NA, lwd = 1)

rect(xleft = (9 * 4) + gaps_wtc_plot[8] - 2.5, ybottom = min_na(wtc_score_regis_ahum) - 30, 
     xright = (11 * 4) + gaps_wtc_plot[11] + 1.5,
     ytop =  max_na(wtc_score_regis_ahum) + 60, col = my_red_rec, border = NA, lwd = 1)

rect(xleft = (15 * 4) + gaps_wtc_plot[14] - 2.5, ybottom = min_na(wtc_score_regis_ahum) - 30, 
     xright = (18 * 4) + gaps_wtc_plot[18] + 1.5,
     ytop =  max_na(wtc_score_regis_ahum) + 60, col = my_red_rec, border = NA, lwd = 1)

rect(xleft = (23 * 4) + gaps_wtc_plot[22] - 2.5, ybottom = min_na(wtc_score_regis_ahum) - 30, 
     xright = (24 * 4) + gaps_wtc_plot[24] + 1.5,
     ytop =  max_na(wtc_score_regis_ahum) + 60, col = my_red_rec, border = NA, lwd = 1)

rect(xleft = (19 * 4) + gaps_wtc_plot[18] - 2.5, ybottom = min_na(wtc_score_regis_ahum) - 30, 
     xright = (21 * 4) + gaps_wtc_plot[21] + 1.5,
     ytop =  max_na(wtc_score_regis_ahum) + 60, col = my_blu_rec, border = NA, lwd = 1)

rect(xleft = (25 * 4) + gaps_wtc_plot[24] - 2.5, ybottom = min_na(wtc_score_regis_ahum) - 30, 
     xright = (25 * 4) + gaps_wtc_plot[25] + 1.5,
     ytop =  max_na(wtc_score_regis_ahum) + 60, col = my_blu_rec, border = NA, lwd = 1)

rect(xleft = (26 * 4) + gaps_wtc_plot[25] - 2.5, ybottom = min_na(wtc_score_regis_ahum) - 30, 
     xright = (26 * 4) + gaps_wtc_plot[26] + 1.5,
     ytop =  max_na(wtc_score_regis_ahum) + 60, col = my_red_rec, border = NA, lwd = 1)

par(new = T)
#Low GWTs
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_ahum[, 1], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_ahum[, 4], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_ahum[, 7], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_ahum[, 10], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
#High GWTs
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_ahum[, 2], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_ahum[, 5], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_ahum[, 8], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_ahum[, 11], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
#Net GWTs
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis_ahum[, 3], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis_ahum[, 6], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis_ahum[, 9], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis_ahum[, 12], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)

axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = ((1:26) * 4) + gaps_wtc_plot -1.5, labels = 1:26, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("d) Humidity: WTE score",    side = 3, line = 0.2,                   cex = 1.0)
mtext("WTE score",    side = 2, line = 2,                padj = 1, cex = 0.8)
mtext("cyclonic",     side = 3, line = -1.8, adj = 0.12, padj = 1, cex = 0.8)
mtext("anticyclonic", side = 1, line = -3.3, adj = 0.45, padj = 1, cex = 0.8)
mtext("indifferent",  side = 3, line = -1.8, adj = 0.79, padj = 1, cex = 0.8)
mtext("low pressure", side = 4, line = -4.2, adj = 0.94, padj = 1, cex = 0.8)
mtext("high press.",  side = 4, line = -2.3, adj = 0.05, padj = 1, cex = 0.8)
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


#Plot c: Humidity WTE index

par(mar = c(1, 0.2, 1.5, 2.5))

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15

plot(gwt_ahum_high, type = "n", main ="",
     ylim = c(gwt_min, gwt_max),
     ylab = "", xlab = "", axes = F)
lines(loess_NA_restore(gwt_ahum_low),  col = my_blu, lwd = 2)
lines(loess_NA_restore(gwt_ahum_high), col = my_red, lwd = 2)
lines(loess_NA_restore(gwt_ahum_high) - loess_NA_restore(gwt_ahum_low), col = "black", lwd = 2)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
axis(4, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
abline(h = 0, lty = "dashed", lwd = 0.9)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
mtext("c) Humidity: WTE index",               side = 3, line = 0.2, cex = 1.0)
# corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
# par(xpd = T) #Draw outside plot area
# text(x = corners[2]+27, y = mean(corners[3:4]), "Trend window prob. [%/dec]", srt = 270, cex = 1.2)
# par(xpd = F)
mtext("Trend window prob. [%/dec]", side = 4, line = 1.3,                padj = 0, cex = 0.8)
legend("topleft", c("                    ","                    "), cex = 0.8, box.col = "white", bg = "white", adj = 0.2)
mtext("moist GWTs", side = 3, line = -0.4, padj = 1, adj = 0.02, cex = 0.7, col = my_red)
mtext("dry GWTs", side = 3, line = -1.1, padj = 1, adj = 0.02, cex = 0.7, col = my_blu)
mtext("WTE index",  side = 3, line = -1.8, padj = 1, adj = 0.02, cex = 0.7, col = "black")
box()


dev.off()


#wtc_cap----

pdf(paste0("u:/RhineFlow/Elevation/weather_type_r.pdf"), width = 6.7, height = 2)

par(oma = c(0,0,0,0))
par(family = "serif")
par(mfrow = c(1,2))


blue_1  <- "skyblue2"
blue_2  <- "blue2"
blue_3  <- rgb(0, 0, 120, maxColorValue=255)
green_1 <- "darkseagreen3"
green_2 <- "darkgreen"

my_cols <- c("orange2", "gold", blue_1, blue_2, blue_3, "grey75",
             "grey42", green_1, green_2)
#Plot1: Frequencies
par(mar = c(1, 2, 1, 0.2))

barplot(as.matrix(wl_data), col = my_cols, axisnames = F, border = NA, space = 0,
        xaxs = "i", yaxs = "i", ylim = c(0, 100), axes = F)

x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,380)-15

axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
axis(2, mgp = c(3, 0.2, 0), tck=-0.04, cex.axis = 0.7)
mtext("Frequency [%]", side = 2, line = 1.3, padj = 1, cex = 0.8)
box()

mtext("Weather type classifications", side = 3, line = 0.6, padj = 1, at = 385, cex = 1)

#Plot 2: Window trends
par(mar = c(1, 0.2, 1, 2))

plot(loess_NA_restore(wt_5), type="n", axes = F, ylab = "", xlab = "", ylim = c(-4.5, 4.5))
lines(loess_NA_restore(wt_data[1, ]), col = my_cols[1], lwd = 2)#High Pressure over the Alps
lines(loess_NA_restore(wt_data[2, ]), col = my_cols[2], lwd = 2)#High Pressure over Central Europe
lines(loess_NA_restore(wt_data[3, ]), col = my_cols[3], lwd = 2)#Westerly flow over Southern Europe, cyclonic
lines(loess_NA_restore(wt_data[4, ]), col = my_cols[4], lwd = 2)#West-SouthWest, cyclonic, flat pressure
lines(loess_NA_restore(wt_data[5, ]), col = my_cols[5], lwd = 2)#West-SouthWest, cyclonic
lines(loess_NA_restore(wt_data[6, ]), col = my_cols[6], lwd = 2)#East, indifferent
lines(loess_NA_restore(wt_data[7, ]), col = my_cols[7], lwd = 2)#NorthEast, indifferent
lines(loess_NA_restore(wt_data[8, ]), col = my_cols[8], lwd = 2)#Westerly flow over Northern Europe
lines(loess_NA_restore(wt_data[9, ]), col = my_cols[9], lwd = 2)#North, cyclonic

axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0, 0), cex.axis = 0.7)
axis(4, mgp = c(3, 0.0, 0), tck=-0.04, cex.axis = 0.7)
abline(h = 0, lty = "dashed", lwd = 0.9)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
box()

mtext("Trend window prob. [%/dec]", side = 4, line = 0.5, padj = 1, cex = 0.8)


dev.off()




#wtc_gwt_26_new_ahum----

pdf(paste0("u:/RhineFlow/Elevation/gwt_26_reg_ahum.pdf"), width = 7.09, height = 4.5)

par(oma = c(0,0,0,0))
par(family = "serif")
par(mar = c(1.8, 2.5, 1.5, 0.5))
layout(matrix(c(1, 3, 1, 2, 1, 2, 1, 4), 2, 4), widths=c(1, 1, 1, 1), heights=c(1.35, 1))
gap_lenght <- 2
lwd_bar <- 2.5
gaps_wtc_plot <- 0:25 * gap_lenght
y_lim <- c(min_na(gwt_sums_ahum_low) - 30, max_na(gwt_sums_ahum_hig) + 60)
x_lim <- c(-0.5,(4 * 26 + gap_lenght*25) + gap_lenght - 0.5)
#col2rgb("blue3")
my_blu     <- rgb(0, 0, 205, max=255, alpha = 255)
my_blu_rec <- rgb(0, 0, 205, max=255, alpha = 40)
#col2rgb("red3")
my_red     <- rgb(205, 0, 0, max=255, alpha = 255)
my_red_rec <- rgb(205, 0, 0, max=255, alpha = 40)
#col2rgb("black")
col2rgb("grey20")
my_bla <- rgb(50, 50, 50,   max=255, alpha = 220)

plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis[, 1], type = "n", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)

rect(xleft = -0.5, ybottom = min_na(gwt_sums_ahum_low) - 30, 
     xright = (6 * 4) + gaps_wtc_plot[6] + 1.5,
     ytop =  max_na(gwt_sums_ahum_hig) + 60, col = my_blu_rec, border = NA, lwd = 1)

rect(xleft = (9 * 4) + gaps_wtc_plot[8] - 2.5, ybottom = min_na(gwt_sums_ahum_low) - 30, 
     xright = (11 * 4) + gaps_wtc_plot[11] + 1.5,
     ytop =  max_na(gwt_sums_ahum_hig) + 60, col = my_red_rec, border = NA, lwd = 1)

rect(xleft = (15 * 4) + gaps_wtc_plot[14] - 2.5, ybottom = min_na(gwt_sums_ahum_low) - 30, 
     xright = (18 * 4) + gaps_wtc_plot[18] + 1.5,
     ytop =  max_na(gwt_sums_ahum_hig) + 60, col = my_red_rec, border = NA, lwd = 1)

rect(xleft = (23 * 4) + gaps_wtc_plot[22] - 2.5, ybottom = min_na(gwt_sums_ahum_low) - 30, 
     xright = (24 * 4) + gaps_wtc_plot[24] + 1.5,
     ytop =  max_na(gwt_sums_ahum_hig) + 60, col = my_red_rec, border = NA, lwd = 1)

rect(xleft = (19 * 4) + gaps_wtc_plot[18] - 2.5, ybottom = min_na(gwt_sums_ahum_low) - 30, 
     xright = (21 * 4) + gaps_wtc_plot[21] + 1.5,
     ytop =  max_na(gwt_sums_ahum_hig) + 60, col = my_blu_rec, border = NA, lwd = 1)

rect(xleft = (25 * 4) + gaps_wtc_plot[24] - 2.5, ybottom = min_na(gwt_sums_ahum_low) - 30, 
     xright = (25 * 4) + gaps_wtc_plot[25] + 1.5,
     ytop =  max_na(gwt_sums_ahum_hig) + 60, col = my_blu_rec, border = NA, lwd = 1)

rect(xleft = (26 * 4) + gaps_wtc_plot[25] - 2.5, ybottom = min_na(gwt_sums_ahum_low) - 30, 
     xright = (26 * 4) + gaps_wtc_plot[26] + 1.5,
     ytop =  max_na(gwt_sums_ahum_hig) + 60, col = my_red_rec, border = NA, lwd = 1)

par(new = T)
#Low GWTs
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis[, 1], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis[, 4], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis[, 7], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis[, 10], type = "h", col = my_blu, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
#High GWTs
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis[, 2], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis[, 5], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis[, 8], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis[, 11], type = "h", col = my_red, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
#Net GWTs
plot(((1:26) * 4 - 3) + gaps_wtc_plot, wtc_score_regis[, 3], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 2) + gaps_wtc_plot, wtc_score_regis[, 6], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 1) + gaps_wtc_plot, wtc_score_regis[, 9], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)
par(new = T)
plot(((1:26) * 4 - 0) + gaps_wtc_plot, wtc_score_regis[, 12], type = "h", col = my_bla, lwd = lwd_bar, lend = 2,
     xaxs = "i", yaxs = "i", axes = F, ylab = "", xlab = "",
     ylim = y_lim, xlim = x_lim)

axis(1, at = c(-0.5, ((1:26) * 4 + 1.5) + gaps_wtc_plot), labels = rep("", 27), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = ((1:26) * 4) + gaps_wtc_plot -1.5, labels = 1:26, tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
abline(h = 0, lty = "dashed", lwd = 0.7)
abline(v = c(8, 16, 24) * 4 + 1.5 + gaps_wtc_plot[c(8, 16, 24)], lty = "dashed", lwd = 0.7)
mtext("a) WTE score",    side = 3, line = 0.2,                   cex = 1.0)
mtext("WTE score",    side = 2, line = 2,                padj = 1, cex = 0.8)
mtext("cyclonic",     side = 3, line = -1.8, adj = 0.12, padj = 1, cex = 0.8)
mtext("anticyclonic", side = 1, line = -3.3, adj = 0.45, padj = 1, cex = 0.8)
mtext("indifferent",  side = 3, line = -1.8, adj = 0.79, padj = 1, cex = 0.8)
mtext("low pressure", side = 4, line = -4.2, adj = 0.94, padj = 1, cex = 0.8)
mtext("high press.",  side = 4, line = -2.3, adj = 0.05, padj = 1, cex = 0.8)
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

#Plot b: WTE index

par(mar = c(1, 2.5, 1.5, 0.1))

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(16,46,74,105,135,166,196,227,258,288,319,349,380)-15

plot(gwt_ahum_high, type = "n", main ="",
     ylim = c(gwt_min, gwt_max),
     ylab = "", xlab = "", axes = F)
lines(loess_NA_restore(gwt_ahum_low),  col = my_blu, lwd = 2)
lines(loess_NA_restore(gwt_ahum_high), col = my_red, lwd = 2)
lines(loess_NA_restore(gwt_ahum_high) - loess_NA_restore(gwt_ahum_low), col = "black", lwd = 2)
axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
     col="black", col.axis="black", tck=-0.04)#plot ticks
axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 1)
axis(2, mgp = c(3, 0.3, 0), tck = -0.015, cex.axis = 1)
abline(h = 0, lty = "dashed", lwd = 0.9)
abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
mtext("b) WTE index",               side = 3, line = 0.2, cex = 1.0)
mtext("Trend window prob. [%/dec]", side = 2, line = 2,                padj = 1, cex = 0.8)
legend("topleft", c("                    ","                    "), cex = 0.8, box.col = "white", bg = "white", adj = 0.2)
mtext("warm GWTs", side = 3, line = -0.4, padj = 1, adj = 0.02, cex = 0.7, col = my_red)
mtext("cold GWTs", side = 3, line = -1.1, padj = 1, adj = 0.02, cex = 0.7, col = my_blu)
mtext("WTE index",  side = 3, line = -1.8, padj = 1, adj = 0.02, cex = 0.7, col = "black")
box()



dev.off()


#seas_vals_category####

# pdf(paste0("u:/RhineFlow/Elevation/seas_vals_cat.pdf"), width = 7.09, height = 4)

tiff(paste0("u:/RhineFlow/Elevation/seas_vals_cat.tiff"), width = 7.09, height = 4,
     units = "in", res = 800)

par(oma = c(0,0,0,0))
par(family = "serif")
par(mfrow = c(2,2))
par(mar = c(1, 2, 0.5, 0.6))
hori_lines <- c(-10, -5, 0, 5, 10, 15, 20, 25)
#col2rgb("blue3")
my_blu <- rgb(0, 0, 205, max=255, alpha = 160)
#col2rgb("red3")
my_red <- rgb(205, 0, 0, max=255, alpha = 160)
#col2rgb("black")
my_bla <- rgb(0, 0, 0,   max=255, alpha = 160)

#DJF
plot(djf_low, type = "n", ylim=c(min_na(c(djf_low, djf_mid, djf_hig))-1.2,
                                 max_na(c(djf_low, djf_mid, djf_hig))+1.2),
     axes = F, ylab = "", xlab = "")
lines(djf_hig, col=my_blu, lwd = 2, type = "l")
lines(djf_mid, col=my_bla, lwd = 2, type = "l")
lines(djf_low, col=my_red, lwd = 2, type = "l")
points(djf_hig, col="blue3", lwd = 2, pch = 16, cex = 0.7)
points(djf_mid, col="black", lwd = 2, pch = 16, cex = 0.7)
points(djf_low, col="red3",  lwd = 2, pch = 16, cex = 0.7)
abline(djf_low_sl, col = "red3", lwd = 1, lty = "longdash")
abline(djf_mid_sl, col = "black", lwd = 1, lty = "longdash")
abline(djf_hig_sl, col = "blue3", lwd = 1, lty = "longdash")
axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.8)

axis(1, at =  c(10, 20, 30), c("1990","2000","2010"), tick = T, tck=-0.02,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.8)
abline(v = c(5, 10, 15, 20, 25, 30, 35), lty = "dotted", lwd = 0.8, col = "grey30")
abline(h = hori_lines, lty = "dotted", lwd = 0.8, col = "grey30")
mtext("Temperature °C", side = 2, line = 1.6, padj = 1, cex = 0.8)
legend("topleft", " ", box.col = "white", bg = "white", adj = 0.2)
mtext("DJF", side = 3, line = -0.6, padj = 1, adj = 0.02, cex = 0.8)
box()


#MAM
plot(mam_low, type = "n", ylim=c(min_na(c(mam_low, mam_mid, mam_hig))-1.5,
                                 max_na(c(mam_low, mam_mid, mam_hig))+2.39),
     axes = F, ylab = "", xlab = "")
lines(mam_hig, col=my_blu, lwd = 2, type = "l")
lines(mam_mid, col=my_bla, lwd = 2, type = "l")
lines(mam_low, col=my_red, lwd = 2, type = "l")
points(mam_hig, col="blue3", lwd = 2, pch = 16, cex = 0.7)
points(mam_mid, col="black", lwd = 2, pch = 16, cex = 0.7)
points(mam_low, col="red3",  lwd = 2, pch = 16, cex = 0.7)
abline(mam_low_sl, col = "red3", lwd = 1, lty = "longdash")
abline(mam_mid_sl, col = "black", lwd = 1, lty = "longdash")
abline(mam_hig_sl, col = "blue3", lwd = 1, lty = "longdash")
axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)

axis(1, at =  c(10, 20, 30), c("1990","2000","2010"), tick = T, tck=-0.02,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
abline(v = c(5, 10, 15, 20, 25, 30, 35), lty = "dotted", lwd = 0.8, col = "grey30")
abline(h = hori_lines, lty = "dotted", lwd = 0.8, col = "grey30")
mtext("Temperature °C", side = 2, line = 1.6, padj = 1, cex = 0.8)
legend("topleft", "   ", box.col = "white", bg = "white", adj = 0.2)
mtext("MAM", side = 3, line = -0.6, padj = 1, adj = 0.02, cex = 0.8)
box()

#JJA
plot(jja_low, type = "n", ylim=c(min_na(c(jja_low, jja_mid, jja_hig))-0.9,
                                 max_na(c(jja_low, jja_mid, jja_hig))+0.9),
     axes = F, ylab = "", xlab = "")
lines(jja_hig, col=my_blu, lwd = 2, type = "l")
lines(jja_mid, col=my_bla, lwd = 2, type = "l")
lines(jja_low, col=my_red, lwd = 2, type = "l")
points(jja_hig, col="blue3", lwd = 2, pch = 16, cex = 0.7)
points(jja_mid, col="black", lwd = 2, pch = 16, cex = 0.7)
points(jja_low, col="red3",  lwd = 2, pch = 16, cex = 0.7)
abline(jja_low_sl, col = "red3", lwd = 1, lty = "longdash")
abline(jja_mid_sl, col = "black", lwd = 1, lty = "longdash")
abline(jja_hig_sl, col = "blue3", lwd = 1, lty = "longdash")
axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)

axis(1, at =  c(10, 20, 30), c("1990","2000","2010"), tick = T, tck=-0.02,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
abline(v = c(5, 10, 15, 20, 25, 30, 35), lty = "dotted", lwd = 0.8, col = "grey30")
abline(h = hori_lines, lty = "dotted", lwd = 0.8, col = "grey30")
mtext("Temperature °C", side = 2, line = 1.6, padj = 1, cex = 0.8)
legend("topleft", " ", box.col = "white", bg = "white", adj = 0.2)
mtext("JJA", side = 3, line = -0.6, padj = 1, adj = 0.02, cex = 0.8)
box()

#SON
plot(son_low, type = "n", ylim=c(min_na(c(son_low, son_mid, son_hig))-1.0,
                                 max_na(c(son_low, son_mid, son_hig))+1.2),
     axes = F, ylab = "", xlab = "")
lines(son_hig, col=my_blu, lwd = 2, type = "l")
lines(son_mid, col=my_bla, lwd = 2, type = "l")
lines(son_low, col=my_red, lwd = 2, type = "l")
points(son_hig, col="blue3", lwd = 2, pch = 16, cex = 0.7)
points(son_mid, col="black", lwd = 2, pch = 16, cex = 0.7)
points(son_low, col="red3",  lwd = 2, pch = 16, cex = 0.7)
abline(son_low_sl, col = "red3", lwd = 1, lty = "longdash")
abline(son_mid_sl, col = "black", lwd = 1, lty = "longdash")
abline(son_hig_sl, col = "blue3", lwd = 1, lty = "longdash")
axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)

axis(1, at =  c(10, 20, 30), c("1990","2000","2010"), tick = T, tck=-0.02,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
abline(v = c(5, 10, 15, 20, 25, 30, 35), lty = "dotted", lwd = 0.8, col = "grey30")
abline(h = hori_lines, lty = "dotted", lwd = 0.8, col = "grey30")
mtext("Temperature °C", side = 2, line = 1.6, padj = 1, cex = 0.8)
legend("topleft", " ", box.col = "white", bg = "white", adj = 0.2)
mtext("SON", side = 3, line = -0.6, padj = 1, adj = 0.02, cex = 0.8)
box()

dev.off()







#seas_vals_region####


pdf(paste0("u:/RhineFlow/Elevation/seas_vals_reg.pdf"), width = 6.7, height = 4)

par(oma = c(0,0,0,0))
par(family = "serif")
par(mfrow = c(2,2))
par(mar = c(1, 2, 1, 0.6))
hori_lines <- c(-10, -5, 0, 5, 10, 15, 20, 25)
#col2rgb("blue3")
my_blu <- rgb(0, 0, 205, max=255, alpha = 160)
#col2rgb("red3")
my_red <- rgb(205, 0, 0, max=255, alpha = 160)
#col2rgb("black")
my_bla <- rgb(0, 0, 0,   max=255, alpha = 160)

#DJF
plot(djf_jur, type = "n", ylim=c(min_na(c(djf_jur, djf_pla, djf_alp))-1.2,
                                 max_na(c(djf_jur, djf_pla, djf_alp))+1.2),
     axes = F, ylab = "", xlab = "")
lines(djf_sal, col="orange2", lwd = 2, type = "l")
lines(djf_alp, col=my_blu, lwd = 2, type = "l")
lines(djf_pla, col=my_bla, lwd = 2, type = "l")
lines(djf_jur, col=my_red, lwd = 2, type = "l")
points(djf_sal, col="orange2", lwd = 2, pch = 16, cex = 0.7)
points(djf_alp, col="blue3", lwd = 2, pch = 16, cex = 0.7)
points(djf_pla, col="black", lwd = 2, pch = 16, cex = 0.7)
points(djf_jur, col="red3",  lwd = 2, pch = 16, cex = 0.7)
abline(djf_jur_sl, col = "red3", lwd = 1, lty = "longdash")
abline(djf_pla_sl, col = "black", lwd = 1, lty = "longdash")
abline(djf_alp_sl, col = "blue3", lwd = 1, lty = "longdash")
abline(djf_sal_sl, col = "orange2", lwd = 1, lty = "longdash")
axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)

axis(1, at =  c(10, 20, 30), c("1990","2000","2010"), tick = T, tck=-0.02,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
abline(v = c(5, 10, 15, 20, 25, 30, 35), lty = "dotted", lwd = 0.8, col = "grey30")
#abline(h = hori_lines, lty = "dotted", lwd = 0.8, col = "grey30")
mtext("Temperature °C", side = 2, line = 1.6, padj = 1, cex = 0.8)
legend("topleft", " ", box.col = "white", bg = "white", adj = 0.2)
mtext("DJF", side = 3, line = -0.6, padj = 1, adj = 0.02, cex = 0.8)
box()


#MAM
plot(mam_jur, type = "n", ylim=c(min_na(c(mam_jur, mam_pla, mam_alp, mam_sal))-1.2,
                                 max_na(c(mam_jur, mam_pla, mam_alp, mam_sal))+1.2),
     axes = F, ylab = "", xlab = "")
lines(mam_sal, col="orange2", lwd = 2, type = "l")
lines(mam_alp, col=my_blu, lwd = 2, type = "l")
lines(mam_pla, col=my_bla, lwd = 2, type = "l")
lines(mam_jur, col=my_red, lwd = 2, type = "l")
points(mam_sal, col="orange2", lwd = 2, pch = 16, cex = 0.7)
points(mam_alp, col="blue3", lwd = 2, pch = 16, cex = 0.7)
points(mam_pla, col="black", lwd = 2, pch = 16, cex = 0.7)
points(mam_jur, col="red3",  lwd = 2, pch = 16, cex = 0.7)
abline(mam_jur_sl, col = "red3", lwd = 1, lty = "longdash")
abline(mam_pla_sl, col = "black", lwd = 1, lty = "longdash")
abline(mam_alp_sl, col = "blue3", lwd = 1, lty = "longdash")
abline(mam_sal_sl, col = "orange2", lwd = 1, lty = "longdash")
axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)

axis(1, at =  c(10, 20, 30), c("1990","2000","2010"), tick = T, tck=-0.02,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
abline(v = c(5, 10, 15, 20, 25, 30, 35), lty = "dotted", lwd = 0.8, col = "grey30")
#abline(h = hori_lines, lty = "dotted", lwd = 0.8, col = "grey30")
mtext("Temperature °C", side = 2, line = 1.6, padj = 1, cex = 0.8)
legend("topleft", " ", box.col = "white", bg = "white", adj = 0.2)
mtext("MAM", side = 3, line = -0.6, padj = 1, adj = 0.02, cex = 0.8)
box()

#JJA
plot(jja_jur, type = "n", ylim=c(min_na(c(jja_jur, jja_pla, jja_alp, jja_sal))-1.2,
                                 max_na(c(jja_jur, jja_pla, jja_alp, jja_sal))+1.2),
     axes = F, ylab = "", xlab = "")
lines(jja_sal, col="orange2", lwd = 2, type = "l")
lines(jja_alp, col=my_blu, lwd = 2, type = "l")
lines(jja_pla, col=my_bla, lwd = 2, type = "l")
lines(jja_jur, col=my_red, lwd = 2, type = "l")
points(jja_sal, col="orange2", lwd = 2, pch = 16, cex = 0.7)
points(jja_alp, col="blue3", lwd = 2, pch = 16, cex = 0.7)
points(jja_pla, col="black", lwd = 2, pch = 16, cex = 0.7)
points(jja_jur, col="red3",  lwd = 2, pch = 16, cex = 0.7)
abline(jja_jur_sl, col = "red3", lwd = 1, lty = "longdash")
abline(jja_pla_sl, col = "black", lwd = 1, lty = "longdash")
abline(jja_alp_sl, col = "blue3", lwd = 1, lty = "longdash")
abline(jja_sal_sl, col = "orange2", lwd = 1, lty = "longdash")
axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)

axis(1, at =  c(10, 20, 30), c("1990","2000","2010"), tick = T, tck=-0.02,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
abline(v = c(5, 10, 15, 20, 25, 30, 35), lty = "dotted", lwd = 0.8, col = "grey30")
#abline(h = hori_lines, lty = "dotted", lwd = 0.8, col = "grey30")
mtext("Temperature °C", side = 2, line = 1.6, padj = 1, cex = 0.8)
legend("topleft", " ", box.col = "white", bg = "white", adj = 0.2)
mtext("JJA", side = 3, line = -0.6, padj = 1, adj = 0.02, cex = 0.8)
box()

#SON
plot(son_jur, type = "n", ylim=c(min_na(c(son_jur, son_pla, son_alp, son_sal))-1.2,
                                 max_na(c(son_jur, son_pla, son_alp, son_sal))+1.2),
     axes = F, ylab = "", xlab = "")
lines(son_sal, col="orange2", lwd = 2, type = "l")
lines(son_alp, col=my_blu, lwd = 2, type = "l")
lines(son_pla, col=my_bla, lwd = 2, type = "l")
lines(son_jur, col=my_red, lwd = 2, type = "l")
points(son_sal, col="orange2", lwd = 2, pch = 16, cex = 0.7)
points(son_alp, col="blue3", lwd = 2, pch = 16, cex = 0.7)
points(son_pla, col="black", lwd = 2, pch = 16, cex = 0.7)
points(son_jur, col="red3",  lwd = 2, pch = 16, cex = 0.7)
abline(son_jur_sl, col = "red3", lwd = 1, lty = "longdash")
abline(son_pla_sl, col = "black", lwd = 1, lty = "longdash")
abline(son_alp_sl, col = "blue3", lwd = 1, lty = "longdash")
abline(son_sal_sl, col = "orange2", lwd = 1, lty = "longdash")
axis(2, mgp = c(3, 0.2, 0), tck=-0.02, cex.axis = 0.7)

axis(1, at =  c(10, 20, 30), c("1990","2000","2010"), tick = T, tck=-0.02,
     col = "black", col.axis = "black", mgp = c(3, 0.0, 0), cex.axis = 0.7)
abline(v = c(5, 10, 15, 20, 25, 30, 35), lty = "dotted", lwd = 0.8, col = "grey30")
#abline(h = hori_lines, lty = "dotted", lwd = 0.8, col = "grey30")
mtext("Temperature °C", side = 2, line = 1.6, padj = 1, cex = 0.8)
legend("topleft", " ", box.col = "white", bg = "white", adj = 0.2)
mtext("SON", side = 3, line = -0.6, padj = 1, adj = 0.02, cex = 0.8)
box()

dev.off()






#graph_abst----

tiff(paste0("u:/RhineFlow/Elevation/graph_abs.tiff"), width = 2.36*2, height = 1.97*2,
     units = "in", res = 800)
par(oma=c(0,0,0,0))
par(family="serif")

par(mfrow = c(2,1))

plot_cycl_elev(data_in = tem0_sl, data_mk = tem0_mk, data_in_me = tem0_sl_an,
               data_meta = stat_meta, main_text = "",
               margins_1 = c(1.4,1.8,1.8,3.5), margins_2 = c(1.4,1.8,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = T,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
mtext("Temperature [°C/dec]", side = 3, line = .3, cex = 1.2)

dev.off()