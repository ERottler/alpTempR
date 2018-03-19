###

#Data visualization

###
library("RColorBrewer")

stat_meta <- read.table(paste0(base_dir,"rawData/IDAweb/stationMeta.csv"), sep=",", header=T)

#cycl_elev_mea----

#pdf(paste0("u:/RhineFlow/Elevation/cycl_elev_mea.pdf"), width = 6.7, height = 8)
png(paste0("u:/RhineFlow/Elevation/cycl_elev_mea.png"), width = 6.7, height = 8,
    units = "in", res = 100)

par(oma=c(0,0,0,0))
par(family="serif")

layout(matrix(c(1,3,5,7,9,11,13,2,4,6,8,10,12,14), 7, 2), widths=c(1, 1), heights=c(1,1,1,1,1,1,1))
#layout.show(m)

plot_cycl_elev(data_in = tem0_me, data_mk = tem0_mk, data_in_me = tem0_me_an,
               data_meta = stat_meta, main_text = "a) Temperature [°C] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = T,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = radi_me, data_mk = radi_mk, data_in_me = radi_me_an,
               data_meta = stat_meta, main_text = "b) Radiation [W/m²] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = suns_me, data_mk = suns_mk, data_in_me = suns_me_an,
               data_meta = stat_meta, main_text = "c) Sunshine duration [min] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = clou_me, data_mk = clou_mk, data_in_me = clou_me_an,
               data_meta = stat_meta, main_text = "d) Clouds [%] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = ahum_me, data_mk = ahum_mk, data_in_me = ahum_me_an,
               data_meta = stat_meta, main_text = "e) Humidity [g/cm³] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = snow_me, data_mk = radi_mk, data_in_me = snow_me_an,
               data_meta = stat_meta, main_text = "f) Snow depth [cm] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = F, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = airp_me, data_mk = airp_mk, data_in_me = airp_me_an,
               data_meta = stat_meta, main_text = "g) Air pressure [hPa] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = T, show_mk = F, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
dev.off()




#cycl_elev_slo----

#pdf(paste0("u:/RhineFlow/Elevation/cycl_elev_slo.pdf"), width = 6.7, height = 8)
png(paste0("u:/RhineFlow/Elevation/cycl_elev_slo.png"), width = 6.7, height = 8,
    units = "in", res = 100)

par(oma=c(0,0,0,0))
par(family="serif")

layout(matrix(c(1,3,5,7,9,11,13,2,4,6,8,10,12,14), 7, 2), widths=c(1, 1), heights=c(1,1,1,1,1,1,1))
#layout.show(m)

plot_cycl_elev(data_in = tem0_sl, data_mk = tem0_mk, data_in_me = tem0_sl_an,
               data_meta = stat_meta, main_text = "a) Temperature [°C/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = T,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = radi_sl, data_mk = radi_mk, data_in_me = radi_sl_an,
               data_meta = stat_meta, main_text = "b) Radiation [(W/m²)/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = suns_sl, data_mk = suns_mk, data_in_me = suns_sl_an,
               data_meta = stat_meta, main_text = "c) Sunshine duration [min/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = clou_sl, data_mk = clou_mk, data_in_me = clou_sl_an,
               data_meta = stat_meta, main_text = "d) Clouds [%/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = ahum_sr, data_mk = ahum_mk, data_in_me = ahum_sr_an,
               data_meta = stat_meta, main_text = "e) Humidity [(g/cm³)/dec/(g/cm³)] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = snow_sl, data_mk = snow_mk, data_in_me = snow_sl_an,
               data_meta = stat_meta, main_text = "f) Snow window likelihood [%/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)

plot_cycl_elev(data_in = airp_sl, data_mk = airp_mk, data_in_me = airp_sl_an,
               data_meta = stat_meta, main_text = "g) Air pressure [hPa/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = T, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T)
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


#pdf("u:/RhineFlow/Elevation/imag_elev_slo.pdf", width = 6.7, height = 7)
png(paste0("u:/RhineFlow/Elevation/imag_elev_slo.png"), width = 6.7, height = 7,
    units = "in", res = 100)

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
                 main="b) Tmax-Tmin [°C/dec]",
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
                 main = "d) Clouds [%/dec]",
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
                 main = "g) Sunshine [min/dec]",
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







