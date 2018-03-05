###

#Data visualization

###

stat_meta <- read.table(paste0(base_dir,"rawData/IDAweb/stationMeta.csv"), sep=",", header=T)

#cycl_elev_mea----

pdf(paste0("u:/RhineFlow/Elevation/cycl_elev_mea.pdf"), width = 6.7, height = 8)

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

pdf(paste0("u:/RhineFlow/Elevation/cycl_elev_slo.pdf"), width = 6.7, height = 8)

par(oma=c(0,0,0,0))
par(family="serif")

layout(matrix(c(1,3,5,7,9,11,13,2,4,6,8,10,12,14), 7, 2), widths=c(1, 1), heights=c(1,1,1,1,1,1,1))
#layout.show(m)

plot_cycl_elev(data_in = tem0_sl, data_mk = tem0_mk, data_in_me = tem0_sl_an,
               data_meta = stat_meta, main_text = "a) Temperature [°C/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = T,
               smooth_val = 0.2, mk_sig_level = 0.01, add_st_num = T)

plot_cycl_elev(data_in = radi_sl, data_mk = radi_mk, data_in_me = radi_sl_an,
               data_meta = stat_meta, main_text = "b) Radiation [(W/m²)/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.01, add_st_num = T)

plot_cycl_elev(data_in = suns_sl, data_mk = suns_mk, data_in_me = suns_sl_an,
               data_meta = stat_meta, main_text = "c) Sunshine duration [min/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.01, add_st_num = T)

plot_cycl_elev(data_in = clou_sl, data_mk = clou_mk, data_in_me = clou_sl_an,
               data_meta = stat_meta, main_text = "d) Clouds [%/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.01, add_st_num = T)

plot_cycl_elev(data_in = ahum_sr, data_mk = ahum_mk, data_in_me = ahum_sr_an,
               data_meta = stat_meta, main_text = "e) Humidity [(g/cm³)/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.01, add_st_num = T)

plot_cycl_elev(data_in = snow_sl, data_mk = snow_mk, data_in_me = snow_sl_an,
               data_meta = stat_meta, main_text = "f) Snow window likelihood [%/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = F, show_mk = T, aggr_cat_mean = T, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.01, add_st_num = T)

plot_cycl_elev(data_in = airp_sl, data_mk = airp_mk, data_in_me = airp_sl_an,
               data_meta = stat_meta, main_text = "g) Air pressure [hPa/dec] ",
               margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
               no_col = T, show_mk = T, aggr_cat_mean = F, with_hom_dat = F,
               smooth_val = 0.2, mk_sig_level = 0.01, add_st_num = T)
dev.off()



