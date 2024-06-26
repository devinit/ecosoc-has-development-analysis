#### Setup ####
list.of.packages <- c("rstudioapi", "dplyr", "data.table", "ggplot2", "scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- dirname(getActiveDocumentContext()$path) 
setwd(wd)
setwd("../")

reds = c(
  "#e84439", "#f8c1b2", "#f0826d", "#bc2629", "#8f1b13", "#fce3dc", "#fbd7cb", "#f6b0a0", "#ec6250", "#dc372d", "#cd2b2a", "#a21e25", "#6b120a"
)
oranges = c(
  "#eb642b", "#f6bb9d", "#f18e5e", "#d85b31", "#973915", "#fde5d4", "#fcdbbf", "#facbad", "#f3a47c", "#ee7644", "#cb5730", "#ac4622", "#7a2e05"
)
yellows = c(
  "#f49b21", "#fccc8e", "#f9b865", "#e48a00", "#a85d00", "#feedd4", "#fee7c1", "#fedcab", "#fac47e", "#f7a838", "#df8000", "#ba6b15", "#7d4712"
)
pinks = c(
  "#c2135b", "#e4819b", "#d64278", "#ad1257", "#7e1850", "#f9cdd0", "#f6b8c1", "#f3a5b6", "#e05c86", "#d12568", "#9f1459", "#8d0e56", "#65093d"
)
purples = c(
  "#893f90", "#c189bb", "#a45ea1", "#7b3b89", "#551f65", "#ebcfe5", "#deb5d6", "#cb98c4", "#af73ae", "#994d98", "#732c85", "#632572", "#42184c"
)
blues = c(
  "#0089cc", "#88bae5", "#5da3d9", "#0071b1", "#0c457b", "#d3e0f4", "#bcd4f0", "#a3c7eb", "#77adde", "#4397d3", "#105fa3", "#00538e", "#0a3a64"
)
greens = c(
  "#109e68", "#92cba9", "#5ab88a", "#1e8259", "#16513a", "#c5e1cb", "#b1d8bb", "#a2d1b0", "#74bf93", "#3b8c61", "#00694a", "#005b3e", "#07482e"
)
greys = c(
  "#6a6569", "#a9a6aa", "#847e84", "#555053", "#443e42", "#d9d4da", "#cac5cb", "#b3b0b7", "#b9b5bb", "#5a545a", "#736e73", "#4e484c", "#302b2e"
)

di_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.minor.x = element_blank()
    ,panel.grid.major.y = element_line(colour = greys[2])
    ,panel.grid.minor.y = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,axis.line.x = element_line(colour = "black")
    ,axis.line.y = element_blank()
    ,axis.ticks = element_blank()
    ,legend.position = "bottom"
  )

rotate_x_text_45 = theme(
  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)
rotate_y_text_45 = theme(
  axis.text.y = element_text(angle = 45, vjust = 1, hjust=1)
)

#### End setup ####

dat = fread("data/merged_crs_iati.csv")
dat = subset(dat, year < 2023)
dat$humanitarian = ifelse(
  dat$sector_code %in% c(720, 730, 740),
  "Humanitarian",
  "Development"
)

ggplot(dat, aes(x=usd_disbursement_iati, y=usd_disbursement_crs, color=humanitarian)) +
  geom_abline(intercept=0, slope=1) +
  geom_point(alpha=0.5) +
  scale_color_manual(values=c(reds[1], blues[1])) + # Choose colour here
  scale_y_continuous(expand = c(0, 0), labels=dollar) + # Force y-grid to start at x-axis
  expand_limits(y=c(0, max(dat$usd_disbursement_crs*1.1))) + # Start at 0 if wanted, add 10% padding to max
  scale_x_continuous(breaks=c(2000, 2001, 2002)) + # Set manually to avoid 2000.0
  di_style +
  labs(
    y="CRS reported value (US$ millions)",
    x="IATI reported value (US$ millions)",
    color="Humanitarian"
  )

dat_recent = subset(dat, year == 2022)

dat_recipient_agg = data.table(dat_recent)[,.(
  usd_disbursement_crs=sum(usd_disbursement_crs),
  usd_disbursement_iati=sum(usd_disbursement_iati)
),
by=.(humanitarian, recipient_name)]

dat_recipient_agg$label = paste(dat_recipient_agg$recipient_name, dat_recipient_agg$humanitarian, sep=" - ")
dat_recipient_agg = dat_recipient_agg[order(dat_recipient_agg$label),]
dat_recipient_agg_l = melt(dat_recipient_agg, id.vars=c("label"), measure.vars=c("usd_disbursement_crs", "usd_disbursement_iati"))
dat_recipient_agg_l$label = factor(dat_recipient_agg_l$label, levels=unique(dat_recipient_agg$label))

ggplot(dat_recipient_agg_l, aes(x=label, y=value, group=variable, fill=variable)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=reds) + # Choose colour here
  scale_y_continuous(expand = c(0, 0), labels=dollar) + # Force y-grid to start at x-axis
  di_style +
  rotate_x_text_45 + # Or chose _90 or remove for horizontal
  labs(
    y="US$ millions",
    x="",
    fill=""
  )

dat_global_trend = data.table(dat)[,.(
  usd_disbursement_crs=sum(usd_disbursement_crs),
  usd_disbursement_iati=sum(usd_disbursement_iati)
),
by=.(year, humanitarian)]

dat_global_trend_l = melt(dat_global_trend, id.vars=c("year", "humanitarian"))
dat_global_trend_m = dcast(dat_global_trend_l, year~humanitarian+variable)
dat_global_trend_m$crs_hum_dev_ratio = 
  dat_global_trend_m$Humanitarian_usd_disbursement_crs /
  dat_global_trend_m$Development_usd_disbursement_crs

dat_global_trend_m$iati_hum_dev_ratio = 
  dat_global_trend_m$Humanitarian_usd_disbursement_iati /
  dat_global_trend_m$Development_usd_disbursement_iati

dat_global_trend_plot = melt(dat_global_trend_m, id.vars=c("year"), measure.vars=c("crs_hum_dev_ratio", "iati_hum_dev_ratio"))
dat_global_trend_plot$variable = as.character(dat_global_trend_plot$variable)
dat_global_trend_plot$variable[which(dat_global_trend_plot$variable=="crs_hum_dev_ratio")] = "CRS"
dat_global_trend_plot$variable[which(dat_global_trend_plot$variable=="iati_hum_dev_ratio")] = "IATI"

ggplot(dat_global_trend_plot, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line() +
  scale_color_manual(values=reds) + # Choose colour here
  scale_y_continuous(expand = c(0, 0), labels=percent) + # Force y-grid to start at x-axis
  expand_limits(y=c(0, max(dat_global_trend_plot$value*1.1))) +
  di_style +
  labs(
    color="",
    x="",
    y="Humanitarian to development ratio (%)"
  )


dat_country_trend = data.table(dat)[,.(
  usd_disbursement_crs=sum(usd_disbursement_crs),
  usd_disbursement_iati=sum(usd_disbursement_iati)
),
by=.(year, humanitarian, recipient_name)]

dat_country_trend_l = melt(dat_country_trend, id.vars=c("year", "humanitarian", "recipient_name"))
dat_country_trend_m = dcast(dat_country_trend_l, year+recipient_name~humanitarian+variable)
dat_country_trend_m$crs_hum_dev_ratio = 
  dat_country_trend_m$Humanitarian_usd_disbursement_crs /
  dat_country_trend_m$Development_usd_disbursement_crs

dat_country_trend_m$iati_hum_dev_ratio = 
  dat_country_trend_m$Humanitarian_usd_disbursement_iati /
  dat_country_trend_m$Development_usd_disbursement_iati

dat_country_trend_plot = melt(dat_country_trend_m, id.vars=c("year", "recipient_name"), measure.vars=c("crs_hum_dev_ratio", "iati_hum_dev_ratio"))
dat_country_trend_plot$variable = as.character(dat_country_trend_plot$variable)
dat_country_trend_plot$variable[which(dat_country_trend_plot$variable=="crs_hum_dev_ratio")] = "CRS"
dat_country_trend_plot$variable[which(dat_country_trend_plot$variable=="iati_hum_dev_ratio")] = "IATI"

ggplot(dat_country_trend_plot, aes(x=year, y=value, color=variable, group=variable)) +
  geom_line() +
  scale_color_manual(values=reds) + # Choose colour here
  scale_y_continuous(expand = c(0, 0), labels=percent) + # Force y-grid to start at x-axis
  expand_limits(y=c(0, max(dat_country_trend_plot$value*1.1))) +
  facet_wrap(~recipient_name) +
  di_style +
  labs(
    color="",
    x="",
    y="Humanitarian to development ratio (%)"
  )
