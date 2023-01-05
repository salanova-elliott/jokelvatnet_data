library(tidyverse)
library (ggpubr)
library(viridis)

EG17_env <- read.csv ("/home/lukedane/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/figures/tyler_traits/avgweight_overall.csv", sep = ",")
weight_no37 <- EG17_env[-c(37), ]

font_size=20

return_lm <- function(input_data, indvar, depvar) {
  depvar_lm <- lm(indvar~depvar, data = input_data)
  return(depvar_lm)
}

output_lmplot <- function(input_data, fname) {
  png(fname, width=10, height=10, units="in", res=300)
  layout(matrix(1:4, ncol = 2))
  plot(input_data)
  layout(1)
  dev.off()
}


output_lmplot(return_lm(EG17_env, EG17_env$Ti_glacial_acticity, EG17_env$avgweight_soildisturb), "lm_soildisturb.png")
output_lmplot(return_lm(weight_no37, EG17_env$Ti_glacial_acticity, weight_no37$avgweight_soildisturb), "lm_soildisturb_no37.png")

output_lmplot(return_lm(EG17_env, EG17_env$chelsa_meanwarmest, EG17_env$inv_avgweight_tempopt), "lm_meanwarmestchelsa.png")
summary(return_lm(EG17_env, EG17_env$chelsa_meanwarmest, EG17_env$inv_avgweight_tempopt))

output_lmplot(return_lm(weight_no37, weight_no37$chelsa_annualprecip, weight_no37$avgweight_moisture), "lm_moisturechelsa.png")
summary(return_lm(weight_no37, weight_no37$chelsa_annualprecip, weight_no37$avgweight_moisture))

create_plot <- function(input_data, xvar, yvar, xlab, ylab) {
  result_plot <- ggplot (input_data, aes (xvar, yvar, label=samplesize_tempopt))+
    geom_point(aes(col=age), show.legend = FALSE, size = 6, alpha=0.7)+
    scale_color_viridis()+
    geom_text()+
    annotate("text", label=expression(~R^2"=0.391\np=0.00001"), x=5500, y=13.2)+
    #geom_smooth(method = "lm", formula = y ~ x, alpha=0.7)+
    geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
    geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
    labs (x=xlab, y=ylab)
  return(result_plot)
}

#warmestmean_tempopt <- create_plot(weight_no37, avgweight_tempopt, )

### FIGURE 4 ####
# Affect of temperature on glacial activity
weight_tempopt <- EG17_env[-c(32,33,37), ]
glac_temp <- ggplot (weight_tempopt, aes (Ti_glacial_acticity, inv_avgweight_tempopt, size=samplesize_tempopt))+
  ggtitle("B. Temperature optimum")+
  geom_point(aes(col=age,stroke=0.5), show.legend = FALSE, alpha=0.7)+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),text = element_text(size=font_size),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(family = "mono"),
        legend.position = "none") +
  scale_color_viridis()+
  annotate("text", label="R^2=0.491\np < 0.001", x=10000, y=7.5, size=6, family="mono")+
  #geom_smooth(method = "lm", formula = y ~ x, alpha=0.7)+
  geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
  geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
  labs (y="Cold-adapted plants         Warm-adapted plants", x="Glacial activity (Ti cps)", size= "Sample size")+ 
  scale_size(range=c(3,12), guide="none") +
  scale_x_continuous(breaks = seq(4000,10000, by=2000), labels = c("4000", "6000", "8000", "10,000"))

# Affect of glacial activity on soil
weight_nosoil <- EG17_env[-c(33,37,38), ]
glac_soil <- ggplot (weight_nosoil, aes (Ti_glacial_acticity, avgweight_soildisturb, size=samplesize_soildisturb))+
  ggtitle("A. Soil disturbance")+
  geom_point(aes(col=age), show.legend = FALSE, alpha=0.7)+
  theme(text = element_text(size=20))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),text = element_text(size=font_size),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(family = "mono"),
        legend.position = "none") +
  scale_color_viridis()+
  annotate("text", label="R^2=0.445\np < 0.001", x=5200, y=3.65, size=6, family="mono")+
  geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
  geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
  labs (y="Low soil disturbance         High soil disturbance", x="Glacial activity (Ti cps)", size= "Sample size")+ 
  scale_size(range=c(3,12), guide = "none") +
  scale_x_continuous(breaks = seq(4000,10000, by=2000), labels = c("4000", "6000", "8000", "10,000"))

# Affect of glacial activity on moisture
glac_moist <- ggplot (weight_no37, aes (Ti_glacial_acticity, avgweight_moisture, size=samplesize_moisture))+
  ggtitle("C. Moisture")+
  geom_point(aes(col=age), alpha=0.7)+
  theme(text = element_text(size=20), legend.text=element_text(size=12), legend.title=element_text(size=14))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),text = element_text(size=font_size),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(family = "mono"),
        legend.position = "none") +
  scale_color_viridis(breaks = seq(2500,10000, by=2500), labels = c("2500", "5000", "7500", "10,000"))+
  annotate("text", label="R^2=0.413\np < 0.001", x=10000, y=6, size=6, family="mono")+
  geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
  geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
  labs (y="Dry-adapted plants         Moist-adapted plants", x="Glacial activity (Ti cps)")+
  theme(legend.position = "right") +
  labs(color="Sample age (ka BP)", size= "Taxa")+ 
  scale_size(range=c(3,12), breaks = c(10,25,40)) +
  scale_x_continuous(breaks = seq(4000,10000, by=2000), labels = c("4000", "6000", "8000", "10,000"))

ggarrange(glac_soil, glac_temp, glac_moist,
          ncol = 3, nrow = 1, widths=c(4,4,6))


### FIGURE 5 ###
# Chelsa annual precip - moisture
annualprecip_moist <- ggplot (weight_no37, aes (chelsa_annualprecip, avgweight_moisture, size=samplesize_moisture))+
  ggtitle("B. Moisture trait value")+
  geom_point(aes(col=age),alpha=0.7)+
  theme(text = element_text(size=24), legend.text=element_text(size=12), legend.title=element_text(size=14))+
  scale_color_viridis(breaks = seq(2500,10000, by=2500), labels = c("2500", "5000", "7500", "10,000"))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),text = element_text(size=font_size),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(family = "mono"),
        legend.position = "none") +
  annotate("text", label="R^2=0.463\np < 0.001", x=705, y=6, size=8, family="mono")+
  geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
  geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
  labs (y="Dry-adapted plants         Moist-adapted plants", x="Annual precipitation (kg m^−2 yr^−1)")+
  scale_size(range=c(3,12), breaks = c(10,25,40)) +
  labs(color="Sample age (ka BP)", size= "Taxa")+
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(4000,10000, by=2000), labels = c("4000", "6000", "8000", "10,000"))

#annualprecip_moist
#ggplot(EG17_env, aes(age, chelsa_annualprecip)) + geom_line() + geom_point() + scale_x_reverse()

# Chelsa meanwarmest - temp
weight_tempchel <- EG17_env[-c(32,33,37), ]
maxtemp_temp <- ggplot (weight_tempchel, aes (chelsa_meanwarmest, inv_avgweight_tempopt, size=samplesize_tempopt))+
  ggtitle("A. Temperature optimum trait value")+
  geom_point(aes(col=age), show.legend = FALSE, alpha=0.7)+
  theme(text = element_text(size=24))+
  scale_color_viridis()+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),text = element_text(size=font_size),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(family = "mono"),
        legend.position = "none") +
  annotate("text", label="R^2=0.476\np < 0.001", x=11, y=7.5, size=8, family="mono")+
  geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
  geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
  labs (y="Cold-adapted plants         Warm-adapted plants", x="Mean temperature of warmest quarter (C)")+
  scale_size(range=c(3,12), guide = "none") +
  scale_x_continuous(breaks = seq(4000,10000, by=2000), labels = c("4000", "6000", "8000", "10,000"))

ggarrange(maxtemp_temp, annualprecip_moist,
          ncol = 2, nrow = 1, widths = c(2,3))

ggplot(EG17_env, aes(age, chelsa_maxtemp, label=samplesize_tempopt))+ 
  geom_line()+ 
  geom_point()+ 
  geom_text()+
  scale_x_reverse()

######## NOT USED ##########
# Chelsa max precip- moisture
maxprecip_moist <- ggplot (weight_no37, aes (chelsa_maxprecip, avgweight_moisture, label=samplesize_moisture, size=samplesize_moisture))+
  ggtitle("C")+
  geom_point(aes(col=age), alpha=0.7)+
  scale_color_viridis()+
  #annotate("text", label="R^2=0.159\np = 0.0076", x=6500, y=6.02)+
  geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
  geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
  labs (y="Moisture trait value", x="chelsa maxprecip")+
  theme(legend.position = c(0.8, 0.8)) +
  labs(color="Sample age (ka BP)", size= "Sample size") + scale_size(range=c(3,12))

maxprecip_moist
ggplot(EG17_env, aes(age, chelsa_maxprecip)) + geom_line() + geom_point() + scale_x_reverse()

# Chelsa meantemp - temp
# OPPOSITE OF EXPECTED
meantemp_temp <- ggplot (weight_no37, aes (chelsa_meantemp, avgweight_tempopt, label=samplesize_tempopt))+
  geom_point(aes(col=age), size = 6, alpha=0.7, show.legend = FALSE)+
  scale_color_viridis()+
  geom_text()+
  #annotate("text", label="R^2=0.159\np = 0.0076", x=6500, y=6.02)+
  geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
  geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
  labs (y="Temperature optimum trait value", x="Annual mean temperature")+
  #theme(legend.position = c(0.8, 0.8)) +
  labs(color="Sample age (ka BP)")
meantemp_temp

# Annual precip compared to glacial activity
glac_annualprecip <- ggplot (weight_no37, aes (Ti_glacial_acticity, chelsa_annualprecip))+
  ggtitle("C")+
  geom_point(aes(col=age), size = 6, alpha=0.7, show.legend = FALSE)+
  scale_color_viridis()+
  #annotate("text", label="R^2=0.159\np = 0.0076", x=6500, y=6.02)+
  geom_line(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.4)+
  geom_ribbon(stat="smooth", method ="lm", formula = y ~ x, alpha = 0.2)+
  labs (y="chelsa annual preceip", x="glacial activity")+
  #theme(legend.position = c(0.8, 0.8)) +
  labs(color="Sample age (ka BP)")
glac_annualprecip


