library(ggplot2)
library(gridExtra)
library(readr)
library(reshape)
library(ggh4x)
library(scales)

#Plotting variables
font_size <- 24
yax_width <- 4

#Glacial activity
ti_counts <- read.csv("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/figures/ti_glacialactivity/Ti_glacialactivity_final.csv", sep="\t")

# Adds moving average to glacial activity
ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}
ti <- ti_counts$Ti..cps.
moving_av <- ma(ti)
ti_counts$moving_av = moving_av /1000
ti_counts$Ti..cps. = ti_counts$Ti..cps. / 1000

# Trait data
weighted_data <- read_csv("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/figures/tyler_traits/avgweight_overall.csv")
weighted_data_minus37 <- weighted_data[-c(37), ]

# Import functional richness data
import_data <- read.table("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/subsets_other/func_richness.tsv", header=TRUE)
# Add depths
depths <- c(17,105,234,365,519,646,783,963,1200,1435,1600,1823,2071,2337,2585,2818,3086,
            3338,3691,4230,4527,4869,5416,5991,6453,6943,7331,7738,8213,8747,9164,9368,9522,
            9663,9811,10078,10203,10453)
import_data$age = depths
reshaped_data <- melt(import_data, id=c("age"))
# Rearrange order groups are plotted in
reshaped_data$variable <- factor(reshaped_data$variable, levels=c("Bryophyte", "Vascular_cryptogam", "Graminoids", 
                                                                  "Forb", "Dwarf_shrub", "Woody_taxa", "Saliceae"))

# Import read data
propreads_data <- read.table("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/subsets_totread/functional_group_percent.tsv", header=TRUE)
# Add depths
depths <- c(17,105,234,365,519,646,783,963,1200,1435,1600,1823,2071,2337,2585,2818,3086,
            3338,3691,4230,4527,4869,5416,5991,6453,6943,7331,7738,8213,8747,9164,9368,9522,
            9663,9811,10078,10203,10453)
propreads_data$age = depths
reshaped_read_data <- melt(propreads_data, id=c("age"))
# Rearrange order groups are plotted in
reshaped_read_data$variable <- factor(reshaped_read_data$variable, levels=c("Bryophyte", "Vascular_cryptogam", "Graminoids", 
                                                                  "Forb", "Dwarf_shrub", "Woody_taxa", "Saliceae"))

#### Plotting #####
# READ PLOT #
read_plot <- ggplot(reshaped_read_data, aes(x=age, y=value, fill=variable)) + geom_area() + 
  scale_fill_manual(values = c("grey10", "yellowgreen", "gold1", "green4", "chocolate1", "chocolate4", "firebrick4"),
                    labels = c("Bryophytes", "Vascular cryptogams", "Graminoids", 
                               "Forbs", "Dwarf shrubs", "Trees", "Saliceae")) +
  ylab("Proportion of total reads")  + coord_cartesian(xlim = c(10100, 0)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.title.x = element_blank(), text = element_text(size=font_size), legend.position = "top",
        axis.text = element_text(family = "mono"), legend.title= element_blank()) +
  ggtitle("A.") +
  scale_x_continuous(breaks = seq(10000,0, by=-1000), labels = c("10,000", "9000", "8000", "7000", "6000", "5000", "4000", "3000", "2000", "1000", "0"))+
  scale_y_continuous(labels = function(x) formatC(x, width = yax_width))+
  geom_vline(xintercept = c(4370,8480,9735), color="white", size=2)

# RICHNESS PLOT #
richness_plot <- ggplot(reshaped_data, aes(x=age, y=value, fill=variable)) + geom_area(show.legend = FALSE) + 
  scale_fill_manual(values = c("grey10", "yellowgreen", "gold1", "green4", "chocolate1", "chocolate4", "firebrick4")) +
  ylab("Taxanomic richness")  + coord_cartesian(xlim = c(10100, 0)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.title.x = element_blank(), text = element_text(size=font_size),
        axis.text = element_text(family = "mono")) +
  ggtitle("B.") + 
  annotate("text", label="Zone 1", x=10130, y=92, angle=90, size=font_size/3, family="mono")+
  annotate("text", label="Zone 2", x=9180, y=92, angle=90, size=font_size/3, family="mono")+
  annotate("text", label="Zone 3", x=6400, y=105, size=font_size/3, family="mono")+
  annotate("text", label="Zone 4", x=2100, y=105, size=font_size/3, family="mono")+
  scale_x_continuous(breaks = seq(10000,0, by=-1000), labels = c("10,000", "9000", "8000", "7000", "6000", "5000", "4000", "3000", "2000", "1000", "0"))+
  scale_y_continuous(labels = function(x) formatC(x, width = yax_width))+
  geom_vline(xintercept = c(4370,8480,9735), color="white", size=2)

# TRAIT VALUE PLOT #
cols <- c("Temperature_optimum"="#D55E00","Moisture"="#0072B2","Soil_disturbance"="#661100")
veg_traits <-ggplot(data = weighted_data_minus37, aes(x= age)) + 
  # SOIL DISTURBANCE
  geom_line(aes(y=avgweight_soildisturb, color="Soil_disturbance")) + 
  geom_point(aes(y=avgweight_soildisturb, color="Soil_disturbance")) + 
  ggh4x::stat_difference(aes(ymin = mean(avgweight_soildisturb), ymax=avgweight_soildisturb), 
                         levels = c("Above soil", "Below soil")) +
  # TEMP OPT
  geom_line(aes(y=inv_avgweight_tempopt, color="Temperature_optimum")) + 
  geom_point(aes(y=inv_avgweight_tempopt, color="Temperature_optimum")) +
  ggh4x::stat_difference(aes(ymin = mean(inv_avgweight_tempopt), ymax=inv_avgweight_tempopt),
                         levels = c("Above temp", "Below temp")) +
  # MOISTURE
  geom_line(aes(y=avgweight_moisture, color="Moisture")) + 
  geom_point(aes(y=avgweight_moisture, color="Moisture")) +
  ggh4x::stat_difference(aes(ymin = mean(avgweight_moisture), ymax=avgweight_moisture),
                         levels = c("Above moist", "Below moist")) +
  scale_fill_manual(values = alpha(c("Above soil" = "#661100", "Below soil" = "#661100", 
                                     "Above temp" = "#D55E00", "Below temp" = "#D55E00",
                                     "Above moist" = "#0072B2", "Below moist" = "#0072B2"), 0.4),
                    guide = "none") +
  scale_x_reverse() + theme_bw() + 
  scale_color_manual(values=cols) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),text = element_text(size=font_size),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(family = "mono"),
        legend.position = "top",legend.title= element_blank()) + 
  ylab("Trait value") + xlab("") + 
  #geom_vline(xintercept = c(4370,8000,9590)) + 
  coord_cartesian(xlim = c(10100, 0)) + geom_vline(xintercept = c(4370,8480,9735)) +
  ggtitle("C. Vascular plant trait values") +
  scale_y_continuous(labels = function(x) formatC(x, width = yax_width))+
  scale_x_continuous(breaks = seq(10000,0, by=-1000), labels = c("10,000", "9000", "8000", "7000", "6000", "5000", "4000", "3000", "2000", "1000", "0"))

# CHELSA PLOTS #
annualprecip_all <- read.delim("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/chelsa/annualprecip_sorted.csv") 
annualprecip_interp <- read.delim("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/chelsa/annualprecip_interp.csv")

annualprecip_all$age <- annualprecip_all$age * 1000
# Trim to timeframe we're intersted in
annualprecip_all <- annualprecip_all[which(annualprecip_all[,1]<11500),]

warmestmean_all <- read.delim("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/chelsa/warmestmean_sorted.csv") 
warmestmean_interp <- read.delim("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/chelsa/warmestmean_interp.csv")

warmestmean_all$age <- warmestmean_all$age * 1000
# Trim to timeframe we're intersted in
warmestmean_all<- warmestmean_all[which(warmestmean_all[,1]<11500),]

chelsa_precip <- ggplot(data = annualprecip_all, aes(age, value)) + 
  ggh4x::stat_difference(aes(ymin = mean(value), ymax=value)) +
  scale_fill_manual(values = alpha(c("#0072B2", "#0072B2"), 0.4)) +
  geom_line(color="#0072B2") + 
  geom_point(data=annualprecip_interp, size=3, alpha=0.6, color="#0072B2") +
  scale_x_reverse() + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),text = element_text(size=font_size),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(family = "mono"),
        legend.position = "none") + 
  ylab(expression("kg "~m^-2 ~yr^-1)) + xlab("") + 
  coord_cartesian(xlim = c(10100, 0)) + geom_vline(xintercept = c(4370,8480,9735)) +
  scale_y_continuous(labels = function(x) formatC(x, width = yax_width))+
  ggtitle("D. Annual precipitation") +
  scale_x_continuous(breaks = seq(10000,0, by=-1000), labels = c("10,000", "9000", "8000", "7000", "6000", "5000", "4000", "3000", "2000", "1000", "0"))

chelsa_maxmean <- ggplot(data = warmestmean_all, aes(age, value)) + 
  ggh4x::stat_difference(aes(ymin = mean(value), ymax=value)) +
  scale_fill_manual(values = alpha(c("#D55E00", "#D55E00"), 0.4)) +
  geom_line(color="#D55E00") + 
  geom_point(data=warmestmean_interp, size=3, alpha=0.6, color="#D55E00") +
  scale_x_reverse() + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),text = element_text(size=font_size),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(family = "mono"),
        legend.position = "none") + 
  ylab("Temperature (C)") + xlab("") + 
  coord_cartesian(xlim = c(10100, 0)) + geom_vline(xintercept = c(4370,8480,9735)) +
  scale_y_continuous(labels = function(x) formatC(x, width = yax_width))+
  ggtitle("E. Mean tempearture of warmest quarter") +
  scale_x_continuous(breaks = seq(10000,0, by=-1000), labels = c("10,000", "9000", "8000", "7000", "6000", "5000", "4000", "3000", "2000", "1000", "0"))

# GLACIAL ACTIVITY PLOT #
glacial_act <- ggplot(ti_counts, aes(x=mean, y=Ti..cps.)) + geom_line(color="grey") + 
  geom_line(data=subset(ti_counts), aes(x=mean, y=moving_av), size=1.2) +
  scale_x_reverse() + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),text = element_text(size=font_size),
        axis.text = element_text(family = "mono"))+
  #axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab("Calibrated years before present") + geom_vline(xintercept = c(4370,8480,9735)) + 
  geom_hline(yintercept = 8.75, linetype="dashed") + ylab("Ti (kcps)") +
  coord_cartesian(xlim = c(10100, 0)) + #geom_text(aes(12000,8750,label = "present day activity"), size=5, vjust=-1) + 
  ggtitle("F. Glacial activity relative to present day") + 
  scale_y_continuous(labels = function(x) formatC(x, width = yax_width))+
  scale_x_continuous(breaks = seq(10000,0, by=-1000), labels = c("10,000", "9000", "8000", "7000", "6000", "5000", "4000", "3000", "2000", "1000", "0"))

# FINAL ARRANGING #
grid.arrange(arrangeGrob(read_plot, richness_plot, nrow = 2),
             arrangeGrob(veg_traits, chelsa_precip, chelsa_maxmean, glacial_act, nrow = 4), heights=c(3,3))

# OUTPUT #
png("~/Documents/ICEAGENT/jokelvatnet/re_pipeline_finaltaxa/figures/large_figure_upd.png", width = 1290, height = 1740, bg="white",
    antialias = "subpixel")
grid.arrange(arrangeGrob(read_plot, nrow = 1),
             arrangeGrob(richness_plot, nrow = 1),
             arrangeGrob(veg_traits, nrow = 1),
             arrangeGrob(chelsa_precip, chelsa_maxmean, glacial_act, nrow = 3), heights=c(5,4,4,6))
dev.off()

