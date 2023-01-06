
library(reshape2)
library(ggplot2)
library(gridExtra)


setwd("~/QNM_WG/Comparison MS")
dat<-read.csv("core_species.csv", sep=",")

head(dat)
dat<- subset(dat, typenode=="core")

dat_long <- melt(dat,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("system", "scen","node"),
                  # The source columns
                  measure.vars=c("catbbn", "catfcm", "catqnm" ),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="model",
                  value.name="response"
)


dat_sign <- melt(dat,
                 # ID variables - all the variables to keep but not split apart on
                 id.vars=c("system", "scen","node"),
                 # The source columns
                 measure.vars=c("bbn", "fcm", "qnm" ),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="model",
                 value.name="response"
)

dat_sign$sign<- "positive"
dat_sign$sign[dat_sign$response< 0] <- "negative"
dat_sign$sign[dat_sign$response==0] <- "zero"

dat_long$sign<-dat_sign$sign

# Order the factor levels QNM, BBN, FCM 
# Order the factor levels weak, moderate, strong 

dat_long$response<-factor(dat_long$response, levels=c("weak", "moderate", "strong"))

dat_long$model<-factor(dat_long$model, levels=c("catqnm",  "catfcm", "catbbn"))
dat_long$system<-factor(dat_long$system, levels=c("PI", "GB", "BAR"))
dat_long$scen<-factor(dat_long$scen, levels=c("t", "w", "tw"))


# Fill symbols if strong or moderate 
dat_long$fill<- "weak"
dat_long$fill[dat_long$response!="weak" & dat_long$sign=="negative"]<- "negstrong"
dat_long$fill[dat_long$response!="weak" & dat_long$sign=="positive"]<- "posstrong"

  
  ########################
# Plot up responses as bubble plots 
dat_pi<- subset(dat_long, system=="PI")
dat_pi$node<-factor(dat_pi$node, levels=c( "HalA","PC","RKCA","BKCA"))

p1<- ggplot(dat_pi)+
  geom_point(aes(y=node, x=model,cex=response, 
                 pch=sign, color=sign, fill=fill)) + 
  scale_color_manual( values=c("lightblue","orangered","grey"))+
  scale_shape_manual(values=c(25,24,22)) + 
  scale_fill_manual(values=c("lightblue","orangered","white")) + 
  facet_grid(~scen) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        strip.background = element_blank()) +
  scale_y_discrete(labels=c( "BKCA" = "BKC-A", 
                             "RKCA" = "RKC-A",
                             "PC" = "Pacific cod",
                             "HalA" = "Halibut-A"))





dat_gb<-subset(dat_long, system=="GB")
dat_gb$node<-factor(dat_gb$node, levels=c( "Seafood","HabitatSeafloorDemersal","Forage Fish", "Groundfish"))

p2<- ggplot(dat_gb)+
  geom_point(aes(y=node, x=model,cex=response, 
                 pch=sign, color=sign, fill=fill)) + 
  scale_color_manual( values=c("lightblue","orangered","grey"))+
  scale_shape_manual(values=c(25,24,22)) + 
  scale_fill_manual(values=c("lightblue","orangered","white")) + 
  facet_grid(~scen) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none" ,
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
    scale_y_discrete(labels=c( "Forage Fish" = "Forage fish",
                              "HabitatSeafloorDemersal" = "HSD"))



dat_bar<-subset(dat_long, system=="BAR")
dat_bar$node<-factor(dat_bar$node, levels=c( "Wetlands","Recreation","Habitable Land", "Fish"))


p3<- ggplot(dat_bar)+
  geom_point(aes(y=node, x=model,cex=response, 
                 pch=sign, color=sign, fill=fill)) + 
  scale_color_manual( values=c("lightblue","orangered","grey"))+
  scale_shape_manual(values=c(25,24,22)) + 
  scale_fill_manual(values=c("lightblue","orangered","white")) + 
  facet_grid(~scen) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  scale_x_discrete(labels=c("catqnm" = "QNM", "catbbn" = "BBN",
                            "catfcm" = "FCM")) + 
  scale_y_discrete(labels=c( "Habitable Land" = "HL"))


grid.arrange(p1, p2, p3, ncol=1)

library(gtable)
library(grid)

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g <- rbind(g1,g2, g3, size = "first")

g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)
grid.newpage()
grid.draw(g)

#ggsave(filename="core_nodes_REV.pdf", width = 15, height = 15, units = "cm", dpi=300)
#ggsave not working for some reason 
grid.newpage()

pdf("core_nodes_REV.pdf", width = 4, height = 5)
grid.draw(g)
dev.off()



# To edit the image further: 
# 1. Export saved pdf into adobe acrobat
# 2. Print and so now we have larger margins
# 3. Export to power point. Add in additional labels, legends that you want. 
# 4. Save as pdf. 


