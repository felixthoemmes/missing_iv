library(tidyverse)
library(ggdist)
library(tikzDevice)


#### Simulation 1 figures ####
rm(list = ls())
load("simulation1_data.RData")

##### Figure 5 #####
simulation1_data$mnar <- cut(abs(simulation1_data$beta_y),c(0,0.2,0.5,1),include.lowest = TRUE, labels = c("Weak","Medium","Strong"))
simulation1_data$iv <- cut(abs(simulation1_data$beta_z),c(0,0.2,0.5,1),include.lowest = TRUE, labels = c("Weak","Medium","Strong"))

dat_rmse_ridges <- simulation1_data %>% select(mnar,iv,iv_rmse) %>%
  rbind(data.frame(mnar=simulation1_data$mnar,iv="Listwise",iv_rmse=simulation1_data$listwise_rmse))


tikz(file = "plot_sim1_ridges.tex", width = 6, height = 4)

g1 <-  ggplot(dat_rmse_ridges,aes(x = iv_rmse, y = iv)) +
  stat_interval() +
  facet_grid(rows = vars(mnar)) +
  xlab("RMSE (log$_{10}$ Scale)") + ylab("IV strength") +
  theme_ggdist()+ scale_x_log10( breaks = c(0.01,0.1,1,10),
                                 label = c("0.01","0.1","1","10"))+
  theme(strip.text.y = element_text(angle = 0,hjust = 0),
        strip.background.y = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        plot.margin = unit(c(1,2.5,1,0.5),"cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        legend.position = c(.74,0.9),
        legend.direction = "horizontal")+
  scale_color_grey("Interval")+
  coord_cartesian(xlim = c(0.05, 10), clip = "off")+  
  geom_text(data = data.frame(x = 50,y = "Strong",
                              label = c(" ","MNAR Strength"," "),
                              mnar = unique(dat_rmse_ridges$mnar)),
            aes(x = x , y = y, label = label), angle = 90)+
  guides(color = guide_legend(title.position="top",title.hjust = 0,
                              title.theme = element_text(size = 9.5)))


print(g1)
dev.off()

##### Figure 6 #####
## line plots for 40% missing and b_y=0.4

temp_int <- simulation1_data %>%
  filter((missing_pct==0.4)&(beta_y==0.4)) %>%
  select(beta_z,listwise_rmse,aux_rmse,iv_rmse)

#interpolate
n_int <- 1000
interp_dat <- data.frame(1:n_int)
for(i in names(temp_int)){
  interp_dat[i] <- approx(temp_int[,1],temp_int[,i],n = n_int)$y
}

#smoothing
interp_dat <- interp_dat %>%
  mutate(listwise_rmse=predict(loess(listwise_rmse~beta_z)),
         aux_rmse=predict(loess(aux_rmse~beta_z)))



tikz(file = "plot_sim1_lines.tex", width = 6, height = 4)
g2 <- interp_dat %>%
  select(beta_z,listwise_rmse,aux_rmse,iv_rmse) %>%
  melt(id="beta_z") %>% 
  ggplot()+
  geom_line(aes(x = beta_z,y = value,lty=variable),lwd=.9)+
  theme_ggdist()+
  scale_color_grey(start = 0.8,end = 0.2)+
  ylab("RMSE")+ ylim(0,1) +
  xlab("IV Strength") +
  scale_linetype_manual("Method",values=c("listwise_rmse"="solid",
                                          "aux_rmse"="dashed",
                                          "iv_rmse"="dotted"),
                        labels=c("Listwise","Auxiliary","IV"))+
  guides(linetype=guide_legend(keywidth = 2,title.position = "top",
                               title.theme = element_text(size = 9.5)))+
  theme(plot.margin = unit(c(1,2.5,1,0.5),"cm"),
        legend.position = c(.9,.8))+
  scale_x_continuous(breaks = c(-0.6,-0.3,0,0.3,0.6))



print(g2)
dev.off()



#### Simulation 2 figures ####
rm(list = ls())
load("simulation2_data.RData")
##### Figure 7 #####
#interpolate
n_int <- 1000

plot_grid <- c()
for(i in c(0,0.3,0.6)){
  param_grid_filtered <- 
    simulation2_data %>%
    filter(beta_v2 >= 0) %>% 
    filter(beta_v1==i) 
  temp <- data.frame(beta_v1=rep(i,each= n_int),
                     beta_v2=approx(param_grid_filtered$beta_v2,param_grid_filtered$iv,n = n_int)$x,
                     iv=approx(param_grid_filtered$beta_v2,param_grid_filtered$iv,n = n_int)$y,
                     sargan=approx(param_grid_filtered$beta_v2,param_grid_filtered$sargan,n = n_int)$y)
  
  plot_grid <- rbind(plot_grid,temp) 
}



tikz(file = "plot_sim2.tex", width = 6, height = 4)

g3 <- ggplot(plot_grid)+
  geom_line(aes(beta_v2,iv,group=beta_v1,col=(sargan*100)),lwd=4)+
  geom_line(aes(beta_v2,iv,lty=as.factor(beta_v1)),
            lwd=4.2,col="white")+
  scale_linetype_manual("IV$_1$ Violation Strength",
                        values=c("0"="blank","0.3"="28","0.6"="24"))+
  scale_color_gradient("\\% Sig. Sargan Tests",low = "black",high="grey",limits=c(0,100))+
  guides(linetype=guide_legend(order=2,
                               keywidth = 1.8,title.position = "top",
                               title.hjust = 0.5,
                               title.theme = element_text(size = 9.5),
                               override.aes = list(color = "black",lwd=4,
                                                   linetype=c("solid","82","42"))),
         color= guide_colorbar(order=1,
                               title.hjust = 0.5,
                               title.position = "top",
                               title.theme = element_text(size = 9.5)))+
  theme_ggdist()+
  theme(legend.direction = "horizontal",
        plot.margin = unit(c(1,2.5,1,0.5),"cm"),
        legend.position = c(.8,0.25),
        legend.box.just = "right")+
  xlab("IV$_2$ Violation Strength")+
  ylab("Bias of IV Estimates")


print(g3)
dev.off()


#### Simulation 3 figures ####


rm(list = ls())
load("simulation3_data.RData")

##### Figure 8 ####

tikz(file = "plot_sim3.tex", width = 6, height = 4)


g4<- 
  simulation3_data %>%
  mutate(strength=as.numeric((beta_z1Rx==0.6)+(beta_z1Ry==0.6)+(beta_z2Rx==0.6)+(beta_z2Ry==0.6))) %>%
  ggplot()+
  geom_point(aes(x = abs(listwise),y = (iv_rmse-listwise_rmse), col=strength),cex=1.7)+
  geom_smooth(aes(x = abs(listwise),y = (iv_rmse-listwise_rmse)),
              method = "loess",se = FALSE,col="black",lwd=1.5)+
  theme_ggdist()+
  scale_colour_gradient(low = "#CCCCCC", high = "#333333")+
  geom_hline(yintercept = 0,col="black",lty="dashed",lwd=1.5)+
  labs(x="Magnitude of bias",y="RMSE Difference",col="IVs Strength")+
  annotate(geom = "text", x = 0.135, y = 0.007, label = "Listwise performs better",size=3.4)+
  annotate(geom = "text", x = 0.14, y = -0.007, label = "IV performs better",size=3.4)+
  theme(legend.direction = "horizontal",
        plot.margin = unit(c(1,2.5,1,0.5),"cm"),
        legend.position = c(.8,0.85),
        legend.box.just = "right")+
  guides(color= guide_colorbar(title.hjust = 0,label = FALSE,
                               title.position = "top",
                               title.theme = element_text(size = 9.5)))


print(g4)
dev.off()



