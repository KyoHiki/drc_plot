The R code below is used to reproduce Figure 5 in Hiki et al. (2021, ET&C; DOI: 10.1002/etc.5199).
------------------------------------------------
  
    </br>
    </br>
```{r setup, include=FALSE,message=FALSE,error=FALSE}
 knitr::opts_chunk$set(echo = TRUE, message=FALSE,warning=FALSE, fig.path = "figure/") #echo=TRUE
```

```{r read.library, warning=FALSE}
require(ggplot2)
require(ggsci)
require(ggsignif)
require(ggprism)
require(cowplot)
require(ggpubr)
require(ggExtra)
require(gridExtra)
require(grid)
require(tidyverse)
require(EnvStats)
require(ggpubr)
require(lemon)
require(drc)
require(plotrix)
require(scales)
```



```{r read.bap, warning=FALSE}
### read data: BaP
data_surv_norm <- read.csv("data_surv_normalized_BaP.csv", )


## fitting
model_surv <- drm(Dead/Total~Csed_norm, data=data_surv_norm, type="binomial", weights=Total, fct = LL.2() )



## preparation for figure: regression curve
# ggplotで描画するために回帰曲線を引き直す。drmで求めた回帰パラメータを使用。
Conc <- expand.grid(Conc=exp(seq(log(10^2.5), log(10^4.5), length=1000))) 

Pred_surv <- predict(model_surv, newdata=Conc, interval="confidence") %>%
  cbind(Conc) 
Mean_surv <- model_surv$data %>% dplyr::select(Csed_norm,"Dead/Total") %>%
  rename(Mortality="Dead/Total") 


#### Plot: survival BaP ####
Figure_BaP <- ggplot(Pred_surv) +
  geom_ribbon(data=Pred_surv, aes(x=Conc, y=Prediction, ymin=Lower, ymax=Upper), fill="#009E73",alpha=0.2) +
  geom_line(data=Pred_surv, aes(x=Conc, y=Prediction)) +
  stat_summary(data=Mean_surv,aes(x=Csed_norm,y=Mortality),fun = mean,
               geom = "point",pch=17,col="#009E73",size=5, alpha=0.7)+
  stat_summary(data=Mean_surv,aes(x=Csed_norm,y=Mortality),fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "errorbar",size=0.8,width = 0.07, alpha=0.7)+
  labs(x=expression(bold(paste(italic(C)[sed]," (mg/kg-oc)"))),
       y="Mortality (%)")+
  scale_y_continuous(breaks=seq(0,1,0.2), labels=c("0","20","40","60","80","100")) +
  scale_x_continuous(trans=log10_trans(),breaks=10^(2:4.5), labels=trans_format("log10",math_format(10^.x))) +
  annotation_logticks( size=1.5,side="b",outside=TRUE )+
  coord_cartesian(clip = "off")+
  theme_prism(base_size=20, border=TRUE) +
  theme(legend.position="none",
        plot.title= element_text(hjust=0) )+
  annotate("text",label="*",x=7600,y=0.37,size=7)+
  annotate("text",label="**",x=15500,y=0.71,size=7)+
  ggtitle("BaP")

Figure_BaP

tiff("Figure_BaP.tiff", units="in", width=6, height=6, res=300)
Figure_BaP
dev.off()

```





