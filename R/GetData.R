
#  Packages              ###########################################################
# preparation
if (!require("unhcRstyle")) remotes::install_github('unhcr-web/unhcRstyle')
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("haven")) install.packages("haven", dependencies = TRUE)
if (!require("sjmisc")) install.packages("sjmisc", dependencies = TRUE)
if (!require("sjPlot")) install.packages("sjPlot", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("ggrepel")) install.packages("ggrepel", dependencies = TRUE)
if (!require("Hmisc")) install.packages("Hmisc", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("ggthemes")) install.packages("ggthemes", dependencies = TRUE)

library(tidyverse)
library(dplyr)
library(haven) 
library(sjlabelled)
library(sjmisc) 
library(sjPlot)
library(ggrepel)
library(Hmisc)
library(ggplot2)
library(ggthemes)

#library(labelled)
#library(surveytoolbox) # install with 
#devtools::install_github("martinctc/surveytoolbox")
 

# Stop View from overloading memory with a large datasets
# RStudioView <- View
# View <- function(x) {
#   if ("data.frame" %in% class(x)) { RStudioView(x[1:500,]) } else { RStudioView(x) }
# }

# Turn off scientific notation
options(digits=3, scipen=8) 



#  Data              ###########################################################

# http://microdatos.dane.gov.co/index.php/catalog/718/get_microdata 

utils::download.file("http://microdatos.dane.gov.co/index.php/catalog/718/download/20607", 
              destfile = "data-raw/data.zip", 
              #method="curl")
              method="wget")



file_path <- "data-raw/Características y composición del hogar.sav"
data1 <- haven::read_sav(file_path)


#unzip( "Condiciones de vida del hogar y tenencia de bienes.zip" )
file_path <- "data-raw/Condiciones de vida del hogar y tenencia de bienes.sav"
data0 <- haven::read_sav(file_path)
#unzip( "Trabajo infantil.zip" )
# file_path <- "data-raw/Trabajo infantil.sav"
# data <- haven::read_sav(file_path)

data <- dplyr::left_join(data1, data0, by = c("DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P",       
                                              "ORDEN", "FEX_C" ) )

## let's create a variable for illegal (P1894 == 5) Venezualan (P756S3 == 3) -- 
data$IllegalVenez <- ifelse( (data$P1894 == 5 & data$P756S3 == 3), paste("Venz_SinDocumento"), paste("other"))
table(data$IllegalVenez, useNA = "ifany")

## Check what is there...
names(data0)

data %>% 
       sjPlot::view_df()

#  Chart theme  ###########################################################
my_theme <-   theme_classic(base_size = 12)  +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, color="red", face="bold.italic"),    # Center title position and size
    plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
    plot.caption = element_text(hjust = 0, face = "italic"), # move caption to the left
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
dir.create(file.path(getwd(), "out"))

#  Simple tabulation ###########################################################

# retrieve value and variable labels
#data.var <- sjlabelled::get_label(data)
data.val <-sjlabelled::get_labels(data)
weight <- as.vector(data$FEX_C)

## generate charts looping around variables
for (i in 6:ncol(data)) {
  # i <- 3
  var <- names(data)[i]
  varlab <- sjlabelled::get_label(data[i]) 
  type <- ifelse(lengths(data.val[i]) > 0, paste0("Categoric"), paste("Numeric"))
  cat(paste0(i, "-", var, "-" ,varlab,"-" ,type,"\n\n")) 
  t <- as.data.frame(table(data[ , i] )) 
  #t <- as.data.frame(table(data[ , i], useNA = "ifany"))
  #tt <- as.data.frame(data.val[i]@Value )
  
  
  if(nrow(t)>1 ) {
    ## test if it is numeric or categoric variable
    if(lengths(data.val[i]) > 0) {
   # if(nrow(t)< 8) {
    p <-   sjPlot::plot_frq(data[ , i], 
                            type = "bar",
                            sort.frq =  "asc",
                            coord.flip = TRUE,
                            weight.by = weight,
                            show.ci = TRUE,
                            geom.colors =  unhcRstyle::unhcr_blue) +
      unhcRstyle::unhcr_theme(base_size = 8)  + ## Insert UNHCR Style
      theme(legend.position = "none",
            panel.grid.major.x  = element_line(color = "#cbcbcb"), 
            panel.grid.major.y  = element_blank(), 
            panel.grid.minor = element_blank()) + ### changing grid line that should appear
      ## and the chart labels
      labs(title = paste0(varlab),
           subtitle = paste0(var),
           x = "",
           y = "",
           caption = "Source: Encusta Nacional de Calidad de Vida ECV 2020, Colombia, DANE")
     } else {
       ## Print an histogramme
       p <-   sjPlot::plot_frq(data[ , i], 
                               type = "bar",
                              # sort.frq =  "asc",
                              # coord.flip = TRUE,
                              weight.by = weight,
                              show.ci = TRUE,
                              show.values = FALSE,
                              show.n = FALSE,
                              show.prc = FALSE,
                               geom.colors =  unhcRstyle::unhcr_blue) +
         unhcRstyle::unhcr_theme(base_size = 8)  + ## Insert UNHCR Style
         theme(legend.position = "none",
               panel.grid.major.y  = element_line(color = "#cbcbcb"), 
               panel.grid.major.x  = element_blank(), 
               panel.grid.minor = element_blank()) + ### changing grid line that should appear
         ## and the chart labels
         labs(title = paste0(varlab),
              subtitle = paste0(var),
              x = "",
              y = "",
              caption = "Source: Encusta Nacional de Calidad de Vida ECV 2020, Colombia, DANE")
     } 
 
 ggsave( paste0("out/",i, "-",var, ".png"),  plot =  p, device = "png", width = 24, height = 12, units = "cm")
  } else {
    cat(paste0("one single value for ", var, "\n"))
  }
}

#  Cross-tabulation ###########################################################

cross <- c("IllegalVenez")
crosslab <- "IllegalVenez"

for (i in 21:ncol(data)) {
  # i <- 20
  var <- names(data)[i]
  varlab <- sjlabelled::get_label(data[i])
  type <- ifelse(lengths(data.val[i]) > 0, paste0("Categoric"), paste("Numeric"))
  cat(paste0(i, "-", var, "-" ,varlab,"-" ,type,"\n\n"))
  t <- as.data.frame(table(data[ , i] ))
  #t <- as.data.frame(table(data[ , i], useNA = "ifany"))
  #tt <- as.data.frame(data.val[i]@Value )


  if(nrow(t)>1 ) {
    ## test if it is numeric or categoric variable
    if(lengths(data.val[i]) > 0) {
      # if(nrow(t)< 8) {
      p <-   sjPlot::plot_xtab(x= data[[i]],
                               grp = data[[cross]],
                               type = "bar",
                               weight.by = weight,
                               # sort.frq =  "asc",
                               # bar.pos = "stack",
                               wrap.labels = 40,
                               show.total = FALSE,
                               show.summary = TRUE,
                               show.prc = FALSE,
                               summary.pos = "r",
                               coord.flip = TRUE) +
        unhcRstyle::unhcr_theme(base_size = 8)  + ## Insert UNHCR Style
        theme(#legend.position = "none",
              panel.grid.major.x  = element_line(color = "#cbcbcb"),
              panel.grid.major.y  = element_blank(),
              panel.grid.minor = element_blank()) + ### changing grid line that should appear
        ## and the chart labels
        labs(title = paste0(varlab),
             subtitle = paste0("Crossed with ", crosslab),
             x = "",
             y = "",
             caption = "Source: Encusta Nacional de Calidad de Vida ECV 2020, Colombia, DANE")
    } else {
      ## Print an histogramme
      p <-   sjPlot::plot_grpfrq(var.cnt = data[[i]],
                                 var.grp = data[[cross]],
                                 type = "bar",
                                 weight.by = weight,
                                 # sort.frq =  "asc",
                                 # bar.pos = "stack",
                                 wrap.labels = 40,
                                 show.summary = TRUE,
                                 show.prc = FALSE,
                                 show.values = FALSE,
                                 show.n = FALSE) +
        unhcRstyle::unhcr_theme(base_size = 8)  + ## Insert UNHCR Style
        theme(#legend.position = "none",
              panel.grid.major.y  = element_line(color = "#cbcbcb"),
              panel.grid.major.x  = element_blank(),
              panel.grid.minor = element_blank()) + ### changing grid line that should appear
        ## and the chart labels
        labs(title = paste0(varlab),
             subtitle = paste0("Crossed with ",crosslab),
             x = "",
             y = "",
             caption = "Source: Encusta Nacional de Calidad de Vida ECV 2020, Colombia, DANE")
    }
    p
    ggsave( paste0("out/",cross," - ", i, "-",var, ".png"),  plot =  p, device = "png", width = 24, height = 16, units = "cm")
  } else {
    cat(paste0("one single value for ", var, "\n"))
  }
}


## Reference #####
# https://martinctc.github.io/blog/working-with-spss-labels-in-r/
