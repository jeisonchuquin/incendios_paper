library(raster)
library(lubridate)
library(geodata)
library(tidyverse)


path <- file.path('./Dataset')

years <- seq(2010, 2023, 1)

years <- c(2010, 2011)


get_year_data <- function(year) {
    
    if (leap_year(year)){
        
        doys <- c('001', '032', '061', '092', '122', '153', '183', '214', '245', '275', '306', '336')
    
    }else{
    
        doys <- c('001', '032', '060', '091', '121', '152', '182', '213', '244', '274', '305', '335')
        
    }
  
  
    data_list <- list()
    for (day in doys){
    
        file_name <- paste0('MCD64A1.061_Burn_Date_doy', year, day, '_aid0001.tif')
        path_read <- file.path(path, year, file_name)
        
        print(path_read)
        
        if (file.exists(path_read)){
            read_rst <- raster(path_read)
            data <- rasterToPoints(read_rst)
            data <- data %>% as.data.frame(data)
          
          
            colnames(data) <- c('x', 'y', 'valor')
            data <- data %>% filter(valor > 0)
            if (length(data$x) > 0){
            
                data$valor <- ifelse(data$valor > 0, 1, 0)
                data$year <- year
            
                data_list[[day]] <- data  
                
          }else{
              
                next
              
            }
        }else{
            
            next
            
        }
    
    }
  
  data_list <- do.call(rbind, data_list)
  return(data_list)
  
}


year <- 2023
data_list <- get_year_data(year)



data_list %>% ggplot(aes(x, y)) +
    geom_point(colour='red') +
    labs(title = '{year}')
  


ecu <- gadm(country="Ecuador", level=1, path=".")
pol <- ecu[ecu$NAME_1 != "Galápagos", ]

plot(pol)
points(data_list$x, data_list$y, cex=0.5, col='red')
