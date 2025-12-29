library("lubridate")

plot_Feret_vassel <- function(x,y) {
  plot(x$Feret, main=y)
} ##Función que relaciona el id de del vaso con su Feret

###Funcion necesaria para posteriores calculos, te hace un data frame con d4,d5 y d4 en metros
Feret_converter <- function(x,measure){
  x$area_Feret <- 0.785*(x$Feret^2) ##calcula el area del vaso
  x$D4 <- x$Feret^4 #diametro a la 4 
  x$D5 <- x$Feret^5 #diametro a la 5
  if(measure == "mm") ###muestra si la medida de Feret se hizo en mm, te devuelve el D4 en m, si se hizo en micrometro
    ##habria que poner "microm" y en cualquier otro caso no se hace la funcion
    {
    x$D_4meter <- (x$Feret^4)/(10^12)
    data.frame <- data.frame("N_vassel" = count(x),"Dmean"= mean(x$Feret ),"Dmin"= min(x$Feret), "Dmax"= max(x$Feret)
                             , "Sum.area"= colSums (select (x, contains ("area_Feret"))),
                             "Sum.D4" = colSums (select (x, contains ("D4"))),"Sum.D5" = colSums (select (x, contains ("D5"))),
                             "Sum D4meter" = colSums(select (x, contains ("D_4meter"))))
  } else if(measure == "microm") {
    x$D_4meter <- (x$Feret^4)/(10^24)
    ddata.frame <- data.frame("N_vassel" = count(x),"Dmean"= mean(x$Feret),"Dmin"= min(x), "Dmax"= max(x)
                              , "Sum area"= colSums (select (x, contains ("area_Feret"))),
                              "Sum D4" = colSums (select (x, contains ("D4_12"))),"Sum D5" = colSums (select (x, contains ("D5_12"))),
                              "Sum D4meter" = colSums(select (x, contains ("D_4meter"))))
  }else { print(False  )}  ###hace un data frame donde se dispone el numero de vaso, la suma de las areas de los vasos,
  ##la suma de los D4, D5 y de los D4 transladado a metro
  return(data.frame)
} 


Feret_converter_analisistemporal <- function(x,measure){
  x$area_Feret <- 0.785*(x$Feret^2) ##calcula el area del vaso
  x$D4 <- x$Feret^4 #diametro a la 4 
  x$D5 <- x$Feret^5 #diametro a la 5
  if(measure == "mm") ###muestra si la medida de Feret se hizo en mm, te devuelve el D4 en m, si se hizo en micrometro
    ##habria que poner "microm" y en cualquier otro caso no se hace la funcion
  {
    x$D_4meter <- (x$Feret^4)/(10^12)
    data.frame <- data.frame("N_vassel" = nrow(x),"Dmean"= mean(x$Feret ),"Dmin"= min(x$Feret), "Dmax"= max(x$Feret)
                             , "Sum.area"= colSums (select (x, contains ("area_Feret"))),
                             "Sum.D4" = colSums (select (x, contains ("D4"))),"Sum.D5" = colSums (select (x, contains ("D5"))),
                             "Sum D4meter" = colSums(select (x, contains ("D_4meter"))))
  } else if(measure == "microm") {
    x$D_4meter <- (x$Feret^4)/(10^24)
    ddata.frame <- data.frame("N_vassel" = nrow(x),"Dmean"= mean(x$Feret),"Dmin"= min(x), "Dmax"= max(x)
                              , "Sum area"= colSums (select (x, contains ("area_Feret"))),
                              "Sum D4" = colSums (select (x, contains ("D4_12"))),"Sum D5" = colSums (select (x, contains ("D5_12"))),
                              "Sum D4meter" = colSums(select (x, contains ("D_4meter"))))
  }else { print(False  )}  ###hace un data frame donde se dispone el numero de vaso, la suma de las areas de los vasos,
  ##la suma de los D4, D5 y de los D4 transladado a metro
  return(data.frame)
} 
####Funcion para coger conjunto de datos con y sin el area de rayo
select_columns<- function(x) {
  if("ray_area" %in% colnames(x) == TRUE ) {
    select(x,indv, TRW_mean, Area,ray_area,Tratam)
  } else {
    select(x,indv, TRW_mean, Area,Tratam)
  }
}

Vessel_measurement <- function(x) {
  x$mean_vessel_area <-  x$Sum.area/x$N_vassel
  x$vessel_frequency <- x$N_vassel/x$Area ###formato if por si esta columna year_Total_area
  x$Dh <- x$Sum.D5/x$Sum.D4
  x$Kh <- ((pi*998.2071)/(128*10^-3))*x$Sum.D4meter
  x$Ks <- x$Kh/x$Area  ###formato if por si esta columna year_Total_area
  x$vul_index <- x$Dmean/x$vessel_frequency  ###convertir lista en data frame
  
  if("ray_area" %in% colnames(x) == TRUE ) {
    x <- data.frame("indv" = x$indv, "treatment" = x$treatment,"N_vassel" = x$n ,"Ring_area"= x$Area,
                    "vessel_area" = x$Sum.area, "mean_vessel_area" = x$mean_vessel_area,
                    "dh"= x$Dh, "Ks"= x$Ks, "kh"= x$Kh, "vul_index" = x$vul_index, "ray_area"= x$ray_area, "TRW"=  x$TRW_mean)}
  else {
    return(x)
  }
}

Vessel_measurement_descorche <- function(x) {
  x$mean_vessel_area <-  x$Sum.area/x$n
  x$Dh <- x$Sum.D5/x$Sum.D4
  x$Kh <- ((pi*998.2071)/(128*10^-3))*x$Sum.D4meter
  data.frame("N_vassel" = x$n ,
                    "vessel_area" = x$Sum.area, "mean_vessel_area" = x$mean_vessel_area,
                    "dh"= x$Dh, "kh"= x$Kh)
}

vessel_ray_perct <- function(x)  {

  x <- data.frame(x, "vessel_percent" = (x$vessel_area*100/x$Ring_area), "rays_percent" = (x$ray_area*100/x$Ring_area) )
}


###error estandar
se<-function(x)
{
  std<-sd(x, na.rm=T)
  # longitud (length) de cuales (which) no son (!) NA (is.na)
  n<-length(which(!is.na(x)))
  return(std/sqrt(n))
}

###Media y error estandar de los datos
anatomy_results <- function(x)
{
  data <- split(x, x$treatment)
  data <- lapply(data, function(x) {
  data1 <- x[!(names(x) %in% c("indv", "treatment"))]
  data1 <- gather(data1, factor_key=TRUE) 
  data1%>% group_by(key)%>%
    summarise(mean= mean(value), se= se(value))}) 
}

##Dendrometro
dendrometer <-  function(x,treatment, plot)
{
  if(plot){
    
    ####poner directamente anatomy_results(x)
  data <- split(x, treatment)
  data1 <- lapply(data, function(x) {
    data1 <- x[!(names(x) %in% c( "indv","treatment"))]
    data1 <- gather(data1, factor_key=TRUE) 
    data1%>% group_by(key)%>%
      summarise(mean= mean(value, na.rm = TRUE), se= se(value))
    
  })
  ID <- names(data)
  data2 <- mapply(cbind, data1, "treatment"= ID,SIMPLIFY = F)
  data2 <- rbindlist(data2)
  data2$errormax <- data2$mean + data2$se
  data2$errormin <-  data2$mean - data2$se
  data2$Date <-  as.Date(data2$key, format = "%d/%m/%Y")
  ggplot(data = data2, aes(x=Date, y=mean, group= treatment)) + geom_line(aes(colour=treatment), lwd= 1)+
    theme(panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(colour="black"),
          axis.line.y = element_line(colour="black"))+
    geom_hline(yintercept = 0, linetype= 2)
    
  
  } else{
    data <- split(x, treatment)
    data <- lapply(data, function(x) {
      data1 <- x[!(names(x) %in% c( "indv","treatment"))]
      data1 <- gather(data1, factor_key=TRUE) 
      data1%>% group_by(key)%>%
        summarise(mean= mean(value), se= se(value))})
  
}}

##Funcion para abrir muchas hojas de un mismo excel
read_excel_allsheets  <- function(filename, tibble = FALSE) { ##te cre la funcion
  sheets <- readxl::excel_sheets(filename) ##te coge todos los nombres de las hojas
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X)) ##te lee todas las hojas, haciendote una lista por hoja, con el nombre de la hoja
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

##Funcion para eliminar filas que contienen Na en cierta columna

eliminar_filas_NA_lista <- function(data, columna_NA ){
  x <- 1
  nombres <-  names(data)
  lista <-  list()
  prueba <- while(x <= length(data)){
    data.frame <- do.call(rbind, data[x])
    delet_row <-data.frame[data.frame[columna_NA] !="",]
    delet_row <- delet_row[rowSums(is.na(delet_row)) != ncol(delet_row), ]
    lista[[x]] <- delet_row
    x <-  x+1
  }
  names(lista) <- nombres
  print(lista)}



######funciones para sumar un conjunto en una columna

sum_every_n <- function(v,n) {sapply(seq(1, length(v) - (n-1)), function(i) sum(v[i:(i + (n-1))]))} ###con la primera parte me creo una secuencia numerica desde el inicio hasta la longitud de n-1 siendo n el numero de datos en cada paquete 
mean_every_n <- function(v,n) {sapply(seq(1, length(v) - (n-1)), function(i) mean(v[i:(i + (n-1))]))} ###la funcion dentro de la formula sapply, significa que a mi vector (v), hago la suma de cada elemento del vector desde la posicion i hasta la i + (1-n)
