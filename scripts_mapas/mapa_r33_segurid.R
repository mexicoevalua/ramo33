#####
# Mapa para el total del Ramo 33
# Esta parte del script produce un mapa interactivo .html para r33_segurid
# Definir var para no tener que hacer cambios en el resto del script
# El fondo de seguridad aparece en 1999
table(r33_segurid$year)
r33_segurid  <- subset(r33_segurid,r33_segurid$year > 1998)

var  <- r33_segurid
fileName  <- "r33_segurid"

# Requiere un servidor local, en la terminal de Mac usar: python -m SimpleHTTPServer 8888
##### 
# Calcular quintiles para intervalos
table(with(var, cut(valReal,
                    breaks=c(quantile(valReal, probs = seq(0, 1, by = 0.20))), dig.lab = 5, include.lowest=T, right=F)))

# Asignar etiquetas manualmente para evitar notación científica
l  <-  c("[0 - 100)","[100 - 136)","[136 - 190)","[190 - 249)","[249 - 875]")

# Quitar decimales y ajustar leyenda
dat <- transform(var,
                 fillKey = cut(valReal,labels=l, breaks=c(quantile(valReal, probs = seq(0, 1, by = 0.20))), dig.lab = 5, include.lowest=T, right=F))
head(dat)
str(dat)

# "name" es el identificador único que liga los datos de R con el mapa (json) éste debe ser un string
dat$name  <- as.character(dat$name)
dat$valReal  <- format(dat$valReal, big.mark=",", digits=0)
str(dat)
keyNames <- levels(dat$fillKey)
keyNames

# Colores
fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'Greens'), '#BD0026'),
  c(levels(dat$fillKey), 'defaultFill')
)
str(fills)

# Estructurar datos para html
dat2 <- plyr::dlply(na.omit(dat), "year", function(x){
  y = rCharts::toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'name')
  return(y)
})
dat2[[1]]

# Existe un bug en la función ichoropleth de rMaps, utilizar el formato propuesto por Diego Valle-Jones 

d1 <- Datamaps$new()
d1$set(
  geographyConfig = list(
    dataUrl = "shapefiles/mx_states.json",
    popupTemplate =  "#! function(geography, data) { //this function should just return a string
    return '<div class=hoverinfo>' + geography.properties.name + ': ' + data.valReal + '</div>';
    }  !#"
  ),
  dom = 'chart_1',
  scope = 'states',
  labels = TRUE,
  bodyattrs = "ng-app ng-controller='rChartsCtrl'",
  setProjection = '#! function( element, options ) {
  
  var projection, path;
  
  projection = d3.geo.mercator()
  .center([-90, 24])
  .scale(element.offsetWidth)
  .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
  
  path = d3.geo.path()
  .projection( projection );
  
  return {path: path, projection: projection};
  } !#',
  fills = fills,
  data = dat2[[1]],
  legend = TRUE,
  labels = TRUE
)
d1$save(paste(fileName,".html",sep=""), cdn = TRUE)

#####
# Mapa con slider 

d1$addAssets(
  jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
)
d1$setTemplate(chartDiv = "
               <div id = 'chart_1' class = 'rChart datamaps'>
               <input id='slider' type='range' min=1999 max=2014 ng-model='year' width=200>
               <span ng-bind='year'></span>
               
               <script>
               function rChartsCtrl($scope){
               $scope.year = '2014';
               $scope.$watch('year', function(newYear){
               mapchart_1.updateChoropleth(chartParams.newData[newYear]);
               })
               }
               </script>
               </div>   "
)
d1$set(newData = dat2)
d1$save(paste(fileName,".html",sep=""), cdn = TRUE)
