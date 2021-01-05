# configurando mi espascio de trabajo
setwd("~/GitHub/ICO22020")

# borrando variables de entorno
rm(list = ls())

# importando librerias
library(rvest)
library(gdata)

#######################################################

# [almacenando informacion 1] creacion dataframe
todaLaInformacion <- data.frame()

for (pag in 1:20) {
  urlpesaschile <- paste('https://pesaschile.cl/2/2-productos?page=',pag,sep = "")
  
  # descargando la pagina pesaschile.cl
  
  pesaschile <- read_html(urlpesaschile)
  
  # listado de productos
  listadoProductos <- html_node(pesaschile, css ="#js-product-list")
  
  # titulos 
  productos <- html_nodes(listadoProductos, css = ".product_name")
  textProductos <- html_text(productos)
  print(textProductos)
  
  
  # precio 
  precio <- html_nodes(listadoProductos, css = ".price")
  textoPrecio <- html_text(precio)
  print(textoPrecio)
  
  
  ##############################################################################################  
  
  # listado productos individuales
  listadoIndividual <- html_nodes(listadoProductos,css = ".item-product") 
  
  # titulos
  productos <- html_nodes(listadoProductos,'.product_name')
  
  linkProducto <- html_attr(productos,'href')
  print(linkProducto)
  
  textoProductos <- html_text(productos)
  print(textoProductos)
  
  # precio
  precio <- html_nodes(listadoProductos,'.price')
  textoPrecio <- html_text(precio)
  textoPrecio <- gsub("[.]","",textoPrecio)
  textoPrecio <- gsub("[$]","",textoPrecio)
  textoPrecio <- trim(textoPrecio)
  textoPrecio <- as.numeric(textoPrecio)
  print(textoPrecio)
  
  # [almacenando informacion 2] creacion data valores
  # con los detalles de cada item
  item <- data.frame(productos = textoProductos, precio = textoPrecio, link = linkProducto)
  
  # [almacenando informacion 3] almacenando informacion
  # del productos con datos totales
  todaLaInformacion <- rbind(todaLaInformacion,item)
}

##para ver los nombres de las variables##

names(todaLaInformacion)

##para ver los seis primeros datos##

head(todaLaInformacion)

##el valor minimo de precio##

min(todaLaInformacion$precio)

##el valor maximo de precio##

max(todaLaInformacion$precio)

##el rango de precio##

range(todaLaInformacion$precio)

##la media##

mean(todaLaInformacion$precio)

##la mediana##

median(todaLaInformacion$precio)

     ##tiene mediana mayor a la media, por ende, la distribución está sesgada a la izquierda

##cantidad de productos

length(todaLaInformacion$productos)

##cuanto tengo que gastar si quiero comprar un producto de cada uno de los existentes

sum(todaLaInformacion$precio)

##desviacion estandar

sd(todaLaInformacion$precio)

##creamos intervalos para la variable productos##

##determinemos cuantos numeros de intervalos son

nclass.Sturges(todaLaInformacion$productos)

## creamos subgrupos
## en la misma base de datos##

##primero editaremos en la base aquellos que incluyan "" y comas para evitar problemas

todaLaInformacionn=edit(todaLaInformacion)

##creamos subgrupos dentro de la nueva base de datos en la seccion de productos

todaLaInformacionn$grupos =0

todaLaInformacionn$grupos[which(todaLaInformacionn$productos %in% c("Piso de Caucho 15mm","Cinturón Lumbar PROmachine","Vendaje K-Tape","Cinturón Lumbar HWM","Callera HWM (Par)","Almohadilla De Barra Con Velcro PROmachine","Guantes FullFit Black (Par)","Muñequeras HWM (Par)","Rodilleras FullFit (Par)","Balón Medicinal 5kg Sin Bote HWM","Calleras Black HWM (Par)","Muñequeras PROmachine Talla S (Par)","Tobilleras Para Polea FullFit (Par)","Slam Ball HWM 60lb","Guantes PROmachine (Par)","Rodilleras Elásticas HWM (Par)","Almohadilla De Barra Eva FullFit","Morral Pesas Chile","Dominada/Fondo Asistida MO Series Obelix","Dominada / Fondo Asistida V8","Collarines HWM (Par)","Set Expander Tube FullFit","Cinturón Lumbar PROmachine","Colchoneta PU PROmachine","Mancuerna Preolímpica 3.8kg Barbell (Unidad)","Chaleco Lastrado 10kg","Chaleco Lastrado 15kg","Chaleco Lastrado 20kg","Rodillera Mc David Protector (Unidad)","Colchoneta Lona","Chaleco Lastrado 5kg","Jaula de Potencia Promachine","Jaula de Potencia Hell Serie HWM","Rodillera Mc David (Unidad)","Codera Mc David","Media Jaula de Potencia Black HWM","Jaula de Potencia Jr Serie PROmachine","Cinturón de Lastre FullFit","Guantes FullFit Pink (Par)","Colchonetas de Impacto HWM (Par)","Muñequera De Género HWM (Par)","Cinturón Lumbar Black/Pink FullFit"))]="Complementos cuidado deportivo"

todaLaInformacionn$grupos[which(todaLaInformacionn$productos %in% c("Push Up PROmachine (Par)","Prensa Horizontal MO Series Obelix","Hip Thrust T8 Series Obelix",
                                                                  "Rack Horizontal Para Mancuernas HWM","Barras Paralelas 31,5cm HWM (Par)","Rueda Abdominal","Balón Pilates 55cm",
                                                                  "Tenaza Olímpica (Par)","Valla de Salto","Mini Foam Roller HWM","Balón Pilates 75cm",
                                                                  "Disco Equilibrio Pvc HWM","Agarre Polea - Soga",
                                                                  "Straps de Levantamiento HWM (Par)","Balón Pilates 65cm","Escalera de Coordinación",
                                                                  "Battle Rope 37.5mm HWM","Bosu Black Mindfullness","Cuerda de Trepa HWM","Cuerda de Salto Pvc",
                                                                  "Set 4 Cajones Pliométricos HWM","Peso Tobillo Gris 1.5kg PROmachine (Par)",
                                                                  "Battle Rope 45mm HWM","Heavy Speed Rope HWM","Bosu Mindfullness","Hip Thrust PROmachine",
                                                                  "Push Up Metal (Par)","Barra Olímpica Log HWM","Slam Ball HWM 40lb","Banco Scott PROmachine",
                                                                  "Set Lentejas Agilidad","Slam Ball HWM 80lb","Rack de Barras Pared Horizontal HWM",
                                                                  "Barra Olímpica Mancuerna (Par)","Press Banco Inclinado MO Series Obelix",
                                                                  "Aperturas MO Series Obelix","Atril de Sentadillas MO Series Obelix",
                                                                  "Curl Femoral Sentado MO Series Obelix","Banco Inclinable MO Series Obelix",
                                                                  "Banco Plano MO Series Obelix","Glúteo Aislado MO Series Obelix","Aducción MO Series Obelix",
                                                                  "Abducción MO Series Obelix","Extensión de Cuádriceps MO Series Obelix",
                                                                  "Abductor / Aductor V8 Series Obelix","Curl Femoral Sentado V8 Series Obelix",
                                                                  "Glúteo Aislado V8 Series Obelix","Press de Hombro T8 Series Obelix",
                                                                  "Press de Pectoral T8 Series Obelix","Remo Sentado T8 Series Obelix",
                                                                  "Dorsalera T8 Series Obelix","Banco Abdominal MO Series Obelix","T - Rowing MO Series Obelix",
                                                                  "Smith MO Series Obelix","Polea Alta MO Series Obelix","Trotadora Obelix GT7",
                                                                  "Press de Hombro V8","Curl de Bíceps V8","Remo Sentado V8",
                                                                  "Curl Femoral Acostado V8 Series Obelix","Aperturas V8","Polea Alta V8 Series Obelix",
                                                                  "Remo Bajo V8","Banco Abdominal V8","Banco Inclinable V8","Banco Scott V8",
                                                                  "Press Banco Inclinado V8","Atril De Sentadillas V8","Banco Lumbar V8",
                                                                  "Banco Vertical MO Series Obelix","Press Banco Declinado MO Series Obelix",
                                                                  "Press Banco Vertical MO Series Obelix","Banco Lumbar MO Series Obelix",
                                                                  "Bicicleta Ergométrica Obelix","Polea Alta/Remo MO Series Obelix",
                                                                  "Banco Scott MO Series Obelix","Mat de Yoga Mindfullness","Banda de Resistencia 32mm HWM",
                                                                  "Banda de Resistencia 32mm HWM","Banda de Resistencia 45mm HWM",
                                                                  "Polea Cruzada MO Series Obelix","Press Banco Plano MO Series Obelix",
                                                                  "Banco Abdominal / Fondo MO Series Obelix","Slam Ball HWM 100lb","Banda Suspensión FullFit",
                                                                  "Smith PROmachine","Rep Timer HWM","Atril de Discos Olímpicos","Tenaza Preolímpica (Par)",
                                                                  "Barras Paralelas 17cm HWM (Par)","Agarre Polea - Remo","Pack Bandas de Resistencia HWM",
                                                                  "Barra Olímpica 17kg Eco Serie","Rack de Barras Suelo HWM","Elíptica Pro Obelix",
                                                                  "Anillas Olímpicas Madera HWM","Media Jaula de Potencia Amarilla HWM",
                                                                  "Barra Preolímpica Mancuerna","Tabla de Escalada HWM","Expander Tube Soft FullFit",
                                                                  "Abs Straps HWM (Par)","Barras Paralelas 70cm HWM (Par)","Crossover Lat Pulldown PROmachine",
                                                                  "Press Banco Olímpico PROmachine","Landmine HWM","Polea Alta Hell Series HWM",
                                                                  "Press Banco Multifuncional PROmachine","Mini Tabla de Escalada HWM",
                                                                  "Polea Alta Remo PROmachine","Banco GHD HWM","Foam Roller HWM",
                                                                  "Reacondicionado - Trotadora Obelix GT7","Remo Contrapeso PROmachine",
                                                                  "Anillas Olímpicas Pvc HWM","Barra Preolímpica Romana","Step Aeróbico FullFit",
                                                                  "Bumper Plate 25kg HWM (Par)","Bumper Plate 15kg HWM (Par)","Bumper Plate 5kg HWM (Par)",
                                                                  "Bumper Plate 10kg HWM (Par)","Atril de Sentadillas PROmachine (Par)",
                                                                  "Banco Inclinable Declinable Pro HWM","Body Pump Obelix","Bicicleta Spinning SP2P PROmachine",
                                                                  "Agarre Polea - Simple Una Mano","Barra Olímpica 20kg Training Serie HWM",
                                                                  "Speed Rope Pvc HWM","Press Banco Preolímpico PROmachine","Banco Plano HWM",
                                                                  "Abdominal / Fondo PROmachine","Press de Tríceps MO Series Obelix","Polea Cruzada PROmachine",
                                                                  "Speed Rope Aluminio HWM","Set Bandas de Resistencia FullFit","Trotadora Obelix GT5",
                                                                  "Set 20 Body Pump Obelix","Extensión de Cuadriceps V8 Series Obelix","Abdominal V8",
                                                                  "Agarre Polea - Barra Corta","Banco Inclinable Declinable Eco PROmachine",
                                                                  "Sissy Squat PROmachine","Banco Lumbar PROmachine","Banco Abdominal Home PROmachine",
                                                                  "Banco Plano V8","Abdominal / Fondo V8","Polea Cruzada Corta V8","Polea Cruzada V8",
                                                                  "Atril de Sentadillas Jr Serie PROmachine (Par)","Elíptica GE1050 Obelix",
                                                                  "Bicicleta Estática Magnética Obelix","Remo Bajo MO Series Obelix",
                                                                  "Remo Sentado MO Series Obelix","Polea Cruzada Corta MO Series Obelix",
                                                                  "Trotadora Obelix GT7 Touch","Bicicleta Spinning Move Obelix","Elíptica Light Obelix",
                                                                  "Elíptica Stepper Light Obelix","Banco Vertical V8","Press Banco Declinado V8",
                                                                  "Rueda Abdominal Cinética","Bicicleta Spinning SP7 Obelix","Bicicleta Spinning SPX Obelix",
                                                                  "Step Aeróbico Pro HWM","2° Selección - Air Bike Hurricane HWM","Banco Scott HWM",
                                                                  "Spotter Arms Hell Series HWM (Par)","Banco Hip Thrust Hell Series HWM","Squat Rack HWM",
                                                                  "Set Glute Bands FullFit","Banda de Resistencia 13mm HWM",
                                                                  "Banco Inclinable Declinable Hell HWM","Banco Inclinable Declinable 2.0 PROmachine",
                                                                  "Air Bike Hurricane HWM","Banda de Resistencia 22mm HWM","Ab Mat HWM",
                                                                  "Atril De Sentadillas Madefe Sport (Par)"))]="Complementos deportivos"


todaLaInformacionn$grupos[which(todaLaInformacionn$productos %in% c("Magnesio 56 gr.",
                                                                  "Bola de Masaje Lacrosse HWM","Máscara de Hipoxia PROmachine",
                                                                  "Balón Medicinal 10kg Sin Bote HWM","Press de Hombro MO Series Obelix","Smith V8",
                                                                  "Balón Medicinal 3kg Sin Bote HWM","Pack Functional Workout","Pack Agility Pro",
                                                                  "Pistola de Masaje HWM","Balón Medicinal Black 5kg HWM","Balón Medicinal Black 3kg HWM",
                                                                  "Balón Medicinal Black 7kg HWM","Pack Functional Workout Full",
                                                                  "Balón Medicinal 7kg Sin Bote HWM","Balón Medicinal 9kg Sin Bote HWM",
                                                                  "TRX Force Kit Tactical","TRX Pro Kit","Balón Medicinal Black 9kg HWM",
                                                                  "Balón Medicinal Black 12kg HWM","Leg Curl HWM"))]="Cuidados"



todaLaInformacionn$grupos[which(todaLaInformacionn$productos %in% c("Shaker Black 600ml PesasChile",
"Kettlebell Acero 4kg HWM","Prensa Inclinada 45° T8 Series Obelix",
"Curl Femoral Acostado MO Series Obelix","Extensión de Tríceps MO Series Obelix",
"Curl de Bíceps MO Series Obelix","Atril de Discos MO Series Obelix",
"Contractora de Pectoral MO Series Obelix","Press de Pectoral MO Series Obelix",
"Curl de Bíceps T8 Series Obelix","Curl de Bíceps T8 Series Black/Yellow Obelix",
"Pack 31kg Balones Medicinales Sin Bote HWM","Pull Up Dip Bar HWM",
"Atril Discos y Barra Olímpica Transportable HWM","Barra Olímpica Hexagonal HWM",
"Barra Olímpica 15kg Monster Serie HWM",'Barra Olímpica "Z"',"Expander Tube Hard FullFit",
"Barra Olímpica 7.5kg Technique Serie HWM","Barra Olímpica Fat HWM","Soporte Para Discos HWM",
"Jaula Retráctil Modular HWM","Mancuerna Preolímpica 7.8kg Barbell (Unidad)",
"Mancuerna Preolímpica 11.8kg Barbell (Unidad)","Mancuerna Preolímpica 13.8kg Barbell (Unidad)",
"Disco Equilibrio Madera HWM","Cajón Pliométrico Madera HWM",
"Peso Tobillo Morado 1.5kg FullFit (Par)","Mancuerna Preolímpica 5.8kg Barbell (Unidad)",
"Mancuerna Preolímpica 15.8KG Barbell (Unidad)","Mancuerna Preolímpica 9.8kg Barbell (Unidad)",
"Barra Dominadas Multi Grip HWM","Set 10 Pares Mancuernas Uretano Obelix",
"Barra Olímpica Corta 1.5mt","Barra Olímpica 15kg Training Serie HWM",
"Set 10 Barras Peso Fijo Obelix","2° Selección - Barra Olímpica Log HWM",
"Cajón Pliométrico Acolchado HWM","Peso Tobillo Gris 2kg FullFit (Par)",
'Barra Preolímpica "W"','Barra Preolímpica "Z"',"Disco Olímpico Acero 2.5kg (Par)",
"Disco Olímpico Acero 5kg (Par)","Disco Olímpico Acero 10kg (Par)","Kettlebell Vinilo 8kg",
"Kettlebell Vinilo 6kg","Kettlebell Vinilo 12kg","Kettlebell Vinilo 4kg",
"Kettlebell Vinilo 10kg","Peso Tobillo Gris 2.5kg FullFit (Par)",
"Mancuerna Preolímpica 21.8kg Barbell (Unidad)","Mancuerna Preolímpica 17.8kg Barbell (Unidad)",
"Mancuerna Hexagonal 5kg HWM (Par)","Mancuerna Hexagonal 25kg HWM (Par)",
"Mancuerna Hexagonal 30kg HWM (Unidad)",'Barra Olímpica "W"',"Barra Olímpica Romana",
"Mancuerna Preolímpica 25.8kg Barbell (Unidad)","Sandbag 10kg HWM",
"Mancuerna Hexagonal 35kg HWM (Unidad)","Disco Preolímpico 15kg Barbell (Par)",
"Disco Preolímpico 10kg Barbell (Par)","Disco Preolímpico 20kg Barbell (Par)",
"Pantorillera MO Series Obelix","Barra Dominadas Puerta PROmachine",
"Rack de Barras Pared Vertical HWM","Crumb Bumper Plate 10lb HWM (Par)",
"Crumb Bumper Plate 25lb HWM (Par)","Disco Preolímpico 3kg Barbell (Par)",
"Barra Olímpica Multi Agarre HWM","Peso Tobillo Morado 2kg PROmachine (Par)",
"Peso Tobillo Morado 2.5kg FullFit (Par)","Barra Dominadas Recta HWM",
"Press de Pectoral V8","Press de Tríceps V8","Prensa Horizontal V8",
"Press Banco Plano V8","Pantorrillera V8","Atril de Discos V8","Disco Olímpico Grip 5kg (Par)","Disco Olímpico Grip 10kg (Par)","Disco Olímpico Grip 15kg (Par)","Disco Olímpico Grip 15kg (Par)","Disco Olímpico Grip 20kg (Par)","Set 10 Body Pump Obelix","Mancuerna Preolímpica 19.8kg Barbell (Unidad)","Mancuerna Preolímpica 23.8kg Barbell (Unidad)","Mancuerna Preolímpica 27.8kg Barbell (Unidad)","Mancuerna Preolímpica 29.8kg Barbell (Unidad)","Mancuerna Preolímpica  31.8kg Barbell (Unidad)","Disco Olímpico 5kg Competición HWM (Par)","Disco Olímpico 25kg Competición HWM","Kettlebell Acero 28kg HWM","Kettlebell Acero 32kg HWM","Disco Olímpico Grip 25kg (Par)","Disco Olímpico ZV 2.5kg (Par)","Disco Olímpico ZV 5kg (Par)","Disco Olímpico ZV 10kg (Par)","Disco Olímpico ZV 15kg (Par)","Disco Olímpico ZV 20kg (Par)","Disco Olímpico ZV 25kg (Par)","Set 20kg Mancuernas ECO","Barra Preolímpica Recta 1.5mt","Disco Olímpico Acero 15kg (Par)","Disco Olímpico Acero 20kg (Par)","Kettlebell Vinilo 16kg","Kettlebell Vinilo 20kg","Mancuerna Hexagonal 15kg HWM (Par)","Bumper Plate 20kg HWM (Par)","Disco Olímpico Acero 1.25kg (Par)","Agarre Polea - Barra Amplia","Sandbag 15kg HWM","Disco Preolímpico 2.5kg Barbell (Par)","Disco Olímpico 5kg PROmachine (Par)","Barra Olímpica 20kg PROmachine Serie","Disco Olímpico Grip 1.25kg (Par)","Disco Olímpico Grip 2.5kg (Par)","Disco Olímpico 10kg Competición HWM (Par)","Disco Olímpico 15kg Competición HWM (Par)","Disco Olímpico 20kg Competición HWM","Barra Olímpica 25lb C-160 Serie HWM","Crumb Bumper Plate 55lb HWM (Par)","Kettlebell Acero 6kg HWM","Set 30kg Mancuernas ECO","Disco Preolímpico 1kg Barbell (Par)","Mancuerna Hexagonal 7.5kg HWM (Par)","Sandbag 20kg HWM","Sandbag 25kg HWM","Pack 70kg Sandbags HWM","Disco Olímpico 1.5kg Fraccional HWM (Par)","Barra Olímpica 35lb C-175 Serie HWM","Kettlebell Acero 24kg HWM","Barra Olímpica 15kg Elite Serie HWM","Disco Olímpico 0.5kg Fraccional HWM (Par)","Disco Olímpico 2kg Fraccional HWM (Par)","Crumb Bumper Plate 15lb HWM (Par)","Crumb Bumper Plate 35lb HWM (Par)","Kettlebell Acero 8kg HWM","Mancuerna Hexagonal 10kg HWM (Par)","Disco Preolímpico 2kg Barbell (Par)","Disco Olímpico 1kg Fraccional HWM (Par)","Disco Olímpico 2.5kg Fraccional HWM (Par)","Barra Olímpica 20kg Monster Serie HWM","Mancuerna Hexagonal 22.5kg HWM (Par)","Mancuerna Hexagonal 2.5kg HWM (Par)","Kettlebell Acero 20kg HWM","Crumb Bumper Plate 45lb HWM (Par)","Kettlebell Acero 16kg HWM","Mancuerna Hexagonal 12.5kg HWM (Par)","Mancuerna Hexagonal 20kg HWM (Par)","Mancuerna Hexagonal 17.5kg HWM (Par)","Kettlebell Acero 10kg HWM","Barra Olímpica 20kg Elite Serie HWM","Servicio vendedor Pesas Chile","Mancuerna Hexagonal 5lb (Par)","Mancuerna Hexagonal 10lb (Par)","Mancuerna Hexagonal 15lb (Par)","Mancuerna Hexagonal 20lb (Par)","Mancuerna Hexagonal 25lb (Par)","Mancuerna Hexagonal 30lb (Par)","Mancuerna Hexagonal 35lb (Par)","Mancuerna Hexagonal 40lb (Par)","Mancuerna Hexagonal 5kg (Par)"))]="Pesas y mancuernas"


##hagamos un sumary

summary(todaLaInformacionn)

## La variable numerica "precio" tiene 1 outliers el cual hay que eliminar

todaLaInformacionn <- todaLaInformacionn[!todaLaInformacionn$precio == 1, ]


##histograma precio

hist(todaLaInformacionn$precio)

## Existe sesgo positivo en la variable precio, dado que el promedio > mediana
## y la concentracion de los datos estan en el lado izquierdo      

## Frecuencia por grupo
tabla <- table(todaLaInformacionn$grupos)

# boxplot grupos por precios

ggplot(todaLaInformacionn, mapping = aes(x=grupos,y=precio))+
  geom_boxplot()

# graficos de barras por grupos
  
ggplot(todaLaInformacionn, mapping = aes(x=grupos))+
  geom_bar()
  
  
##ahora uno para cada grupo hecho dentro de la variable productos

install.packages("tidyverse")
library("tidyverse")

class(todaLaInformacionn$precio)

todaLaInformacionn %>% ggplot(aes(x=precio,group=grupos))+geom_boxplot()+facet_wrap(~grupos, scales="free")

  ##seguimos intentando con otro tipo de grafico para que se vea bien visualmente

todaLaInformacionn %>% ggplot(aes(x=as.factor(grupos),y=precio))+geom_violin()+facet_wrap(~grupos, scales="free")

 ##intentemos con otro

todaLaInformacionn %>% ggplot(aes(x=as.factor(grupos),y=precio))+geom_boxplot(outlier.shape = NA)+facet_wrap(~grupos, scales="free")

##histograma 

todaLaInformacionn %>% ggplot(aes(x=precio,group=grupos))+geom_histogram()+facet_wrap(~grupos, scales="free")
