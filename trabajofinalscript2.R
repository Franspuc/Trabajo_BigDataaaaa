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

     ##tiene mediana mayor a la media, por ende, la distribuci�n est� sesgada a la izquierda

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

todaLaInformacionn$grupos[which(todaLaInformacionn$productos %in% c("Piso de Caucho 15mm","Cintur�n Lumbar PROmachine","Vendaje K-Tape","Cintur�n Lumbar HWM","Callera HWM (Par)","Almohadilla De Barra Con Velcro PROmachine","Guantes FullFit Black (Par)","Mu�equeras HWM (Par)","Rodilleras FullFit (Par)","Bal�n Medicinal 5kg Sin Bote HWM","Calleras Black HWM (Par)","Mu�equeras PROmachine Talla S (Par)","Tobilleras Para Polea FullFit (Par)","Slam Ball HWM 60lb","Guantes PROmachine (Par)","Rodilleras El�sticas HWM (Par)","Almohadilla De Barra Eva FullFit","Morral Pesas Chile","Dominada/Fondo Asistida MO Series Obelix","Dominada / Fondo Asistida V8","Collarines HWM (Par)","Set Expander Tube FullFit","Cintur�n Lumbar PROmachine","Colchoneta PU PROmachine","Mancuerna Preol�mpica 3.8kg Barbell (Unidad)","Chaleco Lastrado 10kg","Chaleco Lastrado 15kg","Chaleco Lastrado 20kg","Rodillera Mc David Protector (Unidad)","Colchoneta Lona","Chaleco Lastrado 5kg","Jaula de Potencia Promachine","Jaula de Potencia Hell Serie HWM","Rodillera Mc David (Unidad)","Codera Mc David","Media Jaula de Potencia Black HWM","Jaula de Potencia Jr Serie PROmachine","Cintur�n de Lastre FullFit","Guantes FullFit Pink (Par)","Colchonetas de Impacto HWM (Par)","Mu�equera De G�nero HWM (Par)","Cintur�n Lumbar Black/Pink FullFit"))]="Complementos cuidado deportivo"

todaLaInformacionn$grupos[which(todaLaInformacionn$productos %in% c("Push Up PROmachine (Par)","Prensa Horizontal MO Series Obelix","Hip Thrust T8 Series Obelix",
                                                                  "Rack Horizontal Para Mancuernas HWM","Barras Paralelas 31,5cm HWM (Par)","Rueda Abdominal","Bal�n Pilates 55cm",
                                                                  "Tenaza Ol�mpica (Par)","Valla de Salto","Mini Foam Roller HWM","Bal�n Pilates 75cm",
                                                                  "Disco Equilibrio Pvc HWM","Agarre Polea - Soga",
                                                                  "Straps de Levantamiento HWM (Par)","Bal�n Pilates 65cm","Escalera de Coordinaci�n",
                                                                  "Battle Rope 37.5mm HWM","Bosu Black Mindfullness","Cuerda de Trepa HWM","Cuerda de Salto Pvc",
                                                                  "Set 4 Cajones Pliom�tricos HWM","Peso Tobillo Gris 1.5kg PROmachine (Par)",
                                                                  "Battle Rope 45mm HWM","Heavy Speed Rope HWM","Bosu Mindfullness","Hip Thrust PROmachine",
                                                                  "Push Up Metal (Par)","Barra Ol�mpica Log HWM","Slam Ball HWM 40lb","Banco Scott PROmachine",
                                                                  "Set Lentejas Agilidad","Slam Ball HWM 80lb","Rack de Barras Pared Horizontal HWM",
                                                                  "Barra Ol�mpica Mancuerna (Par)","Press Banco Inclinado MO Series Obelix",
                                                                  "Aperturas MO Series Obelix","Atril de Sentadillas MO Series Obelix",
                                                                  "Curl Femoral Sentado MO Series Obelix","Banco Inclinable MO Series Obelix",
                                                                  "Banco Plano MO Series Obelix","Gl�teo Aislado MO Series Obelix","Aducci�n MO Series Obelix",
                                                                  "Abducci�n MO Series Obelix","Extensi�n de Cu�driceps MO Series Obelix",
                                                                  "Abductor / Aductor V8 Series Obelix","Curl Femoral Sentado V8 Series Obelix",
                                                                  "Gl�teo Aislado V8 Series Obelix","Press de Hombro T8 Series Obelix",
                                                                  "Press de Pectoral T8 Series Obelix","Remo Sentado T8 Series Obelix",
                                                                  "Dorsalera T8 Series Obelix","Banco Abdominal MO Series Obelix","T - Rowing MO Series Obelix",
                                                                  "Smith MO Series Obelix","Polea Alta MO Series Obelix","Trotadora Obelix GT7",
                                                                  "Press de Hombro V8","Curl de B�ceps V8","Remo Sentado V8",
                                                                  "Curl Femoral Acostado V8 Series Obelix","Aperturas V8","Polea Alta V8 Series Obelix",
                                                                  "Remo Bajo V8","Banco Abdominal V8","Banco Inclinable V8","Banco Scott V8",
                                                                  "Press Banco Inclinado V8","Atril De Sentadillas V8","Banco Lumbar V8",
                                                                  "Banco Vertical MO Series Obelix","Press Banco Declinado MO Series Obelix",
                                                                  "Press Banco Vertical MO Series Obelix","Banco Lumbar MO Series Obelix",
                                                                  "Bicicleta Ergom�trica Obelix","Polea Alta/Remo MO Series Obelix",
                                                                  "Banco Scott MO Series Obelix","Mat de Yoga Mindfullness","Banda de Resistencia 32mm HWM",
                                                                  "Banda de Resistencia 32mm HWM","Banda de Resistencia 45mm HWM",
                                                                  "Polea Cruzada MO Series Obelix","Press Banco Plano MO Series Obelix",
                                                                  "Banco Abdominal / Fondo MO Series Obelix","Slam Ball HWM 100lb","Banda Suspensi�n FullFit",
                                                                  "Smith PROmachine","Rep Timer HWM","Atril de Discos Ol�mpicos","Tenaza Preol�mpica (Par)",
                                                                  "Barras Paralelas 17cm HWM (Par)","Agarre Polea - Remo","Pack Bandas de Resistencia HWM",
                                                                  "Barra Ol�mpica 17kg Eco Serie","Rack de Barras Suelo HWM","El�ptica Pro Obelix",
                                                                  "Anillas Ol�mpicas Madera HWM","Media Jaula de Potencia Amarilla HWM",
                                                                  "Barra Preol�mpica Mancuerna","Tabla de Escalada HWM","Expander Tube Soft FullFit",
                                                                  "Abs Straps HWM (Par)","Barras Paralelas 70cm HWM (Par)","Crossover Lat Pulldown PROmachine",
                                                                  "Press Banco Ol�mpico PROmachine","Landmine HWM","Polea Alta Hell Series HWM",
                                                                  "Press Banco Multifuncional PROmachine","Mini Tabla de Escalada HWM",
                                                                  "Polea Alta Remo PROmachine","Banco GHD HWM","Foam Roller HWM",
                                                                  "Reacondicionado - Trotadora Obelix GT7","Remo Contrapeso PROmachine",
                                                                  "Anillas Ol�mpicas Pvc HWM","Barra Preol�mpica Romana","Step Aer�bico FullFit",
                                                                  "Bumper Plate 25kg HWM (Par)","Bumper Plate 15kg HWM (Par)","Bumper Plate 5kg HWM (Par)",
                                                                  "Bumper Plate 10kg HWM (Par)","Atril de Sentadillas PROmachine (Par)",
                                                                  "Banco Inclinable Declinable Pro HWM","Body Pump Obelix","Bicicleta Spinning SP2P PROmachine",
                                                                  "Agarre Polea - Simple Una Mano","Barra Ol�mpica 20kg Training Serie HWM",
                                                                  "Speed Rope Pvc HWM","Press Banco Preol�mpico PROmachine","Banco Plano HWM",
                                                                  "Abdominal / Fondo PROmachine","Press de Tr�ceps MO Series Obelix","Polea Cruzada PROmachine",
                                                                  "Speed Rope Aluminio HWM","Set Bandas de Resistencia FullFit","Trotadora Obelix GT5",
                                                                  "Set 20 Body Pump Obelix","Extensi�n de Cuadriceps V8 Series Obelix","Abdominal V8",
                                                                  "Agarre Polea - Barra Corta","Banco Inclinable Declinable Eco PROmachine",
                                                                  "Sissy Squat PROmachine","Banco Lumbar PROmachine","Banco Abdominal Home PROmachine",
                                                                  "Banco Plano V8","Abdominal / Fondo V8","Polea Cruzada Corta V8","Polea Cruzada V8",
                                                                  "Atril de Sentadillas Jr Serie PROmachine (Par)","El�ptica GE1050 Obelix",
                                                                  "Bicicleta Est�tica Magn�tica Obelix","Remo Bajo MO Series Obelix",
                                                                  "Remo Sentado MO Series Obelix","Polea Cruzada Corta MO Series Obelix",
                                                                  "Trotadora Obelix GT7 Touch","Bicicleta Spinning Move Obelix","El�ptica Light Obelix",
                                                                  "El�ptica Stepper Light Obelix","Banco Vertical V8","Press Banco Declinado V8",
                                                                  "Rueda Abdominal Cin�tica","Bicicleta Spinning SP7 Obelix","Bicicleta Spinning SPX Obelix",
                                                                  "Step Aer�bico Pro HWM","2� Selecci�n - Air Bike Hurricane HWM","Banco Scott HWM",
                                                                  "Spotter Arms Hell Series HWM (Par)","Banco Hip Thrust Hell Series HWM","Squat Rack HWM",
                                                                  "Set Glute Bands FullFit","Banda de Resistencia 13mm HWM",
                                                                  "Banco Inclinable Declinable Hell HWM","Banco Inclinable Declinable 2.0 PROmachine",
                                                                  "Air Bike Hurricane HWM","Banda de Resistencia 22mm HWM","Ab Mat HWM",
                                                                  "Atril De Sentadillas Madefe Sport (Par)"))]="Complementos deportivos"


todaLaInformacionn$grupos[which(todaLaInformacionn$productos %in% c("Magnesio 56 gr.",
                                                                  "Bola de Masaje Lacrosse HWM","M�scara de Hipoxia PROmachine",
                                                                  "Bal�n Medicinal 10kg Sin Bote HWM","Press de Hombro MO Series Obelix","Smith V8",
                                                                  "Bal�n Medicinal 3kg Sin Bote HWM","Pack Functional Workout","Pack Agility Pro",
                                                                  "Pistola de Masaje HWM","Bal�n Medicinal Black 5kg HWM","Bal�n Medicinal Black 3kg HWM",
                                                                  "Bal�n Medicinal Black 7kg HWM","Pack Functional Workout Full",
                                                                  "Bal�n Medicinal 7kg Sin Bote HWM","Bal�n Medicinal 9kg Sin Bote HWM",
                                                                  "TRX Force Kit Tactical","TRX Pro Kit","Bal�n Medicinal Black 9kg HWM",
                                                                  "Bal�n Medicinal Black 12kg HWM","Leg Curl HWM"))]="Cuidados"



todaLaInformacionn$grupos[which(todaLaInformacionn$productos %in% c("Shaker Black 600ml PesasChile",
"Kettlebell Acero 4kg HWM","Prensa Inclinada 45� T8 Series Obelix",
"Curl Femoral Acostado MO Series Obelix","Extensi�n de Tr�ceps MO Series Obelix",
"Curl de B�ceps MO Series Obelix","Atril de Discos MO Series Obelix",
"Contractora de Pectoral MO Series Obelix","Press de Pectoral MO Series Obelix",
"Curl de B�ceps T8 Series Obelix","Curl de B�ceps T8 Series Black/Yellow Obelix",
"Pack 31kg Balones Medicinales Sin Bote HWM","Pull Up Dip Bar HWM",
"Atril Discos y Barra Ol�mpica Transportable HWM","Barra Ol�mpica Hexagonal HWM",
"Barra Ol�mpica 15kg Monster Serie HWM",'Barra Ol�mpica "Z"',"Expander Tube Hard FullFit",
"Barra Ol�mpica 7.5kg Technique Serie HWM","Barra Ol�mpica Fat HWM","Soporte Para Discos HWM",
"Jaula Retr�ctil Modular HWM","Mancuerna Preol�mpica 7.8kg Barbell (Unidad)",
"Mancuerna Preol�mpica 11.8kg Barbell (Unidad)","Mancuerna Preol�mpica 13.8kg Barbell (Unidad)",
"Disco Equilibrio Madera HWM","Caj�n Pliom�trico Madera HWM",
"Peso Tobillo Morado 1.5kg FullFit (Par)","Mancuerna Preol�mpica 5.8kg Barbell (Unidad)",
"Mancuerna Preol�mpica 15.8KG Barbell (Unidad)","Mancuerna Preol�mpica 9.8kg Barbell (Unidad)",
"Barra Dominadas Multi Grip HWM","Set 10 Pares Mancuernas Uretano Obelix",
"Barra Ol�mpica Corta 1.5mt","Barra Ol�mpica 15kg Training Serie HWM",
"Set 10 Barras Peso Fijo Obelix","2� Selecci�n - Barra Ol�mpica Log HWM",
"Caj�n Pliom�trico Acolchado HWM","Peso Tobillo Gris 2kg FullFit (Par)",
'Barra Preol�mpica "W"','Barra Preol�mpica "Z"',"Disco Ol�mpico Acero 2.5kg (Par)",
"Disco Ol�mpico Acero 5kg (Par)","Disco Ol�mpico Acero 10kg (Par)","Kettlebell Vinilo 8kg",
"Kettlebell Vinilo 6kg","Kettlebell Vinilo 12kg","Kettlebell Vinilo 4kg",
"Kettlebell Vinilo 10kg","Peso Tobillo Gris 2.5kg FullFit (Par)",
"Mancuerna Preol�mpica 21.8kg Barbell (Unidad)","Mancuerna Preol�mpica 17.8kg Barbell (Unidad)",
"Mancuerna Hexagonal 5kg HWM (Par)","Mancuerna Hexagonal 25kg HWM (Par)",
"Mancuerna Hexagonal 30kg HWM (Unidad)",'Barra Ol�mpica "W"',"Barra Ol�mpica Romana",
"Mancuerna Preol�mpica 25.8kg Barbell (Unidad)","Sandbag 10kg HWM",
"Mancuerna Hexagonal 35kg HWM (Unidad)","Disco Preol�mpico 15kg Barbell (Par)",
"Disco Preol�mpico 10kg Barbell (Par)","Disco Preol�mpico 20kg Barbell (Par)",
"Pantorillera MO Series Obelix","Barra Dominadas Puerta PROmachine",
"Rack de Barras Pared Vertical HWM","Crumb Bumper Plate 10lb HWM (Par)",
"Crumb Bumper Plate 25lb HWM (Par)","Disco Preol�mpico 3kg Barbell (Par)",
"Barra Ol�mpica Multi Agarre HWM","Peso Tobillo Morado 2kg PROmachine (Par)",
"Peso Tobillo Morado 2.5kg FullFit (Par)","Barra Dominadas Recta HWM",
"Press de Pectoral V8","Press de Tr�ceps V8","Prensa Horizontal V8",
"Press Banco Plano V8","Pantorrillera V8","Atril de Discos V8","Disco Ol�mpico Grip 5kg (Par)","Disco Ol�mpico Grip 10kg (Par)","Disco Ol�mpico Grip 15kg (Par)","Disco Ol�mpico Grip 15kg (Par)","Disco Ol�mpico Grip 20kg (Par)","Set 10 Body Pump Obelix","Mancuerna Preol�mpica 19.8kg Barbell (Unidad)","Mancuerna Preol�mpica 23.8kg Barbell (Unidad)","Mancuerna Preol�mpica 27.8kg Barbell (Unidad)","Mancuerna Preol�mpica 29.8kg Barbell (Unidad)","Mancuerna Preol�mpica  31.8kg Barbell (Unidad)","Disco Ol�mpico 5kg Competici�n HWM (Par)","Disco Ol�mpico 25kg Competici�n HWM","Kettlebell Acero 28kg HWM","Kettlebell Acero 32kg HWM","Disco Ol�mpico Grip 25kg (Par)","Disco Ol�mpico ZV 2.5kg (Par)","Disco Ol�mpico ZV 5kg (Par)","Disco Ol�mpico ZV 10kg (Par)","Disco Ol�mpico ZV 15kg (Par)","Disco Ol�mpico ZV 20kg (Par)","Disco Ol�mpico ZV 25kg (Par)","Set 20kg Mancuernas ECO","Barra Preol�mpica Recta 1.5mt","Disco Ol�mpico Acero 15kg (Par)","Disco Ol�mpico Acero 20kg (Par)","Kettlebell Vinilo 16kg","Kettlebell Vinilo 20kg","Mancuerna Hexagonal 15kg HWM (Par)","Bumper Plate 20kg HWM (Par)","Disco Ol�mpico Acero 1.25kg (Par)","Agarre Polea - Barra Amplia","Sandbag 15kg HWM","Disco Preol�mpico 2.5kg Barbell (Par)","Disco Ol�mpico 5kg PROmachine (Par)","Barra Ol�mpica 20kg PROmachine Serie","Disco Ol�mpico Grip 1.25kg (Par)","Disco Ol�mpico Grip 2.5kg (Par)","Disco Ol�mpico 10kg Competici�n HWM (Par)","Disco Ol�mpico 15kg Competici�n HWM (Par)","Disco Ol�mpico 20kg Competici�n HWM","Barra Ol�mpica 25lb C-160 Serie HWM","Crumb Bumper Plate 55lb HWM (Par)","Kettlebell Acero 6kg HWM","Set 30kg Mancuernas ECO","Disco Preol�mpico 1kg Barbell (Par)","Mancuerna Hexagonal 7.5kg HWM (Par)","Sandbag 20kg HWM","Sandbag 25kg HWM","Pack 70kg Sandbags HWM","Disco Ol�mpico 1.5kg Fraccional HWM (Par)","Barra Ol�mpica 35lb C-175 Serie HWM","Kettlebell Acero 24kg HWM","Barra Ol�mpica 15kg Elite Serie HWM","Disco Ol�mpico 0.5kg Fraccional HWM (Par)","Disco Ol�mpico 2kg Fraccional HWM (Par)","Crumb Bumper Plate 15lb HWM (Par)","Crumb Bumper Plate 35lb HWM (Par)","Kettlebell Acero 8kg HWM","Mancuerna Hexagonal 10kg HWM (Par)","Disco Preol�mpico 2kg Barbell (Par)","Disco Ol�mpico 1kg Fraccional HWM (Par)","Disco Ol�mpico 2.5kg Fraccional HWM (Par)","Barra Ol�mpica 20kg Monster Serie HWM","Mancuerna Hexagonal 22.5kg HWM (Par)","Mancuerna Hexagonal 2.5kg HWM (Par)","Kettlebell Acero 20kg HWM","Crumb Bumper Plate 45lb HWM (Par)","Kettlebell Acero 16kg HWM","Mancuerna Hexagonal 12.5kg HWM (Par)","Mancuerna Hexagonal 20kg HWM (Par)","Mancuerna Hexagonal 17.5kg HWM (Par)","Kettlebell Acero 10kg HWM","Barra Ol�mpica 20kg Elite Serie HWM","Servicio vendedor Pesas Chile","Mancuerna Hexagonal 5lb (Par)","Mancuerna Hexagonal 10lb (Par)","Mancuerna Hexagonal 15lb (Par)","Mancuerna Hexagonal 20lb (Par)","Mancuerna Hexagonal 25lb (Par)","Mancuerna Hexagonal 30lb (Par)","Mancuerna Hexagonal 35lb (Par)","Mancuerna Hexagonal 40lb (Par)","Mancuerna Hexagonal 5kg (Par)"))]="Pesas y mancuernas"


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
