# --- Cartographie avec R --- #

## ---- Librairies ---- ##
library(leaflet)

# Leaflet c'est une bibliothèque légère de cartographie
long <- 2.333333
lat  <- 48.866667
l <- leaflet() %>% addTiles() %>% 
  setView(long, lat,  zoom = 13)
l

# Pour l'INED : 48.909284°, 2.366624°

longi<-2.366624
lati<-48.909284

#mettre un marker

l %>% leaflet() %>% addTiles() %>% 
  setView(longi, lati,  zoom = 13)%>% addMarkers(longi, lati, 
                 popup = "AssessFirst", 
                 label = "9 Cr des Humanités, 93300 Aubervilliers")


l %>% leaflet() %>% addTiles() %>% 
  setView(longi, lati,  zoom = 13) %>%   addRectangles(
    lng1= 2.364, lat1=48.909280,
    lng2= 2.367, lat2=48.909284,
    fillColor = "transparent"
  )
l
# ||||||||||||||||||||| AUTRE METHODE |||||||||||||||||||| #
### Exemple pour aider clarisse (THAITI) ###
thaiti_edi<-st_read(dsn="C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/R_cartographie/clarisse/shp/SHP/Tahiti_total/export_20201002_115554_SHP/EDI")
thaiti_loc<-st_read(dsn="C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/R_cartographie/clarisse/shp/SHP/Tahiti_total/export_20201002_115554_SHP/LOC")
thaiti_ref<-st_read(dsn="C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/R_cartographie/clarisse/shp/SHP/Tahiti_total/export_20201002_115554_SHP/REF")
thaiti_rel<-st_read(dsn="C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/R_cartographie/clarisse/shp/SHP/Tahiti_total/export_20201002_115554_SHP/REL")
thaiti_voi<-st_read(dsn="C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/R_cartographie/clarisse/shp/SHP/Tahiti_total/export_20201002_115554_SHP/VOI")
thaiti_sol<-st_read(dsn="C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/R_cartographie/clarisse/shp/SHP/Tahiti_total/export_20201002_115554_SHP/SOL")
thaiti_hyd<-st_read(dsn="C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/R_cartographie/clarisse/shp/SHP/Tahiti_total/export_20201002_115554_SHP/HYD")
thaiti_nom<-st_read(dsn="C:/Users/cottet_cor/Documents/Stage/Exemple_quarto/R/R_cartographie/clarisse/shp/SHP/Tahiti_total/export_20201002_115554_SHP/NOM")



mf_map(x=thaiti_rel,pch = 20, cex = .7, col = "ivory4")

mf_map(x=thaiti_ref, lwd = .1, col = "ivory4", add= TRUE)



mf_map(x=thaiti_loc, lwd = .1, col = "ivory3", add = TRUE)


mf_map(x=thaiti_edi, lwd = 5, col = "white", add= TRUE)

mf_map(x=thaiti_voi, lwd=.3, col = "lightblue", add = TRUE)


mf_map(x=thaiti_sol,lwd=.3, col = "blue", add = TRUE)


mf_map(x=thaiti_nom,lwd=.3, col = "darkblue", add = TRUE)

mf_title("Première représentation cartographique de thaïti")

##################


# ---- Librairie ---- #


library(mapsf)
library(sf)

 ## -- code -- ##

## --- chargement des données -- ##

arrondissements <- st_read(dsn = "https://opendata.paris.fr/explore/dataset/arrondissements/download/?format=geojson&timezone=Europe/Berlin&lang=fr")
routes<-st_read(dsn= "https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/troncon_voie/exports/geojson?lang=fr&timezone=Europe%2FBerlin")
cours_deau<-st_read(dsn="https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/plan-de-voirie-voies-deau/exports/geojson?lang=fr&timezone=Europe%2FBerlin")




#les formats de mes fichiers sont geojson. 

mf_theme("default")
mf_arrow()
mf_map(x = arrondissements, border = "black")
mf_map(x=cours_deau,lwd=2,border="lightblue",col="lightblue",add=TRUE)
#mf_map(x=routes, lwd = 2, col = "grey", add = FALSE)



arrondissements<-st_join(x=arrondissements$geom_x_y=="c(48.8873265220258, 2.30677699057441)
", y=arrondissements$geom_x_y=='c(48.8561744287794, 2.31218769148201)')


# ---- fusionner deux polygones ---- #



# Sélectionner les deux polygones que vous souhaitez fusionner
poly1 <- arrondissements[arrondissements$l_aroff == "Passy", ]
poly2 <- arrondissements[arrondissements$l_aroff == "Vaugirard", ]

# Fusionner les polygones
new_poly <- st_union(poly1, poly2)

new_poly$surface<-new_poly$surface + new_poly$surface.1



new_poly$n_sq_co<-new_poly$n_sq_co + new_poly$n_sq_co.1

new_poly$perimetre<-new_poly$perimetre + new_poly$perimetre.1

####---------
#



new_poly$geom_x_y<-(new_poly$geom_x_y+new_poly$geom_x_y.1)

new_poly$n_sq_ar<-new_poly$n_sq_ar+new_poly$n_sq_ar.1

new_poly$l_aroff<-"un nouvel arrondissement"
new_poly$l_ar<-"75"

new_poly$c_ar<-"15+16"
new_poly$c_arinsee<-"75000"

arrondissements_test<- subset(new_poly, select = c(c_ar,l_aroff,surface, l_ar, n_sq_co,c_arinsee,n_sq_ar,perimetre,geom_x_y))
arrondissements[arrondissements$l_aroff=="Passy",]<-arrondissements_test








# Mettre à jour la géométrie du premier polygone dans la table
arrondissements[arrondissements$l_aroff == "Passy", ] <- new_poly

# Écrire la table mise à jour dans un nouveau fichier
st_write(ma_table, "chemin/vers/ma_table_fusionnee.shp")



'''
mf_label(
  x = arrondissements,
  cex= 2,
  var = "chiffres_romains",
  col= "black",
  halo = TRUE,
  overlap = FALSE, 
  lines = FALSE,
  offset = 0.5,
  )
'''
"Les cartes de symboles proportionnels sont utilisées pour représenter
les variables de stocks (variables quantitatives absolues, la somme et la moyenne ont un sens)."

# avec la population par exemple 
arrondissements$pop<-c(100196, 95487, 146699, 136591, 
            58850, 140849, 142462, 118238, 166860, 163445, 139992, 102163, 
            183399, 169214, 165494, 149500, 144657, 104287, 142005, 100831)
arrondissements$chiffres_romains <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
                      "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", "XX")

mf_map(
  x = arrondissements,
  var = "pop",
  type = "prop",
  leg_title = "Population totale\12 271 794",
  col="#F39487"
)
# Titre
mf_title("Distribution de la population dans le Lot")

"Les cartes choroplèthes sont utilisées pour représenter les variables de ratios 
(variables quantitatives relatives, la moyenne a un sens, la somme n’a pas de sens)."

# Densité de population (hab./km2) en utilisant la fonction sf::st_area()
arrondissements$DENS <- 1e6 * arrondissements$pop / as.numeric(st_area(arrondissements))
mf_theme("darkula")
arr_c<-st_centroid(arrondissements)
mf_shadow(x=arrondissements)
mf_map(x=arrondissements, border="white")

mf_map(
  x = arrondissements,
  var = "pop",
  type = "prop",
  leg_title = "Population totale\12 271 794",
  col="#F39487"
)

mf_title("Distribution de la population dans Paris")


mf_map(
  x = arrondissements,
  var = "DENS",
  type = "choro",
  breaks = "quantile",
  pal = "Reds",
  lwd = 1,
  leg_title = "Densité de population\n(habitants par km2)", 
  leg_val_rnd = 0
)
mf_map(x=cours_deau,lwd=2,border="lightblue",col="lightblue",add=TRUE)
mf_title("Distribution de la population dans Paris (2018)")








'''
mf_map(
  x = arr_c,
  var = "DENS",
  type = "choro",
  breaks = "quantile",
  pal = "Inferno",
  pch = 24,
  cex = 2,
  border = "#F29094",
  lwd = 1,
  leg_title = "Densité de population\n(habitants par km2)", 
  leg_val_rnd = 0,
  add= TRUE
)
'''
mf_map(x=cours_deau,lwd=2,border="lightblue",col="lightblue",add=TRUE)
mf_title("Distribution de la population dans Paris (2018)")























