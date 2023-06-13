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

# ---- Librairie ---- #

library(mapview)
library(sf)
library(stringr)
#fonction principale: mapview()

# On travaille sur Paris 


# Télécharger les données des arrondissements de Paris
arrondissements <- st_read(dsn = "https://opendata.paris.fr/explore/dataset/arrondissements/download/?format=geojson&timezone=Europe/Berlin&lang=fr")

# Convertir l'objet sf en un data.frame
arrondissements_df <- st_drop_geometry(arrondissements)

# Supprimer les colonnes qui ne sont pas nécessaires pour la représentation spatiale
cols_to_remove <- c("datasetid", "recordid", "fields", "geometry_name", "type")
arrondissements_df <- arrondissements_df[, !(names(arrondissements_df) %in% cols_to_remove)]


# Convertir les colonnes de type list en caractère
arrondissements_df <- lapply(arrondissements_df, function(x) if (is.list(x)) as.character(x) else x)

# Convertir la liste en data.frame
arrondissements_df <- as.data.frame(arrondissements_df)

#mes données sont sous la forme d'un vecteur c(a,b). Pour travailler plus simplement je travaille sur la colonne avec le vecteur en question pour créer deux colonnes latitude et longitude. 
# Exemple de données

df <- data.frame(id = c(1, 2), coordinates = arrondissements_df$geom_x_y)


# Diviser la chaîne de caractères à la virgule et extraire la deuxième partie
coord_vec <- as.data.frame(strsplit(df$coordinates, ","))
coord_vec[1,] <- gsub("c", "", coord_vec[1,]) # je supprime le c
coord_vec[1,] <- gsub('\\(', "", coord_vec[1,]) # je supprime la parenthèse
coord_vec[2,] <- gsub('\\)', "", coord_vec[2,])


coord_vec[1,]<- as.numeric(coord_vec[1,])

coord_vec[2,]<- as.numeric(coord_vec[2,])


coord_vec_tranposee<-t(coord_vec)

# Afficher le résultat
arrondissements_df$lon<-coord_vec_tranposee[,1]
arrondissements_df$lat<-coord_vec_tranposee[,2]


# Afficher les premières lignes du data.frame
arrondissements_sf <- st_as_sf(arrondissements_df, coords = c("lat","lon"), crs=4326)
mapview(arrondissements_sf, zcol = c("c_ar","l_aroff"))

# --- Library ---- #

library(oceanis)

# ---- On travaille toujours sur Paris ---- #

# création d'un groupe 
# Créer un vecteur d'identifiants de maille pour chaque arrondissement
c_ar <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")

code_groupe <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

# Créer un vecteur de noms officiels d'arrondissements avec les noms que vous avez fournis
l_aroff <- c("Louvre", "Bourse", "Temple", "Hôtel-de-Ville", "Panthéon", "Luxembourg", "Palais-Bourbon", "Élysée", "Opéra", "Enclos-St-Laurent", "Popincourt", "Reuilly", "Gobelins", "Observatoire", "Vaugirard", "Passy", "Batignolles-Monceau", "Buttes-Montmartre", "Buttes-Chaumont", "Ménilmontant")

# Créer un data.frame contenant les trois vecteurs
arrondissements_paris <- data.frame(c_ar, code_groupe, l_aroff)

# Afficher le tableau de données
print(arrondissements_paris)
# Créer le zonage à façon
ze13 <- zonage_a_facon(arrondissements, arrondissements_paris, "c_ar", "code_groupe", "l_aroff")

# Afficher la carte
plot(sf::st_geometry(ze13), col = "grey", border = "black", lwd = 1.5)
title("Zonage à façon des arrondissements de Paris")

# Ajouter le nom de chaque arrondissement à son centre 

# Créer un vecteur de noms d'arrondissements correspondant aux identifiants de chaque arrondissement
nom_arondissement <- arrondissements_paris$l_aroff[match(ze13$CODE_TERR, arrondissements_paris$code_groupe)]

# Ajouter le nom de chaque arrondissement à son centre
ze13$nom_arondissement <- nom_arondissement

# Afficher la carte avec les noms d'arrondissements

# On va colorier un arrondissement (celui ou j'habite en ce moment)

# Sélectionner le polygone correspondant à l'arrondissement 15
arrondissement_15 <- ze13[ze13$CODE_TERR == 15, ]

# Afficher la carte avec l'arrondissement 5 en rouge et les autres en gris
plot(sf::st_geometry(ze13), col = ifelse(ze13$CODE_TERR == 15, "red", "grey"), border = "black", lwd = 1.5)
text(sf::st_coordinates(st_centroid(ze13)), labels = ze13$nom_arondissement, cex = 0.8, col = "black")

title("Zonage à façon des arrondissements de Paris")











