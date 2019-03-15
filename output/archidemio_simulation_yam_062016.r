# packages
library(rvle)
library(dplyr)
library(tidyr)
library(ggplot2)

# source("archidemio_fonctions.R")
rvle.shape <- function (
  object,  		# Objet de classe "rvle", après run
  nVarNormal = 2, # variables non Executive (sans les index de temps)
  nVarExec = 12,	# variables observées par modèle Executive
  nExec=400,  		# nombre de modèles créés par Executive
  view = "debug"  # nom de la vue active dans le modèle
) {
  
  if (class(object)!="list") {
    # durée de la simulation, attention que des variables soient bien observées sur cette durée 
    simLength=rvle.getDuration(object@sim) + 1
    # dataframe des sorties
    sim <- as.data.frame(object@outlist)
  } else {
    # dataframe des sorties (objet directement passé à la fonction)
    sim <- as.data.frame(object[[1]])
    # durée selon la taille du data_frame()
    simLength=dim(sim)[1]
  }
  
  # remplacer le code de date pour time
  sim$time <- 1:simLength
  
  # remplacer le nom des colonnes
  # names(sim) <- sub(".*\\.","", names(sim))
  
  # passage au format "long" : index = time | ThermalTime
  # Construction des index manquants : numero d'unité et type de variable (culture / unité) 
  # TODO index en fonction du nom de colonne
  # TODO detecter la vue active dans le modèle pour éviter les if
  if (view=="debug") {
    # m <- gather(sim, variable, value, -time)
    m <- reshape2::melt(sim, id=names(sim)[c(1,4)])
    unit <- c(rep(rep(NA, each=simLength), each=nVarNormal), rep(rep(1:nExec, each=simLength), each=nVarExec)) 
    scale <- c(rep("crop",nVarNormal*simLength), rep("unit", simLength*nVarExec*nExec))
    # Tout rassembler dans un dataframe
    d <- data.frame(
      time=m$time,
      ThermalTime=m[,4], 
      scale=as.factor(scale), 
      variable=sub(".*\\.","", m$variable), 
      unit=as.factor(unit), 
      value=m$value
    )
  }
  
  if (view=="sensitivity") {
    m <- reshape2::melt(sim, id="time") 
    unit <- c(rep(rep(1:nExec, each=simLength), each=1)) 
    # Tout rassembler dans un dataframe
    d <- data.frame(
      time=m$time,
      variable=sub(".*\\.","", m$variable), 
      unit=as.factor(unit), 
      value=m$value
    )	
  }
  
  return(d)
}

# simulation

# load model
archidemio <- new("Rvle", file="yam.vpz", pkg="archidemio")	

# raw output from rvle
data_output <- archidemio %>% run() %>% results() %>% rvle.shape()

# test plot

data_all <- data_output %>% filter(variable=="ScoreArea")
data_mean <- data_all %>% group_by(time) %>% summarise(value=mean(value))

ggplot(data=data_all, aes(x=time, y=value, group=unit)) +
  geom_line(alpha=0.1) +
  geom_line(data=data_mean, group=1, color="red") +
  theme_bw(base_size = 12, base_family = "Helvetica")

rvle.shape.grid <- function (object, nExec) {
  
  # Dataframe des sorties du mod?le
  sim <- as_data_frame(object)
  simLength=length(sim$time)
  
  # Dataframe au format long	
  m <- reshape2::melt(sim, id="time") 
  unit <- c(rep(rep(1:nExec, each=simLength), each=1)) 
  d <- data_frame(
    time=m$time,
    variable=sub(".*\\.","", m$variable), 
    unit=as.factor(unit), 
    value=m$value
  )	
  
  # Dataframe au format grille : valeur finale de la variable dynamique
  d <- data_frame(
    expand.grid(x=1:sqrt(nExec), y=1:sqrt(nExec)),
    aggregate(value ~ unit, data=d, max)
  )
  
  # Sortie
  return(d)
}

f <- rvle.open("yam.vpz", "archidemio")
rvle.setOutputPlugin(f, "vueSensitivity", "dummy")
rvle.setOutputPlugin(f, "vueDebug", "storage")

archidemio <- new("Rvle", file="yam.vpz", pkg="archidemio")	

d1D <- archidemio %>% run() %>% results() %>% rvle.shape(view="sensitivity", nExec=400)
d2D <- rvle.shape.grid(d1D, nExec=400)
