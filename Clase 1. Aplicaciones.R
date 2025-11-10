##############################################################
# CLASE: Entornos reproducibles en R
# Objetivo: Aprender a fijar versiones de paquetes y documentar datos
##############################################################

# -----------------------------
# 1. Crear un nuevo proyecto reproducible
# -----------------------------
# Cuando trabajas en R, cada paquete (como dplyr, ggplot2, etc.) tiene una versión específica.
# Con el tiempo, esas versiones cambian y los resultados pueden variar o el código puede incluso dejar de correr.
# El paquete renv se encarga de guardar una foto exacta del entorno de trabajo de tu proyecto:
# qué paquetes tienes y qué versión de cada uno.

# Instalar 'renv' si no está
install.packages("renv")

# Inicializar un entorno reproducible en este proyecto
renv::init()
# Esto crea una carpeta /renv y un archivo renv.lock que guarda las versiones actuales de paquetes

# IMPORTANTÍSIMO: que guarde todo lo que usemos
renv::settings$snapshot.type("all")

############################################################
# 2. Instalar y usar el paquete que quieres congelar
############################################################

renv::install("ggplot2")

library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + geom_point()

############################################################
# 3. Tomar la foto (lockfile)
############################################################

# esto, con snapshot.type = "all", debería escribir ggplot2 en renv.lock
renv::snapshot()

# LEER el lock para comprobar
lock <- renv::lockfile_read("renv.lock")

# aquí deberías ver ggplot2
names(lock$Packages)
lock$Packages$ggplot2


############################################################
# 4. Romper el entorno eliminando el paquete
############################################################
renv::remove("ggplot2")

# comprobar que realmente se bajó
packageVersion("ggplot2")  

# renv debería decir que hay diferencias
renv::status()

############################################################
# 5. Restaurar lo que dice el lock
############################################################

renv::restore()

packageVersion("ggplot2")   # debería volver a la versión que estaba en el lock

# -----------------------------
# Ejemplo con 'rang' 
# -----------------------------
# Estos son útiles cuando necesitamos reproducir análisis de hace varios años.
# El paquete rang sirve para reconstruir cómo era el entorno de R (y sus paquetes) en una fecha pasada.
# Mientras que renv trabaja hacia adelante (fotografía tu entorno actual para reproducirlo luego),
# rang trabaja hacia atrás: permite revivir un entorno histórico.

#En investigación reproducible, a veces necesitamos repetir un análisis antiguo, 
# hecho hace 3 o 5 años, cuando:  
# la versión de R era distinta,
# algunos paquetes ya no existen o cambiaron,
# y los scripts ya no corren tal cual.

if (!requireNamespace("rang", quietly = TRUE)) install.packages("rang")
library(rang)

# Supongamos que tu análisis usaba estos paquetes:
pkgs <- c("dplyr", "ggplot2", "tidyr")

# Queremos reconstruir el entorno tal como estaba el 2022-01-15
snap <- rang::resolve(pkgs, snapshot_date = "2022-01-15")

snap

# exportar el entorno a un script de instalación
rang::export_rang(snap, "install_env.R")

# para luego instalar el entorno se usa este código
source("install_env.R")
# NO EJECUTAR
# NOTA: Se requiere tener RTools instalado.

# -----------------------------
# 6. Documentar los datos con 'codebook'
# -----------------------------

install.packages("codebookr")
library(codebookr)

# Ejemplo: dataset simulado
datos <- tibble(
  id = 1:5,
  edad = c(21, 35, 28, 44, 30),
  genero = factor(c("M", "F", "M", "F", "M")),
  consumo = c(10.5, 7.8, 9.2, 6.5, 8.3)
)

# Generar un codebook automático
codebook <- codebookr::codebook(datos)

# Exportar el codebook
print(codebook, "codebook_ejemplo.docx")

# -----------------------------
# 7. Buenas prácticas de cierre
# -----------------------------
# - Guardar el archivo renv.lock en control de versiones (Git)
# - No subir la carpeta /renv (solo el lockfile)
# - Compartir el script o .qmd junto con el codebook de datos
# - Documentar siempre fecha, versión de R y autor
##############################################################