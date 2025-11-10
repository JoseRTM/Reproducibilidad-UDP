##############################################################
# CLASE: Entornos reproducibles en R
# Objetivo: Aprender a fijar versiones de paquetes y documentar datos
##############################################################

# 1. instalar y activar
install.packages("renv")
renv::init()

# 2. para tu clase: que los snapshots sean explícitos
renv::settings$snapshot.type("explicit")

# 3. instalar lo que quieres congelar
renv::install("ggplot2")
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + geom_point()

# 4. tomar la foto OBLIGATORIA
renv::snapshot()

lock <- renv::lockfile_read("renv.lock")
lock$Packages$ggplot2
names(lock$Packages) 
