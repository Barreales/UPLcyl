##############################################################################
################################################################################
############# SCRIPT PARA CONECTAR GITHUB CON R ################################
################################################################################

## ÚLtima actualización: 13 de mayo 2024

# 0. Instalación de paquetes
## install.packages("usethis") #paquete para la conexión
library(usethis)
## install.packages("gitcreds") #paquete para conseguir token
library(gitcreds)


# 1. Debemos conseguir un token de acceso, para ello ejecuta este código:
usethis::create_github_token()
## Abrirá una web. Tendrás que crear un token de acceso. Añade una breve descripción de su uso
## (p.e. "Acceso desde RStudio") y cambia la fecha de experación (30 días es suficiente)
## No hace falta cambiar nada más. Baja hasta el final y selecciona "create token". 
## Guarda este código, es el token que necesitamos.


# 2. Una vez tenemos el token, debemos conectarnos desde RStudio. Para eso, ejecuta este código.
gitcreds::gitcreds_set()
## Ante la pregunta: "Enter password or token"
## introducir el token copiado en el paso anterior

# En teoría ya estaría conectado. Puedes probar a subir algo al repositorio de tu equipo de trabajo
## (¡no al de la clase!). Recuerda: primero se hace selecciona el archivo que se quiere subir o cambiar
## en la pantalla git (arriba a la derecha), se hace un commit (con un comentario de los cambios)
## y después un push (fecha verde, hacia arriba) para subirlo a la web de github.

# x. También se pueden hacer los push, pull y commits usando código en vez de ventanas:

usethis::use_git() #subir un proyecto RStudio a github

## Un paso a paso más concreto de la instalación (con videos) puede verse en este enlace: 
# http://destio.us.es/calvo/asignaturas/ge_esco/tutorialusargitgithubrstudio/UsarGitGithubconRStudio.html