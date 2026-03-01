## 1. Conectar con GitHub

Para poder editar el código de este proyecto y que los cambios se sincronicen entre nosotros, necesitamos configurar nuestros equipos individuales. Solo hace falta hacerlo 1 vez.

**Paso 1: Crear una cuenta en GitHub**
- Entra en GitHub.com y haz clic en `Sign up`.
- Crea tu cuenta.
- Pásame tu nombre de usuario para que yo te añada como "Colaborador" oficial en el proyecto. Te llegará un email de invitación; acéptala.

**Paso 2: Instalar Git**: RStudio necesita este programa de fondo para poder enviar archivos a internet.
- Ve a la página oficial de Git y descarga la versión para tu sistema (Windows o Mac).
- Instálalo dándole a `Next` a todo. No cambies ninguna opción.
- Cierra RStudio si lo tenías abierto y vuélvelo a abrir para que detecte el motor nuevo.

**Paso 3: Traer el proyecto a tu PC (Clonar)**
- Ve a nuestro repositorio en GitHub. Haz clic en el botón verde "<> Code" y copia la dirección web (termina en .git).
- Abre RStudio. Ve a `File` > `New Project`...
- Selecciona `Version Control` > `Git`.
- Pega la dirección que copiaste, elige en qué carpeta de tu ordenador quieres guardarlo y dale a `Create Project`.

**Paso 4: El Token**: GitHub ya no deja usar la contraseña normal desde RStudio por seguridad. Necesitas un "Token".
- En RStudio, ejecuta este código en la Consola de abajo:

`install.packages("usethis")`
`install.packages("gitcreds")`

- Esto abrirá GitHub en tu navegador para crear el Token

`usethis::create_github_token()`

- En la web que se abre, baja del todo y dale al botón verde `Generate token`. Busca en la página su duración, y márcala como indefinida.
- Copia la contraseña larguísima que aparece (empieza por ghp_...). Ten cuidado porque solo podrás verla una vez, como indica la we.b

Vuelve a RStudio y ejecuta:

`gitcreds::gitcreds_set()`
Pega tu Token en la consola y dale a `Enter`. ¡Listo!

## 2. El Ciclo de Trabajo

A partir de ahora, cada vez que quieras trabajar en el código, haz lo siguiente en la pestaña `Git` de RStudio:

- Al empezar:
  - Abre el archivo desde RStudio o dándole al archivo .RProj. NO le des al archivo individual .R que quieras trabajar.
  - Flecha Azul (`Pull`): Haz clic aquí SIEMPRE al empezar a trabajar para bajarte del repositorio en GitHub los cambios que hayamos hecho el resto.
- Trabajando:
  - Haz tus cambios como los harías normalmente.
  - Dale a Guardar (Ctrl + S)
  - Marca `Stage`: En la pestaña Git, marca la casilla de los archivos que has cambiado.
  - `Commit`: Haz clic en `Commit`, escribe un mensaje corto explicando qué has hecho (ej: "Añadido gráfico de barras") y dale a `Commit`.
  - Flecha Verde (`Push`): Haz clic aquí para subir tus cambios a la nube. El resto veremos en la pestaña Git los cambios realizados, y podremos hacer `Pull` de ellos.

Si el botón de `Pull` falla se suele deber a que otro ha editado el mismo archivo simultáneamente y lo ha modificado, así que comenta por el grupo lo que está orcurriendo para que lo solucionemos en conjunto.
