## Variable: Nivel de estudios

Queremos incorporar una variable cualitativa ordinal que represente el nivel de estudios de cada sección censal. Para cada una de ellas conocemos el porcenteje de población que ha alcanzado los siguientes niveles de estudios:

| Nombre de niveles de estudios	| Nivel máximo de educación | Nombre de la variable |
| --- | --- | --- |
| Primaria e inferior | Población analfabeta, con estudios primarios incompletos o completos | `edu_primaria` |
| Primera etapa de Educación Secundaria y similar | Hasta 3º ESO, o FB Básica | `edu_eso` |
| Segunda etapa de Educación Secundaria y Postsecundaria no Superior | 	4º ESO, Bachillerato, o FP Media | `edu_bachiller` |
| Educación Superior | 	Grado universitario, posgrado, o FP Superior | `edu_superior` |

En un primer momento íbamos a tomar como representante de cada sección el nivel de estudios más común en esta: sin embargo, creemos que simplifica en exceso la realidad social española: omite una gran parte de los valores disponibles, pudiendo hacer similares secciones que solo se parecen en el nivel más común, por ejemplo. En vez de eso, creamos un índice compuesto que pondera cada categoría de la siguiente manera:

` años_medios_de_estudios <- (
    4.5 * edu_primaria + 9 * edu_eso + 12 *
      edu_bachiller + 16 * edu_superior
  ) / poblacion_de_mas_de_15_años `

Para asignar los valores de las ponderaciones atendemos al nivel mínimo de años de estudio que se asocian a cada nivel de estudios contemplado en la CNED-2014 (Clasificación Nacional de Educación, clasificación oficial del INE), y calculamos la media para nuestras categorías agregadas atendiendo a los niveles de estudios de la población en España de más de 15 años en 2023:

<table border="1" style="border-collapse: collapse; width: 100%; font-family: sans-serif;">
  <thead>
    <tr style="background-color: #f2f2f2;">
      <th align="left">Nivel máximo de estudios según CNED-2014</th>
      <th>Frecuencia absoluta</th>
      <th>Años mínimos de estudio</th>
      <th>Agregados</th>
      <th>Frecuencia agregada</th>
      <th>Años de estudio (ponderados)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Analfabetos</td>
      <td align="right">108.650</td>
      <td align="center">0</td>
      <td rowspan="3" align="center">Primaria e inferior</td>
      <td rowspan="3" align="right">6.412.483</td>
      <td rowspan="3" align="center">4,5</td>
    </tr>
    <tr>
      <td>Estudios primarios incompletos</td>
      <td align="right">1.532.622</td>
      <td align="center">0</td>
    </tr>
    <tr>
      <td>Educación primaria</td>
      <td align="right">4.771.211</td>
      <td align="center">6</td>
    </tr>
    <tr>
      <td>Primera etapa de Educación Secundaria y similar</td>
      <td align="right">12.377.388</td>
      <td align="center">9</td>
      <td align="center">Primera etapa de Educación Secundaria y similar</td>
      <td align="right">12.377.388</td>
      <td align="center">9</td>
    </tr>
    <tr>
      <td>Segunda etapa de educación secundaria, con orientación general</td>
      <td align="right">5.327.633</td>
      <td align="center">12</td>
      <td rowspan="3" align="center">Segunda etapa de Educación Secundaria y Postsecundaria no Superior</td>
      <td rowspan="3" align="right">9.103.971</td>
      <td rowspan="3" align="center">12,0</td>
    </tr>
    <tr>
      <td>Segunda etapa de educación secundaria con orientación profesional</td>
      <td align="right">3.704.545</td>
      <td align="center">12</td>
    </tr>
    <tr>
      <td>Educación postsecundaria no superior</td>
      <td align="right">71.793</td>
      <td align="center">12</td>
    </tr>
    <tr>
      <td>Enseñanzas de formación profesional, artes plásticas y diseño y deportivas de grado superior y equivalentes</td>
      <td align="right">3.760.118</td>
      <td align="center">14</td>
      <td rowspan="5" align="center">Educación superior</td>
      <td rowspan="5" align="right">13.654.304</td>
      <td rowspan="5" align="center">16,0</td>
    </tr>
    <tr>
      <td>Grados universitarios de hasta 240 créditos ECTS, diplomados universitarios y equivalentes</td>
      <td align="right">4.309.007</td>
      <td align="center">16</td>
    </tr>
    <tr>
      <td>Grados universitarios de más de 240 créditos ECTS, licenciados y equivalentes</td>
      <td align="right">3.714.699</td>
      <td align="center">17</td>
    </tr>
    <tr>
      <td>Másteres, especialidades en Ciencias de la Salud por el sistema de residencia y similares</td>
      <td align="right">1.357.382</td>
      <td align="center">17</td>
    </tr>
    <tr>
      <td>Doctorado universitario</td>
      <td align="right">513.098</td>
      <td align="center">20</td>
    </tr>
    <tr style="font-weight: bold; background-color: #f9f9f9;">
      <td><strong>Total (15 años y mayores)</strong></td>
      <td align="right"><strong>41.548.146</strong></td>
      <td align="center"><strong>11,2</strong></td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
  </tbody>
</table>

Este índice de clasificación es similar al propuesto en la literatura de estos temas por figuras de autoridad (véase <i>A New Data Set of Educational Attainment in the World</i>, de Barro y Lee), y similar también a los índices que en España usan instituciones económicas como el Banco de España o el IVIE.

Para obtener nuestra variable cualitativa ordinal derivada, `nivel_estudios`, agrupamos las secciones en cuartiles ponderados por el total de población, de la siguiente manera:

 |  `nivel_estudios` |  	 Secciones en cuantil |  	 Población de más de 15 años |  	 Mínimo del cuantil |  	 Media del cuantil |  	 Máximo del cuantil 
 | :--- | ---: | ---: | ---: | ---: | ---: |
 | Bajo |  	 10.342 |  	 10.373.026 |  	 6,22 |  	 9,61 |  	 10,30 |  
 |  Bajo-Medio |  	 8.947 |  	 10.372.702 |  	 10,30 |  	 10,70 |  	 11,10  | 
 |  Medio-Alto |  	 8.370 |  	 10.371.473 |  	 11,10 |  	 11,60 |  	 12,20  | 
 |  Alto |  	 8.182 |  	 10.373.717 |  	 12,20 |  	 13,10 |  	 15,00  | 

## Variable: Nivel laboral

Seguimos un razonamiento similar al del apartado de estudios, solo que refiriéndonos a categorías laborales. Para las secciones censales solo tenemos tres grandes categorías de empleo, que agrupan por niveles (medio-alto, bajo, elemental) las diferentes categorías superiores recogidas en la CNO-11 (Clasificación Nacional de Ocupaciones, clasificación oficial del INE). Se propone el siguiente índice:

`elecciones$indice_laboral <- (
    elecciones$ocupados_nivel_bajo * 15 + elecciones$ocupados_nivel_medio *
      35 + elecciones$ocupados_nivel_alto * 70
  ) / elecciones$ocupados_total`

De igual manera que en el caso previo, asociamos un valor numérico a cada tipo de empleo según sus correspondencias con el Índice Socioeconómico Internacional (ISEI, por sus siglas en inglés), índice desarrollado por H. B. G.  Ganzeboom en torno a las categorías de la Organización Internacional del Trabajo, que se encuentra en armonía con INE. Este índice puntúa los diferentes trabajos según su estatus social: tomamos la ponderación según la estructura de ocupación española en 2023, y redondeamos para tener números más interpretables.

<table border="1" style="border-collapse: collapse; width: 100%; font-family: sans-serif;">
  <thead>
    <tr style="background-color: #f2f2f2;">
      <th align="left">Ocupación</th>
      <th>Frecuencia (2023)</th>
      <th>Puntuación ISEI</th>
      <th>Agregados</th>
      <th>Frecuencia agregada</th>
      <th>Puntuación ponderada</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Directores y gerentes</td>
      <td align="right">734.755</td>
      <td align="center">70</td>
      <td rowspan="3" align="center">Directores/gerentes y profesionales/técnicos de nivel medio o alto</td>
      <td rowspan="3" align="right">6.576.432</td>
      <td rowspan="3" align="center">68,68</td>
    </tr>
    <tr>
      <td>Técnicos y profesionales científicos e intelectuales</td>
      <td align="right">3.757.961</td>
      <td align="center">76</td>
    </tr>
    <tr>
      <td>Técnicos; profesionales de apoyo</td>
      <td align="right">2.083.716</td>
      <td align="center">55</td>
    </tr>
    <tr>
      <td>Empleados contables, administrativos y otros empleados de oficina</td>
      <td align="right">1.931.069</td>
      <td align="center">45</td>
      <td rowspan="5" align="center">Trabajadores cualificados y oficiales/operarios de nivel bajo</td>
      <td rowspan="5" align="right">9.906.693</td>
      <td rowspan="5" align="center">33,99</td>
    </tr>
    <tr>
      <td>Trabajadores de los servicios de restauración, personales, protección y vendedores</td>
      <td align="right">4.350.192</td>
      <td align="center">35</td>
    </tr>
    <tr>
      <td>Trabajadores cualificados en el sector agrícola, ganadero, forestal y pesquero</td>
      <td align="right">401.810</td>
      <td align="center">33</td>
    </tr>
    <tr>
      <td>Artesanos y trabajadores cualificados de las industrias manufactureras y la construcción (excepto operadores de instalaciones y maquinaria)</td>
      <td align="right">2.030.727</td>
      <td align="center">28</td>
    </tr>
    <tr>
      <td>Operadores de instalaciones y maquinaria, y montadores</td>
      <td align="right">1.192.895</td>
      <td align="center">23</td>
    </tr>
    <tr>
      <td>Ocupaciones elementales</td>
      <td align="right">2.497.680</td>
      <td align="center">16</td>
      <td align="center">Ocupaciones elementales</td>
      <td align="right">2.497.680</td>
      <td align="center">16,00</td>
    </tr>
    <tr style="font-weight: bold; background-color: #eeeeee;">
      <td><strong>Total ocupados</strong></td>
      <td align="right"><strong>18.980.805</strong></td>
      <td align="center"><strong>43,64</strong></td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
  </tbody>
</table>

Creamos nuestra variable ordinal `nivel_laboral` agrupando los valores en terciles ponderados por el número total de ocupados, que se describen aquí:

| `nivel_laboral` |	 Secciones en cuantil |  	 Ocupados |  	 Mínimo del cuantil |  	 Media del cuantil |  	 Máximo del cuantil |
| :--- | ---: | ---: | ---: | ---: | ---: |
| Bajo | 14.134 | 6.624.875 | 20,8 | 36,0 | 39,3 |
| Medio | 11.423 | 6.625.451 | 39,3 | 41,5 | 44,3 |
| Alto | 10.282 | 6.626.224 | 44,3 | 49,5 | 60,8 |

## `*** WIP a partir de aquí ***`

| Clasificación medio rural | Población (miles de habitantes) | Densidad (habitantes/km2) |
| --- | --- | --- |
|	Rural pequeño	|	<5	|	<100 |
|	Rural mediano	|	>=5 y <30	|	<100 |
|	Rural grande	|	>=30	|	<100 |
|	Urbano pequeño	|	<5	|	>=100 |
|	Urbano mediano	|	>=5 y <30	|	>=100 |
|	Urbano grande	|	>=30	|	>=100 |

	
| Nombre de niveles de estudios	| Nivel máximo de educación | 
| --- | --- | 
| Primera etapa de Educación Secundaria y similar | Entre 1º y 3º ESO, o FB Básica | 
| Segunda etapa de Educación Secundaria y Postsecundaria no Superior | 	Entre 4º ESO y Bachillerato, o FP Media | 
| Educación Superior | 	Grado, Máster o Doctorado, o FP Superior | 
	
| Terminología laboral	| Significado | 
| --- | --- | 
| Población en edad de trabajar (PET)	| Entre 16 y 64 años | 
| Población activa	| PET que trabaja o busca trabajo | 
| Población ocupada | PET que trabaja| 
| Población parada | PET que busca trabajo| 
| Ocupaciones elementales	| Empleados domésticos, recogedores de residuos, peones| 
	
*Elecciones*	
	
Se agrupa en "Abstención y similares" los datos de abstención, voto en blanco y nulos en cada sección. Los votos en blanco suelen considerarse votos protesta; los nulos suelen serlo también pero a veces son errores: la decisión de agruparlos con abstención es que son pocos proporcionalmente y no tienen significado claro. La categoría de "otros" sí recoge votos a partidos en todos los casos, por lo que sí recogen la intención del votante.	
	
| Renta	| | 
| --- | --- | 
| Neta	Después de impuestos (frente a "bruta", que es antes)| 
| Per cápita | 	Por persona| 
