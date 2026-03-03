## Variable: Nivel de estudios

Queremos incorporar una variable cualitativa ordinal que represente el nivel de estudios de cada sección censal. Para cada una de ellas conocemos el porcenteje de población que ha alcanzado los siguientes niveles de estudios:

| Nombre de niveles de estudios	| Nivel máximo de educación | Nombre de la variable |
| --- | --- | --- |
| Primaria e inferior | Población analfabeta, con estudios primarios incompletos o completos | edu_primaria |
| Primera etapa de Educación Secundaria y similar | Hasta 3º ESO, o FB Básica | edu_eso |
| Segunda etapa de Educación Secundaria y Postsecundaria no Superior | 	4º ESO, Bachillerato, o FP Media | edu_bachiller |
| Educación Superior | 	Grado universitario, posgrado, o FP Superior | edu_superior |

En un primer momento íbamos a tomar como representante de cada sección el nivel de estudios más común en esta: sin embargo, creemos que simplifica en exceso la realidad social española: omite una gran parte de los valores disponibles, pudiendo hacer similares secciones que solo se parecen en el nivel más común, por ejemplo. En vez de eso, creamos un índice compuesto que pondera cada categoría de la siguiente manera:

` años_medios_de_estudios <- (
    4.5 * edu_primaria + 9 * edu_eso + 12 *
      edu_bachiller + 16 * edu_superior
  ) / poblacion_de_mas_de_15_años `

Para asignar los valores de las ponderaciones atendemos al nivel mínimo de años de estudio que se asocian a cada nivel de estudios desagregado, y calculamos la media para nuestras categorías agregadas atendiendo a los niveles de estudios de la población en España de más de 15 años en 2023:

<table border="1" style="border-collapse: collapse; width: 100%; font-family: sans-serif;">
  <thead>
    <tr style="background-color: #f2f2f2;">
      <th align="left">Nivel máximo de estudios</th>
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
      <td>Total (15 años y mayores)</td>
      <td align="right">41.548.146</td>
      <td align="center">11,2</td>
      <td></td>
      <td></td>
      <td></td>
    </tr>
  </tbody>
</table>

Este índice de clasificación es similar al propuesto en la literatura de estos temas por figuras de autoridad (Barro, R. J., & Lee, J. W. (2013). A new data set of educational attainment in the world, 1950–2010. Journal of Development Economics, 104, 184-198.), y similar también a los índices que en España usan instituciones económicas como FEDEA o el IVIE.

Para obtener nuestra variable cualitativa ordinal derivada, nivel_estudios, agrupamos las secciones en cuartiles ponderados por el total de población, de la siguiente manera:

 |  nivel_estudios |  	 Secciones en cuantil |  	 Población de más de 15 años |  	 Mínimo del cuantil |  	 Media del cuantil |  	 Máximo del cuantil 
 | --- | --- | --- | --- | --- | --- |
 | Bajo |  	 10.342 |  	 10.373.026 |  	 6,22 |  	 9,61 |  	 10,30 |  
 |  Bajo-Medio |  	 8.947 |  	 10.372.702 |  	 10,30 |  	 10,70 |  	 11,10  | 
 |  Medio-Alto |  	 8.370 |  	 10.371.473 |  	 11,10 |  	 11,60 |  	 12,20  | 
 |  Alto |  	 8.182 |  	 10.373.717 |  	 12,20 |  	 13,10 |  	 15,00  | 

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
