Table: Modelos de Cox para incidencia de cáncer

|modelo          |term                 |    HR| IC_inf| IC_sup| p.value|
|:---------------|:--------------------|-----:|------:|------:|-------:|
|Crudo           |af_cancer_binariaYes | 2.140|  1.329|  3.448|   0.002|
|Edad            |af_cancer_binariaYes | 1.557|  0.921|  2.632|   0.099|
|Edad            |edad                 | 1.082|  1.065|  1.099|   0.000|
|Edad+Sexo       |af_cancer_binariaYes | 1.619|  0.963|  2.720|   0.069|
|Edad+Sexo       |edad                 | 1.084|  1.067|  1.102|   0.000|
|Edad+Sexo       |sexoFemale           | 0.612|  0.377|  0.995|   0.048|
|Modelo completo |af_cancer_binariaYes | 1.790|  1.067|  3.004|   0.027|
|Modelo completo |edad                 | 1.085|  1.066|  1.105|   0.000|
|Modelo completo |sexoFemale           | 0.580|  0.353|  0.956|   0.033|
|Modelo completo |nedu8-12 years       | 0.464|  0.256|  0.841|   0.011|
|Modelo completo |nedu>12 years        | 0.502|  0.197|  1.282|   0.150|
|Modelo completo |zonaRural            | 1.232|  0.719|  2.111|   0.448|
|Modelo completo |AUDITYes             | 1.342|  0.557|  3.229|   0.512|
|Modelo completo |fumaSmoker           | 1.445|  0.805|  2.595|   0.217|
|Modelo completo |imc                  | 0.997|  0.955|  1.040|   0.875|
|Modelo completo |GPAQModerate         | 0.666|  0.358|  1.238|   0.199|
|Modelo completo |GPAQHigh             | 0.889|  0.497|  1.592|   0.693|
