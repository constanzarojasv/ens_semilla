Table: Modelos de Cox para muerte por cáncer

|modelo          |term                 |    HR| IC_inf| IC_sup| p.value|
|:---------------|:--------------------|-----:|------:|------:|-------:|
|Crudo           |af_cancer_binariaYes | 2.539|  1.556|  4.144|   0.000|
|Edad            |af_cancer_binariaYes | 1.868|  1.104|  3.161|   0.020|
|Edad            |edad                 | 1.081|  1.063|  1.099|   0.000|
|Edad+Sexo       |af_cancer_binariaYes | 1.941|  1.149|  3.281|   0.013|
|Edad+Sexo       |edad                 | 1.083|  1.064|  1.103|   0.000|
|Edad+Sexo       |sexoFemale           | 0.643|  0.392|  1.054|   0.080|
|Modelo completo |af_cancer_binariaYes | 2.103|  1.232|  3.589|   0.006|
|Modelo completo |edad                 | 1.083|  1.062|  1.104|   0.000|
|Modelo completo |sexoFemale           | 0.558|  0.333|  0.935|   0.027|
|Modelo completo |nedu8-12 years       | 0.511|  0.277|  0.942|   0.032|
|Modelo completo |nedu>12 years        | 0.503|  0.185|  1.366|   0.178|
|Modelo completo |zonaRural            | 1.196|  0.650|  2.202|   0.564|
|Modelo completo |AUDITYes             | 1.070|  0.454|  2.520|   0.878|
|Modelo completo |fumaSmoker           | 1.587|  0.868|  2.901|   0.133|
|Modelo completo |imc                  | 1.008|  0.965|  1.053|   0.714|
|Modelo completo |GPAQModerate         | 0.637|  0.330|  1.231|   0.180|
|Modelo completo |GPAQHigh             | 0.762|  0.421|  1.378|   0.368|
