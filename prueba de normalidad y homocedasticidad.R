library(readr)
Datos <- read_csv("Datos_experimento_tratamientos.csv")
View(Datos)


###
mod1=lm(Differencia~Muestra, data = Datos)
pre= predict(mod1)
t1= Datos$Differencia-pre+mean(Datos$Differencia)

boxplot(Differencia~Asignacion, ylim= c(-1,2), xlab= "metodo de eliminacion", 
        ylab = "diferencia", data = Datos)


Datos$Muestra <- as.factor(Datos$Muestra)
Datos$Asignacion <- as.factor(Datos$Asignacion)
Datos$factor_no_diseño <- as.factor(Datos$factor_no_diseño)

# 3. Aplicar el test de Bartlett
# Evaluando si las varianzas de la 'Differencia' son iguales entre los grupos de 'Asignacion'
resultado_bartlett <- bartlett.test(Differencia ~ Asignacion, data = Datos)

# 4. Mostrar los resultados en consola
print(resultado_bartlett)



#### todo esta parte es una prueba no oficial### 

library(nlme)
library(car)


mod1=lme(Differencia~Asignacion*factor_no_diseño, random =  ~1|Muestra, data = Datos)

qqPlot(residuals(mod1))
shapiro.test(residuals(mod1))
ks.test(residuals(mod1), "pnorm", mean = 0, sd = sd(residuals(mod1)))
library(readr)
Datos <- read_csv("Datos_experimento_tratamientos.csv")
View(Datos)


