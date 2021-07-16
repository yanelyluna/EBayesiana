########### ESTADÍSTICA BAYESIANA ######################
############### PROYECTO ###############################

# Cargar librerías -------------
library(ggplot2)
# Cargar datos ---------------
heart_failure <- read.csv("Proyecto/datos/heart_failure_clinical_records_dataset.csv")
str(heart_failure) #299 obs. of  13 variables
#1. age                      ---- edad
#2. anaemia                  ---- decremento de células rojas (boolean)
#3. creatinine_phosphokinase ---- nivel de enzima CPK en la sangre
#4. diabetes                 ---- si el paciente tiene diabetes (boolean)
#5. ejection_fraction        ---- % de sangre que deja el corazón en cada contracción
#6. high_blood_pressure      ---- si el paciente tiene hipertensión (boolean)
#7. platelets                ---- plaquetas en la sangre
#8. serum_creatinine         ---- nivel de serum creatinine en la sangre   
#9. serum_sodium             ---- nivel de serum sodio en la sangre
#10. sex                     ---- Mujer u hombre (factor)
#11. smoking                 ---- si el paciente fuma o no (boolean)
#12. time                    ---- días de seguimiento del paciente
#13. DEATH_EVENT             ---- si el paciente murió durante el periodo de seguimiento
heart_failure$anaemia <- as.factor(heart_failure$anaemia)
heart_failure$diabetes <- as.factor(heart_failure$diabetes)
heart_failure$high_blood_pressure <- as.factor(heart_failure$high_blood_pressure)
heart_failure$sex <- as.factor(heart_failure$sex)
heart_failure$smoking <- as.factor(heart_failure$smoking)
heart_failure$DEATH_EVENT <- as.factor(heart_failure$DEATH_EVENT)

attach(heart_failure)
####### ANÁLISIS DESCRIPTIVO ###############

# time --------------
summary(time)
hist(time)

# DEATH_EVENT --------
table(DEATH_EVENT)
barplot(table(DEATH_EVENT))

# age --------
plot(age,DEATH_EVENT)

