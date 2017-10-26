#AGENT LOGIC

#AGENT LOGIC SET UP
#1. 3 assets (undergoing failure/repair in simulation)
#2. data for 5 attributes (real time & static) are randomly generated
#3. neural networks computed using real time& static to predict modelled attributes: Processing time, Failure time and Repair time for each of the 3 assets.

#--------------------------------------------------------------------------------------------------------------------------%
#--------------------------------------------------------------------------------------------------------------------------%

install.packages("neuralnet")
library(neuralnet)


Onshore_Air_flow<- c(runif(100, 10,15))
Onshore_Asset_life<- c(sample(5:10,100, replace=TRUE))
Onshore_Detected_differential_temperature<- c(runif(100,20,30))
Onshore_Detected_differential_pressure<- c(runif(100, 20,30))
Onshore_Gas_input<- c(sample(1:5,100, replace=TRUE))
Onshore_Processing_time<-(Onshore_Air_flow/Onshore_Air_flow)+(Onshore_Asset_life/Onshore_Asset_life)+(Onshore_Detected_differential_temperature/Onshore_Detected_differential_temperature)+(Onshore_Detected_differential_pressure/Onshore_Detected_differential_pressure)+(Onshore_Gas_input/Onshore_Gas_input)
Onshore_Failure_time<-160*((Onshore_Air_flow/Onshore_Air_flow)+(Onshore_Asset_life/Onshore_Asset_life)+(Onshore_Detected_differential_temperature/Onshore_Detected_differential_temperature)+(Onshore_Detected_differential_pressure/Onshore_Detected_differential_pressure)+(Onshore_Gas_input/Onshore_Gas_input))
Onshore_Repair_time<-2*((Onshore_Air_flow/Onshore_Air_flow)+(Onshore_Asset_life/Onshore_Asset_life)+(Onshore_Detected_differential_temperature/Onshore_Detected_differential_temperature)+(Onshore_Detected_differential_pressure/Onshore_Detected_differential_pressure)+(Onshore_Gas_input/Onshore_Gas_input))

Liq_Air_flow<- c(runif(100, 10,15))
Liq_Asset_life<- c(sample(5:10,100, replace=TRUE))
Liq_Detected_differential_temperature<- c(runif(100,20,30))
Liq_Detected_differential_pressure<- c(runif(100, 20,30))
Liq_Gas_input<- c(sample(1:5,100, replace=TRUE))
Liq_Processing_time<-2*((Liq_Air_flow/Liq_Air_flow)+(Liq_Asset_life/Liq_Asset_life)+(Liq_Detected_differential_temperature/Liq_Detected_differential_temperature)+(Liq_Detected_differential_pressure/Liq_Detected_differential_pressure)+(Liq_Gas_input/Liq_Gas_input))
Liq_Failure_time<-20*((Liq_Air_flow/Liq_Air_flow)+(Liq_Asset_life/Liq_Asset_life)+(Liq_Detected_differential_temperature/Liq_Detected_differential_temperature)+(Liq_Detected_differential_pressure/Liq_Detected_differential_pressure)+(Liq_Gas_input/Liq_Gas_input))
Liq_Repair_time<-120*((Liq_Air_flow/Liq_Air_flow)+(Liq_Asset_life/Liq_Asset_life)+(Liq_Detected_differential_temperature/Liq_Detected_differential_temperature)+(Liq_Detected_differential_pressure/Liq_Detected_differential_pressure)+(Liq_Gas_input/Liq_Gas_input))



Dehydration_Air_flow<- runif(100, 10,15)
Dehydration_Asset_life<- sample(5:10,100, replace=TRUE)
Dehydration_Detected_differential_temperature<- c(runif(100,20,30))
Dehydration_Detected_differential_pressure<- c(runif(100, 20,30))
Dehydration_Gas_input<- c(sample(1:5,100, replace=TRUE))
Dehydration_Processing_time<-((Dehydration_Air_flow/Dehydration_Air_flow)+(Dehydration_Asset_life/Dehydration_Asset_life)+(Dehydration_Detected_differential_temperature/Dehydration_Detected_differential_temperature)+(Dehydration_Detected_differential_pressure/Dehydration_Detected_differential_pressure)+(Dehydration_Gas_input/Dehydration_Gas_input))
Dehydration_Failure_time<-160*((Dehydration_Air_flow/Dehydration_Air_flow)+(Dehydration_Asset_life/Dehydration_Asset_life)+(Dehydration_Detected_differential_temperature/Dehydration_Detected_differential_temperature)+(Dehydration_Detected_differential_pressure/Dehydration_Detected_differential_pressure)+(Dehydration_Gas_input/Dehydration_Gas_input))
Dehydration_Repair_time<-2*((Dehydration_Air_flow/Dehydration_Air_flow)+(Dehydration_Asset_life/Dehydration_Asset_life)+(Dehydration_Detected_differential_temperature/Dehydration_Detected_differential_temperature)+(Dehydration_Detected_differential_pressure/Dehydration_Detected_differential_pressure)+(Dehydration_Gas_input/Dehydration_Gas_input))

agent_logicdfl<- data.frame(Liq_Air_flow, Liq_Asset_life, Liq_Detected_differential_temperature, 
                            Liq_Detected_differential_pressure, Liq_Gas_input, Liq_Processing_time, 
                            Liq_Failure_time, Liq_Repair_time, Onshore_Air_flow, Onshore_Asset_life,
                            Onshore_Detected_differential_temperature, Onshore_Detected_differential_pressure,
                            Onshore_Gas_input, Onshore_Processing_time, Onshore_Failure_time, Onshore_Repair_time, Dehydration_Air_flow,
                            Dehydration_Asset_life, Dehydration_Detected_differential_temperature, Dehydration_Detected_differential_pressure
                            ,Dehydration_Gas_input,Dehydration_Processing_time, Dehydration_Failure_time, Dehydration_Repair_time)
#comment here to make a change



neural_1<-neuralnet(Liq_Processing_time ~ Liq_Air_flow + Liq_Asset_life+Liq_Detected_differential_temperature+Liq_Detected_differential_pressure+Liq_Gas_input, agent_logicdfl, hidden = 2, lifesign = "minimal", 
                            linear.output = TRUE, threshold = 0.1)
subsetdf<- subset(agent_logicdfl, select= c(Liq_Air_flow, Liq_Asset_life, Liq_Detected_differential_temperature, Liq_Detected_differential_pressure,Liq_Gas_input))
results<- compute(neural_1, subsetdf)

neural_2<- neuralnet(Liq_Failure_time~Liq_Air_flow+Liq_Asset_life+Liq_Detected_differential_temperature+Liq_Detected_differential_pressure+Liq_Gas_input,agent_logicdfl, hidden=2, lifesign="minimal",
                     linear.output=TRUE, threshold=0.1)
subset_2df<- subset(agent_logicdfl, select=c(Liq_Air_flow, Liq_Asset_life, Liq_Detected_differential_temperature, Liq_Detected_differential_pressure,Liq_Gas_input))
results_2<-compute(neural_2, subset_2df)

neural_3<- neuralnet(Liq_Repair_time~Liq_Air_flow+Liq_Asset_life+
                       Liq_Detected_differential_temperature+Liq_Detected_differential_pressure+
                       Liq_Gas_input,agent_logicdfl, hidden=2, lifesign="minimal",
                     linear.output=TRUE, threshold=0.1)
subset_3<-subset(agent_logicdfl, select=c(Liq_Air_flow, Liq_Asset_life, Liq_Detected_differential_temperature, Liq_Detected_differential_pressure,Liq_Gas_input))
results3<-compute(neural_3, subset_3)

neural_onshoreprocessing<- neuralnet(Onshore_Processing_time ~ Onshore_Air_flow+Onshore_Asset_life+Onshore_Detected_differential_temperature+Onshore_Detected_differential_pressure+Onshore_Gas_input, agent_logicdfl, hidden = 2, lifesign = "minimal", 
                            linear.output = TRUE, threshold = 0.1)

subset_onshoreprocessing<- subset(agent_logicdfl, select=c(Onshore_Air_flow, Onshore_Asset_life, Onshore_Detected_differential_temperature, Onshore_Detected_differential_pressure
                                                        , Onshore_Gas_input))
results_onshoreprocessing<- compute(neural_onshoreprocessing, subset_onshoreprocessing)
predictedvsactual <- data.frame(Liq_Air_flow, Liq_Asset_life, Liq_Detected_differential_temperature, Liq_Detected_differential_pressure, Liq_Gas_input, Liq_Processing_time, Liq_predicted_processing_time = results$net.result, Liq_predicted_failure_time=results_2$net.result, Liq_predicted_repair_time=results$net.result, Onshore_predicted_processing_time=results_onshoreprocessing$net.result)

neural_onshorefailure<- neuralnet(Onshore_Failure_time ~ Onshore_Air_flow+Onshore_Asset_life+Onshore_Detected_differential_temperature+Onshore_Detected_differential_pressure+Onshore_Gas_input, agent_logicdfl, hidden = 2, lifesign = "minimal", 
                                     linear.output = TRUE, threshold = 0.1)

subset_onshorefailure<- subset(agent_logicdfl, select=c(Onshore_Air_flow, Onshore_Asset_life, Onshore_Detected_differential_temperature, Onshore_Detected_differential_pressure
                                                           , Onshore_Gas_input))
results_onshorefailure<- compute(neural_onshorefailure, subset_onshorefailure)

neural_onshorerepair<- neuralnet(Onshore_Repair_time ~ Onshore_Air_flow+Onshore_Asset_life+Onshore_Detected_differential_temperature+Onshore_Detected_differential_pressure+Onshore_Gas_input, agent_logicdfl, hidden = 2, lifesign = "minimal", 
                                  linear.output = TRUE, threshold = 0.1)

subset_onshorerepair<- subset(agent_logicdfl, select=c(Onshore_Air_flow, Onshore_Asset_life, Onshore_Detected_differential_temperature, Onshore_Detected_differential_pressure
                                                        , Onshore_Gas_input))
results_onshorerepair<- compute(neural_onshorerepair, subset_onshorerepair)


neural_dehydprocessing<- neuralnet(Dehydration_Processing_time ~ Dehydration_Air_flow+Dehydration_Asset_life+Dehydration_Detected_differential_temperature+Dehydration_Detected_differential_pressure
                                   +Dehydration_Gas_input, agent_logicdfl, hidden = 2, lifesign = "minimal", 
                                 linear.output = TRUE, threshold = 0.1)

subset_dehydprocessing<- subset(agent_logicdfl, select=c(Dehydration_Air_flow, Dehydration_Asset_life, Dehydration_Detected_differential_temperature, Dehydration_Detected_differential_pressure
                                                       , Dehydration_Gas_input))
results_dehydprocessing<- compute(neural_dehydprocessing, subset_dehydprocessing)

neural_dehydfail2<-neuralnet(Dehydration_Failure_time~Dehydration_Air_flow+Dehydration_Asset_life+Dehydration_Detected_differential_temperature+Dehydration_Detected_differential_pressure
                             +Dehydration_Gas_input, agent_logicdfl, hidden=2, lifesign="minimal", linear.output=TRUE, threshold=0.1)
subset_dehydfail2<- subset(agent_logicdfl, select=c(Dehydration_Air_flow, Dehydration_Asset_life, Dehydration_Detected_differential_temperature, Dehydration_Detected_differential_pressure
                                                    , Dehydration_Gas_input))
results_dehydfail2<- compute(neural_dehydfail2, subset_dehydfail2)

neural_dehydrepair2<- neuralnet(Dehydration_Repair_time~Dehydration_Air_flow+Dehydration_Asset_life+Dehydration_Detected_differential_temperature+Dehydration_Detected_differential_pressure
+Dehydration_Gas_input, agent_logicdfl, hidden=2, lifesign="minimal", linear.output=TRUE, threshold=0.1)
subset_dehydrepair2<- subset(agent_logicdfl, select=c(Dehydration_Air_flow, Dehydration_Asset_life, Dehydration_Detected_differential_temperature, Dehydration_Detected_differential_pressure
                                                      , Dehydration_Gas_input))
results_dehydrepair2<- compute(neural_dehydrepair2, subset_dehydrepair2)



predictedvsactual <- data.frame(Liq_Air_flow, Liq_Asset_life, 
                                Liq_Detected_differential_temperature, Liq_Detected_differential_pressure, 
                                Liq_Gas_input, Liq_Processing_time, Liq_predicted_processing_time = results$net.result, 
                                Liq_Failure_time, Liq_predicted_failure_time=results_2$net.result, 
                                Liq_Repair_time, Liq_predicted_repair_time=results3$net.result, Onshore_Processing_time,
                                Onshore_predicted_processing_time=results_onshoreprocessing$net.result, Onshore_Failure_time, Onshore_predicted_failure_time= results_onshorefailure$net.result,
                                Onshore_Repair_time,
                                Onshore_predicted_repair_time=results_onshorerepair$net.result,
                                Dehydration_Processing_time, Dehydration_predicted_processing_time=results_dehydprocessing$net.result,
                                Dehydration_Failure_time, Dehydration_Prediction_Failure_time=results_dehydfail2$net.result,
                                Dehydration_Repair_time, Dehydration_Prediction_Repair_time=results_dehydrepair2$net.result)

prediction_1df<-as.data.frame(predictedvsactual[50,])
#END1
#-----------------------------------------

install.packages("WriteXLS")
library(WriteXLS)
WriteXLS(prediction_1df, "C:/Users/shereen.ashraf/Documents/ARIS/prediction model2.xls",Encoding = "latin1")
library(xlsx)
install.packages("xlsxjars")
library(xlsxjars)
library(rJava)

install.packages("XLConnect")
library(XLConnect)
library(XLConnectJars)
require("XLConnect")



write.xls(prediction_1df,"C:/Users/shereen.ashraf/Documents/ARIS/prediction model2.xls" )
write.xlsx(prediction_1df, "C:/Users/shereen.ashraf/Documents/ARIS/prediction model2.xls",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

write.csv(prediction_1df, "output2.csv")

read.xls
