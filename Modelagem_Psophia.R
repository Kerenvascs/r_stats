#bibliotecas/Pacotes
library("biomod2")
library("rgdal")
library("fields")

#preparando caminhos                                                                                                                                                
EEV_atual <- "C:/Users/Keren Vasconcelos/Desktop/Keren/ITV/DISCIPLINAS/ANÁLISES ESPACIAIS/2 semana/Base/aula_2/Camadas_Cortadas/atual"                                                                                                                              
EEV_F1 <- "C:/Users/Keren Vasconcelos/Desktop/Keren/ITV/DISCIPLINAS/ANÁLISES ESPACIAIS/2 semana/Base/aula_2/Camadas_Cortadas/rcp26_2070"                                                                                                                        
EEV_F2 <- "C:/Users/Keren Vasconcelos/Desktop/Keren/ITV/DISCIPLINAS/ANÁLISES ESPACIAIS/2 semana/Base/aula_2/Camadas_Cortadas/rcp85_2070"

sel_vars <- read.table("C:/Users/Keren Vasconcelos/Desktop/Keren/ITV/Análises Espaciais/2 semana/Base/aula_2/variaveis_selecao/selecionadas.csv", sep = ",", header = F, as.is = T, fill = T)
sel_vars                                                                              

#lendo ocorrencias                                                                                                                                                  
species_data <- read.csv("C:/Users/Keren Vasconcelos/Desktop/Keren/ITV/Análises Espaciais/2 semana/Aula 3 - Leonardo/ocor_Psophia_final.csv", sep = " ", header = T)                                                                   
head(species_data)
summary(species_data)
table(species_data$aves) 

#empilhando variaveis                                                                                                                                               
atuais <- stack(list.files(path = paste(EEV_atual, sep = ''),                                                                                                       
                           pattern = ".img$", full.names = T))  
atuais

#plot(atuais)
## O plot atuais nada mais é que gerar a imagem das camadas da amazonia cortadas na janelinha ao lado


#definindo limite de corte                                                                                                                                          
eval_threshold <- 0.4    

myRespName <- "Psophia"                                                                                                                                           
myRespCoord <- species_data[species_data$aves==myRespName, c("long", "lat")]                                                                                    
myResp <- rep.int(1, times = nrow(myRespCoord))                                                                                                                    
myExpl <- atuais[[grep("bio|srtm_10min", paste(sel_vars), value = T)]]                                                                                                                              
myExpl


#parametros modelagem ////////// obs.importante: verificar versao do biomod2 3.3-13  

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,                                                                                                             
                                     expl.var = myExpl,                                                                                                             
                                     resp.xy = myRespCoord,                                                                                                         
                                     resp.name = myRespName,                                                                                                        
                                     PA.nb.rep = 1,                                                                                                                 
                                     PA.nb.absences = as.numeric(length(myResp)*10),                                                                                
                                     PA.strategy = 'random')  
myBiomodData


#myBiomodData
myBiomodData

#plot(myBiomodData)
plot(myBiomodData)


#default                                                                                                                                                            
myBiomodOption <- BIOMOD_ModelingOptions()                                                                                                                         



############ MODELAGEM #############                                                                                                                                                  

myBiomodModelOut <- BIOMOD_Modeling(myBiomodData,                                                                                                                   
                                    models = c('GLM','RF'),                                                                                           
                                    models.options = myBiomodOption,                                                                                                
                                    NbRunEval = 2,                                                                                                                 
                                    DataSplit = 75,                                                                                                                 
                                    Yweights = NULL,                                                                                                                
                                    VarImport = 5,                                                                                                                  
                                    models.eval.meth = c('TSS', 'ROC'),                                                                                             
                                    SaveObj = T,                                                                                                                    
                                    rescal.all.models = T,                                                                                                          
                                    do.full.models = F,                                                                                                             
                                    modeling.id = paste(myRespName, sep = ""))


#Salvar os dados dos modelos e avaliacoes                                                                                                                           
capture.output(myBiomodData,                                                                                                                                        
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_BiomodData.txt", sep = "")))                                                                                    
capture.output(myBiomodModelOut,                                                                                                                                    
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_BiomodModelOut.txt", sep = "")))                                                                                
capture.output(myBiomodOption,                                                                                                                                      
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_BiomodOption.txt", sep = "")))                                                                                  
capture.output(get_evaluations(myBiomodModelOut),                                                                                                                   
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_eval_BiomodModelOut.txt", sep = "")))                                                                           
capture.output(get_variables_importance(myBiomodModelOut),                                                                                                          
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_var_importance__BiomodModelOut.txt", sep = "")))                                                                


#projetando modelos em IMG (Atual)                                                                                                                                  

myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut,                                                                                               
                                  new.env = myExpl,           #camadas                                                                                              
                                  proj.name = 'atual',        #nome da pasta                                                                                        
                                  xy.new.env = NULL,          #opcional                                                                                             
                                  selected.models = 'all',                                                                                                          
                                  binary.meth = 'TSS',        #met transformacao bin                                                                                                      
                                  compress = F,                                                                                                                     
                                  build.clamping.mask = F,    #opcional                                                                                             
                                  do.stack = F,               #as projecoes serao armazenadas em arquivos separados                                                 
                                  output.format = '.img')  


#ensemble/algoritmo :: media (atual)                                                                                                                                                   
myBiomodEM_algo <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,                                                                                      
                                           chosen.models =  'all',                            #lista de modelos TSS >= limiar                                       
                                           em.by = 'algo',                                    #'PA_dataset+repet'; 'PA_dataset+algo'; 'Pa_dataset'; 'algo'; 'all'   
                                           eval.metric = c('TSS'),                                                                                                  
                                           eval.metric.quality.threshold = eval_threshold,    #limiar                                                               
                                           prob.mean = T,                                     #Estimate the mean probabilities across predictions                   
                                           prob.cv = F,                                       #Estimate the coefficient of variation across predictions             
                                           prob.ci = F,                                       #Estimate the confidence interval around the prob.mean                
                                           prob.ci.alpha = 0.05,                              #Significance level for estimating the confidence interval            
                                           prob.median = F,                                   #Estimate the mediane of probabilities                                
                                           committee.averaging = F,                           #Estimate the committee averaging across predictions                  
                                           prob.mean.weight = F,                              #Estimate the weighted sum of probabilities                           
                                           prob.mean.weight.decay = 'proportional',           #Define the relative importance of the weights                        
                                           VarImport = 0)                                                                                                           

# salvando os dados do ensemble                                                                                                                                     
capture.output(myBiomodEM_algo,                                                                                                                                     
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_EM_algo.txt", sep = "")))                                                                                       
capture.output(get_evaluations(myBiomodEM_algo),                                                                                                                    
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_eval_EM_algo.txt", sep = ""))) 

#projetando ensemble/algoritmo :: media (atual)                                                                                                                                       
myBiomodProj_EM_algo <- BIOMOD_EnsembleForecasting(EM.output = myBiomodEM_algo,                                                                                     
                                                   projection.output = myBiomodProj,                                                                                
                                                   new.env = NULL,                                                                                                  
                                                   xy.new.env = NULL,                                                                                               
                                                   selected.models = 'all',                                                                                         
                                                   proj.name = 'ensemblebyalgo_atual',                                                                                    
                                                   binary.meth = 'TSS',                                                                                             
                                                   filtered.meth = NULL,                                                                                            
                                                   compress = NULL,                                                                                                 
                                                   output.format = '.img',                                                                                          
                                                   total.consensus = T)                                                                                             


#ensemble total :: committee averaging (atual)                                                                                                                                                   
myBiomodEM_all <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,                                                                                      
                                          chosen.models =  'all',                            #lista de modelos TSS >= limiar                                       
                                          em.by = 'all',                                    #'PA_dataset+repet'; 'PA_dataset+algo'; 'Pa_dataset'; 'algo'; 'all'   
                                          eval.metric = c('TSS'),                                                                                                  
                                          eval.metric.quality.threshold = eval_threshold,    #limiar                                                               
                                          prob.mean = F,                                     #Estimate the mean probabilities across predictions                   
                                          prob.cv = F,                                       #Estimate the coefficient of variation across predictions             
                                          prob.ci = F,                                       #Estimate the confidence interval around the prob.mean                
                                          prob.ci.alpha = 0.05,                              #Significance level for estimating the confidence interval            
                                          prob.median = F,                                   #Estimate the mediane of probabilities                                
                                          committee.averaging = T,                           #Estimate the committee averaging across predictions                  
                                          prob.mean.weight = F,                              #Estimate the weighted sum of probabilities                           
                                          prob.mean.weight.decay = 'proportional',           #Define the relative importance of the weights                        
                                          VarImport = 0)                                                                                                           


# salvando os dados do ensemble                                                                                                                                     
capture.output(myBiomodEM_all,                                                                                                                                     
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_EM_all.txt", sep = "")))                                                                                       
capture.output(get_evaluations(myBiomodEM_all),                                                                                                                    
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_eval_EM_all.txt", sep = "")))                                                                                  


#projetando ensemble total :: committee averaging (atual)                                                                                                                                       

myBiomodProj_EM_all <- BIOMOD_EnsembleForecasting(EM.output = myBiomodEM_all,                                                                                     
                                                  projection.output = myBiomodProj,                                                                                
                                                  new.env = NULL,                                                                                                  
                                                  xy.new.env = NULL,                                                                                               
                                                  selected.models = 'all',                                                                                         
                                                  proj.name = 'ensembletotal_atual',                                                                                    
                                                  binary.meth = 'TSS',                                                                                             
                                                  filtered.meth = NULL,                                                                                            
                                                  compress = NULL,                                                                                                 
                                                  output.format = '.img',                                                                                          
                                                  total.consensus = T)


################################ FUTURO  ##################################                                                                                         

### FUTURO1                                                                                                                                                        
todas_futuro1 <- stack(list.files(path=paste(EEV_F1, sep=''), pattern=".img$", full.names=TRUE))                                                                  
todas_futuro1                                                                                                                                                    


myExpl_F1 <-todas_futuro1[[grep("bio|srtm_10min",paste(sel_vars),value = T)]]                                                                                                                        
myExpl_F1                                                                                                                                                         


#projetando modelos em IMG (FUTURO1)                                                                                                                               
myBiomodProj_F1 <- BIOMOD_Projection(modeling.output = myBiomodModelOut,                                                                                           
                                     new.env = myExpl_F1,                          #layers futuro                                                               
                                     proj.name = 'Futuro1',                        #name of folder                                                              
                                     xy.new.env = NULL,                             #optional                                                                    
                                     selected.models = "all",                                                                                                    
                                     binary.meth = 'TSS',                           #eval for bin transformation                                                 
                                     compress = F,                                  #'xz',#compression format                                                    
                                     build.clamping.mask = F,                       #optional                                                                    
                                     do.stack=F,                                    #projections will be stored into separated files                             
                                     output.format = '.img')                        #raster format                                                               


#ensemble/algoritmo :: media (FUTURO1)                                                                                                                                                
myBiomodEM_algoF1 <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,                                                                                   
                                             chosen.models =  'all',                            #lista de modelos TSS >= limiar                                    
                                             em.by = 'algo',                                    #'PA_dataset+repet'; 'PA_dataset+algo'; 'Pa_dataset'; 'algo'; 'all'
                                             eval.metric = c('TSS'),                                                                                               
                                             eval.metric.quality.threshold = eval_threshold,    #limiar                                                            
                                             prob.mean = T,                                     #Estimate the mean probabilities across predictions                
                                             prob.cv = F,                                       #Estimate the coefficient of variation across predictions          
                                             prob.ci = F,                                       #Estimate the confidence interval around the prob.mean             
                                             prob.ci.alpha = 0.05,                              #Significance level for estimating the confidence interval         
                                             prob.median = F,                                   #Estimate the mediane of probabilities                             
                                             committee.averaging = F,                           #Estimate the committee averaging across predictions               
                                             prob.mean.weight = F,                              #Estimate the weighted sum of probabilities                        
                                             prob.mean.weight.decay = 'proportional',           #Define the relative importance of the weights                     
                                             VarImport = 0)                                                                                                        




# salvando os dados do ensemble (FUTURO1)                                                                                                                          
capture.output(myBiomodEM_algoF1,                                                                                                                                  
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_EM_algoF1.txt", sep = "")))                                                                                    
capture.output(get_evaluations(myBiomodEM_algoF1),                                                                                                                 
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_eval_EM_algoF1.txt", sep = "")))                                                                               



#projetando ensemble/algoritmo :: media (FUTURO1)                                                                                                                              
myBiomodProj_EM_algoF1 <- BIOMOD_EnsembleForecasting(EM.output = myBiomodEM_algoF1,                                                                               
                                                     projection.output = myBiomodProj_F1,                                                                              
                                                     new.env = NULL,                                                                                                    
                                                     xy.new.env = NULL,                                                                                                 
                                                     selected.models = 'all',                                                                                           
                                                     proj.name = 'ensemblebyalgo_Futuro1',                                                                                   
                                                     binary.meth = 'TSS',                                                                                               
                                                     filtered.meth = NULL,                                                                                              
                                                     compress = NULL,                                                                                                   
                                                     output.format = '.img',                                                                                            
                                                     total.consensus = TRUE)                                                                                            




#ensemble total :: committee averaging (FUTURO1)                                                                                                                                                   
myBiomodEM_allF1 <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,                                                                                      
                                            chosen.models =  'all',                            #lista de modelos TSS >= limiar                                       
                                            em.by = 'all',                                    #'PA_dataset+repet'; 'PA_dataset+algo'; 'Pa_dataset'; 'algo'; 'all'   
                                            eval.metric = c('TSS'),                                                                                                  
                                            eval.metric.quality.threshold = eval_threshold,    #limiar                                                               
                                            prob.mean = F,                                     #Estimate the mean probabilities across predictions                   
                                            prob.cv = F,                                       #Estimate the coefficient of variation across predictions             
                                            prob.ci = F,                                       #Estimate the confidence interval around the prob.mean                
                                            prob.ci.alpha = 0.05,                              #Significance level for estimating the confidence interval            
                                            prob.median = F,                                   #Estimate the mediane of probabilities                                
                                            committee.averaging = T,                           #Estimate the committee averaging across predictions                  
                                            prob.mean.weight = F,                              #Estimate the weighted sum of probabilities                           
                                            prob.mean.weight.decay = 'proportional',           #Define the relative importance of the weights                        
                                            VarImport = 0)                                                                                                          




# salvando os dados do ensemble                                                                                                                                     
capture.output(myBiomodEM_allF1,                                                                                                                                     
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_EM_allF1.txt", sep = "")))                                                                                       
capture.output(get_evaluations(myBiomodEM_allF1),                                                                                                                    
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_eval_EM_allF1.txt", sep = "")))                                                                                  


#projetando ensemble total :: committee averaging (atual)                                                                                                                                       
myBiomodProj_EM_allF1 <- BIOMOD_EnsembleForecasting(EM.output = myBiomodEM_allF1,                                                                                     
                                                    projection.output = myBiomodProj_F1,                                                                                
                                                    new.env = NULL,                                                                                                  
                                                    xy.new.env = NULL,                                                                                               
                                                    selected.models = 'all',                                                                                         
                                                    proj.name = 'ensembletotal_F1',                                                                                    
                                                    binary.meth = 'TSS',                                                                                             
                                                    filtered.meth = NULL,                                                                                            
                                                    compress = NULL,                                                                                                 
                                                    output.format = '.img',                                                                                          
                                                    total.consensus = T)



### FUTURO2                                                                                                                                                        
todas_futuro2 <- stack(list.files(path=paste(EEV_F2, sep=''), pattern=".img$", full.names=TRUE))                                                                  
todas_futuro2                                                                                                                                                    


myExpl_F2<-todas_futuro2[[grep("bio|srtm_10min", paste(sel_vars), value = T)]]                                                                                                                       
myExpl_F2                                                                                                                                                         


#projetando modelos/algoritmo :: media (FUTURO2)                                                                                                                               
myBiomodProj_F2 <- BIOMOD_Projection(modeling.output = myBiomodModelOut,                                                                                           
                                     new.env = myExpl_F2,                          #layers futuro                                                               
                                     proj.name = 'Futuro2',                        #name of folder                                                              
                                     xy.new.env = NULL,                             #optional                                                                    
                                     selected.models = "all",                                                                                                    
                                     binary.meth = 'TSS',                           #eval for bin transformation                                                 
                                     compress = F,                                  #'xz',#compression format                                                    
                                     build.clamping.mask = F,                       #optional                                                                    
                                     do.stack=F,                                    #projections will be stored into separated files                             
                                     output.format = '.img')                        #raster format                                                               


#ensemble/algoritmo :: media  (FUTURO2)                                                                                                                                                
myBiomodEM_algoF2 <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,                                                                                   
                                             chosen.models =  'all',                            #lista de modelos TSS >= limiar                                    
                                             em.by = 'algo',                                    #'PA_dataset+repet'; 'PA_dataset+algo'; 'Pa_dataset'; 'algo'; 'all'
                                             eval.metric = c('TSS'),                                                                                               
                                             eval.metric.quality.threshold = eval_threshold,    #limiar                                                            
                                             prob.mean = T,                                     #Estimate the mean probabilities across predictions                
                                             prob.cv = F,                                       #Estimate the coefficient of variation across predictions          
                                             prob.ci = F,                                       #Estimate the confidence interval around the prob.mean             
                                             prob.ci.alpha = 0.05,                              #Significance level for estimating the confidence interval         
                                             prob.median = F,                                   #Estimate the mediane of probabilities                             
                                             committee.averaging = F,                           #Estimate the committee averaging across predictions               
                                             prob.mean.weight = F,                              #Estimate the weighted sum of probabilities                        
                                             prob.mean.weight.decay = 'proportional',           #Define the relative importance of the weights                     
                                             VarImport = 0)                                                                                                        




# salvando os dados do ensemble (FUTURO2)                                                                                                                          
capture.output(myBiomodEM_algoF2,                                                                                                                                  
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_EM_algoF2.txt", sep = "")))                                                                                    
capture.output(get_evaluations(myBiomodEM_algoF2),                                                                                                                 
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_eval_EM_algoF2.txt", sep = "")))                                                                               



#projetando ensemble/algoritmo :: media (FUTURO2)                                                                                                                              
myBiomodProj_EM_algoF2 <- BIOMOD_EnsembleForecasting(EM.output = myBiomodEM_algoF2,                                                                               
                                                     projection.output = myBiomodProj_F2,                                                                              
                                                     new.env = NULL,                                                                                                    
                                                     xy.new.env = NULL,                                                                                                 
                                                     selected.models = 'all',                                                                                           
                                                     proj.name = 'ensemblebyalgo_Futuro2',                                                                                   
                                                     binary.meth = 'TSS',                                                                                               
                                                     filtered.meth = NULL,                                                                                              
                                                     compress = NULL,                                                                                                   
                                                     output.format = '.img',                                                                                            
                                                     total.consensus = TRUE)                                                                                            




#ensemble total :: committee averaging (FUTURO1)                                                                                                                                                   
myBiomodEM_allF2 <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,                                                                                      
                                            chosen.models =  'all',                            #lista de modelos TSS >= limiar                                       
                                            em.by = 'all',                                    #'PA_dataset+repet'; 'PA_dataset+algo'; 'Pa_dataset'; 'algo'; 'all'   
                                            eval.metric = c('TSS'),                                                                                                  
                                            eval.metric.quality.threshold = eval_threshold,    #limiar                                                               
                                            prob.mean = F,                                     #Estimate the mean probabilities across predictions                   
                                            prob.cv = F,                                       #Estimate the coefficient of variation across predictions             
                                            prob.ci = F,                                       #Estimate the confidence interval around the prob.mean                
                                            prob.ci.alpha = 0.05,                              #Significance level for estimating the confidence interval            
                                            prob.median = F,                                   #Estimate the mediane of probabilities                                
                                            committee.averaging = T,                           #Estimate the committee averaging across predictions                  
                                            prob.mean.weight = F,                              #Estimate the weighted sum of probabilities                           
                                            prob.mean.weight.decay = 'proportional',           #Define the relative importance of the weights                        
                                            VarImport = 0)                                                                                                           




# salvando os dados do ensemble                                                                                                                                     
capture.output(myBiomodEM_allF2,                                                                                                                                     
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_EM_allF2.txt", sep = "")))                                                                                       
capture.output(get_evaluations(myBiomodEM_allF2),                                                                                                                    
               file = file.path(myRespName,                                                                                                                         
                                paste(myRespName, "_eval_EM_allF2.txt", sep = "")))                                                                                  


#projetando ensemble total :: committee averaging (atual)                                                                                                                                       
myBiomodProj_EM_allF2 <- BIOMOD_EnsembleForecasting(EM.output = myBiomodEM_allF2,                                                                                     
                                                    projection.output = myBiomodProj_F2,                                                                                
                                                    new.env = NULL,                                                                                                  
                                                    xy.new.env = NULL,                                                                                               
                                                    selected.models = 'all',                                                                                         
                                                    proj.name = 'ensembletotal_F2',                                                                                    
                                                    binary.meth = 'TSS',                                                                                             
                                                    filtered.meth = NULL,                                                                                            
                                                    compress = NULL,                                                                                                 
                                                    output.format = '.img',                                                                                          
                                                    total.consensus = T)



