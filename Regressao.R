

      library(readxl)
      atlas_munic <- read_excel("atlas2013_dadosbrutos_pt_somente municipio.xlsx")
      
      
      #filtroPeloUF
      atlas_m2000<- subset.data.frame(atlas_munic, ANO==2000)
      atlas_m2010<- subset.data.frame(atlas_munic, ANO==2010)
      atlas_m2000RJ<- subset.data.frame(atlas_m2000, UF=="33")
      atlas_m2010RJ<- subset.data.frame(atlas_m2010, UF=="33")
      

      #Adicionando idhm_2010 dentro do resultados para RJ
      idhm_2010RJ<- c(atlas_m2010RJ$IDHM)
      atlas_m2000RJ$idhm_2010RJ <- c(atlas_m2010RJ$IDHM)
      
      #dplyt
      install.packages("dplyr")
      library(dplyr)
      
      RJ <- atlas_m2000RJ %>% 
        select(ANO, UF, RDPC,ESPVIDA,I_ESCOLARIDADE,GINI,E_ANOSESTUDO,T_ANALF25M, T_AGUA, T_BANAGUA,T_LUZ, idhm_2010RJ) %>%
        filter(ANO, UF, RDPC,ESPVIDA,I_ESCOLARIDADE,GINI,E_ANOSESTUDO,T_ANALF25M, T_AGUA, T_BANAGUA,T_LUZ, idhm_2010RJ)
      
      
      #regressão linear múltipla onde a variável resposta é IDHM de 2010 e como fatores (variáveis explicativas)
      regres=lm(idhm_2010RJ ~ 
                 RDPC
                + ESPVIDA
                + I_ESCOLARIDADE
                + GINI
                + E_ANOSESTUDO
                + T_ANALF25M
                + T_AGUA
                + T_BANAGUA
                + T_LUZ, data=RJ)
      summary(regres)
      ##standard error: 0.01315 // R-squared:  0.8848
      
      
      ##Variaveis nao utilizadas
      # I_FREQ_PROP
      # POPT
      
      #histogramasVAiraveisEscolhidas
      #hist(atlas_m2000RJ$HOMEM15A19)
      #hist(atlas_m2000RJ$HOMEM25A29)
      #hist(atlas_m2000RJ$HOMEM30A34)
      #hist(atlas_m2000RJ$HOMEM35A39)
      #hist(atlas_m2000RJ$HOMEM40A44)
      
      #histogramasVAiraveisEscolhidas
       hist(RJ$RDPC)
       hist(RJ$ESPVIDA)
       hist(RJ$I_ESCOLARIDADE)
       hist(RJ$GINI)
       hist(RJ$E_ANOSESTUDO)
       hist(RJ$T_ANALF25M)
       hist(RJ$T_AGUA)
       
       #CorrelaçãoLinear variaves quantitativas
       plot(RJ$RDPC,RJ$E_ANOSESTUDO)
       plot(RJ$RDPC,RJ$ESPVIDA)
       plot(RJ$ESPVIDA,RJ$I_ESCOLARIDADE)
       plot(RJ$ESPVIDA,RJ$E_ANOSESTUDO)
       plot(RJ$ESPVIDA,RJ$T_ANALF25M)
       plot(RJ$GINI,RJ$I_ESCOLARIDADE)
       plot(RJ$GINI,RJ$T_ANALF25M)
       plot(RJ$T_AGUA,RJ$ESPVIDA)
       plot(RJ$T_AGUA,RJ$I_ESCOLARIDADE)
       
       # Reta simples
       plot(RJ$I_ESCOLARIDADE,RJ$idhm_2010RJ)
       abline(lm(RJ$idhm_2010RJ ~ RJ$I_ESCOLARIDADE))
       
       #Coeficientes
       confint(regres)
       
       #resíduos TAxa de erro +- 0.01
       plot(fitted(regres),residuals(regres),xlab="Valores Ajustados",ylab="Residuos")
       abline(h=0)
       
       plot(RJ$T_AGUA,residuals(regres),xlab="T_AGUA",ylab="Residuos")
       abline(h=0)
       
       
       #avaliaÃ§Ã£o da suposiÃ§Ã£o de normalidade dos erros,
       qqnorm(residuals(regres), ylab="Residuos",xlab="Quantis teoricos",main="")
       qqline(residuals(regres))
       
       
       #prediÃ§Ã£o da base completa
       pred <- predict(regres, atlas_m2000RJ, interval="prediction", level=0.95)
       RJ<- data.frame(RJ,pred)
       
       
       #Salvando dados com prediÃ§Ã£o em arquivo excel:
       library(xlsx)
       write.xlsx(RJ, file="prediction.xls") 