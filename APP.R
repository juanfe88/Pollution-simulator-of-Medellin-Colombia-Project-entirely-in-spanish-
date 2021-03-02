library(shiny)
# bases de datos para funciones y modelación
load("DatosAQItoConc.RData")
load("EOD_autos.RData")
load("EOD_motos.RData")
load("EOD_Taxi.RData")
load("EOD_Buses.RData")
load("Datos_reg.RData")
AQItoConc<-function(aqis){
  solu<-c()
  for (i in 1:length(aqis)){
    if (aqis[i]<=50){
      tempAQI<-interpolation_data$AQI[1:2]
      tempconc<-interpolation_data$Conc[1:2]
      reg<-lm(tempconc~tempAQI)
    }
    else{
      if(aqis[i]>50&aqis[i]<=100){
        tempAQI<-interpolation_data$AQI[3:4]
        tempconc<-interpolation_data$Conc[3:4]
        reg<-lm(tempconc~tempAQI) 
      }
      else{
        if(aqis[i]>100&aqis[i]<=150){
          tempAQI<-interpolation_data$AQI[5:6]
          tempconc<-interpolation_data$Conc[5:6]
          reg<-lm(tempconc~tempAQI) 
        }
        else{
          if(aqis[i]>150&aqis[i]<=200){
            tempAQI<-interpolation_data$AQI[7:8]
            tempconc<-interpolation_data$Conc[7:8]
            reg<-lm(tempconc~tempAQI) 
          }
          
          else{
            if(aqis[i]>200&aqis[i]<=300){
              tempAQI<-interpolation_data$AQI[9:10]
              tempconc<-interpolation_data$Conc[9:10]
              reg<-lm(tempconc~tempAQI) 
            }
            
            else{
              if(aqis[i]>300){
                tempAQI<-interpolation_data$AQI[11:12]
                tempconc<-interpolation_data$Conc[11:12]
                reg<-lm(tempconc~tempAQI) 
              }
            }
          }
        }
      }
    }
    m<-reg$coefficients[2]
    b<-reg$coefficients[1]
    solu[i]<-b+m*aqis[i]
  }
  
  return(solu)
}
horas_au<-function(au){
  set.seed(124)
  Horas_auto<-sum(sample(autos$Duración_Viaje,au,TRUE))/60
  return(Horas_auto)
}
horas_mo<-function(mo){
  set.seed(124)
  Horas_moto<-sum(sample(motos$Duración_Viaje,mo,TRUE))/60
  return(Horas_moto)
  
}
horas_ta<-function(ta){
  set.seed(124)
  Horas_moto<-sum(sample(Taxi$Duración_Viaje,ta,TRUE))/60
  return(Horas_moto)
  
}
horas_bu<-function(bu){
  set.seed(124)
  Horas_moto<-sum(sample(Buses$Duración_Viaje,bu,TRUE))/60
  return(Horas_moto)
  
}
Calculo_AQI<-function(te,s,r,rh,cct,a,m,t,b){
 
  reg_2<- nls(New_AQI.Promedio ~ a*Temperature  + b*Sunshine.Duration +c*Shortwave.Radiation+ d/Relative.Humidity+f*Cloud.Cover.Total  + Horas_auto^e +Horas_bus^g + Horas_moto^h + Horas_taxi^i,data = Datos_para_reg,start = list(a=10,b=10,c=10,d=40,e=0.3,g=0.5,h=0.4,i=0.5,f=-40))
  aqi<-predict(reg_2,newdata =  data.frame(Temperature=te,Sunshine.Duration=s,Shortwave.Radiation=r,Relative.Humidity=rh,Cloud.Cover.Total=cct,Horas_auto=a,Horas_bus=b,Horas_moto=m,Horas_taxi=t))
  if (aqi<0){
    aqi<-0
  }
  else{
    if(aqi>500){
      aqi<-"+500"
    }
  }
  return(aqi)
  }

ui<-fluidPage(   fluidRow(
  # dividing header panel in two columns 
  # column one contains the title
  column(width = 10, # width of first column 
         style = "font-size: 40pt; line-height: 150pt; width = 200", # font size etc.
         tags$strong("Modelo de Calidad del Aire en el Valle de Aburrá"),
         tags$hr()), # "tags" is for using html-functions within the shiny app
  # column two
  column(width = 2,
         tags$head(tags$img(src='data:image/png;base64,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', align = "left", width= "200"))) # add image from url
),


sidebarPanel(style = "background-color: #a6cc00;", # choose background color
             width = 4, # set panel width
             tabsetPanel(
             tabPanel(tags$strong("Condiciones Meteorológicas"),style="font-size: 15pt",fluid=TRUE,
  sliderInput(inputId = "Temp",label = "Ingrese Temperatura promedio 24-H [C]",value = 25,min = 20,max=28,step = 0.01),
  sliderInput(inputId = "Suns",label = "Ingrese los minutos de sol del dia",value = 300,min=0,max=570,step = 0.1),
  sliderInput(inputId = "rad",label = "Ingrese Radiacion de onda corta [W/M2]",value = 4000,min=2200,max = 7200,step = 0.1),
  sliderInput(inputId = "Relh",label = "Ingrese la Humedad relativa en poorcentaje",value = 56,min = 50,max=100,step = 0.1),
  sliderInput(inputId = "CCT",label = "Ingrese el poorcentaje de cobertura por nubes",value = 75,min = 55,max=100,step = 0.1)),
            tabPanel(tags$strong("Condiciones de Tráfico"),style="font-size: 15pt",fluid=TRUE,
  sliderInput(inputId = "aut",label = "Ingrese el número de autos",value = 390000,min=0,max=638333, step=10),
  sliderInput(inputId = "mot",label = "Ingrese el número de motos",value = 500000,min=0,max = 893969, step=10),
  sliderInput(inputId = "taxi",label = "Ingrese el número de taxis",value = 25000,min=0,max = 30143, step=10),
  sliderInput(inputId = "busi",label = "Ingrese el número de buses",value = 17000,min=0,max = 22625, step=10)
            ))),
  mainPanel(style = "background-color: #a6cc00",#;font-size: 40pt; line-height: 150pt; width = 200",
            width = 7,

            fluidRow(align="center",
                     column(width = 6,
                            style="font-size: 60pt; line-height: 50pt; width = 200",
            tags$strong("ICA"),
            fluidRow(style="font-size: 60pt; line-height: 100pt;color: white",
                     align="center",
            tags$strong(textOutput("res3")))

            ),
            column(width = 6,
                   style="font-size: 45pt; line-height: 50pt; width = 200",
                   tags$strong(HTML(paste0("PM 2.5 [ug/m",tags$sup(3),"]"))),
                   fluidRow(style="font-size: 60pt; line-height: 100pt;color: white",
                            align="center",
                            tags$strong(textOutput("res4"))))
            ),
            fluidRow(align="center",
                     style="font-size: 40pt; line-height: 50pt; width = 200",
                     tags$strong("Descripcion del estado"),
              conditionalPanel(
                condition = "output.res3<=50",
                tags$img(height = 300,
                         width = 800,src="Bien.png")),
                conditionalPanel(
                  condition = "output.res3>50&&output.res3<=100",
                  tags$img(height = 300,
                           width = 800,src="Moderado.png")),
              conditionalPanel(
                condition = "output.res3>100&&output.res3<=150",
                tags$img(height = 300,
                         width = 800,src="Naran.png")),
              conditionalPanel(
                condition = "output.res3>150&&output.res3<=200",
                tags$img(height = 300,
                         width = 800,src="Roja.png")),
              conditionalPanel(
                condition = "output.res3>200&&output.res3<=300",
                tags$img(height = 300,
                         width = 800,src="Purp.png"))
            )
            )
)
server <- function(input, output) {
  au<-reactive(horas_au(input$aut))
  mo<-reactive(horas_mo(input$mot))
  ta<-reactive(horas_ta(input$taxi))
  bu<-reactive(horas_bu(input$busi))

  output$res3<-renderText(Calculo_AQI(input$Temp,input$Suns,input$rad,input$Relh,input$CCT,au(),mo(),ta(),bu()))
  output$res4<-renderText(AQItoConc(Calculo_AQI(input$Temp,input$Suns,input$rad,input$Relh,input$CCT,au(),mo(),ta(),bu())))

}
shinyApp(ui = ui, server = server)
