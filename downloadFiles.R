require(xlsx)
require(data.table)
library(plotly)
library(reshape2)

Sys.setenv("plotly_username"="RoelPi")
Sys.setenv("plotly_api_key"=readLines("plot.txt",n=1,warn=F))
 
# Voertuigenpark - awesome filenaming /s
park <- list()
park[[16]] <- "http://statbel.fgov.be/nl/binaries/veh_parc_2016_nl_tcm325-280143.xls"
park[[15]] <- "http://statbel.fgov.be/nl/binaries/veh_parc_2015_nl_tcm325-271969.xls"

voerPark <- list()
dir.create("voertuigenpark", showWarnings = FALSE)
for (i in 1:length(park)) {
    if (!is.null(park[[i]]) & !file.exists(paste0("voertuigenpark/voertuigenpark_jaar_",i,".xls"))) {
        download.file(park[[i]],paste0("voertuigenpark/voertuigenpark_jaar_",i,".xls"),mode="wb")
    }
    if (file.exists(paste0("voertuigenpark/voertuigenpark_jaar_",i,".xls"))) {
        voerPark[[i]] <- read.xlsx2(paste0("voertuigenpark/voertuigenpark_jaar_",i,".xls"),5,colClasses="character")
    }
}
stats <- data.table(read.xlsx2("opgevraagde_statistieken.xls",1,colClasses=c("character","character","numeric","numeric","numeric","numeric"),stringsAsFactors=F))
# 2016 - Gent
stats[102,4] <- as.numeric(as.character(voerPark[[16]][169,8]))
# 2016 - Aalst
stats[17,4] <- as.numeric(as.character(voerPark[[16]][137,8]))
# 2016 - Dendermonde
stats[85,4] <- as.numeric(as.character(voerPark[[16]][149,8]))
# 2016 - Kortrijk
stats[119,4] <- as.numeric(as.character(voerPark[[16]][294,8]))
# 2016 - Lokeren
stats[153,4] <- as.numeric(as.character(voerPark[[16]][197,8]))
# 2016 - Lochristi
stats[136,4] <- as.numeric(as.character(voerPark[[16]][171,8]))
# 2016 - Merelbeke
stats[170,4] <- as.numeric(as.character(voerPark[[16]][174,8]))

stats <- setNames(stats, c("Gemeente","NIS","Jaar","Personenwagens","Personenwagens1000","Inwoners"))
stats <- stats[,Personenwagens1000recalc:=Personenwagens/Inwoners]
stats <- stats[,PersonenwagensINC:=Personenwagens/head(Personenwagens,1)*100-100,by=Gemeente]
stats <- stats[,Personenwagens1000INC:=Personenwagens1000recalc/head(Personenwagens1000recalc,1)*100-100,by=Gemeente]
stats <- stats[,Personenperwagen:=Inwoners/Personenwagens]

statsWide <- dcast.data.table(stats,Jaar~Gemeente,value.var="Personenwagens")
p <- plot_ly(statsWide,x=~statsWide$Jaar,y=~statsWide$Gent,name="Gent",type="scatter",mode="lines") %>% 
    add_trace(y = ~statsWide$Dendermonde, name = 'Dendermonde', mode = 'lines') %>% 
    add_trace(y = ~statsWide$Kortrijk, name = 'Kortrijk', mode = 'lines')  %>%
    add_trace(y = ~statsWide$Aalst, name = 'Aalst', mode = 'lines') %>%
    add_trace(y = ~statsWide$Lochristi, name = 'Lochristi', mode = 'lines')  %>%
    add_trace(y = ~statsWide$Lokeren, name = 'Lokeren', mode = 'lines')  %>%
    add_trace(y = ~statsWide$Merelbeke, name = 'Merelbeke', mode = 'lines') %>%
    layout(title="Aantal inschreven personenwagens (2000-2016)",xaxis=list(title="Jaar"),yaxis=list(title="Ingeschreven personenwagens.",rangemode='tozero'))
plotly_POST(p,"1_Wagenpark_Gent_001")

statsWideQ <- dcast.data.table(stats,Jaar~Gemeente,value.var="Personenperwagen")
q <- plot_ly(statsWideQ,x=~statsWideQ$Jaar,y=~statsWideQ$Gent,name="Gent",type="scatter",mode="lines") %>% 
    add_trace(y = ~statsWideQ$Dendermonde, name = 'Dendermonde', mode = 'lines') %>% 
    add_trace(y = ~statsWideQ$Kortrijk, name = 'Kortrijk', mode = 'lines')  %>%
    add_trace(y = ~statsWideQ$Aalst, name = 'Aalst', mode = 'lines') %>%
    add_trace(y = ~statsWideQ$Lochristi, name = 'Lochristi', mode = 'lines')  %>%
    add_trace(y = ~statsWideQ$Lokeren, name = 'Lokeren', mode = 'lines')  %>%
    add_trace(y = ~statsWideQ$Merelbeke, name = 'Merelbeke', mode = 'lines') %>%
    layout(title="Inwoners per personenwagen (2000-2016)",xaxis=list(title="Jaar"),yaxis=list(title="Inwoners per personenwagen",rangemode='tozero'))
plotly_POST(q,"1_Wagenpark_Gent_002")

statsWideR <- dcast.data.table(stats,Jaar~Gemeente,value.var="PersonenwagensINC")
r <- plot_ly(statsWideR,x=~statsWideR$Jaar,y=~statsWideR$Gent,name="Gent",type="scatter",mode="lines") %>% 
    add_trace(y = ~statsWideR$Dendermonde, name = 'Dendermonde', mode = 'lines') %>% 
    add_trace(y = ~statsWideR$Kortrijk, name = 'Kortrijk', mode = 'lines')  %>%
    add_trace(y = ~statsWideR$Aalst, name = 'Aalst', mode = 'lines') %>%
    add_trace(y = ~statsWideR$Lochristi , name = 'Lochristi', mode = 'lines')  %>%
    add_trace(y = ~statsWideR$Lokeren , name = 'Lokeren', mode = 'lines')  %>%
    add_trace(y = ~statsWideR$Merelbeke , name = 'Merelbeke', mode = 'lines') %>%
    add_trace(y = ~statsWideR$`Vlaams Gewest`, name = 'Vlaams Gewest', mode='lines',dash='dot',line=list(color='rgb(0,0,0)',dash='dot')) %>%
    add_trace(y = ~statsWideR$`Resoc.Gent en Gentse rand`, name = 'Gent & Gentse rand', mode='lines',line=list(color='rgb(0,0,0)',dash='dash')) %>%
    layout(title="Procentuele evolutie inschreven personenwagens (2000-2016)",xaxis=list(title="Jaar"),yaxis=list(title="Procentuele stijging ingeschreven personenwagens.",rangemode='tozero'))
plotly_POST(r,"1_Wagenpark_Gent_003")

statsWideS <- dcast.data.table(stats,Jaar~Gemeente,value.var="Personenwagens1000INC")
s <- plot_ly(statsWideS,x=~statsWideS$Jaar,y=~statsWideS$Gent,name="Gent",type="scatter",mode="lines") %>% 
    add_trace(y = ~statsWideS$Dendermonde, name = 'Dendermonde', mode = 'lines') %>% 
    add_trace(y = ~statsWideS$Kortrijk, name = 'Kortrijk', mode = 'lines') %>%
    add_trace(y = ~statsWideS$Aalst, name = 'Aalst', mode = 'lines') %>%
    add_trace(y = ~statsWideS$Lochristi, name = 'Lochristi', mode = 'lines')  %>%
    add_trace(y = ~statsWideS$Lokeren, name = 'Lokeren', mode = 'lines')  %>%
    add_trace(y = ~statsWideS$Merelbeke, name = 'Merelbeke', mode = 'lines') %>%
    add_trace(y = ~statsWideS$`Vlaams Gewest`, name = 'Vlaams Gewest', mode='lines',dash='dot',line=list(color='rgb(0,0,0)',dash='dot')) %>%
    add_trace(y = ~statsWideS$`Resoc.Gent en Gentse rand` , name = 'Gent & Gentse rand', mode='lines',dash='dash',line=list(color='rgb(0,0,0)',dash='dash')) %>%
    layout(title="Procentuele evolutie ingeschreven personenwagens per 1000 inwoners (2000-2016)",xaxis=list(title="Jaar"),yaxis=list(title="Ingeschreven personenwagens per 1000 inwoners.",rangemode='tozero'))
plotly_POST(s,"1_Wagenpark_Gent_004")