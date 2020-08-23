
setwd('~/Projects2/Transcriptome_Portal_07142020/')

library(readxl)
library(ggplot2)

google.red <- '#ea4235'
google.yellow <- '#fabd03'
google.green <- '#34a853'
google.blue <- '#4286f5'

#######################################################################

stat.history <- read_excel('data/GEO/GEO_Summary_08202020.xlsx', sheet='Stat_History')
stat.history <- data.frame(stat.history, stringsAsFactors = F)
stat.history

if (stat.history$Quarter[1]==4) {
  idx <- which(stat.history$Quarter==4)
} else {
  idx <- c(1, which(stat.history$Quarter==4))
}

stat.history <- stat.history[idx,]
stat.history

library(reshape2)
stat.history <- melt(stat.history, id.vars=c("Year",'Quarter'))
colnames(stat.history)[3:4] <- c('Scope','Count')
stat.history

dataForBarPlot <- stat.history
dataForBarPlot[dataForBarPlot==0] <- 1

dataForBarPlot$Scope <- factor(dataForBarPlot$Scope,
                                 levels=c('Platforms','Series','Samples'))

idx <- which(dataForBarPlot$Scope=='Series')
dataForBarPlot <- dataForBarPlot[idx,]


ggplot(dataForBarPlot, aes(y = Count, x = Year, fill = Scope)) + 
  geom_bar(stat="identity", width=0.8, position='dodge') +
  #geom_text(aes(y = pos, x = pathway, label = paste0(percentage, "%")),
  #           data = dataForPlot) +
  labs(x='Year', y='Count') +
  #facet_grid(Category ~ .) + #, scales = "free"
  #scale_fill_manual(values=fill) +
  #scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  scale_y_continuous(trans = 'log2',
                     breaks = c(0,10,100,1000,10000,100000,1000000),
                     labels = scales::comma) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour='white'),
                   panel.background = element_blank()) +
  theme(axis.text=element_text(size=12, color='black', face = 'bold'),
        axis.text.x =element_text(size=12, color='black', face = 'bold'),
        axis.title.x =element_blank(),
        axis.title.y =element_text(size=16, face = 'bold'),
        strip.text = element_text(face = 'bold', size=12)) +
  theme(legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = 'top') #+
  #theme(plot.margin =  margin(t = 0.25, r = 0.5, b = 0.25, l = 0.25, unit = "cm"))


######
dataForBarPlot <- stat.history

idx <- which(dataForBarPlot$Scope=='Series')
dataForBarPlot <- dataForBarPlot[idx,]


ggplot(dataForBarPlot, aes(y = Count, x = Year, fill = Scope)) + 
  geom_bar(stat="identity", width=0.8, position='dodge') +
  #geom_text(aes(y = pos, x = pathway, label = paste0(percentage, "%")),
  #           data = dataForPlot) +
  labs(x='Year', y='Count') +
  geom_text(aes(label=format(Count,big.mark=",",scientific=FALSE)), vjust=-0.5, size=4) +
  #facet_grid(Category ~ .) + #, scales = "free"
  scale_fill_manual(values=google.blue) +
  #scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  # scale_y_continuous(trans = 'log2',
  #                    breaks = c(0,10,100,1000,10000,100000,1000000),
  #                    labels = scales::comma) +
  scale_y_continuous(#trans = 'log2',
    limits = c(0,140000),
    breaks = seq(0,150000,10000),
    labels = scales::comma) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   #panel.grid.major = element_blank(),
                   #panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour='white'),
                   panel.background = element_blank()) +
  theme(axis.text=element_text(size=12, color='black', face = 'bold'),
        axis.text.x =element_text(size=12, color='black', face = 'bold'),
        #axis.title.x =element_blank(),
        axis.title =element_text(size=16, face = 'bold'),
        strip.text = element_text(face = 'bold', size=12)) +
  theme(legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = 'none') #+
#theme(plot.margin =  margin(t = 0.25, r = 0.5, b = 0.25, l = 0.25, unit = "cm"))


dataForBarPlot <- stat.history

idx <- which(dataForBarPlot$Scope=='Samples')
dataForBarPlot <- dataForBarPlot[idx,]


ggplot(dataForBarPlot, aes(y = Count, x = Year, fill = Scope)) + 
  geom_bar(stat="identity", width=0.8, position='dodge') +
  #geom_text(aes(y = pos, x = pathway, label = paste0(percentage, "%")),
  #           data = dataForPlot) +
  labs(x='Year', y='Count') +
  geom_text(aes(label=format(Count,big.mark=",",scientific=FALSE)), vjust=-0.5, size=4) +
  #facet_grid(Category ~ .) + #, scales = "free"
  scale_fill_manual(values=google.red) +
  #scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  # scale_y_continuous(trans = 'log2',
  #                    breaks = c(0,10,100,1000,10000,100000,1000000),
  #                    labels = scales::comma) +
  scale_y_continuous(#trans = 'log2',
    #limits = c(0,145000),
    breaks = seq(0,38000000,300000),
    labels = scales::comma) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   #panel.grid.major = element_blank(),
                   #panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour='white'),
                   panel.background = element_blank()) +
  theme(axis.text=element_text(size=12, color='black', face = 'bold'),
        axis.text.x =element_text(size=12, color='black', face = 'bold'),
        #axis.title.x =element_blank(),
        axis.title =element_text(size=16, face = 'bold'),
        strip.text = element_text(face = 'bold', size=12)) +
  theme(legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = 'none') #+
#theme(plot.margin =  margin(t = 0.25, r = 0.5, b = 0.25, l = 0.25, unit = "cm"))


############################################################################################

series <- read_excel('data/GEO/GEO_Summary_08202020.xlsx', sheet='Series')
series <- data.frame(series, stringsAsFactors = F)
series

dataForBarPlot <- series

dataForBarPlot$Series.type <- factor(dataForBarPlot$Series.type,
                               levels=dataForBarPlot$Series.type)



ggplot(dataForBarPlot, aes(y = Count, x = Series.type, fill = google.blue)) + 
  geom_bar(stat="identity", width=0.8, position='dodge') +
  #geom_text(aes(y = pos, x = pathway, label = paste0(percentage, "%")),
  #           data = dataForPlot) +
  labs(x='Series Type', y='Count') +
  geom_text(aes(label=format(Count,big.mark=",",scientific=FALSE)), vjust=-0.5, size=4) +
  #facet_grid(Category ~ .) + #, scales = "free"
  scale_fill_manual(values=google.blue) +
  #scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  # scale_y_continuous(trans = 'log2',
  #                    breaks = c(0,10,100,1000,10000,100000,1000000),
  #                    labels = scales::comma) +
  scale_y_continuous(#trans = 'log2',
    limits = c(0,65000),
    breaks = seq(0,65000,10000),
    labels = scales::comma) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   #panel.grid.major = element_blank(),
                   #panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour='white'),
                   panel.background = element_blank()) +
  theme(axis.text=element_text(size=12, color='black', face = 'bold'),
        axis.text.x =element_text(size=12, color='black', face = 'bold', angle = 45, hjust = 1),
        #axis.title.x =element_blank(),
        axis.title =element_text(size=16, face = 'bold'),
        strip.text = element_text(face = 'bold', size=12)) +
  theme(legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = 'none') +
theme(plot.margin =  margin(t = 0.25, r = 0.5, b = 0.25, l = 5, unit = "cm"))




###
series <- read_excel('data/GEO/GEO_Summary_08202020.xlsx', sheet='Organisms')
series <- data.frame(series, stringsAsFactors = F)
series

dataForBarPlot <- series

dataForBarPlot <- dataForBarPlot[dataForBarPlot$Top=='YES',]

o <- order(dataForBarPlot$Series, decreasing = T)
dataForBarPlot <- dataForBarPlot[o,]


dataForBarPlot$Organism <- factor(dataForBarPlot$Organism,
                                     levels=dataForBarPlot$Organism)



ggplot(dataForBarPlot, aes(y = Series, x = Organism, fill = google.blue)) + 
  geom_bar(stat="identity", width=0.8, position='dodge') +
  #geom_text(aes(y = pos, x = pathway, label = paste0(percentage, "%")),
  #           data = dataForPlot) +
  labs(x='Organism', y='Count') +
  geom_text(aes(label=format(Series,big.mark=",",scientific=FALSE)), vjust=-0.5, size=4) +
  #facet_grid(Category ~ .) + #, scales = "free"
  scale_fill_manual(values=google.blue) +
  #scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  # scale_y_continuous(trans = 'log2',
  #                    breaks = c(0,10,100,1000,10000,100000,1000000),
  #                    labels = scales::comma) +
  scale_y_continuous(#trans = 'log2',
    limits = c(0,60000),
    breaks = seq(0,60000,10000),
    labels = scales::comma) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),
                   #panel.grid.major = element_blank(),
                   #panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour='white'),
                   panel.background = element_blank()) +
  theme(axis.text=element_text(size=12, color='black', face = 'bold'),
        axis.text.x =element_text(size=12, color='black', face = 'bold', angle = 45, hjust = 1),
        #axis.title.x =element_blank(),
        axis.title =element_text(size=16, face = 'bold'),
        strip.text = element_text(face = 'bold', size=12)) +
  theme(legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = 'none') #+
  #theme(plot.margin =  margin(t = 0.25, r = 0.5, b = 0.25, l = 5, unit = "cm"))


########################################################################


##########################

downloadSeriesSummary <- function(num.series, dest.dir=NULL) {
  
  num.page <- seq_len(as.integer(num.series/5000+1))

  start.acc <- seq(1, num.series - (num.series %% 5000) + 1, 5000)
  end.acc <- start.acc + 5000 - 1
  end.acc[num.page[length(num.page)]] <- num.series

  for (i in rev(num.page)) {
    
    url <- paste0('https://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=date&mode=csv&page=', i, '&display=5000')
    target.file <- file.path(dest.dir, paste0('Series.', i, '.', Sys.Date(), '.csv'))
    
    download.file(url, destfile = target.file)
    
  }
  
}

num.series <- 134564

downloadSeriesSummary(num.series, dest.dir = 'data/')


#########

num.page <- seq_len(as.integer(num.series/5000+1))

start.acc <- seq(1, num.series - (num.series %% 5000) + 1, 5000)
end.acc <- start.acc + 5000 - 1
end.acc[num.page[length(num.page)]] <- num.series


date <- '2020-08-21'
series.table <- c()

for (i in 1:length(start.acc)) {
  fl <- paste0('Series.', i, '.', start.acc[i], '_', end.acc[i], '.', date, '.csv')
  
  series <- read.csv(file.path('data/GEO', fl), header = T, stringsAsFactors = F)
  series.table <- rbind(series.table, series)
  
}

series.table <- data.frame(series.table, stringsAsFactors = F)
saveRDS(series.table, file=paste0('data/GEO/GEO_Series_Table_', date, '.RDS'))


###
series.table <- readRDS(file='data/GEO/GEO_Series_Table_08202020.RDS')
fls <- list.files('data/GEO/Series_SOFT/')
fls <- gsub('.txt', '', fls)
fls

for (accession in series.table$Accession) {
  print (accession)
  
  if (! accession %in% fls) {
    downloadAccessionInfo(accession = accession, dest.dir = 'data/GEO/Series_SOFT/')
  }
  
}


downloadAccessionInfo <- function(scope='series', amount='full', accession=NULL, dest.dir=NULL) {
  ### scope: series, platform, samples, family
  ### amount: brief, quick, full, data
  ### format: SOFT (HTML, MINiML) -- Only SOFT is used
  
  if (scope=='series') {
    targ <- 'gse'
  } else if (scope=='platform') {
    targ <- 'gpl'
  } else if (scope=='samples') {
    targ <- 'gsm'
  } else if (scope=='family') {
    targ <- 'all'
  }
  
  geo.url <- paste0('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', accession, '&targ=', targ, '&form=text&view=', amount)
  
  target.file <- file.path(dest.dir, paste0(accession, '.txt'))
  download.file(url = geo.url, destfile = target.file)
  
}



# series.table <- readRDS(file='data/GEO/GEO_Series_Table_08202020.RDS')
# 
# fls <- list.files('data/GEO/Series_SOFT/')
# fls <- gsub('.txt', '', fls)
# 
# idx <- which(!series.table$Accession %in% fls)
# idx
# 
# 
# for (accession in series.table$Accession[idx]) {
#   print (accession)
#   
#   downloadAccessionInfo(accession = accession, dest.dir = 'data/GEO/Series_SOFT/')
#   
# }


#####################################################

series.table <- readRDS(file='data/GEO/GEO_Series_Table_08202020.RDS')
organisms <- series.table$Accession[series.table$Taxonomy %in% c('Homo sapiens','Mus musculus','Rattus norvegicus')]
length(organisms)


series.type <- series.table$Accession[series.table$Series.Type %in% c('Expression profiling by array','Expression profiling by high throughput sequencing')]

dataForVennDiagram

library(VennDiagram)

###### Two sets
dataForVennDiagram <- list(organisms=organisms,series.type=series.type)

vennDiagramColors <- c(google.blue, google.red)

vennFilename <- 'report/Venn_Diagram_2Sets_Normal.png'

vennPlot <- venn.diagram(dataForVennDiagram,
                         filename = vennFilename,
                         imagetype = 'png',
                         col = vennDiagramColors,
                         fill = vennDiagramColors,
                         lwd = 2,
                         alpha = 0.25,
                         fontfamily = 'sans',
                         margin = 0.2,
                         cat.dist = 0.03,
                         #cat.default.pos = "outer",
                         #cat.pos = c(-27, 27, 135),
                         cat.col = vennDiagramColors,
                         cat.fontface = 'bold',
                         cat.fontfamily = 'sans',
                         cat.cex = 0,
                         cex = 1)



#####

series.table <- readRDS(file='data/GEO/GEO_Series_Table_08202020.RDS')
dim(series.table)

num.series <- 134618

diff.series <- num.series - nrow(series.table)
diff.series

num.page <- seq_len(as.integer(diff.series/5000+1))
num.page


for (i in rev(num.page)) {
  
  url <- paste0('https://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=date&mode=csv&page=', i, '&display=5000')
  target.file <- file.path('data/GEO/', paste0('Series.', i, '.', Sys.Date(), '.csv'))
  
  download.file(url, destfile = target.file)
  
  new.series <- read.csv(target.file, header = T, stringsAsFactors = F)
  new.idx <- which(!new.series$Accession %in% series.table$Accession)
  new.series <- new.series[new.idx,]
  
  # for (accession in new.series$Accession) {
  #   print (accession)
  #   
  #   if (! accession %in% list.files('data/GEO/Series_SOFT/')) {
  #     downloadAccessionInfo(accession = accession, dest.dir = 'data/GEO/Series_SOFT/')
  #   }
  #   
  # }
  
  series.table <- data.frame(rbind(new.series, series.table), stringsAsFactors = F)
  
}


saveRDS(series.table, file=paste0('data/GEO/GEO_Series_Table_', Sys.Date(), '.RDS'))


######
for (i in rev(num.page)) {
  
  url <- paste0('https://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=date&mode=csv&page=', i, '&display=5000')
  target.file <- file.path('data/GEO/', paste0('Series.', i, '.', Sys.Date(), '.csv'))
  
  download.file(url, destfile = target.file)
  
  new.series <- read.csv(target.file, header = T, stringsAsFactors = F)
  new.idx <- which(!new.series$Accession %in% series.table$Accession)
  new.series <- new.series[new.idx,]
  
  for (accession in new.series$Accession) {
    print (accession)
    
    if (! accession %in% list.files('data/GEO/Series_SOFT/')) {
      downloadAccessionInfo(accession = accession, dest.dir = 'data/GEO/Series_SOFT/')
    }
    
  }
  
}

  
  
#####################################################################################

parseSOFTfile <- function(file) {
  
  info <- readLines(file)
  
  accession <- strsplit(info[grep('^!Series_geo_accession', info)], split = ' = ')[[1]][2]
  title <- strsplit(info[grep('^!Series_title', info)], split = ' = ')[[1]][2]
  # series.type <- strsplit(info[grep('^!Series_type', info)], split = ' = ')[[1]][2]
  # series.summary <- strsplit(info[grep('^!Series_summary', info)], split = ' = ')[[1]][2]
  if (length(grep('^!Series_overall_design', info))==0) {
    overall.design <- NA
  } else {
    overall.design <- strsplit(info[grep('^!Series_overall_design', info)], split = ' = ')[[1]][2]
  }
  # platform <- strsplit(info[grep('^!Series_platform_id', info)], split = ' = ')[[1]][2]
  date <- strsplit(info[grep('^!Series_status = Public', info)], split = ' Public on ')[[1]][2]
  date <- gsub('(\\w+) (\\d+) (\\d+)', paste0('\\1',' ', '\\2', ', ', '\\3'), date)
  # sample.size <- sum(grepl('^!Series_sample_id', info))
  
  
  # accession = paste(colsplit(string=info[grep('^!Series_geo_accession', info)], pattern=" = ", names=c('x','value'))$value, collapse = '; ')
  # title <- paste(colsplit(string=info[grep('^!Series_title', info)], pattern=" = ", names=c('x','value'))$value, collapse = '; ')
  series.type <- paste(colsplit(string=info[grep('^!Series_type', info)], pattern=" = ", names=c('x','value'))$value, collapse = '; ')
  series.summary <- paste(colsplit(string=info[grep('^!Series_summary', info)], pattern=" = ", names=c('x','value'))$value, collapse = '; ')
  # overall.design <- paste(colsplit(string=info[grep('^!Series_overall_design', info)], pattern=" = ", names=c('x','value'))$value, collapse = '; ')
  platform <- paste(colsplit(string=info[grep('^!Series_platform_id', info)], pattern=" = ", names=c('x','value'))$value, collapse = '; ')
  sample.size <- sum(grepl('^!Series_sample_id', info))
  
  if (length(grep('^!Series_pubmed_id', info))==0) {
    pubmed.id <- NA
  } else {
    pubmed.id <- paste(colsplit(string=info[grep('^!Series_pubmed_id', info)], pattern=" = ", names=c('x','value'))$value, collapse = ';')
    #  strsplit(info[grep('^!Series_pubmed_id', info)], split = ' = ')[[1]][2]
  }
  
  return(c(accession, title, series.type, sample.size, series.summary, overall.design, platform, date, pubmed.id))
  
}



series.info <- list()

for (accession in series.table$Accession[69237:nrow(series.table)]) {
  print (accession)
  
  fl <- file.path('data/GEO/Series_SOFT/', paste0(accession, '.txt'))
  
  series.info[[accession]] <- parseSOFTfile(fl)
  
}

series.info <- do.call(rbind, series.info)
series.info <- data.frame(series.info, stringsAsFactors = F)
series.info
colnames(series.info) <- c('Accession','Title','Series.Type','Sample.Size','Summary','Overall.Design','Platform','Date','Pubmed.ID')

View(series.info)


# downloadAccessionInfo(accession = 'GSE74038', dest.dir = 'data/GEO/Series_SOFT/')
# which(series.table$Accession=='GSE74038')

saveRDS(series.info, file=paste0('data/GEO_Series_Info_', Sys.Date(), '.RDS'))


####################

downloadPlatformsSummary <- function(num.platforms, dest.dir=NULL) {
  
  num.page <- seq_len(as.integer(num.platforms/5000+1))
  
  start.acc <- seq(1, num.platforms - (num.platforms %% 5000) + 1, 5000)
  end.acc <- start.acc + 5000 - 1
  end.acc[num.page[length(num.page)]] <- num.platforms
  
  for (i in rev(num.page)) {
    
    url <- paste0('https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms&zsort=date&mode=csv&page=', i, '&display=5000')
    target.file <- file.path(dest.dir, paste0('Platforms.', i, '.', Sys.Date(), '.csv'))
    
    download.file(url, destfile = target.file)
    
  }
  
}

num.platforms <- 21300

downloadPlatformsSummary(num.platforms, dest.dir = 'data/')

###
num.page <- seq_len(as.integer(num.platforms/5000+1))

date <- '2020-08-22'
platforms.table <- c()

for (i in 1:length(start.acc)) {
  fl <- paste0('Platforms.', i, '.', date, '.csv')
  
  platforms <- read.csv(file.path('data/GEO', fl), header = T, stringsAsFactors = F)
  platforms.table <- rbind(platforms.table, platforms)
  
}

platforms.table <- data.frame(platforms.table, stringsAsFactors = F)
View(platforms.table)
saveRDS(platforms.table, file=paste0('data/GEO/GEO_Platforms_Table_', date, '.RDS'))


###

series.info$Platform[1:5]

platforms <- lapply(series.info$Platform, function(x) strsplit(x, '; ')[[1]])

idx <- lapply(platforms, function(x) match(x, platforms.table$Accession))
idx

arrays <- lapply(idx, function(x) paste(platforms.table$Title[x], collapse = '; '))
arrays <- unlist(arrays)

series.info$Technology <- arrays

saveRDS(series.info, file=paste0('data/GEO_Series_Info_', Sys.Date(), '.RDS'))



organisms <- c('Homo sapiens', 'Mus musculus', 'Rattus norvegicus')
series.type <- c('Expression profiling by array', 'Expression profiling by high throughput sequencing')

series.summary <- data.frame(series.table, series.info, stringsAsFactors = F)
View(series.summary)
saveRDS(series.summary, file=paste0('data/GEO_Series_Summary_', Sys.Date(), '.RDS'))


#####
idx.organisms <- grep(paste(organisms, collapse = '|'), series.summary$Taxonomy)
idx.series.type <- grep(paste(series.type, collapse = '|'), series.summary$Series.Type)
idx.series.type

idx <- Reduce(intersect, list(idx.organisms, idx.series.type))

series.summary <- series.summary[idx,]





######
txt <- paste(series.info$Title, series.info$summary, series.info$Overall.Design)
idx <- grep('tumor|cancer', txt, ignore.case = TRUE)

txt[idx]

View(series.info)
