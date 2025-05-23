# ---
#   title: "HSQ Network Diagrams"
# author: "Camille Hochheimer, PhD"
# date: "`r Sys.Date()`"
# output: html_document
# ---

library(egor) #for diagrams
library(here) #for sharing this RProject
library(tidyverse) #for data wrangling
library(network) #for diagrams
library(igraph) #for diagrams

#your API token
.token <- Sys.getenv("HSQ_api")

#these are for controlling the size of the circles for the network diagrams
size_vec <- 1:5
scaled <- seq(0.75, 3, length = 5)

#function for wrangling PNA data
wrangle_pna <- function(pna, att_start, att_end, aa_start,
                        a1, a25,
                        namerange = 2:26, tierange = 502:801){

  pna2 <- pna %>%
    rowwise() %>%
    #get rid of people who didn't name an alter or any alter-alter ties
    select(-starts_with("network_diagram_")) %>%
    filter(sum(is.na(across(all_of(namerange)))) == 0 &
             sum(is.na(across(all_of(tierange))), na.rm = TRUE) == 0) %>%
    mutate(across(c(starts_with("alttobacco_a"), starts_with("altsmoke_a")),
                  ~case_when(.x == "Checked" ~ "Yes",
                             .x == "Unchecked" ~ "No")))

  et <- onefile_to_egor(egos = pna2,
                        ID.vars = list(ego = "record_id"),
                        # Name of Variable with the first alter attribute
                        attr.start.col = att_start,
                        # Name of variable with last alter attribute
                        attr.end.col = att_end,
                        # Number of maximum alters that were named by participants
                        max.alters = 25,
                        # Name of first alter-alter variable
                        aa.first.var = aa_start)

  alter_filter2 <- et %>%
    as_tibble() %>%
    arrange(.egoID) %>%
    select(eval(substitute(a1)):eval(substitute(a25))) %>%
    mutate(across(.fns = ~ !is.na(.))) %>%
    as.data.frame() %>%
    tidyr::pivot_longer(cols = everything()) %>%
    pull(value)

  e2 <- et %>%
    activate(alter) %>%
    filter(alter_filter2) %>%
    activate(aatie) %>%
    filter(weight != "")

  #add alter names to the alter data
  e2$alter$aname <- c(t(e2$ego[,2:26]))

  e2$aatie <- e2$aatie %>%
    #show somewhat likely and very likely
    filter(weight != "Not at all likely")

  e2$aatie$weight2 <- ifelse(e2$aatie$weight == "Somewhat likely", 1, 3)

  return(e2)
}

# triangle vertex shape
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }

  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}


# clips as a circle
add_shape("triangle", clip=shapes("circle")$clip,
          plot=mytriangle)


# Baseline PNA -----------------------------------------------------------------
#pull report "ACTION: Needs Network Diagram Baseline"
url <- "https://redcap.ucdenver.edu/api/"
formData <- list("token"=.token,
                 content='report',
                 format='csv',
                 report_id='81493',
                 csvDelimiter='',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)

response <- httr::POST(url, body = formData, encode = "form")
needs_diagram_baseline <- httr::content(response)

#create egor object using that data
pna_baseline <- wrangle_pna(needs_diagram_baseline,
                            att_start = "altgender_a1",
                            att_end = "altquitnow_a25",
                            aa_start = "m1_2",
                            a1 = "alter1",
                            a25 = "alter25")

#what records are included in the report
records <- as.vector(as.numeric(pna_baseline$ego$.egoID))

#this loop creates and uploads a network diagram for all of these records
for (i in records) {
  #select data for nodes and links
  nodes <- pna_baseline$alter %>%
    filter(.egoID == i) %>%
    select(-.egoID) %>%
    rename(id = .altID)

  links <- pna_baseline$aatie %>%
    filter(.egoID == i) %>%
    select(-.egoID) %>%
    rename(from = .srcID, to = .tgtID,
           type = "weight",
           weight = "weight2")

  #turn into an igraph object
  net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)

  #set smoking colors
  smoke_colors <- c("#92c5de", "#f4a582")
  V(net)$smokingstatus <- ifelse(V(net)$alttobacco_a == "No", 1, 2) #vertices of a graph
  V(net)$color <- smoke_colors[V(net)$smokingstatus]
  #weight for edges
  E(net)$width <- E(net)$weight # E() is edges of a graph
  #size for closeness
  V(net)$size <-ifelse(V(net)$altclose_a == "Not close at all", scaled[1],
                       ifelse(V(net)$altclose_a == "A little close", scaled[2],
                              ifelse(V(net)$altclose_a == "Somewhat close", scaled[3],
                                     ifelse(V(net)$altclose_a == "Quite close", scaled[4],
                                            scaled[5]))))*3
  #shapes for gender
  shapes <- c("triangle", "circle", "square")
  V(net)$gender <- ifelse(V(net)$altgender_a == "Man (including transman and transmasculine)", 1,
                          ifelse(V(net)$altgender_a == "Woman (including transwoman and transfeminine)", 2, 3))
  V(net)$shape <- shapes[V(net)$gender]
  # Fruchterman-Reingold layout
  l <- layout_with_fr(net)

  #create a temporary png file
  SimplePlot.file <- tempfile(pattern = paste0("networkdiagram_ID", i, "_"),
                              fileext = ".png")
  png(filename = SimplePlot.file,
      width = 2200, height = 1400, res = 100,
      units = "px",
      pointsize = 24)
  # expand margin on the right side for the legend
  par(mar=c(par("mar")[1:3], 10))
  #plot network diagram
  plot(net, layout = l, vertex.label = V(net)$aname,
       ylim = c(-1, 1), xlim = c(-1, 1), asp = 0)
  # add the first legend and save it's position
  l1 <- legend("topleft",
               legend = c("Smoker", "Non-smoker"),
               pch = 19, bty = "n",
               pt.cex = scaled[3],
               col = c("#f4a582", "#92c5de"),
               title = "Smoking status",
               xpd=TRUE, inset=c(1,0), title.adj=c(0))
  # add second legend and adjust x axis position based on height of first legend
  l2 <- legend(x = l1$rect$left, y = l1$rect$top - l1$rect$h,
               legend = c("Man", "Woman", "Different gender\nor declined"),
               pch = c(17, 19, 15), bty = "n",
               pt.cex = scaled[3], title.adj=c(0),
               title = "Gender", xpd = TRUE,
               inset = c(1,0))
  # add third legend and adjust x axis position based on height of second legend
  legend(x = l2$rect$left, y = l2$rect$top - l2$rect$h,
         legend=c("Not close at all", "A little close",
                  "Somewhat close", "Quite close", "Very close"),
         pt.cex= scaled,col='black',pch=21, pt.bg='black', bty = "n",
         title = "How close to you", xpd = TRUE,
         inset = c(1,0), title.adj=c(0))
  dev.off()
  #upload to REDCap
  REDCapR::redcap_upload_file_oneshot(file_name = SimplePlot.file,
                                      record = i,
                                      field = "network_diagram_baseline",
                                      redcap_uri = url,
                                      token = .token)
  unlink(SimplePlot.file)
}


# 12-month PNA -----------------------------------------------------------------
#pull report "ACTION: Needs Network Diagram 12 - Month" from REDCap
url <- "https://redcap.ucdenver.edu/api/"
formData <- list("token"=.token,
                 content='report',
                 format='csv',
                 report_id='81494',
                 csvDelimiter='',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
needs_diagram_12m <- httr::content(response)

#create egor object using that data
pna_12m <- wrangle_pna(needs_diagram_12m,
                       att_start = "altgender_a1_12m",
                       att_end = "altquitnow_a25_12m",
                       aa_start = "m1_2_12m",
                       a1 = "alter1_12m",
                       a25 = "alter25_12m")

#what records are included in the report
records <- as.vector(as.numeric(pna_12m$ego$.egoID))

#this loop creates and uploads a network diagram for all of these records
for(i in records){
  #select data for nodes and links
  nodes2 <- pna_12m$alter %>%
    filter(.egoID == i) %>%
    select(-.egoID) %>%
    rename(id = .altID)

  links2 <- pna_12m$aatie %>%
    filter(.egoID == i) %>%
    select(-.egoID) %>%
    rename(from = .srcID, to = .tgtID,
           type = "weight",
           weight = "weight2")

  #turn into an igraph object
  net2 <- graph_from_data_frame(d=links2, vertices=nodes2, directed=F)

  #set smoking colors
  smoke_colors <- c("#92c5de", "#f4a582")
  V(net2)$smokingstatus <- ifelse(V(net2)$alttobacco_a == "No", 1, 2)
  V(net2)$color <- smoke_colors[V(net2)$smokingstatus]
  #weight for edges
  E(net2)$width <- E(net2)$weight
  #size for closeness
  V(net)$size <-ifelse(V(net)$altclose_a == "Not close at all", scaled[1],
                       ifelse(V(net)$altclose_a == "A little close", scaled[2],
                              ifelse(V(net)$altclose_a == "Somewhat close", scaled[3],
                                     ifelse(V(net)$altclose_a == "Quite close", scaled[4],
                                            scaled[5]))))*3
  #shapes for gender
  shapes <- c("triangle", "circle", "square")
  V(net2)$gender <- ifelse(V(net2)$altgender_a == "Man (including transman and transmasculine)", 1,
                           ifelse(V(net2)$altgender_a == "Woman (including transwoman and transfeminine)", 2, 3))
  V(net2)$shape <- shapes[V(net2)$gender]
  # Fruchterman-Reingold layout
  l <- layout_with_fr(net2)

  #create a temporary png file
  SimplePlot.file <- tempfile(pattern = paste0("networkdiagram_12m_ID", i, "_"),
                              fileext = ".png")
  png(filename = SimplePlot.file, width = 1100, height = 700, units = "px",
      pointsize = 24)
  # expand margin on the right side for the legend
  par(mar=c(par("mar")[1:3], 10))
  #plot network diagram
  plot(net2, layout = l, vertex.label = V(net2)$aname,
       ylim = c(-1, 1), xlim = c(-1, 1), asp = 0)
  # add the first legend and save it's position
  l1 <- legend("topleft",
               legend = c("Smoker", "Non-smoker"),
               pch = 19, bty = "n",
               pt.cex = scaled[3],
               col = c("#f4a582", "#92c5de"),
               title = "Smoking status",
               xpd=TRUE, inset=c(1,0), title.adj=c(0))
  # add second legend and adjust x axis position based on height of first legend
  l2 <- legend(x = l1$rect$left, y = l1$rect$top - l1$rect$h,
               legend = c("Man", "Woman", "Different gender\nor declined"),
               pch = c(17, 19, 15), bty = "n",
               pt.cex = scaled[3], title.adj=c(0),
               title = "Gender", xpd = TRUE,
               inset = c(1,0))
  # add third legend and adjust x axis position based on height of second legend
  legend(x = l2$rect$left, y = l2$rect$top - l2$rect$h,
         legend=c("Not close at all", "A little close",
                  "Somewhat close", "Quite close", "Very close"),
         pt.cex= scaled,col='black',pch=21, pt.bg='black', bty = "n",
         title = "How close to you", xpd = TRUE,
         inset = c(1,0), title.adj=c(0))
  dev.off()
  #upload to REDCap
  REDCapR::redcap_upload_file_oneshot(file_name = SimplePlot.file,
                                      record = i,
                                      field = "network_diagram_12m",
                                      redcap_uri = url,
                                      token = .token)
  unlink(SimplePlot.file)

}
