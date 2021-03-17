#Installing necessary libs
#install.packages("igraph")
library(igraph)
library(tidyverse)
##Setting the working directory
setwd("E:/BANA277/Social Network")
#Imported the datasets using the file -> Import datasest -> from text(readr)

#1.Delete products that are not books from "products" and "copurchase" files. 
#And then delete the books with salesrank>150,000 or salesrank = -1.

Prod_book <- products %>% filter(group=='Book' & salesrank<=150000 & salesrank != -1)
#check for the group
unique(Prod_book$group)

Copur_book <- copurchase %>% filter(Source %in% Prod_book$id & 
                                         copurchase$Target %in% Prod_book$id)

#2.Create a variable named in-degree, to show how many "Source" products 
#people who buy "Target" products buy; i.e. how many edges are to the focal 
#product in "co-purchase" network.

head(Copur_book)

g <- graph.data.frame(Copur_book, directed = T)
gorder(g)
#20684
gsize(g)
#22460

in_degree <- degree(g, mode = 'in')
head(in_degree)

#3.Create a variable named out-degree, to show how many "Target" products people who
#buy "Source" product also buy; i.e., how many edges are from the focal product in
#"co-purchase" network.
out_degree <- degree(g, mode = 'out')
head(out_degree)

#4.Pick up one of the products (in case there are multiple) with highest degree
#(in-degree + out-degree), and find its subcomponent, i.e., all the products that
#are connected to this focal product. From this point on, you will work only on this subcomponent. 

all_degree <- degree(g, mode = 'all')
max(all_degree)
all_degree[all_degree==53]
##There are two products 4429   33 
## 33 is chosen for further analysis
#get the subcomponent
sub_co <- subcomponent(g, "33",'all')

#5.Visualize the subcomponent using iGraph, trying out different colors, node and edge sizes and
#layouts, so that the result is most appealing. Find the diameter, and color the nodes along the 
#diameter. Provide your insights from the visualizations.

graph <- induced_subgraph(g, sub_co)
#Vertices
V(graph)
#Edges
E(graph)
#Visualization of the graph
V(graph)$label <- V(graph)$name
V(graph)$degree <- degree(graph)
plot(graph,
     vertex.color='green',
     vertex.size= V(graph)$degree*0.5,
     edge.color='grey',
     edge.arrow.size=0.2,
     vertex.label.cex=0.1,
     layout=layout.sphere, main="sphere")

#Diameter
diameter(graph, directed = T, weights = NA)
dia <- get_diameter(graph, weights = NULL)
dia

V(graph)$color<-"green"
V(graph)$color[dia]<-"red"

plot(graph,
     vertex.color=V(graph)$color,
     vertex.size= V(graph)$degree,
     edge.arrow.size=0.01,
     vertex.label.cex=0.01,
     layout=layout.sphere, main="sphere")

#Diameter is the longest distance between two vertices, the 
#diameter is 9 here. In the graph, the 10 red nodes are the vertices 
#that on the longest path, and they are 37895, 27936, 21584, 10889, 11080, 14111, 4429, 2501, 3588, 6676.
#These 904 vertices are the book ids that connected to the book whose id = 33, 
#directly. Size of the vertices represents the number of vertices 
#that are connected to a vertex; the bigger of the vertex, the more vertices link to it.
#The distance between each vertex represents how strong the vertices connect to each other;
#the longer the ties, the weaker the relationship. Therefore, some vertices look like clusters in the middle with short edges, which means these books have 
#strong connections. Some vertices are nodes on the edges, which means weaker connections.


#6.Compute various statistics about this network (i.e., subcomponent), including degree distribution, 
#density, and centrality (degree centrality, closeness centrality and between centrality), 
#hub/authority scores, etc. Interpret your results.

#degree_distribution
dg_all <- degree_distribution(graph, cumulative=T, mode="all")
plot( x=0:max(all_degree), y=1-dg_all, pch=19, cex=1.2, col="green", 
      xlab="Degree", ylab="Cumulative Frequency")
#Degree

t_degree = table(degree(graph))
table(t_degree / sum(t_degree))

#Degree distribution
plot(t_degree / sum(t_degree), xlim=c(1,53), ylim=c(0,.4), xlab="Degree", ylab="Frequency")

#The highly connected nodes, the hubs of the networks, 
#are generally responsible for keeping the network connected. networks with long tail degree 
#distributions are resilient to random removal of nodes (failure) but vulnerable to removal of the the hub nodes (attack).
#Summary stats(the mean is larger than the median the distribution is right skewed )
summary(degree(graph))

#density
edge_density(graph, loops=F)

#centrality
degree(graph, mode="all")
centr_degree(graph)

closeness<-closeness(graph, mode='all', weights=NA)
head(closeness)

betweenness<-betweenness(graph, directed='T', weights=NA)
head(betweenness)

#hub/authority scores
hub_score<- hub_score(graph, weights=NA)$vector 
head(hub_score)

authority_score<- authority_score(graph, weights=NA)$vector
head(authority_score)

#7.Create a group of variables containing the
#information of neighbors that "point to" focal products. The variables include:

prod <-  products
prod$id<-as.vector(prod$id)
sub_id<-as_ids(sub_co)
product_sub_co<-products[products$id %in% sub_id,]
head(product_sub_co)
metrices<-Copur_book %>%
        group_by(Target) %>%
        inner_join(product_sub_co,by=c('Source'='id'))%>%
        summarise(nghb_mn_rating=mean(rating),
                  nghb_mn_salesrank=mean(salesrank), 
                  nghb_mn_review_cnt=mean(review_cnt))
head(metrices)

#convert all igraph lists to data frames
#shift index col to the right and rename col. accordingly
df_in_degree <- as.data.frame(in_degree)
head(df_in_degree)
df_in_degree <- cbind("Nodes" = rownames(df_in_degree), df_in_degree)
rownames(df_in_degree) <- 1:nrow(df_in_degree)
colnames(df_in_degree)[1] = "Nodes"
head(df_in_degree)

df_out_degree <- as.data.frame(out_degree)
df_out_degree <- cbind(newColName = rownames(df_out_degree), df_out_degree)
rownames(df_out_degree) <- 1:nrow(df_out_degree)
colnames(df_out_degree) <- c("Nodes", "out_degree")

df_closeness <- as.data.frame(closeness)
df_closeness <- cbind(newColName = rownames(df_closeness), df_closeness)
rownames(df_closeness) <- 1:nrow(df_closeness)
colnames(df_closeness) <- c("Nodes", "closeness")

df_betweenness <- as.data.frame(betweenness)
df_betweenness <- cbind(newColName = rownames(df_betweenness), df_betweenness)
rownames(df_betweenness) <- 1:nrow(df_betweenness)
colnames(df_betweenness) <- c("Nodes", "betweenness")

df_hub_score <- as.data.frame(hub_score)
df_hub_score <- cbind(newColName = rownames(df_hub_score), df_hub_score)
rownames(df_hub_score) <- 1:nrow(df_hub_score)
colnames(df_hub_score) <- c("Nodes", "hub_score")
head(df_hub_score)

df_authority_score <- as.data.frame(authority_score)
df_authority_score <- cbind(newColName = rownames(df_authority_score), df_authority_score)
rownames(df_authority_score) <- 1:nrow(df_authority_score)
colnames(df_authority_score) <- c("Nodes", "authority_score")
head(df_authority_score)
#combine data frames into one data frame by nodes
library(sqldf)
#install.packages("sqldf")

model_data <- sqldf("SELECT metrices.Target, hub_score, betweenness, authority_score, 
closeness, in_degree, out_degree, nghb_mn_rating, nghb_mn_salesrank, nghb_mn_review_cnt,
prod.review_cnt, prod.downloads, prod.rating, prod.salesrank
                      FROM metrices, prod, df_hub_score, df_betweenness, 
                      df_authority_score, df_closeness, df_in_degree, df_out_degree
                      WHERE metrices.Target = df_betweenness.Nodes 
                      and metrices.Target = df_authority_score.Nodes
                      and metrices.Target = df_closeness.Nodes
                      and metrices.Target = df_in_degree.Nodes
                      and metrices.Target = df_out_degree.Nodes
                      and metrices.Target = df_hub_score.Nodes
                      and metrices.Target = prod.id")
head(model_data)

#run poisson regression
salesrank_prediction<- glm(salesrank ~ review_cnt + downloads + rating + hub_score + betweenness + 
                                             authority_score + closeness + in_degree + out_degree + 
                                             nghb_mn_rating + nghb_mn_salesrank + nghb_mn_review_cnt, family="poisson",
                                     data=model_data)
summary(salesrank_prediction)
