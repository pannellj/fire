# landscape flammability model for NSC BH Farming and Nature Conservation
# J Pannell - September 2019
# Auckland University of Technology
# jennypannell@gmail.com

#### scenario specification ####
# This section tests whether we are running a baseline or replanting scenario and if the latter, reassigns gully 
# polygons as the appropriate vegetation class (pine, kanuka or mixed native)

# Firstly, specify how big the landscape is in total
landscape.total<-sum(polygons$AREA_GEO)
# Make a vector of all of the polygon IDs that are gullies
gullies<-subset(polygons, Veg_Class=="Pasture"&Gully=="Yes")
# set cumulative area and landscape proportion parameters to 0
area<-0 
proportion.assigned<-0
# create a test object set to fail unless becomes pass
test<-"fail"
# now reassign polygons according to which scenario we are in. 
if (scenario=="baseline"){
  print("Baseline scenario - no reassignment necessary")
  test<-"pass"
}else if (scenario=="kanuka"){
  while(proportion.assigned<proportion.replant){
    # while the total area reassigned is less than the proportion of the landscape we want to replant
    # select a random pasture gully and reassign it to kanuka
    a<-sample_n(gullies, 1)
    id<-a$FID
    polygons[polygons$FID == id, "Veg_Class"] <- "Kanuka (Contin.)"
    # for kanuka and mixed native set the sampled column to TRUE so the next bit of the model works
    polygons[polygons$FID == id, "sampled"] <- "TRUE"
    # set area 
    area<-a$AREA_GEO+area
    proportion.assigned<-area/landscape.total
    
  }
  test<-"pass"
}else if (scenario=="pine"){
  while(proportion.assigned<proportion.replant){
  # select a random pasture gully and reassign it to pine
  a<-sample_n(gullies, 1)
  id<-a$FID
  polygons[polygons$FID == id, "Veg_Class"] <- "Pine (Contin.)"
  # set area 
  area<-a$AREA_GEO+area
  proportion.assigned<-area/landscape.total
  }
  test<-"pass"  
}else if (scenario=="native"){
  while(proportion.assigned<proportion.replant){
    # select a random pasture gully and reassign it to pine
    a<-sample_n(gullies, 1)
    id<-a$FID
    polygons[polygons$FID == id, "Veg_Class"] <- "Mixed Native (Contin.)"
    # for kanuka and mixed native set the sampled column to TRUE so the next bit of the model works
    polygons[polygons$FID == id, "sampled"] <- "TRUE"
    # set area 
    area<-a$AREA_GEO+area
    proportion.assigned<-area/landscape.total
  }
  test<-"pass"  
}else{
  print("Scenario incorrectly specified - model not run!")
  test<-"fail"
}
# stop the rest of model running if scenario was incorrectly specified
stopifnot(test=="pass")

#### flammability values assigned ####
# In this section, the loop randomly assigns flam values to each polygon ID according to veg classes and repeats this 
# n times to create n new columns of flam values (hereafter, each iteration is a simulations)
# This takes a while to run - 100 bootstraps = approx 10 minutes
for (i in 1:n) { # for each bootstrap
  run.list<-list() # make empty list of results
  
  for (j in 1:nrow(polygons)){ # read each polygon ID at a time
    if (polygons$sampled[j]=="FALSE"){ #if there's no flam data for that class
      # if it's open water or bare ground give it the non veg value
      if (polygons$Veg_Class[j]=="Non veg")
      {run.list[j]<-nonveg}
      # if polygon is pasture, give it the constant pasture value, and so on for other classes as defined above
      if (polygons$Veg_Class[j]=="Pasture")
      {run.list[j]<-pastureflam}
      if (polygons$Veg_Class[j]=="Pine (Contin.)") 
      {run.list[j]<-pine.cont}
      if (polygons$Veg_Class[j]=="Pine (Diffuse)") 
      {run.list[j]<-pine.diff}
      if (polygons$Veg_Class[j]=="Pine (Sparse)") 
      {run.list[j]<-pine.spar}
      if (polygons$Veg_Class[j]=="Kanuka (Diffuse)")
      {run.list[j]<-kan.diff}
      if (polygons$Veg_Class[j]=="Decidious (Diffuse)")
      {run.list[j]<-dec.diff}
      if (polygons$Veg_Class[j]=="Mixed Native (Sparse)")
      {run.list[j]<-mn.spar}
      if (polygons$Veg_Class[j]=="Old_Growth (Contin.)")
      {run.list[j]<-og.cont}
      if (polygons$Veg_Class[j]=="Old_Growth (Diffuse)")
      {run.list[j]<-og.diff}
      if (polygons$Veg_Class[j]=="Old_Growth (Sparse)")
      {run.list[j]<-og.spar}
    }
    else { # for everything else that has been sampled
      a<-polygons$Veg_Class[j] # temporarily store vegetation class
      sample.list<-subset(sample.plots, Veg_Class==a)%>%.[,3] # make list of all flam values for that class
      run.list[j]<-sample(sample.list, size=1, replace=T) # sample 1 value randomly with replacement
    }
  }
  polygons<-cbind(polygons, unlist(run.list))
  colnames(polygons)[i+5]<-i
  print(paste("Resampling run",i,"out of",n,"completed"))
}

## then you can calculate the mean values for each polygon ID, as well as upper/lower CI, etc
polygons$mean_flam<-apply(polygons[,c(6:(n+5))], 1, mean)
polygons$sd_flam<-apply(polygons[,c(6:(n+5))], 1, sd)
polygons$min_flam<-apply(polygons[,c(6:(n+5))], 1, min)
polygons$max_flam<-apply(polygons[,c(6:(n+5))], 1, max)

polygons_summary<-data.frame(polygons$FID,polygons$Veg_Class, polygons$mean_flam, polygons$sd_flam, polygons$min_flam, polygons$max_flam)
# write the table to csv so can map in arcmap
output.file<-paste("../ls_model_outputs/polygons_bootstrapped_flam_", scenario, ".csv" , sep="")
write.csv(polygons_summary, file=output.file, row.names = F)
output.file<-paste("../ls_model_outputs/polygons_bootstrapped_flam_runs_", scenario, ".csv" , sep="")
write.csv(polygons,file=output.file, row.names=F)

# create data frame for storing network stats
stats<-setNames(data.frame(matrix(ncol = 24, nrow = n)), c("scenario",  "n_nodes", "n_links", "exp", "ks",
                                                           "clustering", "x_patch_area", "sd_patch_area",
                                                           "min_patch_area", "max_patch_area", "sum_patch_area",
                                                           "x_path_wt", "sd_path_wt", "min_path_wt", "max_path_wt", 
                                                           "sum_path_wt", "x_degree", "sd_degree", 
                                                           "min_degree", "max_degree", "sum_degree",
                                                           "skew_degree", "meandistance", "unreachable"))

# The loop in the next section uses the polygon IDs to reclassify the raster to a new one of flammability resistance, 
# then calculates network statistics based on previously defined k-means threshold, repeats this n times. 

# used this to figure out what stats to calculate and whether to use the mpg or grain model 
# http://www.alexchubaty.com/grainscape/articles/grainscape_vignette.html
# and my kereru code 
# https://github.com/pannellj/farmingnatureconservation/blob/master/Preliminary%20Spatial%20Habitat%20Networks.ipynb

# Takes a couple of hours to run!
# read in scenario data if necessary (if you didn't run this all at once) - 
#polygons<-read.csv("../ls_model_outputs/polygons_bootstrapped_flam_runs.csv", header=T, stringsAsFactors = F)

skew = function(x) mean( ((x - mean(x)) / sd(x))^3 )

# run network stats for every scenario in polygons data frame
for (i in 1:n){
  reclass<-polygons[,c(1,(5+i))]
  colnames(reclass)<-c("is", "becomes")
  landscape.temp<-reclassify(landscape, as.matrix(reclass)) # reclassify raster to assign flammability scores
  landscape.temp<-raster.invert(landscape.temp) # invert so low values are high flam
  landscape.temp<-landscape.temp+abs(minValue(landscape.temp)) # rescale so minimum value is 0
  
  temp_mpg<-MPG(landscape.temp, patch = (landscape.temp < threshold)) # calculate the minimum planar graph
  nodeTable <- graphdf(temp_mpg)[[1]]$v # extract node table
  linkTable <- graphdf(temp_mpg)[[1]]$e # extract link table
  
  # write network stats to data frame
  stats$scenario[i]<-i
  stats$n_nodes[i]<-nrow(nodeTable) # number of nodes
  stats$n_links[i]<-nrow(linkTable)/2 # number of links (/2 because they are bidirectional)
  power<-power.law.fit(degree(temp_mpg@mpg))
  stats$exp[i]<-power$KS.stat # the exponent of the power law of degree distribution 
  stats$ks[i]<-power$KS.p # p value for kolmogorov smirnov test on degree distribution
  stats$clustering[i]<-transitivity(temp_mpg@mpg) #clustering coeffficient
  
  stats$x_patch_area[i]<-mean(nodeTable$patchArea) # mean patch area
  stats$sd_patch_area[i]<-sd(nodeTable$patchArea) # sd patch area
  stats$min_patch_area[i]<-min(nodeTable$patchArea) # min patch area
  stats$max_patch_area[i]<-max(nodeTable$patchArea) # max patch area
  stats$sum_patch_area[i]<-sum(nodeTable$patchArea) # total patch area
  
  stats$x_path_wt[i]<-mean(linkTable$lcpPerimWeight) # same for path weights
  stats$sd_path_wt[i]<-sd(linkTable$lcpPerimWeight)
  stats$min_path_wt[i]<-min(linkTable$lcpPerimWeight)
  stats$max_path_wt[i]<-max(linkTable$lcpPerimWeight)
  stats$sum_path_wt[i]<-sum(linkTable$lcpPerimWeight)
  
  stats$x_degree[i]<-mean(degree(temp_mpg@mpg)) # same for degrees
  stats$sd_degree[i]<-sd(degree(temp_mpg@mpg))
  stats$min_degree[i]<-min(degree(temp_mpg@mpg))
  stats$max_degree[i]<-max(degree(temp_mpg@mpg))
  stats$sum_degree[i]<-sum(degree(temp_mpg@mpg))
  stats$skew_degree[i]<-skew(degree(temp_mpg@mpg)) # also calculate skew of degree distributioin
  
  # calculate mean distance between any 2 nodes by randomly sampling 2 nodes and calculating distance 100 times
  meandistance<-0 # create empty parameters
  unreachable<-0
  
  for (j in 1:n){
    a<-sample(1:nrow(nodeTable), 2, replace=T) # sample 2 nodes randomly with replacement
    n1<-a[1]
    n2<-a[2]
    bacon<-distances(temp_mpg$mpg, v=n1, to=n2, weights = E(temp_mpg$mpg)$lcpPerimWeight)[1] # calculate distance between 2 nodes
    # next if node is unreachable just tally it up and leave out from mean distance calc
    if (bacon=="Inf"){
      unreachable=unreachable+1
    } else {meandistance<-meandistance+bacon}# otherwise add to cumulative mean distance parameter
    
    rm(a,n1,n2,bacon)
  }
  stats$meandistance[i]<-meandistance/n # divide cumulative distance by number of bootstraps
  stats$unreachable[i]<-unreachable
  rm(reclass, landscape.temp, temp_mpg,power,meandistance,unreachable)
  print(paste("Network model run",i,"out of",n,"completed"))
}

output.file<-paste("../ls_model_outputs/network_statistics_", scenario, ".csv" , sep="")
write.csv(stats, file=output.file, row.names = F)