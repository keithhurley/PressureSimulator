set.seed(131)
library(tictoc)

#create buffered boat point
m = rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))
p = st_polygon(list(m))
n = 100
l = vector("list", n)
for (i in 1:n)
  l[[i]] = p + 3 * runif(2)
s=st_sfc(l) %>% as.data.frame() %>% st_as_sf() %>% mutate(boat=row_number())


#create possible places for each angler
set.seed(13333172)
p=rbind(c(1,1))
p=st_point(p)
n=50000
l = vector("list", n)
for (i in 1:n)
  l[[i]] = p + 2 * runif(2)
t=st_sfc(l) %>% as.data.frame() %>% st_as_sf() %>% mutate(place=row_number())


ggplot()  +
   geom_sf(data=s, fill="skyblue") +
   geom_sf(data=t, color="red", size=2) + 
   th


#select one random point for angler
set.seed(12344)
#this one works and is fast!
tic()
g<-st_intersects(t, s, sparse = FALSE) %>% 
  data.frame() %>%
  rownames_to_column(var="place") %>%
  gather(key="boat", value="value", names(.)[-1]) %>% 
  filter(value==TRUE) %>% 
  group_by(boat) %>% 
  sample_n(1) %>% 
  ungroup() %>%
  mutate(boat=as.numeric(gsub("X", "",boat)),
         place=as.numeric(place)) %>%
  left_join(t, by=c("place"))
toc()


testFun<-function(myPossiblePlaces, myBufferedBoats, myNumberPerBoat=1) {
  g<-st_intersects(myPossiblePlaces, myBufferedBoats, sparse = FALSE) %>% 
    data.frame() %>%
    rownames_to_column(var="place") %>%
    gather(key="boat", value="value", names(.)[-1]) %>% 
    filter(value==TRUE) %>% 
    group_by(boat) %>% 
    sample_n(myNumberPerBoat) %>% 
    ungroup() %>%
    mutate(boat=as.numeric(gsub("X", "",boat)),
           place=as.numeric(place)) %>%
    left_join(t, by=c("place"))
  return(g)
}

testFun(t,s)

testFun()




ggplot()  +
  geom_sf(data=s[1:9,], fill="skyblue") +
  geom_sf(data=g %>% filter(boat %in% s$boat[1:9]), aes(color=boat), size=3) +
  th +
  facet_wrap(~boat)



#this works too...but is 6X slower with 100 boats and 50000 places
tic()
gg<-st_join(t,s) %>%
  group_by(boat) %>%
  sample_n(1)
toc()

ggplot()  +
  geom_sf(data=s[1:9,], fill="skyblue") +
  geom_sf(data=gg %>% filter(boat %in% s$boat[1:9]), aes(color=boat), size=3) +
  th +
  facet_wrap(~boat)
