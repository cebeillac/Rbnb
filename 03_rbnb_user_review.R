# Extract airbnb listings within a boundbox
# Author : Alexandre Cebeillac
# Université de Rouen Normandie

# returns the reviews of the host or guest
# needs the id of the host or guest (from 02_rbnb_place_review.R)

require(httr)
require(dplyr)
require(rvest)
require(jsonlite)

place_review=place_review_all # from 02_rbnb_place_review.R
list_id=c(unique(place_review$host_id),unique(place_review$reviewer_id))

id=list_id[1]
review_all=NULL
for(i in seq(1,length(list_id))){
  id=list_id[i]
  off=0
  count=1
  #deal with somes error :
  while(off <= count){
    error=500
    while(length(error)>0 ){
      if(error != 404){
        url=paste("https://www.airbnb.com/api/v2/reviews?",
                  "currency=EUR&key=d306zoyjsyarp7ifhu67rjxn52tv0t20&locale=en&",
                  "reviewee_id=",id,
                  "&role=","all",
                  "&_format=for_user_profile_v2&_limit=100",
                  "&_offset=",off,"&_order=recent",sep="")
        # get the data
        webpage <- GET(url,
                       httpheader=c("User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; de; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3"))
        raw_data=content(webpage,as = "text",encoding = "UTF-8")
        data<-jsonlite::fromJSON(raw_data, simplifyDataFrame = TRUE)
        error=data$error_code
        if(length(error)>0){print(error)}
      }
      if(length(error)>0){
        if(error==404){error=NULL}
      }
    }
    
    count=data$metadata$reviews_count
    if(length(count)>0){
      off=off+100
      rev=data$reviews
      if(length(rev)>0){
        listing=rev$listing
        #deal with some different format :
        if( listing %>% class() == "list"){
          pos_nok=summary(listing) %>% as.matrix()
          pos_nok=which(pos_nok[,1]==0)
          if(length(pos_nok)>0){
            for(j in pos_nok){
              listing[[j]]["id"]=0
            }
          }
          listing=do.call("rbind.data.frame",listing)
        }
        if( listing %>% class() == "character"){
          listing=data.frame(id=listing)
        }
        if( listing %>% class() == "logical"){
          listing=data.frame(id=listing)
        }
        listing_id=as.numeric(as.character(listing$id))
        
        #compile the informations in a data.frame
        review=data.frame(
          user_id=id,
          date_review=rev$created_at,
          reviewer_id=rev$reviewer$id,
          reviewer_date=rev$reviewer$created_at,
          reviewer_name=rev$reviewer$first_name,
          reviewer_lang=rev$language,
          reviewer_role=rev$role,
          reviewer_place_vistied=listing_id,
          reviewer_location=rev$reviewer$location
        )
        review_all=rbind(review_all,review)
        print(nrow(review_all))
      }
    }
  }
}

