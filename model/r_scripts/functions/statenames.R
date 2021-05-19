pop <- structure(list(full = c("Andhra Pradesh", "Arunachal Pradesh", 
                               "Assam", "Bihar", "Chhattisgarh", "Goa", "Gujarat", "Haryana", 
                               "Himachal Pradesh", "Jammu and Kashmir", "Jharkhand", "Karnataka", 
                               "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", 
                               "Mizoram", "Nagaland", "Odisha", "Punjab", "Rajasthan", "Sikkim", 
                               "Tamil Nadu", "Tripura", "Uttarakhand", "Uttar Pradesh", "West Bengal", 
                               "Andaman and Nicobar Islands", "Chandigarh", "Dadra and Nagar Haveli", 
                               "Daman and Diu", "Delhi", "Lakshadweep", "Pondicherry", "Telangana", 
                               "Dadra and Nagar Haveli", "Chhattisgarh", "Ladakh", "Uttarakhand", "India"),
                      abbrev = c("ap", "ar", "as", "br", "cg", "ga", "gj", "hr", 
                                 "hp", "jk", "jh", "ka", "kl", "mp", "mh", "mn", "ml", "mz", "nl", 
                                 "or", "pb", "rj", "sk", "tn", "tr", "uk", "up", "wb", "an", "ch", 
                                 "dh", "dd", "dl", "ld", "py", "tg", "dn", "ct", "la", "ut", "tt"), 
                      population = c(52221000, 1504000, 34293000, 119520000, 28724000, 
                                     1540000, 67936000, 28672000, 7300000, 13203000, 37403000, 
                                     65798000, 35125000, 82232000, 122153000, 3103000, 3224000, 
                                     1192000, 2150000, 43671000, 29859000, 77264000, 664000, 75695000, 
                                     3992000, 11141000, 224979000, 96906000, 397000, 1179000, 
                                     344000, 243000, 19814000, 64500, 244000, 37220000, 344000, 
                                     28724000, 293000, 11141000, 1332830000)),
                 row.names = c(NA, -41L),
                 class = "data.frame")

get_pop <- function(state) {
  
  pop %>%
    filter(abbrev == tolower(state)) %>%
    pull(population)
  
}