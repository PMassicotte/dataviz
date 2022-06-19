library(terra)
library(httr2)

url <- "https://www.oceancolour.org/thredds/wcs/CCI_ALL-v5.0-MONTHLY?service=WCS&version=1.0.0&request=GetCapabilities"

request(url) |> 
  req_headers("Accept" = "application/json") |> 
  req_perform() |> 
  resp_body_xml()


rast("ftp://oc-cci-data:ELaiWai8ae@oceancolour.org/occci-v5.0/geographic/netcdf/monthly/chlor_a/2000/ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-200008-fv5.0.nc", 1)

curl

# https://coastwatch.pfeg.noaa.gov/erddap/griddap/pmlEsaCCI42OceanColor8Day.html
r <- curl::curl_download("https://polarwatch.noaa.gov/erddap/griddap/pmlEsaCCI42OceanColorMonthly.geotif?chlor_a%5B(2019-08-01T00:00:00Z):1:(2019-08-04T00:00:00Z)%5D%5B(69):1:(82)%5D%5B(-82.2038):1:(-50.5233)%5D", "~/Desktop/test.tif")

rast(r) |> 
  clamp(lower = -Inf, upper = 10) |> 
  plot()
