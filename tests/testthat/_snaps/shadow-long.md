# shadow_long returns the right dimensions and names etc

    Code
      shadow_long(ocean_shadow)
    Output
      # A tibble: 5,888 x 4
         variable   value        variable_NA   value_NA
         <chr>      <chr>        <chr>         <fct>   
       1 year       1997         year_NA       !NA     
       2 latitude   0            latitude_NA   !NA     
       3 longitude  -110         longitude_NA  !NA     
       4 sea_temp_c 27.59000015  sea_temp_c_NA !NA     
       5 air_temp_c 27.14999962  air_temp_c_NA !NA     
       6 humidity   79.59999847  humidity_NA   !NA     
       7 wind_ew    -6.400000095 wind_ew_NA    !NA     
       8 wind_ns    5.400000095  wind_ns_NA    !NA     
       9 year       1997         year_NA       !NA     
      10 latitude   0            latitude_NA   !NA     
      # i 5,878 more rows

# shadow_long works gives the classes with function value transform

    Code
      shadow_long(ocean_shadow, fn_value_transform = as.numeric)
    Output
      # A tibble: 5,888 x 4
         variable     value variable_NA   value_NA
         <chr>        <dbl> <chr>         <fct>   
       1 year       1997    year_NA       !NA     
       2 latitude      0    latitude_NA   !NA     
       3 longitude  -110    longitude_NA  !NA     
       4 sea_temp_c   27.6  sea_temp_c_NA !NA     
       5 air_temp_c   27.1  air_temp_c_NA !NA     
       6 humidity     79.6  humidity_NA   !NA     
       7 wind_ew      -6.40 wind_ew_NA    !NA     
       8 wind_ns       5.40 wind_ns_NA    !NA     
       9 year       1997    year_NA       !NA     
      10 latitude      0    latitude_NA   !NA     
      # i 5,878 more rows

# shadow_long returns right dimensions, names, etc when filtered

    Code
      shadow_long(ocean_shadow, air_temp_c, humidity)
    Output
      # A tibble: 1,472 x 4
         variable   value       variable_NA   value_NA
         <chr>      <chr>       <chr>         <fct>   
       1 air_temp_c 27.14999962 air_temp_c_NA !NA     
       2 humidity   79.59999847 humidity_NA   !NA     
       3 air_temp_c 27.02000046 air_temp_c_NA !NA     
       4 humidity   75.80000305 humidity_NA   !NA     
       5 air_temp_c 27          air_temp_c_NA !NA     
       6 humidity   76.5        humidity_NA   !NA     
       7 air_temp_c 26.93000031 air_temp_c_NA !NA     
       8 humidity   76.19999695 humidity_NA   !NA     
       9 air_temp_c 26.84000015 air_temp_c_NA !NA     
      10 humidity   76.40000153 humidity_NA   !NA     
      # i 1,462 more rows

# shadow_long returns right dimensions, names, etc when filtered with function value transform

    Code
      shadow_long(ocean_shadow, air_temp_c, humidity, fn_value_transform = as.numeric)
    Output
      # A tibble: 1,472 x 4
         variable   value variable_NA   value_NA
         <chr>      <dbl> <chr>         <fct>   
       1 air_temp_c  27.1 air_temp_c_NA !NA     
       2 humidity    79.6 humidity_NA   !NA     
       3 air_temp_c  27.0 air_temp_c_NA !NA     
       4 humidity    75.8 humidity_NA   !NA     
       5 air_temp_c  27   air_temp_c_NA !NA     
       6 humidity    76.5 humidity_NA   !NA     
       7 air_temp_c  26.9 air_temp_c_NA !NA     
       8 humidity    76.2 humidity_NA   !NA     
       9 air_temp_c  26.8 air_temp_c_NA !NA     
      10 humidity    76.4 humidity_NA   !NA     
      # i 1,462 more rows

