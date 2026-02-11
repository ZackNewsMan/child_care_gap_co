## Test out Census API

      decennial_2020_vars <- load_variables(
        year = 2020, 
        "pl", 
        cache = TRUE
      )
      
      # 2010 Decennial Census Variables
      decennial_2010_vars <- load_variables(
        year = 2010, 
        "pl", 
        cache = TRUE
      )
      
      # 2016 - 2020 5 Year American Community Survey (ACS) Variables
      
      acs_20_vars = load_variables(
        year = 2020, 
        "acs5",
        cache = TRUE
      )

    # Defaults to the most recent ACS and as the 5-year unless otherwise noted
      
      get_acs(
        geography = "county",
        table = "B09001",
        state = "CO",
        year = 2024
      )
      
        # That worked! This was the inspo and it matches which is great
          # https://censusreporter.org/data/table/?table=B09001&geo_ids=04000US08,050|04000US08&primary_geo_id=04000US08#valueType|estimate
      
        # Another helpful resource:
          # https://nicar.r-journalism.com/2024/
      
        # Unsure if Jen wants the number of kids under 6 or a more specific age-range. A more specific age-range would be a bit more complex
          # Based off of this mention of the Colorado Shines program, I think it will be 5 and under: 
            # https://cdec.colorado.gov/for-providers/child-care-licensing-and-administration
      
      
      install.packages("conflicted")
      library(conflicted)
      
      library(dplyr)
      conflicts_prefer(dplyr::filter)
      
  
#### group by county ####
      # There were NA's in Arapahoe that were causing problems so I need to address the NA's within the summarize function 
      
      licensed_child_care %>% 
        group_by(county) %>% 
        summarise(sum = sum(total_licensed_capacity, na.rm = TRUE)) %>% 
        View()
      
      
      licensed_child_care %>% 
        group_by(county) %>% 
        summarise(sum_child_care_spots = sum(total_licensed_capacity, na.rm = TRUE)) %>% 
        arrange(desc(sum_child_care_spots))
      
        # That brought up most spots 
      
      # Least spots to most
      
      licensed_child_care %>% 
        group_by(county) %>% 
        summarise(sum_child_care_spots = sum(total_licensed_capacity, na.rm = TRUE)) %>% 
        arrange(sum_child_care_spots)
      
      
      sqldf("SELECT county, sum(total_licensed_capacity)
            FROM licensed_child_care
            GROUP BY county
            ORDER BY sum(total_licensed_capacity)")
      
    #### Pull out facilities in counties with the least spots ####
      
      licensed_child_care %>% 
        filter(county == "Jackson")
      
      # Worked great :) 
      
      
      licensed_child_care %>% 
        filter(county == "San Juan")
      
        # only one place again! maybe a trend?
      
      
      licensed_child_care %>% 
        group_by(county) %>% 
        summarise(count = count(license_number, na.rm = TRUE)) %>% 
        arrange(count)
      
      # naur
      
      licensed_child_care %>% 
      distinct(license_number, .keep_all = TRUE) %>% 
      group_by(county) %>%
      summarize(count = n()) %>% 
      arrange(count) %>% 
      View()  

      # Worked great
      
      
      sqldf("SELECT county, count(license_number)
            FROM licensed_child_care
            GROUP BY county
            ORDER BY count(license_number)")
        
          # SQL verified 
      
        # To see more
      
      
      licensed_child_care %>% 
        distinct(license_number, .keep_all = TRUE) %>% 
        group_by(county) %>%
        summarize(count = n()) %>% 
        arrange(count) %>% 
        print(n = 20)
      

  #### pull out by toddler, infant and preschool 
      
      licensed_child_care %>% 
        rowwise() %>% 
        mutate(prek_total = sum(c_across(licensed_center_infant_capacity:licensed_center_preschool_capacity), na.rm = TRUE)) %>% 
        View()
      
  ########## Census tests #########
      
      
      # I searched B09001 in the variable column to see what the labels correspond to  
      # So the variables that we need are:
      # B9001_003: number_under_three
      # B9001_004: number_three_four
      # B9001_005: number_five
      # B9001_006: number_six_to_eight
      
      # These numbers are probably going to make the gap look worse than it actually is on the ground but this is the best estimate available based on differing     age ranges for different data sets 
    
  # Inspo from R for journalists
      # https://nicar.r-journalism.com/2024/ 
        
  get_acs(
        geography = "county",
        state = "CA",
        variables = c(percent_high_school = "DP02_0062P", 
                      percent_bachelors = "DP02_0065P",
                      percent_graduate = "DP02_0066P"), 
        year = 2021
      ) 
      
      get_acs(
        geography = "county",
        state = "CO",
        variables = c(number_under_three = "B9001_003", 
                      number_three_four = "B9001_004",
                      number_five = "B9001_005",
                      number_six_to_eight = "B9001_006"), 
        year = 2024
      )   
      
        # Getting this error: Your API call has errors.  The API message returned is error: unknown variable 'B9001_003E'.
      
      
      get_acs(
        geography = "county",
        state = "CO",
        variables = c(number_three_four = "B09001_004"),
        year = 2024
      ) 
      
      # That worked. I forgot a 0 in the variable name! what a silly goose
      
      # Trying again
      
      get_acs(
        geography = "county",
        state = "CO",
        variables = c(number_under_three = "B09001_003", 
                      number_three_four = "B09001_004",
                      number_five = "B09001_005",
                      number_six_to_eight = "B09001_006"), 
        year = 2024
      )   
      
      # Worked swimmingly!
      
    co_kids_count_eight_under <- get_acs(
        geography = "county",
        state = "CO",
        variables = c(number_under_three = "B09001_003", 
                      number_three_four = "B09001_004",
                      number_five = "B09001_005",
                      number_six_to_eight = "B09001_006"), 
        year = 2024
      )   
      
    
    co_kids_count_eight_under %>% 
      group_by(NAME) %>% 
      summarise(sum = sum(estimate))
    
    
    co_kids_count_eight_under %>% 
      group_by(NAME) %>% 
      summarise(sum_kids_eight_under = sum(estimate))
   
        # Check in SQL
   
          sqldf("SELECT name, sum(estimate)
            FROM co_kids_count_eight_under
            GROUP BY name
            ")
      # need to use estimate column because that is numeric 

          co_kids_count_eight_under %>% 
            group_by(NAME) %>% 
            summarise(sum_kids_eight_under = sum(estimate))
          
          # Check in SQL
          
          sqldf("SELECT name, sum(estimate)
            FROM co_kids_count_eight_under
            GROUP BY name
            ")
          
            # Matches so that's good
          
      # need to get column names to match 
    
    
    library(dplyr)
    library(stringr)
    
    df <- df %>%
      mutate(county_name = word(your_column, 1, -2))
    
    sum_co_kids_count_eight_under %>% 
      mutate(county_name = word(NAME, 1, -2))
      
    sum_co_kids_count_eight_under %>%
      mutate(county_name = str_extract(NAME, ".*(?=\\s+[Cc]ounty)"))
    
      # worked great!
      # Needs to be called "county" for the join to work
    
    sum_co_kids_count_eight_under %>%
      mutate(county = str_extract(NAME, ".*(?=\\s+[Cc]ounty)"))
      
    
    renamed_sum_co_kids_count_eight_under %>% 
      full_join(pre_k_county_child_care_capacity, by='county') 

      # Works perfectly      
      
    renamed_sum_co_kids_count_eight_under %>% 
      full_join(pre_k_county_child_care_capacity, by='county') %>% 
      mutate(spot_kids_gap = sum_kids_eight_under - sum_pre_k_total_spots) 
      
      # worked really well :) 
      