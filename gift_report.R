require(dplyr)
require(lubridate)
require(xlsx)


my_db <- src_postgres(dbname = 'cxtd', host = 'localhost', port = 5432, user = 'cxtd', password = 'xintiandi')
#### time prep 

getting_gift_df <- function(db_con, end_time=today(), rpt_dur=7){
    # calculate report duration
    rpt_dur <- ddays(rpt_dur)
    end_time <- ymd(end_time)
    # if end_time is missing use "today":00:00:00 as end time
    # end_time <- ifelse(missing(end_time), floor_date(ymd_hms(Sys.time()), unit = 'day'), ymd(end_time))
    
    # calculate start of time
    start_time <- end_time - rpt_dur
    end_time <- as.character(end_time)
    start_time <- as.character(start_time)
    
    ### actual function
    ## member card, source
    member <- tbl(db_con, 'member') %>% 
        select(member_id, member_card_no, member_source)
    
    ### gift redeem transaction detail
    gift <- tbl(db_con, 'gift') %>% 
        filter(created_datetime >= start_time & created_datetime < end_time) %>%
        select(gift_id, gift_detail_id, member_id, cost_of_redeem, address_id, created_datetime)
    
    # gift DETAIL ---->> the detialed info of the specific gift
    gift_detail <- tbl(db_con, 'gift_detail') %>% 
        filter(status == 0) %>%
        select(gift_detail_id, gift_detail_code, title_sc)
    
    # gift_order_status <- data.frame(tbl(my_db, 'gift_order_status')) %>%
    # getting receipiant name, mobile, addresss
    member_address <- tbl(db_con, 'member_address') %>%
        select(recipient_name, recipient_mobile, address, address_id)
    
    
    # combine data together
    final <- gift %>% 
        inner_join(gift_detail, by="gift_detail_id") %>% 
        inner_join(member_address, by = "address_id") %>% 
        inner_join(member, by = "member_id") %>% 
        collect() 
    if (nrow(final) != 0 ) {
        final <- final %>%
            mutate(redeem_date = as.character(floor_date(created_datetime, unit = 'day')),
                   num_of_redeem = 1) %>%
            select(redeem_date, member_card_no, recipient_mobile, title_sc, 
                   gift_detail_code, cost_of_redeem, num_of_redeem, address, member_source)
    }else {
        final <- data.frame(redeem_date = character(),
                            redeem_date = character(),
                            member_card_no = character(),
                            recipient_mobile = character(),
                            title_sc = character(),
                            gift_detail_code = character(),
                            cost_of_redeem = character(),
                            num_of_redeem = character(),
                            address = character(),
                            member_source = character())
                            
    }
        
    
    return(final)
}




 ##
## DIRECTORY manipulation
#get base directory
# base_dir <- getwd()


getting_file_dir <- function(base_dir=getwd(), output_file_name='gift_status.xlsx') {
    new_dir <- file.path(base_dir)
    
    if (!file.exists(file.path(paste(base_dir, 'gift_reports', today(), sep = '/')))) {
        if (!file.exists(file.path(paste(base_dir, 'gift_reports',sep = '/')))) {
            dir.create(file.path(paste(base_dir, 'gift_reports',sep = '/')))
        }    
        new_dir <- paste(new_dir, 'gift_reports', sep = '/')
        print(new_dir)
        dir.create(file.path(paste(new_dir, today(), sep = '/')))
        new_dir <- paste(new_dir, today(), sep = '/')
        print(new_dir)
    }
    if(base_dir == new_dir){
        ### do something
        new_dir <- file.path(paste(base_dir, 'gift_reports', today(), sep = '/'))
    }
    #create file names
    sales_output_dir <- normalizePath(paste(new_dir, output_file_name, sep = '/'))
    return(sales_output_dir)
}


output_xlsx <- function(big_df, member_source_list, sales_output_dir, db_con){
    wb <- createWorkbook()    
    # summary 
    summary_df <- big_df %>% 
        group_by(gift_detail_code, title_sc) %>%
        summarize(total_redeem = n())
    
    gift_detail <- tbl(db_con, 'gift_detail') %>% 
        filter(status == 0) %>% 
        select(gift_detail_code, title_sc) %>% 
        collect()
    
    summary_df <- summary_df %>% 
        right_join(gift_detail, by = c("gift_detail_code", "title_sc")) %>%
        mutate(total_redeem = as.numeric(ifelse(is.na(total_redeem), 0, total_redeem))) %>%
        arrange(gift_detail_code)
    
    summary_sheet <- createSheet(wb, sheetName="Summary")
    addDataFrame(summary_df, summary_sheet)
    # each mall
    for (ms in names(member_source_list)){
        current_sheet <- createSheet(wb, sheetName=member_source_list[ms])
        current_df <- big_df %>% filter(member_source == ms)
        addDataFrame(current_df, current_sheet)
    }

    
    #save xlsx
    saveWorkbook(wb, sales_output_dir)
}



# acutal FUNCTION CALLS
# final <- getting_gift_df(my_db, rpt_dur = 20)
final <- getting_gift_df(my_db)
sales_output_dir <- getting_file_dir()
# sales_output_dir <- getting_file_dir('~/src/all_reports')
# prep excel 
member_source_list <- c("09"="iTiandi",
                        "03"="RuiHong", 
                        "08" = "The Hub")
output_xlsx(final, member_source_list, sales_output_dir, my_db) 

print(paste(now(), 'Success!', sep = " "))
