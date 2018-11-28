getUniqVec = function(dataVec, datetimeVec, index="YYYYMMDDHHMMDD")
{
    YYYY = substr(datetimeVec, 1, 4)                             #YYYY
    YYYYMM = substr(datetimeVec, 1, 7)                        #YYYY-MM
    YYYYMMDD = substr(datetimeVec, 1, 10)                  #YYYY-MM-DD
    YYYYMMDDHH = substr(datetimeVec, 1, 13)              # YYYY-MM-DD HH
    YYYYMMDDHHMM = substr(datetimeVec, 1, 16)         # YYYY-MM-DD HH:MM
    YYYYMMDDHHMMSS = substr(datetimeVec, 1, 19)     # YYYY-MM-DD HH:MM:SS
    
    
    # index 만들기
    idx = NULL;
    if (index == "YYYY") {
        idx = YYYY;   
    } else if (index == "YYYYMM"){
        idx = YYYYMM;    
    } else if (index == "YYYYMMDD") {
        idx = YYYYMMDD;
    } else if (index == "YYYYMMDDHH") {
        idx = YYYYMMDDHH;    
    } else if (index == "YYYYMMDDHHMM") {
        idx = YYYYMMDDHHMM; 
    } else {
        idx = YYYYMMDDHHMMSS;    
    }
    
    return(idx)
}
