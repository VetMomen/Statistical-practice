#to excute function every specified period 

repeat {
        startTime <- Sys.time()
        Fun()
        sleepTime <- startTime + 2 - Sys.time()
        if (sleepTime > 0)
                Sys.sleep(sleepTime)
}


#Fun() is the function you want to Excute 
# "2" is per second 