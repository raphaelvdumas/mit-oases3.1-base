        integer function getpid()
        data ipid /0/
        save ipid
        ipid = ipid + 1
        if (ipid.gt.9999) ipid=1
        getpid=ipid
        return
        end

