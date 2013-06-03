       integer*2 iyr,imon,iday





       open(unit=12,file='date.tmp')

       read(12,*)iyr,imon,iday

       close(12)

       call setdat(iyr,imon,iday)

       call getdat(iyr,imon,iday)

       write(*,*)iyr,imon,iday

       end

