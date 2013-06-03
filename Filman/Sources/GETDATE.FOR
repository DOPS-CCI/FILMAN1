       integer*2 iyr,imon,iday





       call getdat(iyr,imon,iday)

       open(unit=12,file='date.tmp')

       write(12,*)iyr,imon,iday

       close(12)

       write(*,*)iyr,imon,iday

       iyr=1990

       imon=3

       iday=3

       call setdat(iyr,imon,iday)

       end

