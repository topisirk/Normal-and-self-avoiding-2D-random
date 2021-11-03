module stats
    implicit none

    contains 

    subroutine time(minutes,timearray)
        implicit none

        integer,intent(in) :: minutes
        integer :: a !dummy for 'leftovers'
        integer, dimension(4),intent(out) :: timearray !this time array holds the amount of years,days,hours and mins are in the amount of mins given

        timearray(1)=floor(real(minutes/(365*24*60))) ! amount of years

        !we take into account the years 'lost' when rounding down the years to an integer
        
        a=minutes-timearray(1)*365*24*60 ! Amount of minutes left after whole years
        timearray(2)=floor(real(a/(24*60))) ! amount of days
        a=a-timearray(2)*24*60 !same logic applied again
        timearray(3)=floor(real(a/60))
        a=a-timearray(3)*60
        timearray(4)=a

        !timearray(1) is years, timearray(2) is days etc. We go case by case all 15 possible combinations of time array elements. 
        !4^2 possibilities (theyre 0 or not 0) and all being 0 is not possible for our case so we need 15 if statements.
        

        if (timearray(1).ne.0.and.timearray(2).ne.0.and.timearray(3).ne.0.and.timearray(4).ne.0) then
            print '(i2,x,a,x,i3,x,a,x,i2,x,a,x,i2,x,a)',timearray(1),'years,',timearray(2),'days,',timearray(3),'hours &
            &and ',timearray(4),'minutes' ! string was too long so it had to be cut into two lines
        end if

        if (timearray(4).eq.0) then ! no minutes
            if (timearray(1).ne.0.and.timearray(2).ne.0.and.timearray(3).ne.0) then
                print '(i2,x,a,x,i2,x,a,x,i2,x,a)',timearray(1),'years,',timearray(2),'days and',timearray(3),'hours'
            
            else if (timearray(1).eq.0.and.timearray(2).ne.0.and.timearray(3).ne.0) then
                print '(i2,x,a,x,i2,x,a)',timearray(2),'days and',timearray(3),'hours'

            else if (timearray(1).ne.0.and.timearray(2).eq.0.and.timearray(3).ne.0) then
                print '(i2,x,a,x,i2,x,a)',timearray(1),'years and',timearray(3),'hours'

            else if (timearray(1).ne.0.and.timearray(2).ne.0.and.timearray(3).eq.0) then
                print '(i2,x,a,x,i2,x,a)',timearray(1),'years and',timearray(2),'days'

            else if (timearray(1).ne.0.and.timearray(2).eq.0.and.timearray(3).eq.0) then
                print '(i2,x,a)',timearray(1),'years'

            else if (timearray(1).eq.0.and.timearray(2).ne.0.and.timearray(3).eq.0) then
                print '(i2,x,a)',timearray(2),'days'

            else if (timearray(1).eq.0.and.timearray(2).eq.0.and.timearray(3).ne.0) then
                print '(i2,x,a)',timearray(3),'hours'
            end if
        end if
        
        if (timearray(3).eq.0) then !no hours
            if (timearray(1).ne.0.and.timearray(2).ne.0.and.timearray(4).ne.0) then
                print '(i2,x,a,x,i3,x,a,x,i2,x,a)',timearray(1),'years,',timearray(2),'days and',timearray(4),'minutes'

            else if (timearray(1).eq.0.and.timearray(2).ne.0.and.timearray(4).ne.0) then
                print '(i2,x,a,x,i2,x,a)',timearray(2),'days and',timearray(4),'minutes'

            else if (timearray(1).ne.0.and.timearray(2).eq.0.and.timearray(4).ne.0) then
                print '(i2,x,a,x,i2,x,a)',timearray(1),'years and',timearray(4),'minutes'

            else if (timearray(1).eq.0.and.timearray(2).eq.0.and.timearray(4).ne.0) then
                print '(i2,x,a)',timearray(4),'minutes'
            end if
        end if
        
        if (timearray(2).eq.0) then !no days
            if (timearray(1).ne.0.and.timearray(3).ne.0.and.timearray(4).ne.0) then
                print '(i2,x,a,x,i2,x,a,x,i2,x,a)',timearray(1),'years,',timearray(3),'hours and',timearray(4),'minutes'

            else if (timearray(1).eq.0.and.timearray(3).ne.0.and.timearray(4).ne.0) then
                print '(i2,x,a,x,i2,x,a)',timearray(3),'hours and',timearray(4),'minutes'
            end if
        end if

        if (timearray(1).eq.0) then !no years
            if (timearray(2).ne.0.and.timearray(3).ne.0.and.timearray(4).ne.0) then
                print '(i3,x,a,x,i2,x,a,x,i2,x,a)',timearray(2),'days,',timearray(3),'hours and',timearray(4),'minutes'
            end if
        end if

    end subroutine time
    
    !subroutine statistics will call the time subroutine for the time related statistics
    
    subroutine statistics(walktype,n,successes,fails,maxsteps,minsteps,avgsteps) ! in the case of nrw fail means died of old age and for saw it means a dead end
        implicit none
        character(len=5) :: walktype
        integer :: successes, fails, n, maxsteps,minsteps,avgsteps,timearray(4)
        real :: prob_fail,prob_success
        if (walktype=='nrw') then
            prob_fail=real(fails)/real(n) !fail in nrw means death of old age
            prob_success=real(successes)/real(n)

            print'(i6,x,a,x,i5,x,a,x,i5,x,a,/)',n,'sailors started, of which ',successes,'made it in time and',fails,'died &
            &of old age'
            print'(a,x,f6.4)', 'probability of dying of old age:',prob_fail
            print'(a,x,f6.4/)', 'probability of making it in time:',prob_success
            print'(a,x,f10.2,x,a)','max dist:',maxsteps*0.1,'(km)'!0.1 comes from multiplying by 100 and dividing by 1000(for km)
            print'(a,x,f10.2,x,a)','min dist:',minsteps*0.1,'(km)'
            print'(a,f10.2,x,a/)','avg dist:',avgsteps*0.1,'(km)'
            print*,'max time:'
            call time(maxsteps,timearray) ! call subroutine time() from above to turn maxsteps, which is also max time in minutes, into sensible units of time.
            print*,' '
            print*,'min time:'
            call time(minsteps,timearray)
            print*,' '
            print*,'avg time:'
            call time(avgsteps,timearray)
            print*,' '
            
        else if (walktype=='saw') then
            prob_fail=real(fails)/real(n) !fail in nrw means death of old age
            prob_success=real(successes)/real(n)

            print'(i6,x,a,x,i5,x,a,x,i5,x,a,/)',n,'sailors started, of which ',successes,'made it in time&
            & and',fails,'walked into dead ends'
            print'(a,x,f6.4)', 'probability of running into a dead end:',prob_fail
            print'(a,x,f6.4/)', 'probability of making it in time:',prob_success
            print'(a,x,f10.2,x,a)','max dist:',maxsteps*0.1,'(km)'
            print'(a,x,f10.2,x,a)','min dist:',minsteps*0.1,'(km)'
            print'(a,f10.2,x,a/)','avg dist:',avgsteps*0.1,'(km)'
            print*,'max time:'
            call time(maxsteps,timearray)
            print*,' '
            print*,'min time:'
            call time(minsteps,timearray)
            print*,' '
            print*,'avg time:'
            call time(avgsteps,timearray)
            print*,' '
        end if
    end subroutine statistics   


end module stats