program sailor
    use mtmod !Random number generator
    use stats !statistics and time formatting
    implicit none

    character(len=5) :: traj !string for whether to use trajectories
    integer,dimension(:,:),allocatable :: coordhistory !holds the historical coordinates for a sailor
    integer :: coord(2) !holds the sailors' x,y coordinates
    real :: start,finish !used to get run time
    character(len=50) :: fn,trajdir='Trajectories/',fullname !strings for sailor path datafile location
    integer :: n,sailors,age,agemax,ran,i,l,successes=0,agedeaths=0,deadendcounter,deadends=0,avgtime !roughly in the order of appearance
    integer,dimension(:),allocatable :: nofsteps !saves the amount of steps for each sailor
    character(len=10) :: arg,walktype
    
    !Get user inputs from command line

    call get_command_argument(1,arg); read(arg,*) n !how many sailors?
    call get_command_argument(2,traj) !trajectories? 'yes' or 'no'
    call get_command_argument(3,walktype) !normal random walk('nrw') or self-avoiding walk('saw')?
    
    call cpu_time(start)

    allocate(coordhistory(50*365*24*60+2,2)) !allocate space for coordinate history
    allocate(nofsteps(n)) !nofsteps holds the number of steps each sailor takes (for statistics)

    if (traj.ne.'yes'.and.traj.ne.'no') then
        print*,'Error: the 2nd argument must be either ''yes'' or ''no'' '
        stop
    end if

    if (walktype=='nrw') then
        call nrw
    else if (walktype =='saw') then 
        call saw        
    else
        print *,'Error: The 3rd argument must be either ''nrw'' or ''saw'' '
        stop
    end if
    
    call cpu_time(finish)
    print*,'Runtime: ', finish-start

    contains

    subroutine nrw()
        implicit none
        call sgrnd(getseed()) !get seed for rng
        
        do sailors=1,n
            coord=0 !positions of sailors originally 0
            coordhistory=0
            agemax=50*365*24*60 !50 years in minutes
            nofsteps(sailors)=0 ! a sailor hasnt taken any steps yet
            do age=1,agemax !coordhistory(age+1)=coordhistory(1)=0,0 when age=0
                ran=igrnd(1,4) ! generate random number

                ! Determine the direction of a single step (every direction equally likely). coord(1) is x-coordinate
                ! and coord(2) is y-coordinate

                if (ran.eq.1) then
                    coord(2)=coord(2)+1
                else if (ran.eq.2) then
                    coord(1)=coord(1)+1
                else if (ran.eq.3) then
                    coord(2)=coord(2)-1
                else
                    coord(1)=coord(1)-1
                end if
                
                if (coord(1).ge.10) then
                    if(age.le.600) then
                        successes=successes+1
                        coordhistory(age+1,1:2)=coord !since we exit loop we need to add last coordinate here
                        exit
                    else !if sailor makes it to the beach, but late. They are not kept count of.
                        coordhistory(age+1,1:2)=coord
                        exit
                    end if
                end if

                if (age.eq.agemax) then
                    agedeaths=agedeaths+1
                end if

                coordhistory(age+1,1:2)=coord ! add coordinates to coordhistory
            end do

            if (traj=='yes') then
                write(fn,fmt='(i0,a)') sailors, '.dat' !for example sailor 1 data is in 1.dat
                fullname=trajdir(1:len_trim(trajdir))//fn !full filepath of data file since the data is in folder 'Trajectories'
                open(unit=2,file=fullname,action='write',status='replace')
                
                do i=1,age+1
                    write(2,*) (coordhistory(i,l),l=1,2) !l is a dummy index for column number (x or y)
                end do
            end if
            
            nofsteps(sailors)=age !age+1 is the the age+1 th coordinate, and the amount of steps taken to visit age+1 coordinates
            !is age+1-1 steps. This is also the time walked in minutes, as each step(100m) takes a minute
        
        end do

        avgtime=(sum(nofsteps)/n)!average time is average amount of steps, because we use time units where 1 unit=1step (1 min)
        call statistics(walktype,n,successes,agedeaths,maxval(nofsteps),minval(nofsteps),avgtime) !statistics() will calculate and print all wanted statistics in good format
        
    end subroutine nrw

    subroutine saw
        implicit none

        do sailors=1,n
            age=2 ! since age is used for coordinate history array and age=1 corresponds to coord 0,0
            coord=0 ! set x and y coords to 0 everytime we have a new sailor
            coordhistory=0 !same for historical coords
            
            !first step of saw can be in any of the 4 directions:

            call sgrnd(getseed())
            ran=igrnd(1,4)

            if (ran.eq.1) then
                coord(2)=coord(2)+1
            else if (ran.eq.2) then
                coord(1)=coord(1)+1
            else if (ran.eq.3) then
                coord(2)=coord(2)-1
            else
            coord(1)=coord(1)-1
            end if

            coordhistory(age,1:2)=coord

            !from 2nd coordinate onward we have to avoid old coordinates.

            whileloop : do while (coord(1).lt.10)
                ran=igrnd(1,4)

                if (deadendcounter.gt.100) then !the counter is a probabilistic construct to determine whether we are in a dead end or not
                    deadends=deadends+1
                    exit whileloop!to the next sailor
                end if

                !Now we determine the direction of each step (equiprobable)

                if (ran.eq.1) then
                    coord(2)=coord(2)+1
                    do i=1,age+1 !shouldnt this be 1,age
                        if (all(coord.eq.coordhistory(i,1:2))) then !if any row of coordhistory equals current coords, we take a step back and try again
                            coord(2)=coord(2)-1
                            deadendcounter=deadendcounter+1 !everytime we take a step back, we add 1 to the counter. when the counter reaches 100, we conclude we are stuck
                            cycle whileloop       !start the loop again from the same starting coordinate and randomize a direction to step in again                 
                        else
                            continue !the coordinate was a new one and we can go on
                        end if                    
                    end do

                    deadendcounter=0 !if we didnt have to cycle, it means the step was successful and we werent in a dead end so we reset counter
                
                else if (ran.eq.2) then
                    coord(1)=coord(1)+1
                    do i=1,age+1 
                        if (all(coord.eq.coordhistory(i,1:2))) then 
                            coord(1)=coord(1)-1
                            deadendcounter=deadendcounter+1
                            cycle whileloop
                        else
                            continue
                        end if                    
                    end do

                    deadendcounter=0

                else if (ran.eq.3) then
                    coord(2)=coord(2)-1
                    do i=1,age+1 
                        if (all(coord.eq.coordhistory(i,1:2))) then 
                            coord(2)=coord(2)+1
                            deadendcounter=deadendcounter+1
                            cycle whileloop
                        else
                            continue
                        end if 
                    end do

                    deadendcounter=0

                else
                    coord(1)=coord(1)-1
                    do i=1,age+1 
                        if (all(coord.eq.coordhistory(i,1:2))) then 
                            coord(1)=coord(1)+1
                            deadendcounter=deadendcounter+1
                            cycle whileloop
                        else
                            continue
                        end if
                    end do

                    deadendcounter=0
                    
                end if
                
                age=age+1 ! add 1 to age after we have taken a successful step
                coordhistory(age,1:2)=coord !add current coordinate to the history of coordinates
            
            end do whileloop

            deadendcounter=0 ! reset deadendcounter for next sailor

            if (coord(1).ge.10.and.age.le.600) then !count successes
                successes=successes+1
            end if

            if (traj=='yes') then ! write coordinates to file
            
                write(fn,fmt='(i0,a)') sailors, '.dat'
                fullname=trajdir(1:len_trim(trajdir))//fn ! Create file path as string
                open(unit=2,file=fullname,action='write',status='replace')
            
                do i=1,age
                    write(2,*) (coordhistory(i,l),l=1,2) !write the x,y coordinate pairs line by line
                end do

                close(2)
            end if

            nofsteps(sailors)=age-1 !before going to next sailor, we store the amount of steps taken

        end do
        
        avgtime=(sum(nofsteps)/n)!average time is average amount of steps, because we use time units where 1 unit=1step (1 min)
        call statistics(walktype,n,successes,deadends,maxval(nofsteps),minval(nofsteps),avgtime)

    end subroutine saw

end program sailor
