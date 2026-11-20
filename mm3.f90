! Mega-boom
! G = ursprung (Gustav)
! T = TNT
! . = blankt golv
! explosion -> två block ut -> 5x5 kvadrat med center på TNT

program mm3
    use filereader
    integer                         :: fileLength, count, ntnt, c, new, x,y
    integer, dimension(2)           :: gloc
    integer, allocatable            :: tlocs(:,:)
    logical                         :: exploding
    logical, allocatable            :: exploded(:)
    character(len=:), allocatable   :: fp
    character(len=64), allocatable  :: rawVec(:)
    count = 1 ! start count at one with the explosion at G
    ntnt = 0 ! excludes the initial at G
    exploding = .true.
    fp = "mm3.dat"

    call lengthOfFile(nLines=fileLength, filePath=fp, lfp=len(fp))

    allocate(rawVec(fileLength))
    call readStringArray(rawVec, fileLength, fp, len(fp))

    do i = 1,fileLength
        print*, rawVec(i)
    end do

! find G and number of T's
    do i = 1,fileLength
        do j = 1, len_trim(rawVec(i))
            select case (rawVec(i)(j:j))
            case ('G')
                ! print*, 'G', " @:", i, j
                gloc = [i, j]
            case ('T')
                ntnt = ntnt + 1
                ! print*, 'T', ntnt, " @:", i, j
            end select
        end do
    end do
    
    ! print*, 'G loc', gloc
    ! print*, 'num tnt', ntnt

    allocate(tlocs(2,ntnt))
    allocate(exploded(ntnt))


    rawVec(gloc(1))(gloc(2):gloc(2)) = 'X'
    print*, "G has exploded @:", gloc(1), gloc(2)
    ! do k = 1,fileLength
        ! print*, rawVec(k)
    ! end do

! save pos of all T's
    c = 1
    do i = 1,fileLength
        do j = 1, len_trim(rawVec(i))
            select case (rawVec(i)(j:j))
            case ('T')
                tlocs(:,c) = [i,j]
                exploded(c) = .false.
                ! print*, 'c=', c, '@:', i, j
                ! tlocs(1,c) = i
                ! tlocs(2,c) = j
                c = c+1
            end select
        end do
    end do

    ! do i= 1,ntnt
    !     print*, tlocs(:,i)
    ! end do
    do i=1,ntnt
        if (inRange(2, gloc, tlocs(:,i))) then
            exploded(i) = .true.
            y = tlocs(1,i)
            x = tlocs(2,i)
            ! print*, tlocs(:,i), "has exploded"
            rawVec(y)(x:x) = 'X'
            count = count + 1
            ! do k = 1,fileLength
                ! print*, rawVec(k)
            ! end do
        end if
        ! print*, tlocs(:,i), exploded (i)
    end do

tmp = 0
dw: do while (exploding)
        new = 0
outer:  do i = 1,ntnt
exp:        if (exploded(i)) then
inner:          do j = 1,ntnt
inR:                if ((.not. exploded(j)) .and. inRange(2, tlocs(:,i), tlocs(:,j))) then
                        exploded(j) = .true.
                        new = new + 1
                        count = count + 1
                        ! print*, ''
                        ! print*, tlocs(:,j), "has exploded"
                        ! print*, ''
                        y = tlocs(1,j)
                        x = tlocs(2,j)
                        rawVec(y)(x:x) = 'X'
! prt:                    do k = 1,fileLength
                            ! print*, rawVec(k)
                        ! end do prt
                    end if inR
                end do inner
            end if exp
        end do outer
                if (new == 0) then
            exploding = .false.
        end if
    end do dw


    print*, "num explosions:", count

contains
    ! range r will always be 2
    function inRange(r, explodingLoc, otherLoc) result(willExplode)
        implicit none
        integer, intent(in) :: r, explodingLoc(2), otherLoc(2)
        logical :: willExplode, xrange, yrange
        yrange = abs(explodingLoc(1) - otherLoc(1)) <= r
        xrange = abs(explodingLoc(2) - otherLoc(2)) <= r
        ! print*, explodingLoc, otherLoc, yrange, xrange

        willExplode = xrange .AND. yrange

    end function inRange

end program