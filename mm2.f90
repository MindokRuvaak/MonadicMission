program mm2
    use filereader
    integer :: fileLength, count, nDosor, i, nInstructions, x, y, outp, i_setupLogc
    integer, allocatable :: rawVecPos(:,:)
    ! integer, allocatable :: xPos(:), yPos(:)
    character(len=42), allocatable :: rawVecPath(:)
    character(len=:), allocatable :: fp
    character(len=1) :: dir
    logical, allocatable :: stillThere(:)
    ! logical :: resx, resy
    fp = "mm2.dat"
    count = 0
    x = 0
    y = 0
    dir = "N"

    ! nDosor = 6
    nDosor = 10000
    allocate(rawVecPos(2,nDosor))
    allocate(stillThere(nDosor))
    do i_setupLogc = 1, nDosor
        stillThere(i_setupLogc) = .TRUE.
    end do
    ! allocate(xPos(nDosor))
    ! allocate(yPos(nDosor))


    call readMatrixInt(resMatrix=rawVecPos, m=nDosor, n=2, filePath=fp, lfp=len(fp))

    ! do j = 1,nDosor
    !     ! print*, rawVecPos(1,j), rawVecPos(2,j)
    !     xPos(j) = rawVecPos(1,j)
    !     yPos(j) = rawVecPos(2,j)
    ! end do

    call lengthOfFile(nLines=fileLength, filePath=fp, lfp=len(fp))
    nInstructions = (fileLength-1)-nDosor
    ! print*, fileLength, nInstructions

    allocate(rawVecPath(nInstructions))
    call readLines(resArr=rawVecPath, n=nInstructions, startLine=(nDosor+1), stopLine=fileLength-1, filePath=fp, lfp=len(fp))

    ! write(*,*) (mat(1,i), mat(2,i), new_line("a"), i = 1, 1000)
    ! write(*,*) (rawVecPos(1,i), rawVecPos(2,i), new_line("a"), i = 1, nDosor)
    ! write(*,*) (xPos(i), yPos(i), new_line("a"), i = 1, nDosor)
    ! print *, "S", dir, x, y, count
    ! open(newunit=outp, file="path.dat", status="replace", action="write")
    ! write(outp,*) x, y, "S"
    do i = 1, nInstructions
        ! print*, "i:", i, rawVecPath(i)
        select case(rawVecPath(i))
        case("G")
            select case(dir)
            case("N")
                y = y + 1
            case("E")
                x = x + 1
            case("S")
                y = y - 1
            case("W")
                x = x - 1
            end select
            ! print*, "x-pos"
            ! resx = intInVec(xPos, nDosor, x)
            ! print*, "y-pos"
            ! resy = intInVec(yPos, nDosor, y)
            ! print*, resx, resy
            if ( hasDosa(rawVecPos, nDosor, x, y) ) then
                print*,"found dosa location: ", x, y
                if ( newDosa(x, y) ) then
                    ! write(outp,*) x, y
                    count = count + 1
                end if
            end if
        case("V")
            select case(dir)
            case("N")
                dir = "W"
            case("E")
                dir = "N"
            case("S")
                dir = "E"
            case("W")
                dir = "S"
            end select
            ! print*, dir, x, y
        case("H")
            select case(dir)
            case("N")
                dir = "E"
            case("E")
                dir = "S"
            case("S")
                dir = "W"
            case("W")
                dir = "N"
            end select
            ! print*, dir, x, y
        end select
        ! write(outp,*) x, y, dir
        ! print *, rawVecPath(i), dir, x, y, count
    end do
    close(outp)
    print *, count

contains

    function hasDosa(dosPos, numDosor, xPos, yPos) result(retval)
        integer, intent(in) :: xPos, yPos, numDosor
        integer, intent(in) :: dosPos(2, numDosor)
        logical :: retval
        integer :: d, dosX, dosY
        retval = .FALSE.
        ! print*, "has?", xPos, yPos
        do d = 1, numDosor
            dosX = dosPos(1,d)
            dosY = dosPos(2,d)
            ! print*, d, dosX, dosY
            if ( (dosX .EQ. xPos) .AND. (dosY .EQ. yPos)  ) then
                retval = .TRUE.
                exit
            end if
        end do

    end function hasDosa

    function newDosa(posX, posY) result(found)
        integer, intent(in) :: posX, posY
        logical :: found
        integer :: loop
        ! stillThere, rawVecPos,
        do loop = 1, nDosor
            if ((rawVecPos(1,loop) .EQ. posX) .AND. (rawVecPos(2,loop) .EQ. posY) ) then
                found = stillThere(loop)
                print*, "Dosa still there?", stillThere(loop)
                stillThere(loop) = .FALSE.
            end if
        end do
        

    end function newDosa

end program mm2