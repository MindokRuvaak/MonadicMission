program mm1
    use filereader
    integer :: n, total
    character(len=42), allocatable :: rawVec(:)
    character(len=:), allocatable :: fp, rec
    ! integer, allocatable :: lenVector(:)
    fp = "mm1.dat"
    total = 0

    call lengthOfFile(nLines=n, filePath=fp, lfp=len(fp))
    allocate(rawVec(n))
    ! allocate(lenVector(n))

    call readStringArray(resVector=rawVec, n=n, filePath=fp, lfp=len(fp))
    
    do concurrent (i = 1:n)
        rec = trim(rawVec(i))
        total = total + parseRecipt(len(rec), rec)
    end do
    print *, total

contains

    pure function parseRecipt(orderSize, recipt) result(amount)
        implicit none
        character(*), intent(in) :: recipt
        integer, intent(in) :: orderSize
        integer :: amount, char, mCount, gCount
        amount = 0
        mCount = 0
        gCount = 0

        do char = 1, orderSize
            select case (recipt(char:char))
            case ('M')
                amount = amount + 20
                mCount = mCount + 1
            case ('G')
                amount = amount + 37
                gCount = gCount + 1
            case ('H')
                amount = amount + 12
            end select
        end do
        amount = amount - 5*(min(mCount,gCount))
    end function parseRecipt
end program mm1