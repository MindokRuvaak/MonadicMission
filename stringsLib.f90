module Strings
    implicit none
    
   private

   public splitOn, integerToString
contains

! splits string on the first occurence of a character, removing the seperator
    subroutine splitOn(results, strLen, string, seperator)
        character(len=:), dimension(:), allocatable, intent(out) :: results
        integer, intent(in)                 :: strLen
        character(len=strLen), intent(in)   :: string
        character, intent(in)               :: seperator
        integer                             :: resultLength, i, sepInd

        do i = 1, strLen
            if (string(i:i) == seperator) then
                sepInd = i
                exit
            end if
        end do
        resultLength = max(sepInd-1, strLen-(sepInd+1))
        ! print*, sepInd, strLen, resultLength
        ! print*, string(:(sepInd-1))//" :-: "//string((sepInd+1):)
        allocate(character(len=resultLength) :: results(0:2))
        results(0) = integerToString(resultLength)
        results(1) = string(:(sepInd-1))
        results(2) = string((sepInd+1):)
    end subroutine

! splits string at index
    subroutine splitAt(results, strLen, string, at)
        character(len=:), dimension(:), allocatable, intent(out) :: results
        integer, intent(in)                 :: strLen, at
        character(len=strLen), intent(in)   :: string
        integer                             :: resLen
        
        resLen = max(at, strLen-at)
        allocate(character(len=resLen) :: results(2))
        results(1) = string(:at)
        results(2) = string(at+1:)
        
    end subroutine splitAt
    
    function integerToString(inputInteger) result(returnString)
        integer, intent(in) :: inputInteger
        character(len=1024) :: returnString, fmt

        ! print*, inputInteger
        fmt = trim(intfmt(inputInteger))
        ! print*, fmt

        write(returnString, fmt) inputInteger

    end function integerToString

    function intfmt(arg) result(retval)
        integer, intent(in) :: arg
        character(len=1024) :: retval
        character :: oom
        integer :: oomInt
        oomInt = floor(log10(real(arg))+1)
        ! print*, oomInt
        if (oomInt < 10) then
            write(oom, '(i1)') oomInt
        else if (oomInt < 100) then
            write(oom, '(i2)') oomInt
        end if
        ! select case (floor(log10(real(arg))+1))
        ! case (1)
        !     oom = '1'
        ! case (2)
        ! end select
        retval = '(i'//oom//')'
    end function intfmt

    ! function stringToInteger(string) result(res)
    !     character(len=*), intent(in) :: string
    !     integer                      :: res, i
        

    !     do concurrent (i=len(string):1:-1)
    !         ichar
    !     end do
        
    ! end function stringToInteger

end module Strings