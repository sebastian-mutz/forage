module dsp

! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Display module for Fortran expedition simulator.                   |
! |                                                                    |
! | license : MIT                                                      |
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.eu)           |
! |--------------------------------------------------------------------|

! load modules
  use typ

! basic options
  implicit none
  private

! declare public
  public :: start, viewInventory, viewTeam

contains

! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine start()

! ==== Instructions
! splash
  write(std_o, *), ""
  write(std_o, *), "///////////////////////////////////"
  write(std_o, *), "// FORage - Expedition Simulator //"
  write(std_o, *), "///////////////////////////////////"
  write(std_o, *), ""
  write(std_o, *), "1. Continue game."
  write(std_o, *), "2. New game (overwrites progress)."
  write(std_o, *), "3. Exit."

end subroutine start


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine viewInventory(n, inv)

! ==== Description
! out: eventCode - code for specific events; to betied to procedures.

! ==== Declarations
  integer(i4), intent(in)        :: n
  type(TYP_resource), intent(in) :: inv(n)
  integer(i4)                    :: i
  character(len=5)               :: cyan = char(27) // '[36m'
  character(len=5)               :: reset = char(27) // '[0m'

! ==== Instructions
! splash
  write(std_o, *), cyan, ""
  write(std_o, *), "// Inventory //"
  write(std_o, *), "==============="
  do i=1,n
     write(std_o, '(a11,i5)'), inv(i)%name, inv(i)%stock
  enddo
  write(std_o, *), "", reset

end subroutine viewInventory


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine viewTeam(n, actor)

! ==== Description
! out: eventCode - code for specific events; to betied to procedures.

! ==== Declarations
  integer(i4)    , intent(in) :: n
  type(TYP_actor), intent(in) :: actor(n)
  integer(i4)                 :: i
  character(len=5)            :: cyan = char(27) // '[36m'
  character(len=5)            :: reset = char(27) // '[0m'

! ==== Instructions
! splash
  write(std_o, *), cyan, ""
  write(std_o, *), "// Expedition Members //"
  write(std_o, *), "========================"
  do i=1,n
     write(std_o, '(a15,a5,i2,a1)'), actor(i)%name, "(ID: ", actor(i)%id, ")"
     write(std_o, '(a10,i3,a10,i3)'), "Health: ", actor(i)%health, "Sanity:", actor(i)%sanity
     write(std_o, *), "------------------------"
  enddo
     write(std_o, *), "", reset

end subroutine viewTeam

end module dsp
