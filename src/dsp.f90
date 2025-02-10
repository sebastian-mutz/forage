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
  use :: typ
  use :: ini

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
  write(INI_io%pUnit, *), ""
  write(INI_io%pUnit, *), "///////////////////////////////////"
  write(INI_io%pUnit, *), "// FORage - Expedition Simulator //"
  write(INI_io%pUnit, *), "///////////////////////////////////"
  write(INI_io%pUnit, *), ""
  write(INI_io%pUnit, *), "1. Continue game."
  write(INI_io%pUnit, *), "2. New game (overwrites progress)."
  write(INI_io%pUnit, *), "3. Exit."

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
  write(INI_io%pUnit, *), cyan, ""
  write(INI_io%pUnit, *), "// Inventory //"
  write(INI_io%pUnit, *), "==============="
  do i=1,n
     write(INI_io%pUnit, '(a11,i5)'), inv(i)%name, inv(i)%stock
  enddo
  write(INI_io%pUnit, *), "", reset

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
  write(INI_io%pUnit, *), cyan, ""
  write(INI_io%pUnit, *), "// Expedition Members //"
  write(INI_io%pUnit, *), "========================"
  do i=1,n
     write(INI_io%pUnit, '(a15,a5,i2,a1)'), actor(i)%name, "(ID: ", actor(i)%id, ")"
     write(INI_io%pUnit, '(a10,i3,a10,i3)'), "Health: ", actor(i)%health, "Sanity:", actor(i)%sanity
     write(INI_io%pUnit, *), "------------------------"
  enddo
     write(INI_io%pUnit, *), "", reset

end subroutine viewTeam

end module dsp
