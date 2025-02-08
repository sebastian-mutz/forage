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
  write(io%pUnit, *), ""
  write(io%pUnit, *), "///////////////////////////////////"
  write(io%pUnit, *), "// FORage - Expedition Simulator //"
  write(io%pUnit, *), "///////////////////////////////////"
  write(io%pUnit, *), ""
  write(io%pUnit, *), "1. Continue game."
  write(io%pUnit, *), "2. New game (overwrites progress)."
  write(io%pUnit, *), "3. Exit."

end subroutine start


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine viewInventory(inv, rsc)

! ==== Description
! out: eventCode - code for specific events; to betied to procedures.

! ==== Declarations
  type(TYP_inventory), intent(in) :: inv
  type(TYP_item)     , intent(in) :: rsc(4)
  integer(i4)                     :: i
  character(len=5)                :: cyan = char(27) // '[36m'
  character(len=5)                :: reset = char(27) // '[0m'

! ==== Instructions
! splash
  write(io%pUnit, *), cyan, ""
  write(io%pUnit, *), "// Inventory //"
  write(io%pUnit, *), "==============="
  do i=1,inv%n
     write(io%pUnit, '(a15,i5)'), rsc(((inv%id(i))))%name, inv%stock(i)
  enddo
  write(io%pUnit, *), "", reset

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
  write(io%pUnit, *), cyan, ""
  write(io%pUnit, *), "// Expedition Members //"
  write(io%pUnit, *), "========================"
  do i=1,n
     write(io%pUnit, '(a15,a5,i2,a1)'), actor(i)%name, "(ID: ", actor(i)%id, ")"
     write(io%pUnit, '(a10,i3,a10,i3)'), "Health: ", actor(i)%health, "Sanity:", actor(i)%sanity
     write(io%pUnit, *), "------------------------"
  enddo
     write(io%pUnit, *), "", reset

end subroutine viewTeam

end module dsp
