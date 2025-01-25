module forage
!
! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Main procedures (game loop) for Fortran expedition simulator.      |
! |                                                                    |
! | license                                                            |
! | -------                                                            |
! | This code is released under the MIT license.                       |
! |                                                                    |
! | author                                                             |
! | ------                                                             |
! | Sebastian G. Mutz (sebastian@sebastianmutz.eu)                     |
! |--------------------------------------------------------------------|
!
! load modules
  use :: typ
!
  implicit none
  private
!
  public :: game_loop
!
!
contains
!
!
! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine game_loop(nAct, actor)
!
! ==== Description
!
!! in:
!! nAct  - number of actors
!! actor - actors/PCs
!
! ==== Declarations
!
! game world
  integer(kind=4), intent(in) :: nAct
  type(TYP_actor), intent(in) :: actor(nAct)
!
! game mechanics
  logical         :: done=.false.
  integer(kind=4) :: keyIn
!
! ==== Instructions
!
! game loop
  do while (.not. done)
     print *, "Waiting for user input"
     read *, keyIn
     if (keyIn .eq. 1) then
        done = .true.
     elseif (keyIn .eq. 2) then
        print *, "Hello, forage!"
     endif
!
  enddo
!
  print *, "exiting"
!
end subroutine game_loop
!
!
end module forage
