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
  use :: ini
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
  integer(kind=4) :: eventCode
!
! ==== Instructions
!
! splash screen and user input
  call start(eventCode)
!
! determine action
  select case (eventCode)
     case (1)
        write(io%pUnit, *), "> Loading game ..."
     case (2)
        write(io%pUnit, *), "> Starting new game ..."
        call new(nAct, actor)
     case (3)
        write(io%pUnit, *), "> Exiting ..."
  end select
!
! game loop
  do while (.not. done)
     write(io%pUnit, *), "> Waiting for user input"
     read *, eventCode
     if (eventCode .eq. 1) then
        done = .true.
     elseif (eventCode .eq. 2) then
        write(io%pUnit, *), "Hello, forage!"
     endif
!
  enddo
!
end subroutine game_loop
!
!
! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine start(eventCode)
!
! ==== Description
!
! out: eventCode - code for specific events; to betied to procedures.
!
! ==== Declarations
!
  integer(kind=4) :: eventCode
!
! ==== Instructions
!
! splash
  write(io%pUnit, *), ""
  write(io%pUnit, *), "///////////////////////////////////"
  write(io%pUnit, *), "// FORage - Expedition Simulator //"
  write(io%pUnit, *), "///////////////////////////////////"
  write(io%pUnit, *), ""
  write(io%pUnit, *), "> What's next?"
  write(io%pUnit, *), "1. Continue game."
  write(io%pUnit, *), "2. New game (overwrites progress)."
  write(io%pUnit, *), "3. Exit."
!
! get input
  read *, eventCode
  write(io%pUnit, *), ""
!
end subroutine start
!
!
! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine new(nAct, actor)
!
! ==== Description
!
! out: eventCode - code for specific events; to betied to procedures.
!
! ==== Declarations
!
! game world
  integer(kind=4), intent(in) :: nAct
  type(TYP_actor), intent(in) :: actor(nAct)
!
! ==== Instructions
!
! splash
  write(io%pUnit, *), ""
  write(io%pUnit, *), "> Select expedition members by typing their IDs. Example: 1,3,4"
  write(io%pUnit, *), ""
!
  write(io%pUnit, "(a11)")       , "Information"
  write(io%pUnit, "(a11)")       , "==========="
  write(io%pUnit, "(a11,2x,i5)") , "ID        :", actor(1)%id
  write(io%pUnit, "(a11,2x,a15)"), "Name      :", trim(actor(1)%name)
  write(io%pUnit, "(a11,2x,i5)") , "Vitality  :", actor(1)%att_vitality
  write(io%pUnit, "(a11,2x,i5)") , "Resilience:", actor(1)%att_resilience
  write(io%pUnit, *), ""
  write(io%pUnit, "(a6)")       , "Skills"
  write(io%pUnit, "(a6)")       , "======"
  write(io%pUnit, "(a11,2x,i5)") , "Forage    :", actor(1)%skill_forage
  write(io%pUnit, "(a11,2x,i5)") , "Scout     :", actor(1)%skill_scout
  write(io%pUnit, "(a11,2x,i5)") , "Guard     :", actor(1)%skill_guard
  write(io%pUnit, "(a11,2x,i5)") , "Heal      :", actor(1)%skill_heal
  write(io%pUnit, *), ""
!
end subroutine new
!
!
end module forage
