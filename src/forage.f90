module forage

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

! load modules
  use :: typ
  use :: ini

! basic options
  implicit none
  private

! declare public
  public :: game_loop

contains

! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine game_loop(nAct, actor)

! ==== Description
!! in:
!! nAct  - number of actors
!! actor - actors/PCs

! ==== Declarations
! game world
  integer(kind=4), intent(in) :: nAct
  type(TYP_actor), intent(in) :: actor(nAct)

! game mechanics
  logical         :: done=.false., aB
  integer(kind=4) :: eventCode, aI
  real(kind=4)    :: aR


! ==== Instructions
! splash screen and user input
  call start(eventCode)

! determine action
  select case (eventCode)
     case (1)
        write(io%pUnit, *), "> Loading game ..."
     case (2)
        write(io%pUnit, *), "> Starting new game ..."
     case (3)
        write(io%pUnit, *), "> Exiting ..."
  end select

! game loop
  do while (.not. done)
     write(io%pUnit, *), "> Waiting for user input (1=play, 2=exit)"
     read *, eventCode
     if (eventCode .eq. 2) then
        done = .true.
     elseif (eventCode .eq. 1) then
        write(io%pUnit, *), "Define p (in range 0.0 - 1.0)"
        read *, aR
        call eventBool(aR, aB)
        if (aB) then
           print *, "The event occurred!"
           call eventDice(1, 6, aI)
           print *, aI
        else
           print *, "The event did not occur."
        end if

     endif

  enddo

end subroutine game_loop


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine start(eventCode)

! ==== Description
! out: eventCode - code for specific events; to betied to procedures.

! ==== Declarations
  integer(kind=4) :: eventCode

! ==== Instructions
! splash
  write(io%pUnit, *), ""
  write(io%pUnit, *), "///////////////////////////////////"
  write(io%pUnit, *), "// FORage - Expedition Simulator //"
  write(io%pUnit, *), "///////////////////////////////////"
  write(io%pUnit, *), ""
  write(io%pUnit, *), "What's next?"
  write(io%pUnit, *), "1. Continue game."
  write(io%pUnit, *), "2. New game (overwrites progress)."
  write(io%pUnit, *), "3. Exit."

! get input
  read *, eventCode
  write(io%pUnit, *), ""

end subroutine start


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine eventDice(a, b, e)

! ==== Description
! Simulates a dice roll event (integers only) with specified number
! range, using  uniform weights ("fair dice").
! in:  r1, r2  - min and max possible number (e.g., 1,6 for 6-sided die)
! out: e       - event outcome

! ==== Declarations
  integer(kind=4), intent(in)  :: a, b
  integer(kind=4), intent(out) :: e
  real(kind=4)                 :: rnd

! ==== Instructions
  call random_number(rnd)
  e=floor(float(a)+rnd*(float(b+1)-float(a)))

end subroutine eventDice


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine eventBool(p, e)

! ==== Description
! Boolean event simulator; simulates if an event occurs (true or false)
! based on a specified event probability.
! in :   p   - probability of event
! out:   e   - outcome; true (event occured) or false (event did not occur)
! other: rnd - generated random number

! ==== Declarations
    real(kind=4), intent(in) :: p
    logical, intent(out)     :: e
    real(kind=4)             :: rnd

! ==== Instructions
! (re-)initialise random number generator (optional)
  call random_seed()
! generate a random number
  call random_number(rnd)
! determine if the event occurs
  e = (rnd .lt. p)

end subroutine eventBool


end module forage
