module forage

! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Main procedures (game loop) for Fortran expedition simulator.      |
! |                                                                    |
! | license : MIT                                                      |
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.eu)           |
! |--------------------------------------------------------------------|

! load modules
  use :: typ
  use :: ini
  use :: data

! basic options
  implicit none
  private

! declare public
  public :: game_loop

contains

! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine game_loop()

! ==== Description
!! in:
!! nAct  - number of actors
!! actor - actors/PCs

! ==== Declarations
! game world
  type(TYP_inventory) :: rsc

! game mechanics
  logical     :: done=.false., aB
  integer(i4) :: eventCode, aI
  real(sp)    :: aR

! general
  integer(i4) :: i


! ==== Instructions
! initialisation

! splash screen and user input
  call start(eventCode)

! load game state
  call load(rsc)
  print*, rsc%n
  do i=1,rsc%n
     write(io%pUnit, *), rsc%id(i), rsc%stock(i)
  enddo

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
        call event(size(actor), actor, rsc)
!         write(io%pUnit, *), "Define p (in range 0.0 - 1.0)"
!         read *, aR
!         call eventBool(aR, aB)
!         if (aB) then
!            print *, "The event occurred!"
!            call eventDice(1, 6, aI)
!            print *, aI
!         else
!            print *, "The event did not occur."
!         end if

     endif

  enddo

end subroutine game_loop


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine start(eventCode)

! ==== Description
! out: eventCode - code for specific events; to betied to procedures.

! ==== Declarations
  integer(i4) :: eventCode

! ==== Instructions
! splash
  write(io%pUnit, *), ""
  write(io%pUnit, *), "///////////////////////////////////"
  write(io%pUnit, *), "// FORage - Expedition Simulator //"
  write(io%pUnit, *), "///////////////////////////////////"
  write(io%pUnit, *), ""
!   write(io%pUnit, *), "1. Continue game."
!   write(io%pUnit, *), "2. New game (overwrites progress)."
!   write(io%pUnit, *), "3. Exit."

! get input
!   read *, eventCode
!   write(io%pUnit, *), ""

end subroutine start


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine event(n, actor, inventory)

! ==== Description
! Simulates the outcome of events (per day), incl. gain of resources:
! 1. Uses actor skills to determine the probability of success (e.g.,
!    for foraging).
! 2. Determines if activity was successful.
! 3. If successful, uses actor skills to determine dice to be rolled
!    (i.e. the value range) to determine the extent of the success
!    (e.g., how much food was gained from foraging).
! 4. Update and return inventory

! ==== Declarations
  integer(i4)        , intent(in) :: n
  type(TYP_actor)    , intent(in) :: actor(n)
  type(TYP_inventory), intent(in) :: inventory
  integer(i4)                     :: i, j, a, b
  real(sp)                        :: p
  logical                         :: e

! ==== Instructions
!
! TODO: loop only through actors commanded to carry out action
! TODO: simply pass to correct inventory slot

  do i=1,n

  ! foraging
  if (actor(i)%can_forage) then
     ! probability of success (based on skill; max 10)
     p = float(actor(i)%skill_forage)/10.0
     ! calculate possible range of gain based on skill
     select case (actor(i)%skill_forage)
        case (1:3);  a=1; b=3
        case (4:6);  a=3; b=5
        case (7:10); a=5; b=8
     end select
     ! determine if successful
     call eventBool(p,e)
     ! if successful, determine extent of success
     if (e) then
        call eventDice(a, b, j)
        write(io%pUnit, *), "Foraging: ", trim(actor(i)%name), " was successful and gained", j, "food"
     else
        write(io%pUnit, *), "Foraging: ", trim(actor(i)%name), " was unsuccessful"
     endif
  endif

  ! scout
  if (actor(i)%can_scout) then
     ! probability of success (based on skill; max 10)
     p = float(actor(i)%skill_scout)/10.0
     ! calculate possible range of gain based on skill
     select case (actor(i)%skill_scout)
        case (1:3);  a=1; b=3
        case (4:6);  a=3; b=5
        case (7:10); a=5; b=8
     end select
     ! determine if successful
     call eventBool(p,e)
     ! if successful, determine extent of success
     if (e) then
        call eventDice(a, b, j)
        write(io%pUnit, *), "Scouting: ", trim(actor(i)%name), " was successful and gained", j, "treasure"
     else
        write(io%pUnit, *), "Scouting: ", trim(actor(i)%name), " was unsuccessful"
     endif
  endif

  ! guard
  if (actor(i)%can_guard) then
     ! probability of success (based on skill; max 10)
     p = float(actor(i)%skill_guard)/10.0
     ! calculate possible range of gain based on skill
     select case (actor(i)%skill_guard)
        case (1:3);  a=1; b=3
        case (4:6);  a=3; b=5
        case (7:10); a=5; b=8
     end select
     ! determine if successful
     call eventBool(p,e)
     ! if successful, determine extent of success
     if (e) then
        call eventDice(a, b, j)
        write(io%pUnit, *), "Guarding: ", trim(actor(i)%name), " was successful", j
     else
        write(io%pUnit, *), "Guarding: ", trim(actor(i)%name), " was unsuccessful"
     endif
  endif

  ! heal
  if (actor(i)%can_heal) then
     ! probability of success (based on skill; max 10)
     p = float(actor(i)%skill_heal)/10.0
     ! calculate possible range of gain based on skill
     select case (actor(i)%skill_heal)
        case (1:3);  a=1; b=3
        case (4:6);  a=3; b=5
        case (7:10); a=5; b=8
     end select
     ! determine if successful
     call eventBool(p,e)
     ! if successful, determine extent of success
     if (e) then
        call eventDice(a, b, j)
        write(io%pUnit, *), "Healing: ", trim(actor(i)%name), " was successful and healed", j
     else
        write(io%pUnit, *), "Healing: ", trim(actor(i)%name), " was unsuccessful"
     endif
  endif

  enddo

end subroutine event


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine eventDice(a, b, e)

! ==== Description
! Simulates a dice roll event (integers only) with specified number
! range, using  uniform weights ("fair dice").
! in:  a, b - min and max possible number (e.g., 1,6 for 6-sided die)
! out: e    - event outcome

! ==== Declarations
  integer(i4), intent(in)  :: a, b
  integer(i4), intent(out) :: e
  real(sp)                 :: rnd

! ==== Instructions
! generate a random number (in range 0.0 - 1.0)
  call random_number(rnd)
! adjust for range and convert to integer
  e=floor(float(a)+rnd*float(b+1-a))

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
    real(sp), intent(in) :: p
    logical, intent(out) :: e
    real(sp)             :: rnd

! ==== Instructions
! (re-)initialise random number generator (optional)
  call random_seed()
! generate a random number (in range 0.0 - 1.0)
  call random_number(rnd)
! determine if the event occurs
  e = (rnd .lt. p)

end subroutine eventBool


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine load(rsc)

! ==== Description
! Loads game progress (inventory and actors)
! out:   rsc - inventory of resources as read from file

! ==== Declarations
  integer(i4)                      :: i
  type(TYP_inventory), intent(out) :: rsc

! ==== Instructions
!
  open(io%wUnit, file="data/001.sav", action="read")

     ! read number of inventory item types and allocate
     read(io%wUnit,*) rsc%n
     allocate(rsc%id(rsc%n))
     allocate(rsc%stock(rsc%n))

     ! read inventory
     do i=2,rsc%n+1
        read(io%wUnit,*) rsc%id(i-1), rsc%stock(i-1)
        print*, i, rsc%n
     enddo

  close(io%wUnit)

end subroutine load


end module forage
