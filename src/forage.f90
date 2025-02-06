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
  type(TYP_inventory) :: inv

! game mechanics
  logical           :: done=.false., aB
  integer(i4)       :: eventCode, aI
  real(sp)          :: aR
  character(len=5)  :: blue = char(27) // '[34m'
  character(len=5)  :: reset = char(27) // '[0m'

! general
  integer(i4) :: i

! ==== Instructions
! initialisation

! splash screen and user input
  call start(eventCode)

! load game state
  call load(inv)

! determine action
  select case (eventCode)
     case (1); write(io%pUnit, *), "> Loading game ..."
     case (2); write(io%pUnit, *), "> Starting new game ..."
     case (3); done = .true.
  end select

! game loop
  do while (.not. done)
     write(io%pUnit, *), "> Waiting for user input ", blue, "(1=play, 2=view inventory, 3=view team, 4=exit)", reset
     read *, eventCode
     select case (eventCode)
        case (4); done = .true.
        case (3); call viewTeam(size(actor), actor)
        case (2); call viewInventory(inv, rsc)
        case (1); call event(size(actor), actor, inv)
     end select
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
  write(io%pUnit, *), "1. Continue game."
  write(io%pUnit, *), "2. New game (overwrites progress)."
  write(io%pUnit, *), "3. Exit."

! get input
  read *, eventCode
  write(io%pUnit, *), ""

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
     write(io%pUnit, '(a10,i3,a10,i3)'), "Health: ", actor(i)%hp, "Sanity:", actor(i)%sp
     write(io%pUnit, *), "------------------------"
  enddo
     write(io%pUnit, *), "", reset

end subroutine viewTeam

! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine event(n, actor, inv)

! ==== Description
! Simulates the outcome of events (per day), incl. gain of resources:
! 1. Uses actor skills to determine the probability of success (e.g.,
!    for foraging).
! 2. Determines if activity was successful.
! 3. If successful, uses actor skills to determine dice to be rolled
!    (i.e. the value range) to determine the extent of the success
!    (e.g., how much food was gained from foraging).
! 4. Update and return inventory (inv)

! ==== Declarations
  integer(i4)        , intent(in) :: n
  type(TYP_actor)    , intent(in) :: actor(n)
  type(TYP_inventory), intent(in) :: inv
  integer(i4)                     :: i, j, a, b
  real(sp)                        :: p
  logical                         :: e, f

! ==== Instructions
!
! TODO: loop only through actors commanded to carry out action
! TODO: simply pass to correct inventory slot

  do i=1,n

  ! foraging check
  if (actor(i)%can_forage) then
     ! probability of success (based on skill; max 9) and determine if successful
     p = float(actor(i)%skill_forage)/10.0
     call eventBool(p,e)
     ! if successful, determine extent of success
     if (e) then
        ! calculate possible range of gain based on skill
        select case (actor(i)%skill_forage)
           case (1:3);  a=1; b=4
           case (4:6);  a=1; b=6
           case (7:9);  a=1; b=12
        end select
        call eventDice(a, b, j)
        ! determine if food (65% chance) or medicine (35% change)
        call eventBool(0.65,f)
        write(io%pUnit, *), "Foraging: ", trim(actor(i)%name), " was successful"
        if (f) then
           write(io%pUnit, *), j, "food"
        else
           write(io%pUnit, *), j, "medicine"
        endif
     else
        write(io%pUnit, *), "Foraging: ", trim(actor(i)%name), " was unsuccessful"
     endif
  endif

  ! scouting check
  if (actor(i)%can_scout) then
     ! probability of success (based on skill; max 9) and determine if successful
     p = float(actor(i)%skill_scout)/10.0
     call eventBool(p,e)
     ! if successful, determine extent of success
     if (e) then
        ! calculate possible range of gain based on skill
        select case (actor(i)%skill_scout)
           case (1:3);  a=1; b=6
           case (4:6);  a=1; b=12
           case (7:9);  a=1; b=20
        end select
        call eventDice(a, b, j)
        write(io%pUnit, *), "Scouting: ", trim(actor(i)%name), " was successful and gained", j, "treasure"
     else
        write(io%pUnit, *), "Scouting: ", trim(actor(i)%name), " was unsuccessful"
     endif
  endif

  ! guarding check
  if (actor(i)%can_guard) then
     ! probability of success (based on skill; max 9) and determine if successful
     p = float(actor(i)%skill_guard)/10.0
     call eventBool(p,e)
     ! if not successful, determine extent of success
     if (.not. e) then
     ! calculate possible range of losses (hp or material) based on skill
        select case (actor(i)%skill_guard)
           case (1:3);  a=1; b=20
           case (4:6);  a=1; b=12
           case (7:9);  a=1; b=6
        end select
        call eventDice(a, b, j)
        ! determine what is stolen
        call eventDice(1, 4, a)
        select case (a)
           case (1); write(io%pUnit, *), "Guarding: unsuccessful. ", j, "food stolen"
           case (2); write(io%pUnit, *), "Guarding: unsuccessful. ", j, "medicine stolen"
           case (3); write(io%pUnit, *), "Guarding: unsuccessful. ", j, "treasure stolen"
           case (4)
              ! determine who was hurt
              call eventDice(1, n, b)
              write(io%pUnit, *), "Guarding: unsuccessful. ", j, "damage taken (", trim(actor(b)%name), ")"
        end select
     else
        write(io%pUnit, *), "Guarding: was successful"
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
     enddo

    ! TODO: read actor selection and their states (hp, etc.)

  close(io%wUnit)

end subroutine load


end module forage
