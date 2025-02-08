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
  use :: dsp
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
  call start()
  read *, eventCode

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
        case (1); call actions(size(actor), actor, size(skill), skill, inv)
     end select
  enddo

end subroutine game_loop


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine actions(na, actor, ns, skill, inv)

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
  integer(i4)        , intent(in) :: na, ns
  type(TYP_actor)    , intent(in) :: actor(na)
  type(TYP_skill)    , intent(in) :: skill(ns)
  type(TYP_inventory), intent(in) :: inv
  integer(i4)                     :: i, j, a
  real(sp)                        :: p
  logical                         :: e

! ==== Instructions
! TODO: simply pass to correct inventory slot

  do j=1,ns    ! action/skill loop
     write(io%pUnit, *), ""
     write(io%pUnit, *), "Action: ", skill(j)%name
     do i=1,na ! actor loop

     if (actor(i)%action .eq. j) then

        ! get probability of success (max skill=5, max p=0.9) and determine if successful
        p = float(actor(i)%skill(j))/5.555
        call eventBool(p,e)

        ! if successful, determine extent of success
        if (e) then
           if (skill(j)%name .eq. "Guard") then
              a=0
           else
              call eventDice(skill(j)%dice(actor(i)%skill(j),1), skill(j)%dice(actor(i)%skill(j),2), a)
           endif
           write(io%pUnit, *), trim(actor(i)%name), " was successful", a
        else
           if (skill(j)%name .eq. "Guard") then
              call eventDice(skill(j)%dice(actor(i)%skill(j),1), skill(j)%dice(actor(i)%skill(j),2), a)
           else
              a=0
           endif
           write(io%pUnit, *), trim(actor(i)%name), " was unsuccessful", a
        endif
     endif

     enddo ! actor loop
  enddo    ! action/skill loop

end subroutine actions


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
subroutine load(inv)

! ==== Description
! Loads game progress (inventory and actors)
! out:   inv - inventory of resources as read from file

! ==== Declarations
  integer(i4)                      :: i
  type(TYP_inventory), intent(out) :: inv

! ==== Instructions
!
  open(io%wUnit, file="sav/inv.sav", action="read")

     ! read number of inventory item types and allocate
     read(io%wUnit,*) inv%n
     allocate(inv%id(inv%n))
     allocate(inv%stock(inv%n))

     ! read inventory
     do i=2,inv%n+1
        read(io%wUnit,*) inv%id(i-1), inv%stock(i-1)
     enddo

    ! TODO: read actor selection and their states (hp, etc.)

  close(io%wUnit)

end subroutine load


end module forage
