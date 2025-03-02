module forage

! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Main procedures (game loop) for Fortran expedition simulator.      |
! |                                                                    |
! | license : MIT                                                      |
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.com)          |
! |--------------------------------------------------------------------|

! load modules
  use typ
  use dsp
  use dat
  use stdlib_ansi, only : fg_color_cyan, fg_color_blue, fg_color_magenta&
                       &, fg_color_green, style_bold, style_reset, ansi_code&
                       &, operator(//), operator(+)

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
!! done  - true to exit game loop
!! code  - code for user input and other events
!! day   - current time (day)
!! ne, na, ni, ns  - no. of events, actors, inventory items, skills
!
! defined in dat module:
!! DAT_actor - actors/PCs

! ==== Declarations
! game mechanics
  logical                          :: done=.false.
  integer(i4)                      :: code, day
  integer(i4)                      :: ne, na, ns, ni
  type(TYP_event)    , allocatable :: event(:)
  type(TYP_actor)    , allocatable :: actor(:)
  type(TYP_skill)    , allocatable :: skill(:)
  type(TYP_resource) , allocatable :: inv(:)
  type(TYP_eventlog) , allocatable :: eventlog(:)
  type(TYP_actionlog), allocatable :: actionlog(:)
  integer(i4)        , allocatable :: roster(:)

! display
  type(TYP_ansi) :: ansi

! general
  integer(i4) :: i

! ==== Instructions
! initialise display
  call initansi(ansi)

! splash screen and user input
  call start(ansi)
!  read *, code

! determine action
!   select case (code)
!      case (1); write(std_o, *) "> Loading game ..."
!      case (2); write(std_o, *) "> Starting new game ..."
!      case (3); done = .true.
!   end select

! team selection
  call viewTeam(size(DAT_actor), DAT_actor, size(DAT_skill), DAT_skill, ansi)
  call buildRoster(size(DAT_actor), na, roster, ansi)

! event, skill, resurce selection (if implemented)
! for now, pass data size
  ne=size(DAT_event)
  ns=size(DAT_skill)
  ni=size(DAT_inv)

! initialisation of game world
  call initialise(size(DAT_event), DAT_event, size(DAT_actor), DAT_actor&
    &, size(DAT_skill), DAT_skill, size(DAT_inv), DAT_inv&
    &, ne, event, na, roster, actor, ns, skill, ni, inv, eventlog, actionlog)

! clear camping log
  call clear(ne, eventlog, na, actionlog)

! reset days
  day=0

! load game state/update inventory
  call load(ni, inv)

! game loop
  do while (.not. done)

     ! prompt
     write(std_o, *) "> Waiting for user input " // ansi%info&
        &, "(1) play (2) view log (3) view inventory " &
        &, "(4) view team (5) exit"&
        & // ansi%reset
     read *, code

     ! determine action
     select case (code)
       ! camping
        case (1)
           ! update day
           day=day+1
           ! clear camplog from previous day
           call clear(ne, eventlog, na, actionlog)
           ! camp (compute activities and events)
           call camp(ne, event, ns, skill, na, actor&
             &, ni, inv, eventlog, actionlog)
           ! view camping log
           call viewLog(ne, event, ns, skill, na, actor&
             &, ni, inv, day, eventlog, actionlog, ansi)
        ! view camplog
        case (2)
           call viewLog(ne, event, ns, skill, na, actor&
             &, ni, inv, day, eventlog, actionlog, ansi)
        ! view inventory
        case (3)
           call viewInventory(ni, inv, ansi)
        ! view team
        case (4)
           call viewTeam(na, actor, ns, skill, ansi)
        ! exit
        case (5)
           done = .true.
     end select

  enddo

! deallocate before exiting
  deallocate(event)
  deallocate(actor)
  deallocate(skill)
  deallocate(inv)
  deallocate(eventlog)
  deallocate(actionlog)
  deallocate(roster)

end subroutine game_loop


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine camp(ne, event, ns, skill, na, actor, ni, inv, eventlog, actionlog)

! ==== Description
! Simulates the outcome of activities and events when camping (1 per turn/day):
!
! 1. Determines outcomes of activities:
!    a. Uses actor skills to determine the probability of success for
!       different activities (e.g., for foraging).
!    b. Determines if activity was successful.
!    c. If successful, uses actor skills to determine dice to be rolled
!       (i.e. the value range) to determine the extent of the success
!       (e.g., how much food was gained from foraging).
!    d. Update inventory, activity log and event log.
!
! 2. Determines outcome of random events (theft, accidents, etc.)
!    a. Roll for events from a predefined list
!    b. Determine outcome of events; may depend on actor skills.
!    c. Update actors, inventory, activity log and event log as necessary.
!
! in:
! na, ns, ni, ne - no. of actors, skills, inventory items, evcnts
! skills, events - skill and event attributes
!
! inout:
! activity log, event log, actor and inventory (inv)
!
! other:
! i, j, a, b, c - integers for looping and calculations
! p - probability (real)
! e - event occurence (true or false)

! ==== Declarations
  integer(i4)              , intent(in)    :: na, ns, ni, ne
  type(TYP_skill)          , intent(in)    :: skill(ns)
  type(TYP_event)          , intent(in)    :: event(ne)
  type(TYP_actor)          , intent(inout) :: actor(na)
  type(TYP_resource)       , intent(inout) :: inv(ni)
  type(TYP_eventlog)       , intent(inout) :: eventlog(ne)
  type(TYP_actionlog)      , intent(inout) :: actionlog(na)
  integer(i4)                              :: i, j, a, b, c
  real(wp)                                 :: p
  logical                                  :: e

! ==== Instructions

! ---- activities

! action/skill loop
  do j=1,3 ! only forage (1), scout (2) and heal (3)

     ! actor loop
     do i=1,na
        a=0 ! impact reset
        c=0 ! target reset

        ! check if actor tasked with action
        if (actor(i)%action .eq. j) then

           ! get probability of success (max skill=5, max p=0.9) and determine if successful
           p = float(actor(i)%skill(j))/(5.0_wp/0.9_wp)
           call eventBool(p,e)

           ! if determine extent of success/failure
           call eventDice(skill(j)%dice(actor(i)%skill(j),1)&
                &, skill(j)%dice(actor(i)%skill(j),2), a)

           ! determine what was gained or lost (if not 0) and update inventory
           if (a .ne. 0) then
              select case (j)
                 ! forage
                 case (1)
                    ! update food entry in inventory
                    c=1
                    inv(c)%stock=inv(c)%stock+a
                 ! scout
                 case (2)
                   ! determine what was gained and update inventory
                    call eventDice(2,4,c)
                    inv(c)%stock=inv(c)%stock+a
              end select

              ! update activity logs
              actionlog(i)%actor_success=j
              actionlog(i)%actor_target=c
              actionlog(i)%actor_impact=a
           endif
        endif
     enddo
  enddo

! ---- events

! event loop
  do j=1,ne
     e=.false. ! event occurance reset
     a=0       ! impact reset
     c=0       ! target reset

     select case (event(j)%name)

        ! Theft => lose resources. Preventable; relevant skill = 4 (guarding)
        case ("Theft")

           ! calculate pooled guard skill (used as exponent for event p modifier)
           ! if on guard duty, update pooled skill sum
           b = 1 ! min =1
           do i=1,na
              if (actor(i)%action .eq. 4) b=b+actor(i)%skill(4)
           enddo

           ! apply event probability modifier (p approaches 0) and determine outcome
           p = event(j)%p/(2.0_wp**(0.5_wp*(float(b)-1.0_wp)))
           call eventBool(p,e)

           ! determine event impact if needed
           if (e) then

              ! use pooled skill level to determine amount lost; cap at 5 total
              if (b .gt. 5) b=5
              call eventDice(skill(4)%dice(b,1), skill(4)%dice(b,2), a)

              ! determine what was lost
              call eventDice(1,4,c) ! 25% all resources

              ! update what's lost to prevent negatives, update inventory
              if ((inv(c)%stock-a) .le. 0) a=a+(inv(c)%stock-a)
              inv(c)%stock = inv(c)%stock-a

           endif

        ! Storms => lose resources; active scouts lessens impact
        case ("Storm")

           ! determine if event happens
           call eventBool(event(j)%p,e)

           ! determine event impact if needed
           if (e) then

              ! calculate pooled scouting skill of scouts on duty
              b = 1 ! min =1
              do i=1,na
                 if (actor(i)%action .eq. 2) b=b+actor(i)%skill(2)
              enddo

              ! use pooled skill level to determine amount lost; cap at 5 total
              if (b .gt. 5) b=5
              call eventDice(1,20-b, a)

              ! determine what was lost
              call eventDice(1,4,c) ! 25% all resources

              ! update what's lost to prevent negatives, update inventory
              if ((inv(c)%stock-a) .le. 0) a=a+(inv(c)%stock-a)
              inv(c)%stock = inv(c)%stock-a

           endif


        ! Bad weather => health loss; Accident => health loss; active healers lessen impacts
        case ("Weather", "Accident")

           ! determine if event happens
           call eventBool(event(j)%p,e)

           ! determine event impact if needed
           if (e) then

              ! calculate pooled scouting skill of scouts on duty
              b = 1 ! min =1
              do i=1,na
                 if (actor(i)%action .eq. 3) b=b+actor(i)%skill(3)
              enddo

              ! use pooled skill level to determine amount lost; cap at 5 total
              if (b .gt. 5) b=0
              call eventDice(1,20-b, a)

              ! determine who lost health
              call eventDice(1,na,c)

              ! update health lost to prevent negatives, update health
              if (actor(c)%health-a .le. 0) a=a+(actor(c)%health-a)
              actor(c)%health = actor(c)%health-a

           endif

      end select

     ! update event logs
     eventlog(j)%event=e
     eventlog(j)%event_target=c
     eventlog(j)%event_impact=a

  enddo

end subroutine camp


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
  real(wp)                 :: rnd

! ==== Instructions
! (re-)initialise random number generator (optional)
  call random_seed()
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
  real(wp), intent(in)  :: p
  logical , intent(out) :: e
  real(wp)              :: rnd

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
subroutine buildRoster(na0, na, roster, ansi)

! ==== Description
! Procedure for team selection.
! out: na     - number of actors on roster
!      roster - index values for selected actors (actor IDs)

! ==== Declarations
  type(TYP_ansi)          , intent(in)  :: ansi
  integer(i4)             , intent(in)  :: na0
  integer(i4)             , intent(out) :: na
  integer(i4), allocatable, intent(out) :: roster(:)
  integer(i4)                           :: i, j
  logical                               :: next

! ==== Instructions
  na = 3
  allocate(roster(na))

  ! reset roster
  do i=1,na
     roster(i)=0
  enddo

  ! select team
  do i=1,na
     next=.false.
     write(std_o, *) "> select next team member for roster"&
        &, " (max. 3)" // ansi%info // " [type actor id]" // ansi%reset
     do while (.not. next)
        read *, roster(i)
        next=.true.
        ! check if number is valid
        if (roster(i) .le. 0 .or. roster(i) .gt. na0) then
           write(std_o, *) ansi%loss // "Invalid number!" // ansi%reset
           next=.false.
        endif
        ! check if already in roster
        if (i .gt. 1) then
           do j=1,i-1
              if (roster(i) .eq. roster(j)) then
                 write(std_o, *) ansi%loss // "Already selected!" // ansi%reset
                 next=.false.
              endif
           enddo
        endif
     enddo
     write(std_o, *) ansi%gain // "Member accepted!" // ansi%reset
     write(std_o, *)
  enddo


end subroutine buildRoster


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine initansi(ansi)

! ==== Description
!! Create ansi style using derived type TYP_ansi.

! ==== Declarations
  type(TYP_ansi), intent(out) :: ansi

! ==== Instructions

! define colours and styles
  ansi%info    = fg_color_blue
  ansi%heading = fg_color_blue + style_bold
  ansi%gain    = fg_color_green
  ansi%loss    = fg_color_magenta
  ansi%reset   = style_reset

end subroutine initansi

! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine initialise(ne0, event0, na0, actor0, ns0, skill0, ni0, inv0&
                    &, ne, event, na, roster, actor, ns, skill, ni, inv&
                    &, eventlog, actionlog)

! ==== Description
!! Initialisation procedure.
!! Builds key arrays from options and game data.
! TODO: think about implement selecting a subset of events (skills and resources?)

! ==== Declarations
  integer(i4)                     , intent(in)  :: ne0, na0, ns0, ni0
  integer(i4)                     , intent(in)  :: na, ne, ns, ni
  type(TYP_event)                 , intent(in)  :: event0(ne0)
  type(TYP_actor)                 , intent(in)  :: actor0(na0)
  type(TYP_skill)                 , intent(in)  :: skill0(ns0)
  type(TYP_resource)              , intent(in)  :: inv0(ni0)
  integer(i4)                     , intent(in)  :: roster(na)
  type(TYP_actor)    , allocatable, intent(out) :: actor(:)
  type(TYP_event)    , allocatable, intent(out) :: event(:)
  type(TYP_skill)    , allocatable, intent(out) :: skill(:)
  type(TYP_resource) , allocatable, intent(out) :: inv(:)
  type(TYP_eventlog) , allocatable, intent(out) :: eventlog(:)
  type(TYP_actionlog), allocatable, intent(out) :: actionlog(:)
  integer(i4)                                   :: i, j

! ==== Instructions

! init events
  allocate(event(ne))
  event(:)=event0(:)

! init actors (select subset)
  allocate(actor(na))
  do i=1,na
     do j=1,na0
        if (roster(i) .eq. actor0(j)%id) actor(i)=actor0(j)
     enddo
  enddo

! init skills
  allocate(skill(ns))
  skill(:)=skill0(:)

! init inventory
  allocate(inv(ni))
  inv(:)=inv0(:)

! event log
  allocate(eventlog(ne))

! activity log
  allocate(actionlog(na))

end subroutine initialise


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine clear(ne, eventlog, na, actionlog)

! ==== Description
!! clear the camp logs.

! ==== Declarations
  integer(i4)        , intent(in)    :: ne, na
  type(TYP_eventlog) , intent(inout) :: eventlog(ne)
  type(TYP_actionlog), intent(inout) :: actionlog(na)
  integer(i4)                        :: i

! ==== Instructions

! clear events
  do i=1,ne
     eventlog(i)%event=.false.
     eventlog(i)%event_target=0
     eventlog(i)%event_impact=0
  enddo

! clear activities
  do i=1,na
     actionlog(i)%actor_success=0
     actionlog(i)%actor_target=0
     actionlog(i)%actor_impact=0
  enddo

end subroutine clear


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine load(n, inv)

! ==== Description
! Loads game progress (inventory and actors)
! out:   inv - inventory of resources as read from file

! ==== Declarations
  integer(i4)       , intent(in)    :: n
  type(TYP_resource), intent(inout) :: inv(n)
  integer(i4)                       :: i

! ==== Instructions
  open(std_rw, file="sav/inv.sav", action="read")

     ! read inventory
     do i=1,n
        read(std_rw,*) inv(i)%stock
     enddo

    ! TODO: read actor selection and their states (hp, etc.)

  close(std_rw)

end subroutine load


end module forage
