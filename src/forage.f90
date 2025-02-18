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
  logical                   :: done=.false.
  integer(i4)               :: code, day
  integer(i4), parameter    :: ne=size(DAT_event), na=size(DAT_actor)&
                            &, ni=size(DAT_inv)  , ns=size(DAT_skill)
  type(TYP_camplog(ne, na)) :: camplog

! display
  type(TYP_ansi) :: ansi

! general
  integer(i4) :: i

! ==== Instructions
! initialisation
  call initialise(ansi)
! clear camping log
  call clear(ne, na, camplog)
! reset days
  day=0

! splash screen and user input
  call start(ansi)
  read *, code

! load game state/update inventory
  call load(ni, DAT_inv)

! determine action
  select case (code)
     case (1); write(std_o, *) "> Loading game ..."
     case (2); write(std_o, *) "> Starting new game ..."
     case (3); done = .true.
  end select

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
           call clear(ne, na, camplog)
           ! camp (compute activities and events)
           call camp(ne, DAT_event, ns, DAT_skill, na, DAT_actor&
             &, ni, DAT_inv, camplog)
           ! view camping log
           call viewLog(ne, DAT_event, ns, DAT_skill, na, DAT_actor&
             &, ni, DAT_inv, day, camplog, ansi)
        ! view camplog
        case (2)
           call viewLog(ne, DAT_event, ns, DAT_skill, na, DAT_actor&
             &, ni, DAT_inv, day, camplog, ansi)
        ! view inventory
        case (3)
           call viewInventory(ni, DAT_inv, ansi)
        ! view team
        case (4)
           call viewTeam(na, DAT_actor, ns, DAT_skill, ansi)
        ! exit
        case (5)
           done = .true.
     end select

  enddo

end subroutine game_loop


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine camp(ne, event, ns, skill, na, actor, ni, inv, camplog)

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
!    d. Update inventory and camplog.
!
! 2. Determines outcome of random events (theft, accidents, etc.)
!    a. Roll for events from a predefined list
!    b. Determine outcome of events; may depend on actor skills.
!    c. Update actors, inventory, and camplog as necessary.
!
! in:
! na, ns, ni, ne - no. of actors, skills, inventory items, evcnts
! skills, events - skill and event attributes
!
! inout:
! camplog, actor and inventory (inv)
!
! other:
! i, j, a, b, c - integers for looping and calculations
! p - probability (real)
! e - event occurence (true or false)

! ==== Declarations
  integer(i4)              , intent(in)    :: na, ns, ni, ne
  type(TYP_skill)          , intent(in)    :: skill(ns)
  type(TYP_event)          , intent(in)    :: event(ne)
  type(TYP_camplog(ne, na)), intent(inout) :: camplog
  type(TYP_actor)          , intent(inout) :: actor(na)
  type(TYP_resource)       , intent(inout) :: inv(ni)
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
           p = float(actor(i)%skill(j))/(5./0.9)
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

              ! update camplog
              camplog%actor_success(i)=j
              camplog%actor_target(i)=c
              camplog%actor_impact(i)=a
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
           p = event(j)%p/(2.0**(0.5*(float(b)-1.0)))
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

     ! update camplog
     camplog%event(j)=e
     camplog%event_target(j)=c
     camplog%event_impact(j)=a

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
subroutine initialise(ansi)

! ==== Description
!! create ansi style using derived type TYP_ansi

! ==== Declarations
  type(TYP_ansi), intent(out) :: ansi

! ==== Instructions

! define colours and styles
  ansi%info    = fg_color_blue
  ansi%heading = fg_color_blue + style_bold
  ansi%gain    = fg_color_green
  ansi%loss    = fg_color_magenta
  ansi%reset   = style_reset

end subroutine


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine clear(ne, na, camplog)

! ==== Description
!! clear the camplog.

! ==== Declarations
  integer(i4)              , intent(in)    :: na, ne
  type(TYP_camplog(ne, na)), intent(inout) :: camplog

! ==== Instructions

! clear events
  camplog%event(:)=.false.
  camplog%event_target(:)=0
  camplog%event_impact(:)=0

! clear activities
  camplog%actor_success(:)=0
  camplog%actor_target(:)=0
  camplog%actor_impact(:)=0

end subroutine


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
