module dsp

! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Display module for Fortran expedition simulator.                   |
! |                                                                    |
! | license : MIT                                                      |
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.com)          |
! |--------------------------------------------------------------------|

! load modules
  use typ
  use stdlib_ansi, only : fg_color_cyan, fg_color_blue, fg_color_magenta&
                       &, fg_color_green, style_bold, style_reset, ansi_code&
                       &, operator(//)

! basic options
  implicit none
  private

! declare public
  public :: start, viewInventory, viewTeam, viewLog

contains

! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine start(ansi)

! ==== Description
! Just a "splash screen"

! ==== Declarations
  type(TYP_ansi), intent(in) :: ansi

! ==== Instructions
! splash
  write(std_o, *) ansi%heading // ""
  write(std_o, *) "///////////////////////////////////"
  write(std_o, *) "// FORage - Expedition Simulator //"
  write(std_o, *) "///////////////////////////////////"
  write(std_o, *) ansi%info // ""
!   write(std_o, *) "1. Continue game."
!   write(std_o, *) "2. New game (overwrites progress)."
!   write(std_o, *) "3. Exit." // ansi%reset

end subroutine start


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine viewInventory(n, inv, ansi)

! ==== Description
! out: eventCode - code for specific events; to betied to procedures.

! ==== Declarations
  integer(i4), intent(in)        :: n
  type(TYP_resource), intent(in) :: inv(n)
  type(TYP_ansi)    , intent(in) :: ansi
  integer(i4)                    :: i

! ==== Instructions
! splash
  write(std_o, *) ansi%heading // ""
  write(std_o, *) "// Inventory //"
  write(std_o, *) "===============" // ansi%info
  do i=1,n
     write(std_o, '(a11,i5)'), inv(i)%name, inv(i)%stock
  enddo
  write(std_o, *) "" // ansi%reset

end subroutine viewInventory


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine viewTeam(na, actor, ns, skill, ansi)

! ==== Description
! out: eventCode - code for specific events; to betied to procedures.

! ==== Declarations
  integer(i4)    , intent(in) :: na, ns
  type(TYP_actor), intent(in) :: actor(na)
  type(TYP_skill), intent(in) :: skill(ns)
  type(TYP_ansi) , intent(in) :: ansi
  integer(i4)                 :: i, j

! ==== Instructions
! splash
  write(std_o, *) ansi%heading // ""
  write(std_o, *) "// Expedition Members //"
  write(std_o, *) "========================"
  do i=1,na
     write(std_o, *) ""
     write(std_o, '(a,a15,a5,i2,a1)'), ansi%info // "", actor(i)%name, "" // ansi%reset
     write(std_o, '(a8,i3,a10,i3)'), "Health: ", actor(i)%health, "Sanity:", actor(i)%sanity
     write(std_o, *) "---"
     do j=1,ns
        write(std_o, '(a8,i2)') skill(j)%name, actor(i)%skill(j)
     enddo
  write(std_o, *) "---"
  enddo
  write(std_o, *) ""

end subroutine viewTeam


! ==================================================================== !
! -------------------------------------------------------------------- !
subroutine viewLog(ne, event, ns, skill, na, actor, ni, inv, day&
        &, eventlog, actionlog, ansi)

! ==== Description
! Generates and displays text from the camplog

! ==== Declarations
  integer(i4)              , intent(in) :: na, ns, ni, ne, day
  type(TYP_skill)          , intent(in) :: skill(ns)
  type(TYP_event)          , intent(in) :: event(ne)
  type(TYP_ansi)           , intent(in) :: ansi
  type(TYP_eventlog)       , intent(in) :: eventlog(ne)
  type(TYP_actionlog)      , intent(in) :: actionlog(na)
  type(TYP_actor)          , intent(in) :: actor(na)
  type(TYP_resource)       , intent(in) :: inv(ni)
  integer(i4)                           :: i
  logical                               :: empty


! ==== Instructions
  write(std_o, *) ansi%heading // ""
  write(std_o, *) "// End-of-Day Results //"
  write(std_o, *) "========================"
  write(std_o, *) "Day: ", day
  write(std_o, *) "" // ansi%reset

  ! render and display activity log
  write(std_o, *) ansi%info // "Activity Log"
  write(std_o, *) "------------" // ansi%reset
  empty=.true.
  do i=1,na
     select case (actionlog(i)%actor_success)
        ! material gain (forage or scout)
        case (1,2)
           empty=.false.
           write(std_o, *) trim(actor(i)%name), " was successful."&
           &, " (",trim(skill(actionlog(i)%actor_success)%name), ")"
           write(std_o, *) ansi%gain // " + "&
           &, trim(inv(actionlog(i)%actor_target)%name), " gained: "&
           & , actionlog(i)%actor_impact, "" // ansi%reset
        ! health gain (heal)
        case (3)
           empty=.false.
           write(std_o, *) trim(actor(i)%name), " was successful."
           write(std_o, *) ansi%gain // " + "&
           &, trim(actor(actionlog(i)%actor_target)%name), " healed: "&
           & , actionlog(i)%actor_impact, "" // ansi%reset
     end select
  enddo
  if (empty) write(std_o, *) "No successful activities."

  ! render and display event log
  write(std_o, *) ansi%info // ""
  write(std_o, *) "Event Log"
  write(std_o, *) "---------" // ansi%reset
  empty=.true.
  do i=1,ne
     if (eventlog(i)%event) then
        select case (event(i)%name)
           ! material loss
           case ("Theft", "Storm")
              empty=.false.
              write(std_o, *) trim(event(i)%text)
              write(std_o, *) ansi%loss // " - "&
              &, trim(inv(eventlog(i)%event_target)%name), " lost: "&
              &, eventlog(i)%event_impact, "" // ansi%reset
           ! health loss
           case ("Weather", "Accident")
              empty=.false.
              write(std_o, *) trim(event(i)%text)
              write(std_o, *) ansi%loss // " - "&
              &, trim(actor(eventlog(i)%event_target)%name), " lost health: "&
              &, eventlog(i)%event_impact, "" // ansi%reset
        end select
     endif
  enddo
  if (empty) write(std_o, *) "It was a quiet, uneventful day."
  write(std_o, *)

end subroutine viewLog


end module dsp
