module typ
!
! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Kinds and derived types for Fortran expedition simulator.          |
! |                                                                    |
! | license : MIT                                                      |
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.com)          |
! |--------------------------------------------------------------------|

! load modules
  use iso_fortran_env, only: int32, int64, real32, real64&
                          &, input_unit, output_unit, error_unit
  use stdlib_ansi, only : ansi_code

! basic options
  implicit none
  private

! declare public
  public :: dp, sp, wp, i4, i8
  public :: std_i, std_o, std_e, std_rw
  public :: TYP_skill, TYP_actor, TYP_resource, TYP_event, TYP_eventlog, TYP_actionlog
  public :: TYP_ansi

! ==== Definitions =================================================== !

! define kinds (used consistently and explicitly in derived types and entire project)
  integer, parameter :: sp = real32  ! single precision
  integer, parameter :: dp = real64  ! double precision
  integer, parameter :: wp = sp      ! working precision
  integer, parameter :: i4 = int32
  integer, parameter :: i8 = int64

  ! standard i/o
  integer, parameter :: std_i  = input_unit
  integer, parameter :: std_o  = output_unit
  integer, parameter :: std_e  = error_unit
  integer, parameter :: std_rw = 21

! skills
  type :: TYP_skill
     !! Derived type for skills.
     !!
     !! id      : skill ID
     !! name    : skill name
     !! dice    : dice number range (min & max) for each of the 5 skill levels
     integer(i4)       :: id
     character(len=20) :: name
     integer(i4)       :: dice(5,2)
  end type TYP_skill

! actor
  type :: TYP_actor
     !! Derived type for actors/playable characters.
     !!
     !! id      : actor ID
     !! name    : actor name
     !! hp, sp  : hit points (health), sanity points (morale) - can be modified
     !! att_*   : character attributes
     !! skill   : levels in each of the 4 different skills
     !! action  : current action index
     integer(i4)       :: id
     character(len=20) :: name
     integer(i4)       :: health, sanity
     integer(i4)       :: att_vitality, att_resilience
     integer(i4)       :: skill(4), action
  end type TYP_actor

! resource
  type :: TYP_resource
     !! Derived type for recources & stock.
     !!
     !! name   : resourcse/item name
     !! stock  : amount of each item/resource
     character(len=10) :: name
     integer(i4)       :: stock
  end type TYP_resource

! event
  type :: TYP_event
     !! Derived type for events.
     !!
     !! name     : event name
     !! type     : event type (guardcheck_stock, scoutcheck_stock, healercheck_health)
     !! text     : short event description
     !! skill    : skill index used for cost calculationg
     !! passfail : true if pass-fail/all-or-nothing situations (e.g., guarding)
     !! p        : probability of event
     character(len=10)  :: name
     character(len=100) :: text
     integer(i4)        :: skill
     logical            :: passfail
     real(wp)           :: p
  end type TYP_event

! event log
  type :: TYP_eventlog
     !! Derived type to store the camping outcomes.
     !!
     !! event         : true = event occured
     !! event_target  : index of affected item or actor
     !! event_impact  : quantitative impact on target (e.g., no of items lost)
     logical    , allocatable :: event
     integer(i4), allocatable :: event_target, event_impact
  end type TYP_eventlog

! activity log
  type :: TYP_actionlog
     !! Derived type to store the camping outcomes.
     !!
     !! actor_success : index of successful action (0 if none/unsuccesfful)
     !! actor_target  : index of affected item or actor
     !! actor impact  : quantitative impact on target (e.g., healed hitpoints)
     integer(i4), allocatable :: actor_success, actor_target, actor_impact
  end type TYP_actionlog

! ansi style set
  type :: TYP_ansi
     !! Derived type for colour/style sets.
     !!
     !! TODO: add RGB as ansi alternative for sdl text render
     type(ansi_code) :: info, heading, gain, loss, reset
  end type TYP_ansi


end module typ
