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
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.eu)           |
! |--------------------------------------------------------------------|

! load modules
  use iso_fortran_env, only: int32, int64, real32, real64, &
                             & input_unit, output_unit, error_unit
  use stdlib_ansi, only : fg_color_cyan, fg_color_blue, fg_color_magenta, fg_color_green, &
                          & style_bold, style_reset, ansi_code, &
                          & operator(//), operator(+)

! basic options
  implicit none
  private

! declare public types/kinds
  public :: dp, sp, wp, i4, i8
  public :: std_i, std_o, std_e, std_rw
  public :: TYP_skill, TYP_actor, TYP_resource, TYP_event, TYP_ansi

! declare public procedures
  public :: initialise

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
     !! Derived type for actors/playable characters.
     !!
     !! id      : actor ID
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
     !! name   : event name
     !! text   : short event description
     !! p      : probability of event
     character(len=10)  :: name
     character(len=100) :: text
     real(wp)           :: p
  end type TYP_event

! ansi style set
  type :: TYP_ansi
     !! Derived type for colour/style sets.
     !!
     !! TODO: add RGB as ansi alternative for sdl text render
     type(ansi_code) :: info, heading, gain, loss, reset
  end type TYP_ansi


contains


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






end module typ
