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

! basic options
  implicit none
  private

! declare public
  public :: dp, sp, i4, i8
  public :: TYP_skill, TYP_actor, TYP_resource, TYP_event, TYP_io!, TYP_col

! ==== Definitions =================================================== !

! define kinds (used consistently and explicitly in derived types and entire project)
 integer, parameter :: dp=selected_real_kind(15,307), sp=selected_real_kind(6,37) &
                    &, i4=selected_int_kind(9),i8=selected_int_kind(18)

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
     real(sp)           :: p
  end type TYP_event

! input/output
  type :: TYP_io
     !! Derived type for input/output.
     !!
     !! pUnit : unit for printing messages on screen, typically 6
     !! wUnit : unit for writing output, e.g. 21
     integer(i4) :: pUnit, wUnit
  end type TYP_io

! input/output
!   type :: TYP_col
!      !! Derived type for colours.
!      !!
!      !! name : name of function of colour (e.g., warning), not name of actual colour
!      !! ansi : ansi colours
!      !! TODO: add RGB as ansi alternative for sdl text render
!      character(len=10) :: name
!      character(len=5)  :: ansi
!   end type TYP_col

end module typ
