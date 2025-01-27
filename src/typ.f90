module typ
!
! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Derived typesfor Fortran expedition simulator.                     |
! |                                                                    |
! | license                                                            |
! | -------                                                            |
! | This code is released under the MIT license.                       |
! |                                                                    |
! | author                                                             |
! | ------                                                             |
! | Sebastian G. Mutz (sebastian@sebastianmutz.eu)                     |
! |--------------------------------------------------------------------|

! basic options
  implicit none
  private

! declare public
  public :: TYP_actor, TYP_io

! ==== Definitions =================================================== !

! actor
  type :: TYP_actor
     !! Derived type for actors/playable characters.
     !!
     !! id      : actor ID
     !! name    : actor name
     !! hp, sp  : hit points (health), sanity points (morale) - can be modified
     !! att_*   : character attributes
     !! skill_* : skill level in different aspects
     !! can_*   : is the character currently able to do this
     integer(kind=4)   :: id
     character(len=20) :: name
     integer(kind=4)   :: hp, sp
     integer(kind=4)   :: att_vitality, att_resilience
     integer(kind=4)   :: skill_forage, skill_scout, skill_guard, skill_heal
     logical           :: can_forage, can_scout, can_guard, can_heal, can_chill, can_explore
  end type TYP_actor

! input/output
  type :: TYP_io
     !! Derived type for input/output.
     !!
     !! pUnit : unit for printing messages on screen, typically 6
     !! wUnit : unit for writing output, e.g. 21
     integer(kind=4) :: pUnit, wUnit
  end type TYP_io

end module typ
