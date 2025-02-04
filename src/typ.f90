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
  public :: TYP_actor, TYP_io, TYP_item, TYP_inventory

! ==== Definitions =================================================== !

! define kinds (used consistently and explicitly in derived types and entire project)
 integer, parameter :: dp=selected_real_kind(15,307), sp=selected_real_kind(6,37) &
                    &, i4=selected_int_kind(9),i8=selected_int_kind(18)

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
     integer(i4)       :: id
     character(len=20) :: name
     integer(i4)       :: hp, sp
     integer(i4)       :: att_vitality, att_resilience
     integer(i4)       :: skill_forage, skill_scout, skill_guard, skill_heal
     logical           :: can_forage, can_scout, can_guard, can_heal, can_chill, can_explore
  end type TYP_actor

! items/resources
  type :: TYP_item
     !! Derived type for items/resources.
     !!
     !! id   : item/resource ID
     !! name : item/resource name
     integer(i4)       :: id
     character(len=20) :: name
  end type TYP_item

! inventory
  type :: TYP_inventory
     !! Derived type for inventory.
     !!
     !! n      : number of item/resource types in inventory
     !! id     : item/resource ID (n)
     !! stock  : amount of each item/resource (n)
     integer(i4)              :: n
     integer(i4), allocatable :: id(:), stock(:)
  end type TYP_inventory

! input/output
  type :: TYP_io
     !! Derived type for input/output.
     !!
     !! pUnit : unit for printing messages on screen, typically 6
     !! wUnit : unit for writing output, e.g. 21
     integer(i4) :: pUnit, wUnit
  end type TYP_io

end module typ
