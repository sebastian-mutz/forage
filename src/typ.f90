module typ
  implicit none
  private

  public :: TYP_actor

  type :: TYP_actor
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
  end type  
  
end module typ
