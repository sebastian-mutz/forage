module data
!
! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Data module; defines some key game data.                           |
! |                                                                    |
! | license : MIT                                                      |
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.eu)           |
! |--------------------------------------------------------------------|
!
! load modules
  use :: typ

! basic options
  implicit none
  private

! declare public
  public :: actor, rsc

! ==== Declarations

! game world
  type(TYP_actor) :: actor(2)
  type(TYP_item)  :: rsc(4)

! ==== Data
! game world
  data rsc(1)%id/1/ &
       &, rsc(1)%name/"Food"/ &
       &, rsc(2)%id/2/ &
       &, rsc(2)%name/"Medicine"/ &
       &, rsc(3)%id/3/ &
       &, rsc(3)%name/"Equipment"/ &
       &, rsc(4)%id/3/ &
       &, rsc(4)%name/"Treasure"/

  data actor(1)%id/1/ &
       &, actor(1)%name/"Holde Knirbe"/ &
       &, actor(1)%hp/100/, actor(1)%sp/100/ &
       &, actor(1)%att_vitality/5/, actor(1)%att_resilience/7/ &
       &, actor(1)%skill_forage/8/, actor(1)%skill_scout/7/ &
       &, actor(1)%skill_guard/2/, actor(1)%skill_heal/2/ &
       &, actor(1)%can_forage/.true./, actor(1)%can_scout/.true./ &
       &, actor(1)%can_guard/.true./, actor(1)%can_heal/.true./ &
       &, actor(1)%can_chill/.true./, actor(1)%can_explore/.true./ &
       &, actor(2)%id/2/ &
       &, actor(2)%name/"Rough George"/ &
       &, actor(2)%hp/100/, actor(1)%sp/100/ &
       &, actor(2)%att_vitality/8/, actor(2)%att_resilience/7/ &
       &, actor(2)%skill_forage/3/, actor(2)%skill_scout/3/ &
       &, actor(2)%skill_guard/8/, actor(2)%skill_heal/2/ &
       &, actor(2)%can_forage/.true./, actor(2)%can_scout/.true./ &
       &, actor(2)%can_guard/.true./, actor(2)%can_heal/.true./ &
       &, actor(2)%can_chill/.true./, actor(2)%can_explore/.true./


end module data
