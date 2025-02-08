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
  public :: actor, skill, rsc

! ==== Declarations

  integer(i4) :: i

! game world
  type(TYP_actor) :: actor(4)
  type(TYP_item)  :: rsc(4)
  type(TYP_skill) :: skill(4)

! ==== Data
! game world
  data rsc(1)%id/1/ &
       &, rsc(1)%name/"Food"/ &
       &, rsc(2)%id/2/ &
       &, rsc(2)%name/"Medicine"/ &
       &, rsc(3)%id/3/ &
       &, rsc(3)%name/"Equipment"/ &
       &, rsc(4)%id/4/ &
       &, rsc(4)%name/"Treasure"/

  data skill(1)%id/1/, skill(1)%name/"Forage"/ &
       &, skill(1)%dice(1,1)/1/, skill(1)%dice(1,2)/4/ &
       &, skill(1)%dice(2,1)/1/, skill(1)%dice(2,2)/4/ &
       &, skill(1)%dice(3,1)/1/, skill(1)%dice(3,2)/6/ &
       &, skill(1)%dice(4,1)/1/, skill(1)%dice(4,2)/6/ &
       &, skill(1)%dice(4,1)/1/, skill(1)%dice(4,2)/12/ &
       &, skill(2)%id/2/, skill(2)%name/"Scout"/ &
       &, skill(2)%dice(1,1)/1/, skill(2)%dice(1,2)/6/ &
       &, skill(2)%dice(2,1)/1/, skill(2)%dice(2,2)/6/ &
       &, skill(2)%dice(3,1)/1/, skill(2)%dice(3,2)/12/ &
       &, skill(2)%dice(4,1)/1/, skill(2)%dice(4,2)/12/ &
       &, skill(2)%dice(4,1)/1/, skill(2)%dice(4,2)/20/ &
       &  skill(3)%id/3/, skill(3)%name/"Guard"/ &
       &, skill(3)%dice(1,1)/1/, skill(3)%dice(1,2)/20/ &
       &, skill(3)%dice(2,1)/1/, skill(3)%dice(2,2)/20/ &
       &, skill(3)%dice(3,1)/1/, skill(3)%dice(3,2)/12/ &
       &, skill(3)%dice(4,1)/1/, skill(3)%dice(4,2)/12/ &
       &, skill(3)%dice(4,1)/1/, skill(3)%dice(4,2)/6/ &
       &, skill(4)%id/4/, skill(4)%name/"Heal"/ &
       &, skill(4)%dice(1,1)/1/, skill(4)%dice(1,2)/4/ &
       &, skill(4)%dice(2,1)/1/, skill(4)%dice(2,2)/6/ &
       &, skill(4)%dice(3,1)/1/, skill(4)%dice(3,2)/8/ &
       &, skill(4)%dice(4,1)/1/, skill(4)%dice(4,2)/12/ &
       &, skill(4)%dice(4,1)/1/, skill(4)%dice(4,2)/20/

  data actor(1)%id/1/             , actor(1)%name/"Holde Knirbe"/ &
       &, actor(1)%health/100/    , actor(1)%sanity/100/ &
       &, actor(1)%att_vitality/5/, actor(1)%att_resilience/7/ &
       &, actor(1)%skill(1)/5/    , actor(1)%skill(2)/4/ &
       &, actor(1)%skill(3)/1/    , actor(1)%skill(4)/1/ &
       &, actor(1)%action/1/ &
       &, actor(2)%id/2/          , actor(2)%name/"Rough George"/ &
       &, actor(2)%health/100/    , actor(2)%sanity/100/ &
       &, actor(2)%att_vitality/8/, actor(2)%att_resilience/7/ &
       &, actor(2)%skill(1)/2/    , actor(2)%skill(2)/2/ &
       &, actor(2)%skill(3)/5/    , actor(2)%skill(4)/1/ &
       &, actor(2)%action/3/ &
       &, actor(3)%id/3/          , actor(3)%name/"El Capitan"/ &
       &, actor(3)%health/100/    , actor(3)%sanity/100/ &
       &, actor(3)%att_vitality/5/, actor(3)%att_resilience/5/ &
       &, actor(3)%skill(1)/3/    , actor(3)%skill(2)/4/ &
       &, actor(3)%skill(3)/1/    , actor(3)%skill(4)/2/ &
       &, actor(3)%action/2/ &
       &, actor(4)%id/4/          , actor(4)%name/"Oldboi Olaf"/ &
       &, actor(4)%health/100/    , actor(4)%sanity/100/ &
       &, actor(4)%att_vitality/5/, actor(4)%att_resilience/9/ &
       &, actor(4)%skill(1)/3/    , actor(4)%skill(2)/2/ &
       &, actor(4)%skill(3)/1/    , actor(4)%skill(4)/5/ &
       &, actor(4)%action/1/

end module data
