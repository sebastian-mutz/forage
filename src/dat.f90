module dat
!
! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Data module; defines some key game data.                           |
! |                                                                    |
! | license : MIT                                                      |
! | author  : Sebastian G. Mutz (sebastian@sebastianmutz.com)          |
! |--------------------------------------------------------------------|
!
! load modules
  use typ

! basic options
  implicit none
  private

! declare public
  public :: DAT_actor, DAT_skill, DAT_inv, DAT_event

! ==== Declarations

  integer(i4) :: i

! game world
  type(TYP_resource) :: DAT_inv(4)
  type(TYP_event)    :: DAT_event(4)
  type(TYP_actor)    :: DAT_actor(4)
  type(TYP_skill)    :: DAT_skill(4)

! ==== Data
! display

! game world
  data DAT_inv(1)%name/"Food"/ &
       &, DAT_inv(1)%stock/0/ &
       &, DAT_inv(2)%name/"Medicine"/ &
       &, DAT_inv(2)%stock/0/ &
       &, DAT_inv(3)%name/"Equipment"/ &
       &, DAT_inv(3)%stock/0/ &
       &, DAT_inv(4)%name/"Treasure"/ &
       &, DAT_inv(4)%stock/0/

  data DAT_event(1)%name/"Theft"/ &
       &, DAT_event(1)%text/"Thieves snuck past your guards."/ &
       &, DAT_event(1)%p/0.8/ &
       &, DAT_event(2)%name/"Storm"/ &
       &, DAT_event(2)%text/"A big storm ravaged your camp and damaged some goods."/ &
       &, DAT_event(2)%p/0.1/ &
       &, DAT_event(3)%name/"Weather"/ &
       &, DAT_event(3)%text/"The weather was particularly bad. One of your team members is sick."/ &
       &, DAT_event(3)%p/0.1/ &
       &, DAT_event(4)%name/"Accident"/ &
       &, DAT_event(4)%text/"One of your team members tripped and fell. Ouch!"/ &
       &, DAT_event(4)%p/0.1/

  data DAT_skill(1)%name/"Forage"/ &
       &, DAT_skill(1)%dice(1,1)/1/, DAT_skill(1)%dice(1,2)/4/ &
       &, DAT_skill(1)%dice(2,1)/1/, DAT_skill(1)%dice(2,2)/4/ &
       &, DAT_skill(1)%dice(3,1)/1/, DAT_skill(1)%dice(3,2)/6/ &
       &, DAT_skill(1)%dice(4,1)/1/, DAT_skill(1)%dice(4,2)/6/ &
       &, DAT_skill(1)%dice(5,1)/1/, DAT_skill(1)%dice(5,2)/12/ &
       &, DAT_skill(2)%name/"Scout"/ &
       &, DAT_skill(2)%dice(1,1)/1/, DAT_skill(2)%dice(1,2)/6/ &
       &, DAT_skill(2)%dice(2,1)/1/, DAT_skill(2)%dice(2,2)/6/ &
       &, DAT_skill(2)%dice(3,1)/1/, DAT_skill(2)%dice(3,2)/12/ &
       &, DAT_skill(2)%dice(4,1)/1/, DAT_skill(2)%dice(4,2)/12/ &
       &, DAT_skill(2)%dice(5,1)/1/, DAT_skill(2)%dice(5,2)/20/ &
       &, DAT_skill(3)%name/"Heal"/ &
       &, DAT_skill(3)%dice(1,1)/1/, DAT_skill(3)%dice(1,2)/4/ &
       &, DAT_skill(3)%dice(2,1)/1/, DAT_skill(3)%dice(2,2)/6/ &
       &, DAT_skill(3)%dice(3,1)/1/, DAT_skill(3)%dice(3,2)/8/ &
       &, DAT_skill(3)%dice(4,1)/1/, DAT_skill(3)%dice(4,2)/12/ &
       &, DAT_skill(3)%dice(5,1)/1/, DAT_skill(3)%dice(5,2)/20/ &
       &  DAT_skill(4)%name/"Guard"/ &
       &, DAT_skill(4)%dice(1,1)/1/, DAT_skill(4)%dice(1,2)/20/ &
       &, DAT_skill(4)%dice(2,1)/1/, DAT_skill(4)%dice(2,2)/20/ &
       &, DAT_skill(4)%dice(3,1)/1/, DAT_skill(4)%dice(3,2)/12/ &
       &, DAT_skill(4)%dice(4,1)/1/, DAT_skill(4)%dice(4,2)/12/ &
       &, DAT_skill(4)%dice(5,1)/1/, DAT_skill(4)%dice(5,2)/6/

  data DAT_actor(1)%id/1/             , DAT_actor(1)%name/"Holde Knirbe"/ &
       &, DAT_actor(1)%health/100/    , DAT_actor(1)%sanity/100/ &
       &, DAT_actor(1)%att_vitality/5/, DAT_actor(1)%att_resilience/7/ &
       &, DAT_actor(1)%skill(1)/5/    , DAT_actor(1)%skill(2)/4/ &
       &, DAT_actor(1)%skill(3)/1/    , DAT_actor(1)%skill(4)/1/ &
       &, DAT_actor(1)%action/1/ &
       &, DAT_actor(2)%id/2/          , DAT_actor(2)%name/"Rough George"/ &
       &, DAT_actor(2)%health/100/    , DAT_actor(2)%sanity/100/ &
       &, DAT_actor(2)%att_vitality/8/, DAT_actor(2)%att_resilience/7/ &
       &, DAT_actor(2)%skill(1)/2/    , DAT_actor(2)%skill(2)/2/ &
       &, DAT_actor(2)%skill(3)/1/    , DAT_actor(2)%skill(4)/4/ &
       &, DAT_actor(2)%action/4/ &
       &, DAT_actor(3)%id/3/          , DAT_actor(3)%name/"El Capitan"/ &
       &, DAT_actor(3)%health/100/    , DAT_actor(3)%sanity/100/ &
       &, DAT_actor(3)%att_vitality/5/, DAT_actor(3)%att_resilience/5/ &
       &, DAT_actor(3)%skill(1)/3/    , DAT_actor(3)%skill(2)/4/ &
       &, DAT_actor(3)%skill(3)/2/    , DAT_actor(3)%skill(4)/1/ &
       &, DAT_actor(3)%action/2/ &
       &, DAT_actor(4)%id/4/          , DAT_actor(4)%name/"Oldboi Olaf"/ &
       &, DAT_actor(4)%health/100/    , DAT_actor(4)%sanity/100/ &
       &, DAT_actor(4)%att_vitality/5/, DAT_actor(4)%att_resilience/9/ &
       &, DAT_actor(4)%skill(1)/3/    , DAT_actor(4)%skill(2)/2/ &
       &, DAT_actor(4)%skill(3)/5/    , DAT_actor(4)%skill(4)/1/ &
       &, DAT_actor(4)%action/1/

end module dat
