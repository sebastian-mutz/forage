module ini
!
! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Initialisation module for Fortran expedition simulator.            |
! |                                                                    |
! | license                                                            |
! | -------                                                            |
! | This code is released under the MIT license.                       |
! |                                                                    |
! | author                                                             |
! | ------                                                             |
! | Sebastian G. Mutz (sebastian@sebastianmutz.eu)                     |
! |--------------------------------------------------------------------|
!
! load modules
  use :: typ

! basic options
  implicit none
  private

! declare public
  public :: io, actor

! ==== Declarations
! game mechanics
  type(TYP_io) :: io

! game world
  type(TYP_actor) :: actor(1)

! ==== Data
! game mechanics
  data io%pUnit/6/, io%wUnit/21/

! game world
  data actor(1)%id/1/ &
       &, actor(1)%name/"Holde Knirbe"/ &
       &, actor(1)%hp/100/, actor(1)%sp/100/ &
       &, actor(1)%att_vitality/5/, actor(1)%att_resilience/7/ &
       &, actor(1)%skill_forage/8/, actor(1)%skill_scout/7/ &
       &, actor(1)%skill_guard/2/, actor(1)%skill_heal/2/ &
       &, actor(1)%can_forage/.true./, actor(1)%can_scout/.true./ &
       &, actor(1)%can_guard/.true./, actor(1)%can_heal/.true./ &
       &, actor(1)%can_chill/.true./, actor(1)%can_explore/.true./

end module ini
