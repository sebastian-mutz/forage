program main

! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Main procedures (game loop) for Fortran expedition simulator.      |
! |                                                                    |
! | license                                                            |
! | -------                                                            |
! | This code is released under the MIT license.                       |
! |                                                                    |
! | author                                                             |
! | ------                                                             |
! | Sebastian G. Mutz (sebastian@sebastianmutz.eu)                     |
! |--------------------------------------------------------------------|

! load modules
  use :: typ
  use :: forage

! basic options
  implicit none

! ==== Declarations
! game world
  integer(kind=4), parameter :: nAct=2
  type(TYP_actor)            :: actor(nAct)

! data
  data actor(1)%id/1/ &
       &, actor(1)%name/"Holde Knirbe"/ &
       &, actor(1)%hp/100/, actor(1)%sp/100/ &
       &, actor(1)%att_vitality/5/, actor(1)%att_resilience/7/ &
       &, actor(1)%skill_forage/8/, actor(1)%skill_scout/7/ &
       &, actor(1)%skill_guard/2/, actor(1)%skill_heal/2/ &
       &, actor(1)%can_forage/.true./, actor(1)%can_scout/.true./ &
       &, actor(1)%can_guard/.true./, actor(1)%can_heal/.true./ &
       &, actor(1)%can_chill/.true./, actor(1)%can_explore/.true./

! ==== Instructions
! game loop
  call game_loop(nAct, actor)

end program main
