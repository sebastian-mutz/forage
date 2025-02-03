program main

! |--------------------------------------------------------------------|
! | Forage: Expedition Simulator                                       |
! |                                                                    |
! | about                                                              |
! | -----                                                              |
! | Main procedures (game loop) for Fortran expedition simulator.      |
! |                                                                    |
! | license: MIT                                                       |
! | author:  Sebastian G. Mutz (sebastian@sebastianmutz.eu)            |
! |--------------------------------------------------------------------|

! load modules
  use :: typ
  use :: forage

! basic options
  implicit none

! ==== Declarations

! ==== Instructions
! game loop
  call game_loop()

end program main
