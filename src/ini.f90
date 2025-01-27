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
  public :: io

! ==== Declarations
! game mechanics
  type(TYP_io) :: io

! data
  data io%pUnit/6/, io%wUnit/21/
!
!
end module ini
