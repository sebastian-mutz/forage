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
!
  implicit none
  private

  public :: io
!
!
! ==== Declarations
!
! ---- game mechanics
  type(TYP_io) :: io
!
! ---- data
  data io%pUnit/6/, io%wUnit/21/
!
!
end module ini
